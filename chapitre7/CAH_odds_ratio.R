library(dplyr)
library(tidyr)
library(sf)
#######Table principal#####


setwd("~/Shapes/IRIS")
list.files()

IrisInsee<- st_read("IRIS_LAMB93_IDF_2008.shp",
                    stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
IrisInsee  <-  st_transform(IrisInsee , crs = 2154)
# "simplifier geometry"
IrisInsee  <- st_buffer(IrisInsee, 0)
# plot(st_geometry(IrisInsee), col = "grey90", lwd = 0.2)

#obtenir shape commune par aggregation 
CommunesInsee<- IrisInsee%>% 
  group_by(DepCom)%>%
  summarize()



#departement
setwd("~/Shapes/shpIDF_dep_lamb93")
list.files()
DepInsee<- st_read("ile-de-france.shp",
                   stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
DepInsee <-  st_transform(DepInsee, crs = 2154)
# plot(st_geometry(DepInsee), col = , lwd = 0.2)
 # data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest_BIEN<-data_redresse1570533transacs[,c("Periode","CSP_AC", "X.x", "Y.x")]


mytest_BIEN$CSP_AC<- ifelse(mytest_BIEN$CSP_AC == 10 , "Agriculteurs_AC",
                            ifelse(mytest_BIEN$CSP_AC >= 20 & mytest_BIEN$CSP_AC < 30, "ComArtCEts_AC",
                                   ifelse(mytest_BIEN$CSP_AC >= 30 & mytest_BIEN$CSP_AC < 40, "CPIS_AC",
                                          ifelse(mytest_BIEN$CSP_AC >= 40 & mytest_BIEN$CSP_AC < 50, "Prof_intermediaires_AC",
                                                 ifelse(mytest_BIEN$CSP_AC >= 50 & mytest_BIEN$CSP_AC < 60, "Employes_AC",
                                                        ifelse(mytest_BIEN$CSP_AC >= 60 & mytest_BIEN$CSP_AC < 70, "Ouvriers_AC",
                                                               ifelse(mytest_BIEN$CSP_AC >= 70 & mytest_BIEN$CSP_AC < 80, "retraite_AC",
                                                                      ifelse(mytest_BIEN$CSP_AC == 80, "autres_inactifs_AC", "non_rs_AC" ))))))))

mytest_BIEN$Periode<-ifelse(mytest_BIEN$Periode==1996|mytest_BIEN$Periode==1999|mytest_BIEN$Periode==2003, "Periode_96_2003",
                            ifelse(mytest_BIEN$Periode>=2004&mytest_BIEN$Periode<=2007, "Periode_04_2007",
                                   ifelse(mytest_BIEN$Periode>=2008&mytest_BIEN$Periode<=2012, "Periode_08_2012", NA)))


#######################


spVente <- st_as_sf(mytest_BIEN,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)



#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]
spVente$Ylamb93<-coords[,2]


Communes_jointure<-st_join(CommunesInsee, spVente, join = st_contains, left=T)
mytest_BIEN<- as.data.frame(Communes_jointure)

rm(Communes_jointure)
##########

# Years<- c(1999,2008,2012)
Acquereurs<- c("CPIS_AC", "Prof_intermediaires_AC","Employes_AC", "Ouvriers_AC")
mytest<- mytest_BIEN%>%
  filter(CSP_AC)
  group_by(DepCom,Periode, CSP_AC)%>%
  filter(CSP_AC %in% Acquereurs)%>%
  summarise(n_acq=n())%>%
  ungroup() %>%
  spread(CSP_AC,n_acq,fill = 0)
# rm(mytest_BIEN)

mytest$total_AC<- rowSums(mytest[,c(3:8)])

mytest<-mytest%>%
  group_by(DepCom)%>%
  filter(Periode>1996, total_AC>=5)


Communes_maisons<- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_533communes_Maisons", stringsAsFactors=FALSE)
Communes_appartements<-read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_communes_appartements", stringsAsFactors=FALSE)
tableauID<-full_join(Communes_appartements,Communes_maisons, by="DepCom")
rm(Communes_appartements,Communes_maisons)
tableauID<-tableauID[,c(1,2)]
mytest<-right_join(mytest,tableauID)
mytest<-mytest[,-c(10)]

mytest<-as.data.frame(mytest) %>% group_by(DepCom,Periode) %>% mutate_at(Acquereurs, funs(./total_AC*100))
################################################


CSP_residentsCommunes_IDF_990812 <- read.csv2("~/Projets/Chapitre7/CAH_acquereur_residents/CSP_residentsCommunes_IDF_990812.csv", stringsAsFactors=FALSE)
CSP_residentsCommunes_IDF_990812$Periode<-ifelse(CSP_residentsCommunes_IDF_990812$annee==1999, "Periode_96_2003",
                                                 ifelse(CSP_residentsCommunes_IDF_990812$annee==2008, "Periode_04_2007",
                                                        ifelse(CSP_residentsCommunes_IDF_990812$annee==2012, "Periode_08_2012", NA)))

test_residents<-CSP_residentsCommunes_IDF_990812%>%
  select(CODGEO, annee,everything())
Residents<- c("CPIS","Prof_intermediaires","Employes","Ouvriers")


test_residents$Agriculteurs<-as.numeric(test_residents$Agriculteurs)
test_residents$ComArtCEts<-as.numeric(test_residents$Liberales)
test_residents$CPIS<-as.numeric(test_residents$CPIS)
test_residents$Prof_intermediaires<-as.numeric(test_residents$Prof_intermediaires)
test_residents$Employes<-as.numeric(test_residents$Employes)
test_residents$Ouvriers<-as.numeric(test_residents$Ouvriers)
test_residents<-test_residents%>% select(-Liberales, -Actifs)

test_residents$total_residents<- rowSums(test_residents[,c(3:8)])

test_residents<-gather(test_residents, "variables", "Values", 3:ncol(test_residents))%>%
  spread(annee,Values)

test_residents<- test_residents%>% group_by(variables)%>%
  mutate(TCAM_9908=(((`2008`/`1999`)^(1/9)-(1))),
         TCAM_9913=(((`2012`/`2008`)^(1/4)-(1))))

test_residents$`2000`<-test_residents$`1999`*test_residents$TCAM_9908 + test_residents$`1999`
test_residents$`2001`<-test_residents$`2000`*test_residents$TCAM_9908+ test_residents$`2000`
test_residents$`2002`<-test_residents$`2001`*test_residents$TCAM_9908+ test_residents$`2001`
test_residents$`2003`<-test_residents$`2002`*test_residents$TCAM_9908+ test_residents$`2002`
test_residents$`2004`<-test_residents$`2003`*test_residents$TCAM_9908+ test_residents$`2003`
test_residents$`2005`<-test_residents$`2004`*test_residents$TCAM_9908+ test_residents$`2004`
test_residents$`2006`<-test_residents$`2005`*test_residents$TCAM_9908+ test_residents$`2005`
test_residents$`2007`<-test_residents$`2006`*test_residents$TCAM_9908+ test_residents$`2006`

test_residents$`2009`<-  test_residents$`2008`*test_residents$TCAM_9913+test_residents$`2008`
test_residents$`2010`<-  test_residents$`2009`*test_residents$TCAM_9913+test_residents$`2009`
test_residents$`2011`<-  test_residents$`2010`*test_residents$TCAM_9913+test_residents$`2010`
test_residents$`2012`<-  test_residents$`2011`*test_residents$TCAM_9913+test_residents$`2011`


#########
test_residents_solde<- test_residents%>% group_by(variables)%>%
  mutate(Solde_Residents = `2012`/`1999`)
########


test_residents<-test_residents[,c(1:5, 10:ncol(test_residents))]

test_residents<- test_residents%>%
  gather(annee, "Values", 3: ncol(.))%>%
  spread(variables, Values, fill= 0)
test_residents<-as.data.frame(test_residents) %>% 
  group_by(CODGEO,annee) %>%
  mutate_at(Residents, funs(./total_residents*100))
###
Mytest_Poids<- left_join(mytest[,c("DepCom","Periode","total_AC")], test_residents[,c("CODGEO","annee","total_residents")],by= c("DepCom"="CODGEO", "Periode"= "annee"))
Mytest_Poids$Poids_Marche<-(Mytest_Poids$total_AC/Mytest_Poids$total_residents)*100
Mytest_Poids<- left_join(Mytest_Poids, test_residents_solde[,c("CODGEO","variables","Solde_Residents")]%>%
                           group_by(CODGEO)%>%
                           mutate(variables = paste0(variables,"Solde"))%>%
                           spread(variables, Solde_Residents)%>%
                           select(-AgriculteursSolde, -total_residentsSolde), by = c("DepCom"="CODGEO"))

Poids_mean_med<- Mytest_Poids%>%
  filter(DepCom!=77166)%>%
  group_by(Periode)%>%
  summarise(Moyenne= mean(Poids_Marche, na.rm=T),
            Mediane= median(Poids_Marche, na.rm=T),
            Somme_moy= sum(mean(Poids_Marche, na.rm=T)))


Poids_total<-Mytest_Poids%>%
  filter(DepCom!=77166)%>%
  group_by(DepCom,Periode)%>%
  summarise(Somme_moy= mean(Poids_Marche, na.rm=T))%>%
  spread(Periode, Somme_moy)
Poids_total$total<-rowSums(Poids_total[,c(2:12)])
mean(Poids_total$total)
  ggplot(Poids_total,aes(x = total) ) +
  geom_histogram()+
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = c(5,60))+
  theme(legend.position='none') +
  geom_vline(data= Poids_total, aes(xintercept=mean(total,na.rm=T)),
             color="blue", linetype="dashed", size=1)+
  geom_vline(data=Poids_total, aes(xintercept=median(total, na.rm=T)),
             color="red", linetype="dashed", size=1)


Poids_Marche.plot<-
  Mytest_Poids%>%
  filter(DepCom!=77166)%>%
  ggplot(.,aes(x = Poids_Marche) ) +
  geom_histogram()+
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = c(0,5))+
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~Periode) +
  geom_vline(data= Poids_mean_med, aes(xintercept=Moyenne),
             color="blue", linetype="dashed", size=1)+
  geom_vline(data=Poids_mean_med, aes(xintercept=Mediane),
             color="red", linetype="dashed", size=1)

Poids_Marche.plot
####
mytest$DepCom<-as.numeric(mytest$DepCom)
test_residents$CODGEO<-as.numeric(test_residents$CODGEO)
test_residents$annee<-as.numeric(test_residents$annee)
mytest_ODDS<- left_join(mytest,test_residents, by= c("DepCom"="CODGEO", "Periode"= "annee"))%>%
  filter(!is.na(total_residents) & DepCom!=77166 & total_AC>10)


oddCPIS<-as.data.frame((mytest_ODDS$CPIS_AC/(mytest_ODDS$CPIS_AC-100))/ (mytest_ODDS$CPIS/(mytest_ODDS$CPIS-100)))
names(oddCPIS)[1]<-"oddCPIS"
oddComArtCEts<-as.data.frame((mytest_ODDS$ComArtCEts_AC/(mytest_ODDS$ComArtCEts_AC-100))/ (mytest_ODDS$ComArtCEts/(mytest_ODDS$ComArtCEts-100)))
names(oddComArtCEts)[1]<-"oddComArtCEts"
oddProf_inter<-as.data.frame((mytest_ODDS$Prof_intermediaires_AC/(mytest_ODDS$Prof_intermediaires_AC-100))/ (mytest_ODDS$Prof_intermediaires/(mytest_ODDS$Prof_intermediaires-100)))
names(oddProf_inter)[1]<-"oddProf_inter"
oddEmployes<-as.data.frame((mytest_ODDS$Employes_AC/(mytest_ODDS$Employes_AC-100))/(mytest_ODDS$Employes/(mytest_ODDS$Employes-100)))
names(oddEmployes)[1]<-"oddEmployes"
oddOuvriers<-as.data.frame((mytest_ODDS$Ouvriers_AC/(mytest_ODDS$Ouvriers_AC-100))/(mytest_ODDS$Ouvriers/(mytest_ODDS$Ouvriers-100)))
names(oddOuvriers)[1]<-"oddOuvriers"

mytest_ODDS<-cbind.data.frame(mytest_ODDS,oddCPIS,oddProf_inter,oddEmployes,oddOuvriers)

####CAH sur les odds ratio#

myDF <- subset(mytest_ODDS[,c(17:21)])
#Sans CSP2
myDF <- subset(mytest_ODDS[,c(17,19:21)])

str(myDF)

library(ade4)
library(FactoClass)
AFC <- dudi.coa(df=myDF, scannf=FALSE, nf=ncol(myDF))
plot.dudi(AFC)



distMat <- dist.dudi(AFC, amongrow=TRUE)


CAH <- ward.cluster(distMat, peso = apply(X=myDF, MARGIN=1, FUN=sum) , plots = TRUE, h.clust = 1)

pdf(file=paste0("~/Projets/Chapitre7/CAH_acquereur_residents/Odd_ratio/CAHOddratio_inertie.pdf"),width = 10, height = 5)

par(mfrow=c(1,2))
barplot(sort(CAH$height / sum(CAH$height), decreasing = TRUE)[1:15] * 100,
        xlab = "Noeuds", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie selon le partitionnement")

barplot(cumsum(sort(CAH$height / sum(CAH$height), decreasing = TRUE))[1:15] * 100,
        xlab = "Nombre de classes", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie expliquée")
dev.off()

myDF$clusters <- cutree(tree = CAH, k = 6)


mytest_ODDS<-cbind(mytest_ODDS,myDF[,c(5)] )
names(mytest_ODDS)[27]<-"clusters"

mytest_ODDS<-mytest_ODDS[,c("DepCom","Periode","oddCPIS","oddProf_inter","oddEmployes","oddOuvriers","clusters")]

specificCol <- c("1"="#F4A6D7",
  "2"= "#C5E86C",
  "3"="#5FAD41",
"4"= "#067BC2",
"5"= "#FF8811",
"6"="#FF1D15")

scale_color_manual(values=myColors)

pdf(file=paste0("~/Projets/Chapitre7/CAH_acquereur_residents/Odd_ratio/CAHOddratio_ValeurAxes.pdf"),width = 10, height = 5)

mytest_ODDS %>%
  group_by(clusters) %>%
  # summarise_all(funs(mean),na.rm=T) %>%
  summarise_at(.vars = vars(oddCPIS:oddOuvriers), .funs = funs(mean(.)))%>%
  gather(key = "Dimension", value = "Data", 2:5) %>%
  ggplot() +
  geom_bar(aes(Dimension, Data, fill=factor(clusters)), stat = "identity") +
  scale_fill_manual(breaks = c("1","2","3","4","5","6"),
                    values=c("#F4A6D7", "#C5E86C","#5FAD41","#067BC2","#FF8811","#FF1D15"))+
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~clusters) +
  coord_flip()

dev.off()

mytest_ODDS_scale<-as.data.frame(scale(mytest_ODDS[,c(3:6)]))
mytest_ODDS_scale<-cbind(mytest_ODDS_scale,mytest_ODDS[,c(7)])
names(mytest_ODDS_scale)[5]<-"clusters"

pdf(file=paste0("~/Projets/Chapitre7/CAH_acquereur_residents/Odd_ratio/CAHOddratio_ValeurAxesStd.pdf"),width = 10, height = 5)

mytest_ODDS_scale %>%
  group_by(clusters) %>%
  # summarise_all(funs(mean),na.rm=T) %>%
  summarise_at(.vars = vars(oddCPIS:oddOuvriers), .funs = funs(mean(.)))%>%
  gather(key = "Dimension", value = "Data", 2:5) %>%
  ggplot() +
  geom_bar(aes(Dimension, Data, fill = factor(clusters)), stat = "identity") +
  scale_fill_manual(breaks = c("1","2","3","4","5","6"),
                    values=c("#F4A6D7", "#C5E86C","#5FAD41","#067BC2","#FF8811","#FF1D15"))+
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~clusters) +
  coord_flip()

dev.off()

library(SpatialPosition)
library(cartography)
library(RColorBrewer)


CommunesInsee$DepCom<-as.integer(CommunesInsee$DepCom)


mytest_ODDS$clusters_Code <-  recode(mytest_ODDS$clusters, "1"= "Sous représentations des catégories populaires par le marché",
                       "2"= "Représentations par le marché proches des moyennes régionales",
                       "3"= "fortes surreprésentations des Professions Intermédiaires par le marché",
                       "4"= "fortes surreprésentations des CPIS par le marché",
                       "5"= "Surreprésentations des Employés par le marché",
                       "6"= "Surreprésentations des Ouvriers par le marché")



specificCol <- c("Agriculteurs" = "#503D3F",
                 "Com_art_Chef_entreprises" = "#861388",
                 "CPIS"= "#067BC2" ,
                 "Prof_intermediaires"="#5FAD41",
                 "Employes"= "#FF8811",
                 "Ouvriers"="#FF1D15",
                 "retraites"= "#77878B",
                 "autres_inactifs"="#8D775F",
                 "Entreprise_Marchands_SCI"= "#E9DF00",
                 "Biens_publics_et_HLM"= "black",
                 "SAFER"="grey")



specificCol <- c("1"="#F4A6D7",
                 "2"= "#C5E86C",
                 "3"="#5FAD41",
                 "4"= "#067BC2",
                 "5"= "#FF8811",
                 "6"="#FF1D15")

opacity <- 95 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
specificCol <- paste0(specificCol, opacity)

Col_NoVal<- "#FFFFFF"
  opacity2 <- 10
Col_NoVal<- paste0(Col_NoVal, opacity2)
  
  dev.off()
Test_map<- left_join(CommunesInsee, mytest_ODDS%>%
                       filter(Periode == "1999"), by = "DepCom")


pdf(file="~/Projets/Chapitre7/CAH_acquereur_residents/Odd_ratio/CAHOddratio_1999.pdf",width = 10, height = 5)

tilesLayer(osmTiles)
layoutLayer()
typoLayer(x=Test_map,
          var = "clusters_Code",
          col= specificCol, ##palettes de couleurs predefinies## 
          border = NA,
          legend.values.order = c("Sous représentations des catégories populaires par le marché",
                                  "Représentations par le marché proches des moyennes régionales",
                                  "fortes surreprésentations des Professions Intermédiaires par le marché",
                                  "fortes surreprésentations des CPIS par le marché",
                                  "Surreprésentations des Employés par le marché",
                                  "Surreprésentations des Ouvriers par le marché"),
          legend.pos = "bottomleft",
          legend.title.txt = "1999",
          legend.values.cex= 0.5,
          colNA=Col_NoVal,add=TRUE)
plot(lim_IDF.st$geometry,add=T)

dev.off()

Test_map<- left_join(CommunesInsee, mytest_ODDS%>%
                       filter(Periode == "2008"), by = "DepCom")

pdf(file="~/Projets/Chapitre7/CAH_acquereur_residents/Odd_ratio/CAHOddratio_2008.pdf",width = 10, height = 5)

tilesLayer(osmTiles)
layoutLayer()
typoLayer(x=Test_map,
          var = "clusters_Code",
          col= specificCol, ##palettes de couleurs predefinies## 
          border = NA,
          legend.values.order = c("Sous représentations des catégories populaires par le marché",
                                  "Représentations par le marché proches des moyennes régionales",
                                  "fortes surreprésentations des Professions Intermédiaires par le marché",
                                  "fortes surreprésentations des CPIS par le marché",
                                  "Surreprésentations des Employés par le marché",
                                  "Surreprésentations des Ouvriers par le marché"),
          legend.pos = "bottomleft",
          legend.title.txt = "2008",
          legend.values.cex= 0.5,
          colNA=Col_NoVal,add=TRUE)
plot(lim_IDF.st$geometry,add=T)

dev.off()

Test_map<- left_join(CommunesInsee, mytest_ODDS%>%
                       filter(Periode == "2012"), by = "DepCom")



pdf(file="~/Projets/Chapitre7/CAH_acquereur_residents/Odd_ratio/CAHOddratio_2012.pdf",width = 10, height = 5)

tilesLayer(osmTiles)
layoutLayer()
typoLayer(x=Test_map,
          var = "clusters_Code",
          col= specificCol, ##palettes de couleurs predefinies## 
          border = NA,
          legend.values.order = c("Sous représentations des catégories populaires par le marché",
                                  "Représentations par le marché proches des moyennes régionales",
                                  "fortes surreprésentations des Professions Intermédiaires par le marché",
                                  "fortes surreprésentations des CPIS par le marché",
                                  "Surreprésentations des Employés par le marché",
                                  "Surreprésentations des Ouvriers par le marché"),
          legend.pos = "bottomleft",
          legend.title.txt = "2012",
          legend.values.cex= 0.5,
          colNA=Col_NoVal,add=TRUE)
plot(lim_IDF.st$geometry,add=T)

dev.off()
