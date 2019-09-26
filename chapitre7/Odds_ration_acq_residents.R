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
data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest_BIEN<-data_redresse1570533transacs[,c("annee.x","CSP_AC", "X.x", "Y.x")]


mytest_BIEN$CSP_AC<- ifelse(mytest_BIEN$CSP_AC == 10 , "Agriculteurs",
                            ifelse(mytest_BIEN$CSP_AC >= 20 & mytest_BIEN$CSP_AC < 30, "Liberales",
                                   ifelse(mytest_BIEN$CSP_AC >= 30 & mytest_BIEN$CSP_AC < 40, "CPIS",
                                          ifelse(mytest_BIEN$CSP_AC >= 40 & mytest_BIEN$CSP_AC < 50, "Prof_intermediaires",
                                                 ifelse(mytest_BIEN$CSP_AC >= 50 & mytest_BIEN$CSP_AC < 60, "Employes",
                                                        ifelse(mytest_BIEN$CSP_AC >= 60 & mytest_BIEN$CSP_AC < 70, "Ouvriers",
                                                               ifelse(mytest_BIEN$CSP_AC >= 70 & mytest_BIEN$CSP_AC < 80, "retraite",
                                                                      ifelse(mytest_BIEN$CSP_AC == 80, "autres_inactifs_AC", "non_rs_AC" ))))))))

mytest_BIEN$Periode<-ifelse(mytest_BIEN$annee.x==1996|mytest_BIEN$annee.x==1999|mytest_BIEN$annee.x==2003, "Periode_96_2003",
                            ifelse(mytest_BIEN$annee.x>=2004&mytest_BIEN$annee.x<=2007, "Periode_04_2007", 
                                   ifelse(mytest_BIEN$annee.x>=2008&mytest_BIEN$annee.x<=2012, "Periode_08_2012", NA)))


#######################




Communes_jointure<-st_join(CommunesInsee, spVente, join = st_contains, left=T)
mytest_BIEN<- as.data.frame(Communes_jointure)

rm(Communes_jointure)
##########

# Years<- c(1999,2008,2012)
Acquereurs<- c("Agriculteurs","Liberales","CPIS", "Prof_intermediaires","Employes", "Ouvriers")
mytest<- mytest_BIEN%>%
  group_by(DepCom,Periode, CSP_AC)%>%
  filter(CSP_AC %in% Acquereurs)%>%
 summarise(n_acq=n())%>%
ungroup() %>%
  spread(CSP_AC,n_acq,fill = 0)
rm(mytest_BIEN)

mytest$total<- rowSums(mytest[,c(3:8)])

mytest<-mytest%>%
  group_by(DepCom)%>%
  filter(total>=20)

mytest<-as.data.frame(mytest) %>% group_by(DepCom,Periode) %>% mutate_at(Acquereurs, funs(./total*100))
################################################


CSP_residentsCommunes_IDF_990812 <- read.csv2("~/Projets/Chapitre7/CAH_acquereur_residents/CSP_residentsCommunes_IDF_990812.csv", stringsAsFactors=FALSE)
CSP_residentsCommunes_IDF_990812$Periode<-ifelse(CSP_residentsCommunes_IDF_990812$annee==1999, "Periode_96_2003",
                                                 ifelse(CSP_residentsCommunes_IDF_990812$annee==2008, "Periode_04_2007", 
                                                        ifelse(CSP_residentsCommunes_IDF_990812$annee==2012, "Periode_08_2012", NA)))
test_residents<-CSP_residentsCommunes_IDF_990812
Residents<- c("Agriculteurs","Liberales","CPIS","Prof_intermediaires","Employes","Ouvriers")

test_residents$Agriculteurs<-as.numeric(test_residents$Agriculteurs)
test_residents$Liberales<-as.numeric(test_residents$Liberales)
test_residents$CPIS<-as.numeric(test_residents$CPIS)
test_residents$Prof_intermediaires<-as.numeric(test_residents$Prof_intermediaires)
test_residents$Employes<-as.numeric(test_residents$Employes)
test_residents$Ouvriers<-as.numeric(test_residents$Ouvriers)


test_residents$total_residents<- rowSums(test_residents[,c(3:8)])

test_residents<-as.data.frame(test_residents) %>% 
  group_by(CODGEO,Periode) %>%
  mutate_at(Residents, funs(./total_residents*100))

####
mytest$DepCom<-as.numeric(mytest$DepCom)
test_residents$CODGEO<-as.numeric(test_residents$CODGEO)
mytest_ODDS<- left_join(mytest,test_residents, by= c("DepCom"="CODGEO", "Periode"))%>%
  filter(!is.na(total_residents) & Liberales>0)


oddCPIS<-as.data.frame((mytest_ODDS$CPIS_AC/(mytest_ODDS$CPIS_AC-100))/ (mytest_ODDS$CPIS/(mytest_ODDS$CPIS-100)))
names(oddCPIS)[1]<-"oddCPIS"
oddLiberals<-as.data.frame((mytest_ODDS$Liberales_AC/(mytest_ODDS$Liberales_AC-100))/ (mytest_ODDS$Liberales/(mytest_ODDS$Liberales-100)))
names(oddLiberals)[1]<-"oddLiberals"
oddProf_inter<-as.data.frame((mytest_ODDS$Prof_intermediaires_AC/(mytest_ODDS$Prof_intermediaires_AC-100))/ (mytest_ODDS$Prof_intermediaires/(mytest_ODDS$Prof_intermediaires-100)))
names(oddProf_inter)[1]<-"oddProf_inter"
oddEmployes<-as.data.frame((mytest_ODDS$Employes_AC/(mytest_ODDS$Employes_AC-100))/(mytest_ODDS$Employes/(mytest_ODDS$Employes-100)))
names(oddEmployes)[1]<-"oddEmployes"
oddOuvriers<-as.data.frame((mytest_ODDS$Ouvriers_AC/(mytest_ODDS$Ouvriers_AC-100))/(mytest_ODDS$Ouvriers/(mytest_ODDS$Ouvriers-100)))
names(oddOuvriers)[1]<-"oddOuvriers"

mytest_ODDS<-cbind.data.frame(mytest_ODDS,oddCPIS,oddLiberals,oddProf_inter,oddEmployes,oddOuvriers)

####CAH sur les odds ratio#

myDF <- subset(mytest_ODDS[,c(19:23)])

str(myDF)

library(ade4)
library(FactoClass)
AFC <- dudi.coa(df=myDF, scannf=FALSE, nf=ncol(myDF))
plot.dudi(AFC)



distMat <- dist.dudi(AFC, amongrow=TRUE)


CAH <- ward.cluster(distMat, peso = apply(X=myDF, MARGIN=1, FUN=sum) , plots = TRUE, h.clust = 1)


par(mfrow=c(1,2))
barplot(sort(CAH$height / sum(CAH$height), decreasing = TRUE)[1:15] * 100,
        xlab = "Noeuds", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie selon le partitionnement")

barplot(cumsum(sort(CAH$height / sum(CAH$height), decreasing = TRUE))[1:15] * 100,
        xlab = "Nombre de classes", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie expliquée")


myDF$clusters <- cutree(tree = CAH, k = 6)


mytest_ODDS<-cbind(mytest_ODDS,myDF[,c(6)] )
names(mytest_ODDS)[24]<-"clusters"

mytest_ODDS %>%
  group_by(clusters) %>%
  # summarise_all(funs(mean),na.rm=T) %>%
  summarise_at(.vars = vars(oddCPIS:oddOuvriers), .funs = funs(mean(.)))%>%
  gather(key = "Dimension", value = "Data", 2:6) %>%
  ggplot() +
  geom_bar(aes(Dimension, Data, fill = clusters), stat = "identity") +
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~clusters) +
  coord_flip()

mytest_ODDS_scale<-as.data.frame(scale(mytest_ODDS[,c(19:23)]))
mytest_ODDS_scale<-cbind(mytest_ODDS_scale,mytest_ODDS[,c(24)])
names(mytest_ODDS_scale)[6]<-"clusters"

mytest_ODDS_scale %>%
  group_by(clusters) %>%
  # summarise_all(funs(mean),na.rm=T) %>%
  summarise_at(.vars = vars(oddCPIS:oddOuvriers), .funs = funs(mean(.)))%>%
  gather(key = "Dimension", value = "Data", 2:6) %>%
  ggplot() +
  geom_bar(aes(Dimension, Data, fill = clusters), stat = "identity") +
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~clusters) +
  coord_flip()

library(SpatialPosition)
library(cartography)
library(RColorBrewer)

cols <- brewer.pal(12,"Set3")

opacity <- 95 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)
CommunesInsee$DepCom<-as.integer(CommunesInsee$DepCom)
Test_map<- left_join(CommunesInsee, mytest_ODDS%>%
                       filter(Periode == "Periode_08_2012"), by = "DepCom")


# Periode_96_2003# Periode_04_2007
osmTiles <- getTiles(x = Spvente, type = "stamenbw",  crop = FALSE)


tilesLayer(osmTiles)
layoutLayer()
typoLayer(x=Test_map,
          var = "clusters",
          col= cols, ##palettes de couleurs predefinies## 
          border = "black",
          legend.values.order = c("1",
                                  "2", 
                                  "3",
                                  "4",
                                  "5",
                                  "6"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          lwd = 0.2,
          colNA="grey",add=TRUE)
plot(lim_IDF.st$geometry,add=T)

# mytest$insee<- ifelse(mytest$insee == 75001 , "75101",
#                   ifelse(mytest$insee == 75002 , "75102",
#                          ifelse(mytest$insee == 75003 , "75103",
#                                 ifelse(mytest$insee == 75004 , "75104",
#                                        ifelse(mytest$insee == 75005 , "75105",
#                                               ifelse(mytest$insee == 75006 , "75106",
#                                                      ifelse(mytest$insee == 75007 , "75107",
#                                                             ifelse(mytest$insee == 75008 , "75108",
#                                                                    ifelse(mytest$insee == 75009 , "75109",
#                                                                           ifelse(mytest$insee == 75010 , "75110",
#                                                                                  ifelse(mytest$insee == 75011 , "75111",
#                                                                                         ifelse(mytest$insee == 75012 , "75112",
#                                                                                                ifelse(mytest$insee == 75013 , "75113",
#                                                                                                       ifelse(mytest$insee == 75014 , "75114",
#                                                                                                              ifelse(mytest$insee == 75015 , "75115",
#                                                                                                                     ifelse(mytest$insee == 75016 , "75116",
#                                                                                                                            ifelse(mytest$insee == 75017 , "75117",
#                                                                                                                                   ifelse(mytest$insee == 75018 , "75118",
#                                                                                                                                          ifelse(mytest$insee == 75019 , "75119",
#                                                                                                                                                 ifelse(mytest$insee == 75020 , "75120",mytest$insee))))))))))))))))))))
# 







