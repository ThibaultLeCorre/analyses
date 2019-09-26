
library(spdep)
library(ggplot2)
library(sf)
library(sp)
library(SpatialPosition)
library(dplyr)
library(tidyr)
#################
BASE_PTZ_2016 <- read.delim("~/Projets/PTZ/PTZ/BASE_PTZ_2016.txt", stringsAsFactors=FALSE)
PTZ<- BASE_PTZ_2016%>% 
  select (cins,iden,ccsp,tegp, an, dept, dtpp, age,vtto,vtpp,cpfl,vtpz)
PTZ<-PTZ %>% filter (tegp<16, 
                     ccsp!=10 & ccsp!=11 & ccsp!=12 & ccsp!=13 ,
                     dept==75 |dept==91 | dept==92| dept==93 | dept==95 | dept==95 | dept==77 | dept==78   )%>%
  filter( dept==75 |dept==91 | dept==92| dept==93 | dept==94 | dept==95 | dept==77 | dept==78 , vtto<450000, vtpp>762 & vtpp<350000 )%>%
  filter(an==1996 | an==1999 |an>=2003 & an<=2012)

PTZ$ccsp<-ifelse(PTZ$ccsp >= 30 & PTZ$ccsp < 40, "CPIS",
                 ifelse(PTZ$ccsp >= 40 & PTZ$ccsp < 50, "Prof_intermediaires",
                        ifelse(PTZ$ccsp >= 50 & PTZ$ccsp < 60, "Employes",
                               ifelse(PTZ$ccsp >= 60 & PTZ$ccsp < 70, "Ouvriers",NA))))
# 
# PTZ$age<-
#   ifelse(PTZ$ccsp== "retraites", 60, PTZ$age )
# Box plot TEG

PTZ$cins<-ifelse(PTZ$cins >=75000 & PTZ$cins <=76000, 75056, PTZ$cins)

PTZ$age<- ifelse(PTZ$age>=18 & PTZ$age<30, "[18,30[",
                 ifelse(PTZ$age>=30 & PTZ$age<50, "[30,50[",
                        ifelse( PTZ$age>=50, "[50+", NA)))

PTZ<- PTZ%>%
  group_by(ccsp,an)%>%
  filter(!is.na(age), !is.na(ccsp), cins>=75000)%>%
  summarise(n_operation_PTZ=n(),
            TEG_median=median(tegp),
            Duree_median=median(dtpp),
            PTZ_moyen = mean (vtpz))

#################################################################

revenus_dispo_csp <- read.csv("~/Projets/Modele_access/revenus_dispo_csp.csv", sep=";", stringsAsFactors=FALSE)

test_revenus_region<- revenus_dispo_csp%>%
  gather("Annee", "Revenus_moyen", c(2:18))
         
test_revenus_region$Annee<-str_replace(string = test_revenus_region$Annee, pattern = "X", replacement = "")
  

# Test_modele<- left_join(CommunesInsee,Test_extrapo, by =c("DepCom"= "idcom") )

test_revenus_region$CSP<-ifelse(test_revenus_region$CSP=="CSP3" , "CPIS",
                              ifelse(test_revenus_region$CSP=="CSP4", "Prof_intermediaires",
                                     ifelse(test_revenus_region$CSP=="CSP5", "Employes",
                                            ifelse(test_revenus_region$CSP=="CSP6", "Ouvriers",NA))))

test_revenus_region$Annee<- as.integer(test_revenus_region$Annee)
Test_modele_region<- left_join(test_revenus_region,PTZ, by=c("Annee"="an", "CSP"= "ccsp"))

capacite_emprunts<- function (m,t,n){
  out1<-(1+t)^n - 1
  out2<-t*((1+t)^n)
  out<- out1/out2
  c <- m*out
  return(c)
}
Test_modele_region$tx_int_mensuels <- ((1+Test_modele_region$TEG_median/100)^(1/12)-1)


Test_modele_region$Capacite_total<-capacite_emprunts ((Test_modele_region$Revenus_moyen*0.33)/12,Test_modele_region$tx_int_mensuels ,Test_modele_region$Duree_median)

# Test_modele$Capacite_total<- Test_modele$Capacite_mensuel*Test_modele$Duree_median
# Prepa_PTZ<- left_join(CommunesInsee,PTZ, by =c("DepCom"= "cins") )

specificCol <- c("Com_art_Chef_entreprises" = "#861388",
                 "CPIS"= "#067BC2" ,
                 "Prof_intermediaires"="#5FAD41",
                 "Employes"= "#FF8811",
                 "Ouvriers"="#FF1D15",
                 "retraites"= "#77878B",
                 "autres_inactifs"="#8D775F",
                 "Entreprise_Marchands_SCI"= "#E9DF00",
                 "Biens_publics_et_HLM"= "black",
                 "Promoteurs"="#503D3F")

options(scipen = 999)
Montants_emprunts_residents_IDF<-as.data.frame(Test_modele_region)%>%
  group_by(CSP,Annee )%>%
  filter(!is.na(CSP),Annee==1996 | Annee==1999 |Annee>=2003)%>%
  summarise(Capacite = mean(Capacite_total))%>%
  ggplot(., aes(Annee , Capacite, fill=CSP)) +
  geom_line()+
  geom_path(aes(color = CSP))+
  scale_color_manual(values = specificCol )+
  # scale_y_continuous(expand = c(0.01, 0),breaks = c(-10000,0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000,140000,150000,160000,170000))+
  scale_x_continuous(breaks = c(1996, 1999,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  labs(title = "Montants moyens théoriques des capacités d'emprunts de la population francilienne résidente selon leurs revenus moyens diponsibles", x= "Année" , y= "Montant empruntable théorique moyen")+ 
  labs(subtitle = "Le calcul a été réalisé en tenant compte de la durée médiane du crédit et du TEG médian par CSP et par année renseignés dans les données du SGFGAS\nLes montants moyens théoriques dépendent des revenus moyens disponibles des ménages par CSP et par année en considérant que cette capacité est égale à 33%\ndes revenus mensuels des ménages.")+
  labs(caption = "Sources : SGFGAS (Duréee emprunt et TEG), Insee (revenus par CSP en IDF) ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")


setwd("~/Projets/Modele_access")
ggsave("Montants_emprunts_residents_IDF.pdf",plot= Montants_emprunts_residents_IDF, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
ggsave("Montants_emprunts_residents_IDF.png",plot= Montants_emprunts_residents_IDF, device = "png", width = 380, height = 180, units = "mm", dpi = 330)

#reprendre table script credit LTV par csp
Acquereurs_LTV<-mytest%>% 
  filter(  acquereurs!="Agriculteurs" & acquereurs!="SAFER" & acquereurs!="Entreprise_Marchands_SCI" & acquereurs!="Biens_publics_et_HLM")%>%
  filter(acquereurs != "autres_inactifs" & acquereurs != "Liberales" )%>%
  filter(!is.na(acquereurs) & MTCRED>1000)%>% 
  group_by(annee.x, acquereurs)%>% 
  summarise(LTV=sum(MTCRED)/sum(REQ_PRIX)*100)


Test_modele_region<- left_join(Test_modele_region, Acquereurs_LTV, by = c("CSP"="acquereurs", "Annee"="annee.x"))

Test_modele_region<- Test_modele_region%>%
  drop_na()

Test_modele_region<- left_join(Test_modele_region,PTZ[,c("ccsp","an","PTZ_moyen")],by =c("CSP"="ccsp", "Annee"="an"))

Test_modele_region$Capacite_total_With_Apport<- Test_modele_region$Capacite_total* ((1-(Test_modele_region$LTV/100))+1)

Test_modele_region$Capacite_total_With_Apport_nd_PTZ <- Test_modele_region$Capacite_total_With_Apport + Test_modele_region$PTZ_moyen

Montants_capcite_totale_IDF<-as.data.frame(Test_modele_region)%>%
  group_by(CSP,Annee )%>%
  filter(!is.na(CSP),Annee==1996 | Annee==1999 |Annee>=2003)%>%
  summarise(Capacite = mean(Capacite_total_With_Apport))%>%
  ggplot(., aes(Annee , Capacite, fill=CSP)) +
  geom_line()+
  geom_path(aes(color = CSP))+
  scale_color_manual(values = specificCol )+
  # scale_y_continuous(expand = c(0.01, 0),breaks = c(-10000,0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000,140000,150000,160000,170000))+
  scale_x_continuous(breaks = c(1996, 1999,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  labs(title = "Montants moyens théoriques des capacités totale de l'investissement (emprunt + apport) de la population francilienne résidente", x= "Année" , y= "Montant empruntable théorique moyen")+ 
  labs(subtitle = "Le calcul a été réalisé en tenant compte de la durée médiane du crédit et du TEG médian par CSP et par année renseignés dans les données du SGFGAS\nLes montants moyens théoriques dépendent des revenus moyens disponibles des ménages par CSP et par année en considérant que cette capacité est égale à 33%\ndes revenus mensuels des ménages. L'apport moyen est calculé selon le LTV moyen pour chaque année et pour chaque CSP.")+
  labs(caption = "Sources : échantillon BIEN redressé (LTV) SGFGAS (Duréee emprunt et TEG), Insee (revenus par CSP en IDF) ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")

setwd("~/Projets/Modele_access")

ggsave("Montants_capcite_totale_IDF.png",plot= Montants_capcite_totale_IDF, device = "png", width = 380, height = 180, units = "mm", dpi = 330)




mytest<-data_redresse1570533transacs[,c("ID","annee.x","X.x","Y.x","REQTYPBIEN", "NBRPIECE","REQ_PRIX", "CSP_VE")]



test<- mytest%>%
  filter(CSP_VE>= 30 & CSP_VE<= 39)

spVente <- st_as_sf(test,
                    
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
Communes_jointure<- as.data.frame(Communes_jointure)

Communes_jointure$Type_Bien<-paste0(Communes_jointure$REQTYPBIEN, Communes_jointure$NBRPIECE)

Communes_jointure$Type_logement<- ifelse (Communes_jointure$Type_Bien=="AP1" | Communes_jointure$Type_Bien=="AP2", "Petit_appartement",
                                          ifelse (Communes_jointure$Type_Bien=="AP3" | Communes_jointure$Type_Bien=="AP4" | Communes_jointure$Type_Bien=="AP5", "Appartement_familial",   
                                                  ifelse (Communes_jointure$Type_Bien=="MA1" | Communes_jointure$Type_Bien=="MA2" | Communes_jointure$Type_Bien=="MA3", "Petite_maison",   
                                                          ifelse ( Communes_jointure$Type_Bien=="MA4" | Communes_jointure$Type_Bien=="MA5" | Communes_jointure$Type_Bien=="MA6", "Maison_familiale",NA))))


# table(Communes_jointure$Type_logement)
# 
Communes_jointure$Type<- ifelse(Communes_jointure$Type_logement == "Petit_appartement" | Communes_jointure$Type_logement == "Petite_maison", "Petit logement",
                                ifelse(Communes_jointure$Type_logement == "Appartement_familial" | Communes_jointure$Type_logement == "Maison_familiale", "Logement familial",NA))

# table(Communes_jointure$Type)

Communes_prix<-Communes_jointure%>%
  filter(annee.x>=1996 )%>%
  group_by(DepCom,annee.x, Type) %>%
  summarise(Nombre_transacs = n(),
            Prix_median = median(REQ_PRIX)) %>%
  filter(Nombre_transacs>5) %>%
  drop_na()



Test_modele_region_2<-Test_modele_region%>%
  select(CSP ,Annee ,Capacite_total_With_Apport)%>%
  drop_na()
  # group_by(CSP,Annee)%>%
  # gather("Type", "Value", c(4,5))%>%
 


Test_modele_region_2<-left_join(Communes_prix,Test_modele_region_2, by = c( "annee.x"="Annee"))


Test_modele_region_2$Ratio_diff_Capacite_Achat<- Test_modele_region_2$Capacite_total_With_Apport / Test_modele_region_2$Prix_median

Test_modele_region_2$Type_Appart_CSP<- paste(Test_modele_region_2$Type, Test_modele_region_2$CSP, sep="_")


# Test_modele_region_2$Access_catego<-ifelse(Test_modele_region_2$CSP=="Ouvriers">=1 & Test_modele_region_2$CSP=="Employes">=1, "Accessible aux ouvriers et employes", "Autre")

Test_modele_region_3<-Test_modele_region_2%>%
  group_by(DepCom, annee.x,Type)%>%
  summarise(Access_type= ifelse(Ratio_diff_Capacite_Achat[CSP=='CPIS']<1,"4.Inaccessible",
                     # ifelse(Ratio_diff_Capacite_Achat[CSP=='CPIS']>1,"",
                     ifelse(Ratio_diff_Capacite_Achat[CSP=='Prof_intermediaires']<1,"3.Accessible CPIS",
                       ifelse(Ratio_diff_Capacite_Achat[CSP=='Ouvriers']<1 & Ratio_diff_Capacite_Achat[CSP=='Employes']<1 ,"2.Accessible Prof_intermediaires", "1.Accessible Ouvriers et Employes"))))


cols=c("#006837","#a6d96a","#fdae61","#a50026")
opacity = 85
cols<- paste0(cols,opacity)

Annee <- unique(Test_modele_region_3$annee.x)
Annee <- sort(Annee[!is.na(Annee)])


# opacity <- 85
# cspColors <- list()
# cspColors["1.Accessible Ouvriers et Employes"] <- paste0("#006837",opacity)
# cspColors["2.Accessible Prof_intermediaires"] <- paste0("#a6d96a",opacity)
# cspColors["3.Accessible CPIS"] <- paste0("#fdae61",opacity)
# cspColors["4.Inaccessible"] <- paste0("#a50026",opacity)



Type_BIEN <- unique(Test_modele_region_3$Type)
library(cartography)
osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)

dev.off()
for (cetteAnneeLa in Annee){
  
  Communes_filtred <- Test_modele_region_3 %>%
    filter(annee.x == cetteAnneeLa)
  
  for (ceTypeLa in Type_BIEN){
  
    
    Communes_filtred2 <- Communes_filtred %>%
      filter(Type == ceTypeLa)
  # carreaux_filtred <-  carreaux_filtred[sample(1: nrow(carreaux_filtred), 15000,replace=T), ]
  
  Map_Acces_Comm<-left_join(CommunesInsee, Communes_filtred2, by= "DepCom")
   
catego<-unique(Map_Acces_Comm$Access_type)
catego<-sort(catego) 
      
pdf(file=paste0("~/Projets/Modele_access/Carto/",cetteAnneeLa,ceTypeLa,".pdf"),width = 10, height = 5)
      tilesLayer(osmTiles)
      
      typoLayer(spdf =  Map_Acces_Comm, 
                  var ="Access_type", 
                border = NA,
                  lwd=0.1,
                col = cols,
                legend.values.order = catego,
                  legend.title.txt = sprintf("Accessibilité au marché des CSP"),
                  legend.pos = "topright",add=TRUE)
      layoutLayer(title = paste("Accessibilité au marché des CSP en ", cetteAnneeLa, ceTypeLa, sep = " "))
      plot(st_geometry(DepInsee), col = , lwd = 0.4, add=T)
      
      dev.off()
      
      
    }
}
















Test_ACP<-Test_modele_region_2%>%
  select(DepCom,annee.x,Type_Appart_CSP,Ratio_diff_Capacite_Achat)%>%
  drop_na()%>% 
  # select(-geometry)%>%
  group_by(DepCom,annee.x)%>%
  spread( key = Type_Appart_CSP, value =  Ratio_diff_Capacite_Achat)

























row.names(Test_ACP)<-paste(Test_ACP$DepCom, Test_ACP$annee.x, sep="_")

Var_active<-Test_ACP[,c(3:ncol(Test_ACP))]      


res.pca<-PCA(Var_active,graph = TRUE,ncp = 5,scale.unit = TRUE)
summary(res.pca)
dimdesc(res.pca, axes=c(1:5))



myDF <- subset(Test_ACP[,c(3:ncol(Test_ACP))])
myDF <- myDF%>%
  drop_na()
str(myDF)

library(ade4)
library(FactoClass)
AFC <- dudi.coa(df=myDF, scannf=FALSE, nf=ncol(myDF))
plot.dudi(AFC)



distMat <- dist.dudi(AFC, amongrow=TRUE)


CAH <- ward.cluster(distMat, peso = apply(X=myDF, MARGIN=1, FUN=sum) , plots = TRUE, h.clust = 1)
dev.off()

pdf(file=paste0("~/Projets/Chapitre7/CAH_acquereur_residents/Odd_ratio/CAHOddratio_inertie.pdf"),width = 10, height = 5)

par(mfrow=c(1,2))
barplot(sort(CAH$height / sum(CAH$height), decreasing = TRUE)[1:15] * 100,
        xlab = "Noeuds", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie selon le partitionnement")

barplot(cumsum(sort(CAH$height / sum(CAH$height), decreasing = TRUE))[1:15] * 100,
        xlab = "Nombre de classes", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie expliquée")
dev.off()

myDF$clusters <- cutree(tree = CAH, k = 5)


mytest_ODDS<-cbind(mytest_ODDS,myDF[,c(5)] )
names(mytest_ODDS)[22]<-"clusters"

mytest_ODDS<-mytest_ODDS[,c("DepCom","annee.x","oddCPIS","oddProf_inter","oddEmployes","oddOuvriers","clusters")]

# specificCol <- c("1"="#F4A6D7",
#                  "2"= "#C5E86C",
#                  "3"="#5FAD41",
#                  "4"= "#067BC2",
#                  "5"= "#FF8811",
#                  "6"="#FF1D15")

specificCol <- c("1"="#F4A6D7",
                 # "2"= "#C5E86C",
                 "2"="#5FAD41",
                 "3"= "#067BC2",
                 "4"="#FF1D15",
                 "5"= "#FF8811")
# "6"="#FF1D15")


pdf(file=paste0("~/Projets/Chapitre7/CAH_acquereur_residents/Odd_ratio/CAHOddratio_ValeurAxes.pdf"),width = 10, height = 5)

mytest_ODDS %>%
  group_by(clusters) %>%
  # summarise_all(funs(mean),na.rm=T) %>%
  summarise_at(.vars = vars(oddCPIS:oddOuvriers), .funs = funs(mean(.)))%>%
  gather(key = "Dimension", value = "Data", 2:5) %>%
  ggplot() +
  geom_bar(aes(Dimension, Data, fill=factor(clusters)), stat = "identity") +
  scale_fill_manual(breaks = c("1","2","3","4","5"),
                    values=c("#F4A6D7", "#5FAD41", "#067BC2","#FF1D15","#FF8811"))+
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~clusters) +
  coord_flip()

