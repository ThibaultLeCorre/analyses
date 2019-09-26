library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(reshape2)
library(ggridges)
library(viridis)

 data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)

mytest<-data_redresse1570533transacs[,c("ID","annee.x","X.x","Y.x","REQ_PRIX","REQTYPBIEN", "REQ_VALUE","DATMUTPREC", "TYPMUTPREC", "PXMUTPREC", "NBRPIECE")]


mytest<- mytest%>% 
  filter(`X.x`!= 0 & `Y.x`!=0, !is.na(REQ_VALUE))

mytest$Annee_MUTPREC<- as.numeric(substr(mytest$DATMUTPREC, start = 7, stop = 10))

mytest$Annee_MUTPREC<- ifelse(mytest$Annee_MUTPREC<1000,as.numeric(substr(mytest$DATMUTPREC, start = 6, stop = 10)),mytest$Annee_MUTPREC)
str(mytest)

mytest$Temps_Pocession<- mytest$annee.x - mytest$Annee_MUTPREC
mytest<-mytest%>%
  filter(Temps_Pocession>=1 & Temps_Pocession<150)

mytest$TYPMUTPREC_2<- ifelse(mytest$TYPMUTPREC == "A","Acquisition",
                             ifelse(mytest$TYPMUTPREC == "D" | mytest$TYPMUTPREC == "S" ,"Donation ou héritage", NA))



#Shape

spVente <- st_as_sf(mytest,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)



#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]   
spVente$Ylamb93<-coords[,2]


## essai repr?sentation

# osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)

#Importation carroyage Insee 200*200 et 1*1km
setwd("~/Shapes/datidf")
list.files()
carreauInsee200 <- st_read("car200m_idf_reparGeom.shp",
                           stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee200 <-  st_transform(carreauInsee200, crs = 2154)


#pour carreau Insee
Cadrillage_200<- carreauInsee200%>%
  st_sf(geometry = .)

Cadrillage_200$Carreau_ID<-1:length(Cadrillage_200$TARGET_FID)

#####
Typo_1999_2012_carreaux200_appartement_37158tiles <- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_carreaux200_appartement_37158tiles.csv", stringsAsFactors=FALSE)
Typo_1999_2012_carreaux200_Maison_36693tiles <- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_carreaux200_Maison_36693tiles.csv", stringsAsFactors=FALSE)


###TYpo_Appart

Typo_Appart<-Typo_1999_2012_carreaux200_appartement_37158tiles[,c("Carreau_ID","categorie_finale")]
Cadrillage_200_jointure<-left_join(Cadrillage_200, Typo_Appart, by="Carreau_ID")

Cadrillage_200_jointure2<-st_join(Cadrillage_200_jointure, spVente, join = st_contains, left=T)
table(Cadrillage_200_jointure2$categorie_finale)
PV_appart<- as.data.frame(Cadrillage_200_jointure2)%>%
  filter(NBRPIECE == 2 | NBRPIECE == 3) %>%
  select(Carreau_ID,categorie_finale,annee.x,REQ_VALUE,REQTYPBIEN,Temps_Pocession,TYPMUTPREC_2)%>%
  filter(REQTYPBIEN=="AP" & TYPMUTPREC_2=="Acquisition", !is.na(categorie_finale), 
         # Temps_Pocession==1 | Temps_Pocession==5 | Temps_Pocession==10 | Temps_Pocession==15 | Temps_Pocession==20,
         annee.x == 1999 | annee.x == 2008 |annee.x == 2012)
options(scipen = 999)
# ggplot(PV_appart, aes(x=factor(Temps_Pocession), y=REQ_VALUE)) + 
#   geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9),fill="#F8766D") +
#   # xlab("Année") + ylab("Prix au m²") + 
#   geom_boxplot(width=0.1,outlier.colour=NA)+
#   ylim(-300000,300000)+
#   stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
#   stat_summary(fun.y=median, geom="point", size=1, color="red")+
#   theme_tmd()+
#   theme(legend.position="none")+
#   facet_grid(annee.x ~ categorie_finale)
#   labs(x=NULL,y=NULL,
#        title="Prix nominaux au m²\ndu segment des appartements\nde 2 et 3 pièces en Île-de-France",
#        subtitle="190 transactions écartées de la visualisation (>15000 euros au m²)",
#        # \nLa moyenne est représentée par les points bleus, la médiane par les points rouges ",
#        caption="Source: BIEN\n réalisation sous R avec le package ggplot2\n T.Le Corre, UMR Géographie-Cités, 2017")

  

  
mytest_PV_appart<-PV_appart%>%
    group_by(annee.x,categorie_finale,Temps_Pocession)%>%
    summarise(PV_Mediane= median(REQ_VALUE),
              Premier_quartile = quantile(REQ_VALUE, probs=0.25),
              Dernier_quartile = quantile(REQ_VALUE, probs=0.75),
              nombre=n())%>%
    filter(Temps_Pocession<=30) %>%
  gather("Type","Value", 4:6)


unique(mytest_PV_appart$categorie_finale)
mytest_PV_appart$clusters <-  recode(mytest_PV_appart$categorie_finale, 
                                     "Prix initiaux élevés (mean + sd)evolution_plus_fort_que_la _prevision_"   = "1" ,   
                                     "Prix initiaux élevés (mean + sd)evolution_conforme_au_modele_"   = "2",
                                     "Prix initiaux élevés (mean + sd)evolution_plus_faible_que_la_prevision_" ="3",
                                     "Prix initiaux dans la moyenne (mean +/- sd)evolution_plus_fort_que_la _prevision_" = "4",
                                     "Prix initiaux dans la moyenne (mean +/- sd)evolution_conforme_au_modele_" = "5",
                                     "Prix initiaux dans la moyenne (mean +/- sd)evolution_plus_faible_que_la_prevision_" = "6",
                                     "Prix initiaux faibles (mean - sd)evolution_plus_fort_que_la _prevision_"="7",
                                     "Prix initiaux faibles (mean - sd)evolution_conforme_au_modele_" ="8",
                                     "Prix initiaux faibles (mean - sd)evolution_plus_faible_que_la_prevision_" ="9")


mytest_PV_appart_plot<- mytest_PV_appart%>%
  filter(clusters!="9")%>%
  ggplot(., aes(Temps_Pocession, Value, fill=Type)) +
  geom_line()+
  geom_path(aes(color = Type))+
  scale_x_continuous(expand = c(0.01, 0),breaks = c(0,5,10,15,20,25,30)) +
   theme_tmd() +
  facet_grid(annee.x~clusters,scale="fix")+
  labs(title = "Montants des plus-values sur les appartements en fonction des catégories de trajectoires des prix et de la durée de détention des biens", x= "Nombre d'années de détention du bien" , y= "Montant de la plus-value")+ 
  labs(subtitle = "Les calculs ont été réalisés sur le carroyage 200m²\nLes montants des plus-value ont été calculé sur les appartements de 2 et 3 pièces.\nLes biens reçus en donation ou en héritage ont été exclus.\nLa dernière catégorie de trajectoires des prix est absente de l'analyse")+
   labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")
setwd("~/Projets/Plus_value/Plus_value")
ggsave("mytest_PV_appart_plot.png",plot= mytest_PV_appart_plot,  device = "png", width = 300, height = 320, units = "mm", dpi = 330)


mytest_PV_appart_plot<- mytest_PV_appart%>%
  filter(clusters!="9")%>%
  ggplot(., aes(Temps_Pocession, Value, fill=Type)) +
  geom_line()+
  geom_path(aes(color = Type))+
  scale_x_continuous(expand = c(0.01, 0),breaks = c(0,5,10,15,20,25,30)) +
   theme_tmd() +
  facet_grid(annee.x~clusters,scale="fix")+
  labs(title = "Montants des plus-values en fonction des catégories de trajectoires des prix et de la durée de détention des biens", x= "Nombre d'années de détention du bien" , y= "Montant de la plus-value")+ 
  labs(subtitle = "Les calculs ont été réalisés sur le carroyage 200m²\nLes montants des plus-value ont été calculés sur les appartements de 2 et 3 pièces.\nLes biens reçus en donation ou en héritage ont été exclus.\nLa dernière catégorie de trajectoires des prix est absente de l'analyse")+
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")
setwd("~/Projets/Plus_value/Plus_value")
ggsave("mytest_PV_appart_plot.pdf",plot= mytest_PV_appart_plot,  device = "pdf", width = 260, height = 400, units = "mm", dpi = 330)




##############
# Typo_Maison

Typo_Maison<-Typo_1999_2012_carreaux200_Maison_36693tiles[,c("Carreau_ID","categorie_finale")]
Cadrillage_200_jointure<-left_join(Cadrillage_200, Typo_Maison, by="Carreau_ID")

Cadrillage_200_jointure2<-st_join(Cadrillage_200_jointure, spVente, join = st_contains, left=T)
table(Cadrillage_200_jointure2$categorie_finale)
PV_Maisons<- as.data.frame(Cadrillage_200_jointure2)%>%
  filter(NBRPIECE == 4| NBRPIECE == 5) %>%
  select(Carreau_ID,categorie_finale,annee.x,REQ_VALUE,REQTYPBIEN,Temps_Pocession,TYPMUTPREC_2)%>%
  filter(REQTYPBIEN=="MA" & TYPMUTPREC_2=="Acquisition", !is.na(categorie_finale), 
         # Temps_Pocession==1 | Temps_Pocession==5 | Temps_Pocession==10 | Temps_Pocession==15 | Temps_Pocession==20,
         annee.x == 1999 | annee.x == 2008 |annee.x == 2012)
options(scipen = 999)
# ggplot(PV_appart, aes(x=factor(Temps_Pocession), y=REQ_VALUE)) + 
#   geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9),fill="#F8766D") +
#   # xlab("Année") + ylab("Prix au m²") + 
#   geom_boxplot(width=0.1,outlier.colour=NA)+
#   ylim(-300000,300000)+
#   stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
#   stat_summary(fun.y=median, geom="point", size=1, color="red")+
#   theme_tmd()+
#   theme(legend.position="none")+
#   facet_grid(annee.x ~ categorie_finale)
#   labs(x=NULL,y=NULL,
#        title="Prix nominaux au m²\ndu segment des appartements\nde 2 et 3 pièces en Île-de-France",
#        subtitle="190 transactions écartées de la visualisation (>15000 euros au m²)",
#        # \nLa moyenne est représentée par les points bleus, la médiane par les points rouges ",
#        caption="Source: BIEN\n réalisation sous R avec le package ggplot2\n T.Le Corre, UMR Géographie-Cités, 2017")




mytest_PV_Maisons<-PV_Maisons%>%
  group_by(annee.x,categorie_finale,Temps_Pocession)%>%
  summarise(PV_Mediane= median(REQ_VALUE),
            Premier_quartile = quantile(REQ_VALUE, probs=0.25),
            Dernier_quartile = quantile(REQ_VALUE, probs=0.75),
            nombre=n())%>%
  filter(Temps_Pocession<=30) %>%
  gather("Type","Value", 4:6)


unique(PV_Maisons$categorie_finale)
mytest_PV_Maisons$clusters <-  recode(mytest_PV_Maisons$categorie_finale, 
                                     "Prix initiaux élevés (mean + sd)evolution_plus_fort_que_la _prevision_"   = "1" ,   
                                     "Prix initiaux élevés (mean + sd)evolution_conforme_au_modele_"   = "2",
                                     "Prix initiaux élevés (mean + sd)evolution_plus_faible_que_la_prevision_" ="3",
                                     "Prix initiaux dans la moyenne (mean +/- sd)evolution_plus_fort_que_la _prevision_" = "4",
                                     "Prix initiaux dans la moyenne (mean +/- sd)evolution_conforme_au_modele_" = "5",
                                     "Prix initiaux dans la moyenne (mean +/- sd)evolution_plus_faible_que_la_prevision_" = "6",
                                     "Prix initiaux faibles (mean - sd)evolution_plus_fort_que_la _prevision_"="7",
                                     "Prix initiaux faibles (mean - sd)evolution_conforme_au_modele_" ="8",
                                     "Prix initiaux faibles (mean - sd)evolution_plus_faible_que_la_prevision_" ="9")


PV_Maisons_plot<- mytest_PV_Maisons%>%
  filter(clusters!="9")%>%
  ggplot(., aes(Temps_Pocession, Value, fill=Type)) +
  geom_line()+
  geom_path(aes(color = Type))+
  scale_x_continuous(expand = c(0.01, 0),breaks = c(0,5,10,15,20,25,30)) +
  theme_tmd() +
  facet_grid(annee.x~clusters,scale="fix")+
  labs(title = "Montants des plus-values sur les maisons en fonction des catégories de trajectoires des prix et de la durée de détention des biens", x= "Nombre d'années de détention du bien" , y= "Montant de la plus-value")+ 
  labs(subtitle = "Les calculs ont été réalisés sur le carroyage 200m²\nLes montants des plus-value ont été calculés sur les maisons de 4 et 5 pièces.\nLes biens reçus en donation ou en héritage ont été exclus.\nLa dernière catégorie de trajectoires des prix est absente de l'analyse")+
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")
setwd("~/Projets/Plus_value/Plus_value")
ggsave("PV_Maisons_plot.png",plot= PV_Maisons_plot,  device = "png", width = 300, height = 320, units = "mm", dpi = 330)


PV_Maisons_plot<- mytest_PV_Maisons%>%
  filter(clusters!="9")%>%
  ggplot(., aes(Temps_Pocession, Value, fill=Type)) +
  geom_line()+
  geom_path(aes(color = Type))+
  scale_x_continuous(expand = c(0.01, 0),breaks = c(0,5,10,15,20,25,30)) +
  theme_tmd() +
  facet_grid(annee.x~clusters,scale="fix")+
  labs(title = "Montants des plus-values sur les maisons en fonction des catégories de trajectoires des prix et de la durée de détention des biens", x= "Nombre d'années de détention du bien" , y= "Montant de la plus-value")+ 
  labs(subtitle = "Les calculs ont été réalisés sur le carroyage 200m²\nLes montants des plus-value ont été calculés sur les maisons de 4 et 5 pièces.\nLes biens reçus en donation ou en héritage ont été exclus.\nLa dernière catégorie de trajectoires des prix est absente de l'analyse")+
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")
setwd("~/Projets/Plus_value/Plus_value")
ggsave("PV_Maisons_plot.pdf",plot= PV_Maisons_plot,  device = "pdf", width = 260, height = 400, units = "mm", dpi = 330)

