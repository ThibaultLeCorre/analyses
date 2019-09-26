
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggridges)
library(viridis)
##courbe de comby
###



# data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest_AC<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","CSP_AC","ANNAIS_AC")]



mytest_AC$Acteur<- ifelse(mytest_AC$QUALITE_AC == "AD"  &!is.na(mytest_AC$QUALITE_AC),"Biens_publics_et_HLM",
                           ifelse(mytest_AC$QUALITE_AC== "EN"& !is.na(mytest_AC$QUALITE_AC),"Entreprise_Marchands_SCI",
                                  ifelse(mytest_AC$QUALITE_AC== "PR"&!is.na(mytest_AC$QUALITE_AC),"Entreprise_Marchands_SCI",
                                         ifelse(mytest_AC$QUALITE_AC== "SA"&!is.na(mytest_AC$QUALITE_AC),"SAFER",
                                                ifelse(mytest_AC$QUALITE_AC== "SC"&!is.na(mytest_AC$QUALITE_AC),"Entreprise_Marchands_SCI",
                                                       ifelse(mytest_AC$QUALITE_AC== "SO"&!is.na(mytest_AC$QUALITE_AC),"Biens_publics_et_HLM", "particulier_ou_na"))))))



mytest_AC$Acteur<- ifelse(mytest_AC$CSP_AC == 10 & mytest_AC$Acteur == "particulier_ou_na", "Agriculteurs",
                           ifelse(mytest_AC$CSP_AC >= 20 & mytest_AC$CSP_AC < 30 & mytest_AC$Acteur == "particulier_ou_na", "Com_art_Chef_entreprises",
                                  ifelse(mytest_AC$CSP_AC >= 30 & mytest_AC$CSP_AC < 40 & mytest_AC$Acteur == "particulier_ou_na", "CPIS",
                                         ifelse(mytest_AC$CSP_AC >= 40 & mytest_AC$CSP_AC < 50 & mytest_AC$Acteur == "particulier_ou_na", "Prof_intermediaires",
                                                ifelse(mytest_AC$CSP_AC >= 50 & mytest_AC$CSP_AC < 60 & mytest_AC$Acteur == "particulier_ou_na", "Employes",
                                                       ifelse(mytest_AC$CSP_AC >= 60 & mytest_AC$CSP_AC < 70 & mytest_AC$Acteur == "particulier_ou_na", "Ouvriers",
                                                              ifelse(mytest_AC$CSP_AC >= 70 & mytest_AC$CSP_AC < 80 & mytest_AC$Acteur == "particulier_ou_na", "retraites",
                                                                     ifelse(mytest_AC$CSP_AC == 80 & mytest_AC$Acteur == "particulier_ou_na", "autres_inactifs", mytest_AC$Acteur))))))))


mytest_VE<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_VE","CSP_VE","ANNAIS_VE")]

mytest_VE$Acteur<- ifelse(mytest_VE$QUALITE_VE == "AD" & !is.na(mytest_VE$QUALITE_VE),"Biens_publics_et_HLM",
                         ifelse(mytest_VE$QUALITE_VE== "EN"& !is.na(mytest_VE$QUALITE_VE),"Entreprise_Marchands_SCI",
                                ifelse(mytest_VE$QUALITE_VE== "PR"& !is.na(mytest_VE$QUALITE_VE),"Entreprise_Marchands_SCI",
                                       ifelse(mytest_VE$QUALITE_VE== "SA"& !is.na(mytest_VE$QUALITE_VE),"SAFER",
                                              ifelse(mytest_VE$QUALITE_VE== "SC"& !is.na(mytest_VE$QUALITE_VE),"Entreprise_Marchands_SCI",
                                                     ifelse(mytest_VE$QUALITE_VE== "SO"& !is.na(mytest_VE$QUALITE_VE),"Biens_publics_et_HLM", "particulier_ou_na"))))))


mytest_VE$Acteur<- ifelse(mytest_VE$CSP_VE == 10 & mytest_VE$Acteur == "particulier_ou_na", "Agriculteurs",
                         ifelse(mytest_VE$CSP_VE >= 20 & mytest_VE$CSP_VE < 30 & mytest_VE$Acteur == "particulier_ou_na", "Com_art_Chef_entreprises",
                                ifelse(mytest_VE$CSP_VE >= 30 & mytest_VE$CSP_VE < 40 & mytest_VE$Acteur == "particulier_ou_na", "CPIS",
                                       ifelse(mytest_VE$CSP_VE >= 40 & mytest_VE$CSP_VE < 50 & mytest_VE$Acteur == "particulier_ou_na", "Prof_intermediaires",
                                              ifelse(mytest_VE$CSP_VE >= 50 & mytest_VE$CSP_VE < 60 & mytest_VE$Acteur == "particulier_ou_na", "Employes",
                                                     ifelse(mytest_VE$CSP_VE >= 60 & mytest_VE$CSP_VE < 70 & mytest_VE$Acteur == "particulier_ou_na", "Ouvriers",
                                                            ifelse(mytest_VE$CSP_VE >= 70 & mytest_VE$CSP_VE < 80 & mytest_VE$Acteur == "particulier_ou_na", "retraites",
                                                                   ifelse(mytest_VE$CSP_VE == 80 & mytest_VE$Acteur == "particulier_ou_na", "autres_inactifs", mytest_VE$Acteur))))))))


mytest_AC$Age= mytest_AC$annee.x-mytest_AC$ANNAIS_AC
mytest_VE$Age = mytest_VE$annee.x-mytest_VE$ANNAIS_VE

mytest_AC$Type<-"Acquereurs"
mytest_VE$Type<-"Vendeurs"



mytest_Age<- rbind(mytest_AC[,c(2,6,7,8)], mytest_VE[,c(2,6,7,8)])

mytest_Age<-mytest_Age%>% 
  filter(!is.na(Acteur), Acteur!="SAFER" & Acteur!="Biens_publics_et_HLM" & Acteur!="Agriculteurs" &  Acteur!="Entreprise_Marchands_SCI" )%>% 
  group_by(annee.x)

mytest_Age_test<-mytest_Age%>%
  filter(Type=="Acquereurs" & annee.x==2009)
  
table_age<-as.data.frame.matrix(table(mytest_Age_test$Age,mytest_Age_test$Acteur))
table_age_2<-as.data.frame.matrix(table(mytest_Age_test$Age,mytest_Age_test$Acteur))

Distribution_CSP_age_JoyDivision<-mytest_Age%>%
  filter(Type=="Acquereurs")%>%
ggplot(., aes(Age, Acteur)) +
  geom_density_ridges2(scale = 2, calc_ecdf = TRUE,
                       quantiles = 4, quantile_lines = FALSE)+
  scale_x_continuous(expand = c(0.01, 0),breaks = c(20, 30,40, 50,60,70,80,90)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_color_manual(values = specificCol ) +
  theme_tmd() +
  facet_wrap(~annee.x,scale="fix")+
  labs(title = "La distribution des acquéreurs par âge pour chaque catégorie sociale ", x= "Age des acquéreurs", y= NULL)+ 
  labs(caption = "Sources : Echantillon redressé de la base BIEN. Réalisation : Thibault Le Corre, 2017")


setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
ggsave("Distribution_CSP_age_JoyDivision.png",plot= Distribution_CSP_age_JoyDivision,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)
