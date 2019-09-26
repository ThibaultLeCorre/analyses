library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
##courbe de comby


data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","REQ_PRIX","QUALITE_AC","CSP_AC", "PRESCREDIT", "MTCRED", "ANNAIS_AC")]

mytest$acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM",
                           ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                  ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                         ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                                       ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM", "particulier_ou_na"))))))

mytest$acquereurs<- ifelse(mytest$CSP_AC == 10 & mytest$acquereurs == "particulier_ou_na", "Agriculteurs",
                           ifelse(mytest$CSP_AC >= 20 & mytest$CSP_AC < 30 & mytest$acquereurs == "particulier_ou_na", "Liberales",
                                  ifelse(mytest$CSP_AC >= 30 & mytest$CSP_AC < 40 & mytest$acquereurs == "particulier_ou_na", "CPIS",
                                         ifelse(mytest$CSP_AC >= 40 & mytest$CSP_AC < 50 & mytest$acquereurs == "particulier_ou_na", "Prof_intermediaires",
                                                ifelse(mytest$CSP_AC >= 50 & mytest$CSP_AC < 60 & mytest$acquereurs == "particulier_ou_na", "Employes",
                                                       ifelse(mytest$CSP_AC >= 60 & mytest$CSP_AC < 70 & mytest$acquereurs == "particulier_ou_na", "Ouvriers",
                                                              ifelse(mytest$CSP_AC >= 70 & mytest$CSP_AC < 80 & mytest$acquereurs == "particulier_ou_na", "retraites",
                                                                     ifelse(mytest$CSP_AC == 80 & mytest$acquereurs == "particulier_ou_na", "autres_inactifs", mytest$acquereurs))))))))



mytest$Age_acq = mytest$annee.x-mytest$ANNAIS_AC

mytest$fourchette_age_acq<-mytest$Age_acq
mytest$fourchette_age_acq<- ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<30 & mytest$acquereurs!="retraites", "[18,30[",
                                   ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<50 & mytest$acquereurs!="retraites", "[30,50[",
                                          ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=50 | !is.na(mytest$CSP_AC) & mytest$acquereurs=="retraites", "[50+",NA)))







Acquereurs_Pourc_Credit<-mytest%>% 
  filter(!is.na(acquereurs))%>% 
  group_by(annee.x, acquereurs, fourchette_age_acq)%>% 
  summarise(Pourcentage_sans_credit=length(which(PRESCREDIT=="N"))/length(acquereurs)*100)

Acquereurs_MedCredit<-mytest%>% 
  filter(!is.na(acquereurs)& MTCRED>1000)%>%
  group_by(annee.x, acquereurs, fourchette_age_acq)%>%
  summarise(Credit=median(MTCRED))
Acquereurs_MedCredit$Type<-"Median"
  
Acquereurs_MoyCredit<-mytest%>% 
  filter(!is.na(acquereurs)& MTCRED>1000)%>%
  group_by(annee.x, acquereurs,fourchette_age_acq)%>%
  summarise(Credit=mean(MTCRED))
Acquereurs_MoyCredit$Type<-"Moyenne"
  
Acquereurs_Credit<- rbind(Acquereurs_MedCredit,Acquereurs_MoyCredit )

Acquereurs_Credit<-left_join(Acquereurs_Credit,Acquereurs_Pourc_Credit, by = c("annee.x","acquereurs","fourchette_age_acq"))


Acquereurs_Credit<-Acquereurs_Credit%>%
  filter(acquereurs!="Agriculteurs" & acquereurs!="SAFER" & acquereurs!="Entreprise_Marchands_SCI" & acquereurs!="Biens_publics_et_HLM", !is.na(fourchette_age_acq))
unique(Acquereurs_Credit$acquereurs)
options(scipen=999)

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


Acquereurs_Credit_plot_Select_catego<- 
  Acquereurs_Credit%>%
  filter(acquereurs != "autres_inactifs" & acquereurs != "Liberales" )%>%
  ggplot(., aes(Pourcentage_sans_credit, Credit, group=acquereurs)) +
  geom_path(aes(color = acquereurs))+
  geom_path(aes(color =acquereurs),size=2,alpha=0.2,show.legend =F)+
  geom_path(aes(color = acquereurs,linetype = "dashed"), show.legend =F) +  
  geom_point(size=0) +geom_text(aes(label = ifelse(annee.x==1999 | annee.x==2003 | annee.x==2008 |annee.x==2012,annee.x, "")), size=2.5, angle=45, family = "Century Gothic", hjust=0,vjust=1, check_overlap = TRUE)+
  # geom_point(size=0.75)+
  scale_color_manual(values = specificCol )+
  scale_x_continuous() +
  scale_y_continuous(breaks = c(50000,100000, 150000,200000,250000,300000)) +
  theme_tmd() +
  # scale_y_log10() +
  facet_grid(fourchette_age_acq~Type,scale="fix")+
  labs(title = "Expansion et rétraction du crédit immobilier", x= "Pourcentage d'achats sans crédit", y= "Montant des prêts")+ 
  labs(subtitle = "Lecture : en 2012, 30% des CPIS entre 30 et 50 ans achètent un bien immobilier sans prêt immobilier.\nPour ceux qui contractent un prêt celui ci est en moyenne de 300 000 euros et de 250 000 euros en médiane.")+
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")

library(extrafont)
loadfonts()
fonts()
setwd("~/Projets/Credit/Credit")
ggsave("Acquereurs_Credit_plot_Select_catego.pdf",plot= Acquereurs_Credit_plot_Select_catego, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
ggsave("Acquereurs_Credit_plot_Select_catego.png",plot= Acquereurs_Credit_plot_Select_catego, device = "png", width = 380, height = 180, units = "mm", dpi = 330)

###LTV par CSP

mytest<-data_redresse1570533transacs[,c("ID","annee.x","REQ_PRIX","QUALITE_AC","CSP_AC", "PRESCREDIT", "MTCRED", "ANNAIS_AC")]

mytest$acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM",
                           ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                  ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                         ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                                       ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM", "particulier_ou_na"))))))

mytest$acquereurs<- ifelse(mytest$CSP_AC == 10 & mytest$acquereurs == "particulier_ou_na", "Agriculteurs",
                           ifelse(mytest$CSP_AC >= 20 & mytest$CSP_AC < 30 & mytest$acquereurs == "particulier_ou_na", "Liberales",
                                  ifelse(mytest$CSP_AC >= 30 & mytest$CSP_AC < 40 & mytest$acquereurs == "particulier_ou_na", "CPIS",
                                         ifelse(mytest$CSP_AC >= 40 & mytest$CSP_AC < 50 & mytest$acquereurs == "particulier_ou_na", "Prof_intermediaires",
                                                ifelse(mytest$CSP_AC >= 50 & mytest$CSP_AC < 60 & mytest$acquereurs == "particulier_ou_na", "Employes",
                                                       ifelse(mytest$CSP_AC >= 60 & mytest$CSP_AC < 70 & mytest$acquereurs == "particulier_ou_na", "Ouvriers",
                                                              ifelse(mytest$CSP_AC >= 70 & mytest$CSP_AC < 80 & mytest$acquereurs == "particulier_ou_na", "retraites",
                                                                     ifelse(mytest$CSP_AC == 80 & mytest$acquereurs == "particulier_ou_na", "autres_inactifs", mytest$acquereurs))))))))



mytest$Age_acq = mytest$annee.x-mytest$ANNAIS_AC

mytest$fourchette_age_acq<-mytest$Age_acq
mytest$fourchette_age_acq<- ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<30 & mytest$acquereurs!="retraites", "[18,30[",
                                   ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<50 & mytest$acquereurs!="retraites", "[30,50[",
                                          ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=50 | !is.na(mytest$CSP_AC) & mytest$acquereurs=="retraites", "[50+",NA)))

Acquereurs_LTV<-mytest%>% 
  filter(!is.na(acquereurs) & MTCRED>1000)%>% 
  group_by(annee.x, acquereurs, fourchette_age_acq)%>% 
  summarise(LTV=sum(MTCRED)/sum(REQ_PRIX)*100)

Acquereurs_LTV<-Acquereurs_LTV%>%
  filter(acquereurs!="Agriculteurs" & acquereurs!="SAFER" & acquereurs!="Entreprise_Marchands_SCI" & acquereurs!="Biens_publics_et_HLM", !is.na(fourchette_age_acq))
unique(Acquereurs_LTV$acquereurs)
options(scipen=999)

Acquereurs_LTV_plot<-Acquereurs_LTV%>%
  filter(acquereurs != "autres_inactifs" & acquereurs != "Liberales" )%>%
  ggplot(., aes(annee.x, LTV, group=acquereurs)) +
  geom_path(aes(color = acquereurs))+
  geom_path(aes(color =acquereurs),size=2,alpha=0.2,show.legend =F)+
  geom_path(aes(color = acquereurs,linetype = "dashed"), show.legend =F) +  
  # geom_point(size=0.75)+
  scale_color_manual(values = specificCol )+
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  theme_tmd() +
  facet_grid(~fourchette_age_acq,scale="free")+
  labs(title = "Loan-to-Value par CSP", x= "Année", y= "Loan-to-Value moyen")+ 
  labs(subtitle = "Lecture : en 2003, le Loan-to-value des acquéreurs employés entre 18 et 30 ans était en moyenne de 91%.")+
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")

library(extrafont)
loadfonts()
fonts()
setwd("~/Projets/Credit/Credit")
ggsave("Acquereurs_LTV_plot.pdf",plot= Acquereurs_LTV_plot, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
ggsave("Acquereurs_LTV_plot.png",plot= Acquereurs_LTV_plot, device = "png", width = 380, height = 180, units = "mm", dpi = 330)

####################################

Acquereurs_Sans_Credit<-mytest%>% 
  filter(!is.na(acquereurs))%>% 
  group_by(annee.x, acquereurs, fourchette_age_acq)%>% 
  summarise(Nombre_sans_credit=length(which(PRESCREDIT=="N")))

Acquereurs_Sans_Credit<-Acquereurs_Sans_Credit%>%
  filter(acquereurs!="Agriculteurs" & acquereurs!="SAFER" & acquereurs!="Entreprise_Marchands_SCI" & acquereurs!="Biens_publics_et_HLM", !is.na(fourchette_age_acq))
unique(Acquereurs_Sans_Credit$acquereurs)

Acquereurs_Sans_Credit_plot<-
  Acquereurs_Sans_Credit%>%
  filter(acquereurs != "autres_inactifs" & acquereurs != "Liberales" )%>%
  ggplot(., aes(annee.x, Nombre_sans_credit, group=acquereurs)) +
  geom_path(aes(color = acquereurs))+
  geom_path(aes(color =acquereurs),size=2,alpha=0.2,show.legend =F)+
  geom_path(aes(color = acquereurs,linetype = "dashed"), show.legend =F) +  
  # geom_point(size=0.75)+
  scale_color_manual(values = specificCol )+
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  theme_tmd() +
  # scale_y_log10() +
  facet_wrap(~fourchette_age_acq,scale="fix")+
  labs(title = "Nombre d'achats sans crédit immobilier", x= "Année", y= "Nombre de transactions par auto-financement")+ 
  labs(subtitle = "Lecture : en 2003, 1658 employés entre 30 et 50 ans ont acheté un bien par auto-financement.")+
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")

library(skimr)
skim(Acquereurs_Sans_Credit)
# library(extrafont)
loadfonts()
fonts()
setwd("~/Projets/Credit/Credit")
ggsave("Acquereurs_Sans_Credit_plot.pdf",plot= Acquereurs_Sans_Credit_plot, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
setwd("~/Projets/Credit/Credit")
ggsave("Acquereurs_Sans_Credit_plot.png",plot= Acquereurs_Sans_Credit_plot, device = "png", width = 380, height = 180, units = "mm", dpi = 330)


######

Acquereurs_Apport<-mytest%>% 
  filter(!is.na(acquereurs) & MTCRED>1000)%>% 
  group_by(annee.x, acquereurs, fourchette_age_acq)%>% 
  summarise(Apport=mean(REQ_PRIX-MTCRED))

Acquereurs_Apport<-Acquereurs_Apport%>%
  filter(acquereurs!="Agriculteurs" & acquereurs!="SAFER" & acquereurs!="Entreprise_Marchands_SCI" & acquereurs!="Biens_publics_et_HLM", !is.na(fourchette_age_acq))
unique(Acquereurs_Apport$acquereurs)
options(scipen=999)

Acquereurs_Apport_plot<-Acquereurs_Apport%>%
  filter(acquereurs != "autres_inactifs" & acquereurs != "Liberales" )%>%
  ggplot(., aes(annee.x, Apport, group=acquereurs)) +
  geom_path(aes(color = acquereurs))+
  geom_path(aes(color =acquereurs),size=2,alpha=0.2,show.legend =F)+
  geom_path(aes(color = acquereurs,linetype = "dashed"), show.legend =F) +  
  # geom_point(size=0.75)+
  scale_color_manual(values = specificCol )+
  scale_y_continuous(breaks = c(2500,5000, 7500,10000,12500,15000,20000,25000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000)) +
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  theme_tmd() +
  facet_grid(~fourchette_age_acq,scale="free")+
  labs(title = "Montant moyen de l'apport par CSP", x= "Année", y= "Montant moyen de l'apport")+ 
  labs(subtitle = "Lecture : en 2003, pour les ménages employés entre 18 et 30 ans qui ont acquis un bien par crédit, l'apport moyen est de 9545 euros." , x= NULL, y= NULL)+
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")
# 
# library(extrafont)
loadfonts()
fonts()
setwd("~/Projets/Credit/Credit")
ggsave("Acquereurs_Apport_plot.pdf",plot= Acquereurs_Apport_plot, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
ggsave("Acquereurs_Apport_plot.png",plot= Acquereurs_Apport_plot, device = "png", width = 380, height = 180, units = "mm", dpi = 330)
