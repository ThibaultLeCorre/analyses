library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
#structure emploi-pop-acquereurs



data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","CSP_AC")]



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


table(mytest$acquereurs, useNA = "ifany")
mytest_Acq_BIEN<-mytest%>% 
  filter(!is.na(acquereurs)| is.na(QUALITE_AC))%>% 
  group_by(annee.x)%>% 
  summarise(Agriculteurs=length(which(acquereurs=="Agriculteurs"))/length(acquereurs)*100,
            Liberales=length(which(acquereurs=="Liberales"))/length(acquereurs)*100,
            CPIS=length(which(acquereurs=="CPIS"))/length(acquereurs)*100,
            Prof_intermediaires=length(which(acquereurs=="Prof_intermediaires"))/length(acquereurs)*100,
            Employes=length(which(acquereurs=="Employes"))/length(acquereurs)*100,
            Ouvriers=length(which(acquereurs=="Ouvriers"))/length(acquereurs)*100,
            retraites=length(which(acquereurs=="retraites"))/length(acquereurs)*100,
            autres_inactifs=length(which(acquereurs=="autres_inactifs"))/length(acquereurs)*100,
            Biens_publics_et_HLM=length(which(acquereurs=="Biens_publics_et_HLM"))/length(acquereurs)*100,
            Entreprise_Marchands_SCI=length(which(acquereurs=="Entreprise_Marchands_SCI"))/length(acquereurs)*100,
            SAFER=length(which(acquereurs=="SAFER"))/length(acquereurs)*100)%>%
  gather("Acquereurs","Value_marche", 2:12)



emploi<-CSP_emploi_CommunesFrance990812
str(emploi)
emploi<-emploi%>% 
  filter(DepCom>=75000 & DepCom<76000 & DepCom!=75056| DepCom>=77000 & DepCom<79000 | DepCom>=91000 & DepCom<96000 )%>%
  group_by(Annee)%>% 
  summarise(Agriculteurs=sum(Agriculteurs)/sum(Emplois)*100,
            Liberales=sum(Liberales)/sum(Emplois)*100,
            CPIS=sum(CPIS)/sum(Emplois)*100,
            Prof_intermediaires=sum(Prof_intermediaires)/sum(Emplois)*100,
            Employes=sum(Employes)/sum(Emplois)*100,
            Ouvriers=sum(Ouvriers)/sum(Emplois)*100)


#####
test<-gather(emploi, "variables", "Value_emploi", 2:ncol(emploi))%>%
  spread( Annee, Value_emploi)%>% 
  group_by(variables)%>%
  mutate(TCAM_9908=(((`2008`/`1999`)^(1/9)-(1))),
         TCAM_9913=(((`2013`/`2008`)^(1/5)-(1))))

test$`2000`<-test$`1999`*test$TCAM_9908 + test$`1999`
test$`2001`<-test$`2000`*test$TCAM_9908+ test$`2000`
test$`2002`<-test$`2001`*test$TCAM_9908+ test$`2001`
test$`2003`<-test$`2002`*test$TCAM_9908+ test$`2002`
test$`2004`<-test$`2003`*test$TCAM_9908+ test$`2003`
test$`2005`<-test$`2004`*test$TCAM_9908+ test$`2004`
test$`2006`<-test$`2005`*test$TCAM_9908+ test$`2005`
test$`2007`<-test$`2006`*test$TCAM_9908+ test$`2006`

test$`2009`<-  test$`2008`*test$TCAM_9913+test$`2008`
test$`2010`<-  test$`2009`*test$TCAM_9913+test$`2009`
test$`2011`<-  test$`2010`*test$TCAM_9913+test$`2010`
test$`2012`<-  test$`2011`*test$TCAM_9913+test$`2011`

test$`2012`*test$TCAM_9913+test$`2012`

test<-test[,c(1:3, 10:ncol(test))]
test<-gather(test, "annee", "Value_emploi", c(2:ncol(test)))
test$annee<-as.numeric(test$annee)
test<- left_join(test,mytest_Acq_BIEN,by = c("variables"="Acquereurs", "annee"="annee.x"))
test$rapport<- test$Value_marche / test$Value_emploi
test$Type<-"Rapport part des emplois / part du marché"

#########################


# data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","CSP_AC")]



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



mytest_effectifs_Acq_BIEN<-mytest%>% 
  filter(!is.na(acquereurs)| is.na(QUALITE_AC))%>% 
  group_by(annee.x)%>% 
  summarise(Agriculteurs=length(which(acquereurs=="Agriculteurs")),
            Liberales=length(which(acquereurs=="Liberales")),
            CPIS=length(which(acquereurs=="CPIS")),
            Prof_intermediaires=length(which(acquereurs=="Prof_intermediaires")),
            Employes=length(which(acquereurs=="Employes")),
            Ouvriers=length(which(acquereurs=="Ouvriers")),
            retraites=length(which(acquereurs=="retraites")),
            autres_inactifs=length(which(acquereurs=="autres_inactifs")),
            Biens_publics_et_HLM=length(which(acquereurs=="Biens_publics_et_HLM")),
            Entreprise_Marchands_SCI=length(which(acquereurs=="Entreprise_Marchands_SCI")),
            SAFER=length(which(acquereurs=="SAFER")))%>%
  gather("Acquereurs","Value_marche", 2:12)



emploi<-CSP_emploi_CommunesFrance990812
str(emploi)
emploi<-emploi%>% 
  filter(DepCom>=75000 & DepCom<76000 & DepCom!=75056| DepCom>=77000 & DepCom<79000 | DepCom>=91000 & DepCom<96000 )%>%
  group_by(Annee)%>% 
  summarise(Agriculteurs=sum(Agriculteurs),
            Liberales=sum(Liberales),
            CPIS=sum(CPIS),
            Prof_intermediaires=sum(Prof_intermediaires),
            Employes=sum(Employes),
            Ouvriers=sum(Ouvriers))


#####
test_effectifs<-gather(emploi, "variables", "Value_emploi", 2:ncol(emploi))%>%
  spread( Annee, Value_emploi)%>% 
  group_by(variables)%>%
  mutate(TCAM_9908=(((`2008`/`1999`)^(1/9)-(1))),
         TCAM_9913=(((`2013`/`2008`)^(1/5)-(1))))

test_effectifs$`2000`<-test_effectifs$`1999`*test_effectifs$TCAM_9908 + test_effectifs$`1999`
test_effectifs$`2001`<-test_effectifs$`2000`*test_effectifs$TCAM_9908+ test_effectifs$`2000`
test_effectifs$`2002`<-test_effectifs$`2001`*test_effectifs$TCAM_9908+ test_effectifs$`2001`
test_effectifs$`2003`<-test_effectifs$`2002`*test_effectifs$TCAM_9908+ test_effectifs$`2002`
test_effectifs$`2004`<-test_effectifs$`2003`*test_effectifs$TCAM_9908+ test_effectifs$`2003`
test_effectifs$`2005`<-test_effectifs$`2004`*test_effectifs$TCAM_9908+ test_effectifs$`2004`
test_effectifs$`2006`<-test_effectifs$`2005`*test_effectifs$TCAM_9908+ test_effectifs$`2005`
test_effectifs$`2007`<-test_effectifs$`2006`*test_effectifs$TCAM_9908+ test_effectifs$`2006`

test_effectifs$`2009`<-  test_effectifs$`2008`*test_effectifs$TCAM_9913+test_effectifs$`2008`
test_effectifs$`2010`<-  test_effectifs$`2009`*test_effectifs$TCAM_9913+test_effectifs$`2009`
test_effectifs$`2011`<-  test_effectifs$`2010`*test_effectifs$TCAM_9913+test_effectifs$`2010`
test_effectifs$`2012`<-  test_effectifs$`2011`*test_effectifs$TCAM_9913+test_effectifs$`2011`

test_effectifs$`2012`*test_effectifs$TCAM_9913+test_effectifs$`2012`

test_effectifs<-test_effectifs[,c(1:3, 10:ncol(test_effectifs))]
test_effectifs<-gather(test_effectifs, "annee", "Value_emploi", c(2:ncol(test_effectifs)))
test_effectifs$annee<-as.numeric(test_effectifs$annee)
test_effectifs<- left_join(test_effectifs,mytest_effectifs_Acq_BIEN,by = c("variables"="Acquereurs", "annee"="annee.x"))
test_effectifs$rapport<- (test_effectifs$Value_marche / test_effectifs$Value_emploi)*1000
test_effectifs$Type<-"Nombre d'acquéreurs pour 1000 emplois"
#############

Comparaison_emploi_marche<-rbind(test,test_effectifs)
Comparaison_emploi_marche<-Comparaison_emploi_marche%>%
  filter(variables!="Agriculteurs")
######
specificCol <- c("Agriculteurs" = "#503D3F",
                 "Liberales" = "#861388",
                 "CPIS"= "#067BC2" ,
                 "Prof_intermediaires"="#5FAD41",
                 "Employes"= "#FF8811",
                 "Ouvriers"="#FF1D15",
                 "retraites"= "#77878B",
                 "autres_inactifs"="#8D775F",
                 "Entreprise_Marchands_SCI"= "#E9DF00",
                 "Biens_publics_et_HLM"= "black",
                 "SAFER"="grey")





Comparaison_emploi_marche_plot<-
  Comparaison_emploi_marche%>%
  filter(variables!="Liberales")%>%
  ggplot(., aes(annee, rapport, group=variables)) +
  geom_line(aes(color = variables))+
  geom_line(aes(color =variables),size=1,show.legend =F)+
  geom_line(aes(color = variables,linetype = "dashed"), show.legend =F) + 
  scale_color_manual(values = specificCol ) +
  # geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  theme_tmd() +
  facet_wrap(~Type, ncol = 2,scale="free")+
  labs(title = "Comparaison entre la structure des emplois franciliens et celle du marché selon les CSP", x= NULL, y= NULL)+ 
  labs(subtitle = "lecture : Sur le graphique de droite un rapport égal à 1 signifie une équivalence parfaite entre la structure des emplois par CSP et celle des acquéreurs\n
       " , x= NULL, y= NULL)+
  labs(caption = "Sources : Insee, Recensements de la population 1999, 2008 et 2013\nEchantillon redressé de la base BIEN. Réalisation : Thibault Le Corre, 2017")


setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
ggsave("Comparaison_emploi_marche_plot.png",plot= Comparaison_emploi_marche_plot,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)
