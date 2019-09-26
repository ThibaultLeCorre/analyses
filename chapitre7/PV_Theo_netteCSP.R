library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(reshape2)
library(ggridges)
library(viridis)


# data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","REQ_PRIX","QUALITE_AC","CSP_AC", "QUALITE_VE","CSP_VE","REQTYPBIEN", "REQ_VALUE","DATMUTPREC", "TYPMUTPREC", "PXMUTPREC")]
DBSCAN_results_table_Promoteurs <- read.csv("~/Projets/DBSCAN/DBSAN/DBSCAN_results_table_Promoteurs.csv", sep="", stringsAsFactors=FALSE)
mytest$ID<-as.numeric(mytest$ID)
mytest<-left_join(mytest,DBSCAN_results_table_Promoteurs, by="ID")
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


mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" & !is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM",
                         ifelse(mytest$QUALITE_VE== "EN"& !is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                ifelse(mytest$QUALITE_VE== "PR"& !is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                       ifelse(mytest$QUALITE_VE== "SA"& !is.na(mytest$QUALITE_VE),"SAFER",
                                              ifelse(mytest$QUALITE_VE== "SC"& !is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                                     ifelse(mytest$QUALITE_VE== "SO"& !is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM", "particulier_ou_na"))))))


mytest$Vendeurs<- ifelse(mytest$CSP_VE == 10 & mytest$Vendeurs == "particulier_ou_na", "Agriculteurs",
                         ifelse(mytest$CSP_VE >= 20 & mytest$CSP_VE < 30 & mytest$Vendeurs == "particulier_ou_na", "Liberales",
                                ifelse(mytest$CSP_VE >= 30 & mytest$CSP_VE < 40 & mytest$Vendeurs == "particulier_ou_na", "CPIS",
                                       ifelse(mytest$CSP_VE >= 40 & mytest$CSP_VE < 50 & mytest$Vendeurs == "particulier_ou_na", "Prof_intermediaires",
                                              ifelse(mytest$CSP_VE >= 50 & mytest$CSP_VE < 60 & mytest$Vendeurs == "particulier_ou_na", "Employes",
                                                     ifelse(mytest$CSP_VE >= 60 & mytest$CSP_VE < 70 & mytest$Vendeurs == "particulier_ou_na", "Ouvriers",
                                                            ifelse(mytest$CSP_VE >= 70 & mytest$CSP_VE < 80 & mytest$Vendeurs == "particulier_ou_na", "retraites",
                                                                   ifelse(mytest$CSP_VE == 80 & mytest$Vendeurs == "particulier_ou_na", "autres_inactifs", mytest$Vendeurs))))))))

mytest$Vendeurs<- ifelse(mytest$cluster>=1 & !is.na(mytest$cluster),"Promoteurs", mytest$Vendeurs)
table(mytest$Vendeurs, useNA = "ifany")

###
mytest<-mytest%>%
  filter(Vendeurs!= "Promoteurs" & Vendeurs!= "Agriculteurs",!is.na(DATMUTPREC))
mytest$Annee_MUTPREC<- as.numeric(substr(mytest$DATMUTPREC, start = 7, stop = 10))

mytest$Annee_MUTPREC<- ifelse(mytest$Annee_MUTPREC<1000,as.numeric(substr(mytest$DATMUTPREC, start = 6, stop = 10)),mytest$Annee_MUTPREC)
str(mytest)

mytest$Temps_Pocession<- mytest$annee.x - mytest$Annee_MUTPREC
mytest<-mytest%>%
  filter(Temps_Pocession>=1 & Temps_Pocession<150)

mytest$TYPMUTPREC_2<- ifelse(mytest$TYPMUTPREC == "A","Acquisition",
                             ifelse(mytest$TYPMUTPREC == "D" | mytest$TYPMUTPREC == "S" ,"Donation ou héritage", NA))



mytest_PV<-mytest%>%
  filter(annee.x==1999| annee.x==2008|annee.x==2012, !is.na(REQ_VALUE), TYPMUTPREC_2=="Acquisition",
         Vendeurs!="Entreprise_Marchands_SCI" &  Vendeurs!="Biens_publics_et_HLM" &  Vendeurs!="Liberales")%>%
  group_by(annee.x,Vendeurs,Temps_Pocession)%>%
  summarise(Moyenne_PV=mean(REQ_VALUE),
            Median_PV=median(REQ_VALUE),
            First_quartile = quantile(REQ_VALUE, probs=0.25),
            Last_quartile = quantile(REQ_VALUE, probs=0.75),
            nombre=n())%>%
  filter(Temps_Pocession<=30 )%>%
  gather("Type","Value", 4:7)