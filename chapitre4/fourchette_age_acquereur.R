#Age_actifs_acquereurs_vendeurs
library(dplyr)

library(tidyr)


mytest<-data_redresse_code_promo_2[,c("ID","annee.x","PRESCREDIT","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","cluster.y","X.x","Y.x","ANNAIS_AC","ANNAIS_VE")]
# table(data_redresse_code_promo_2$typeVar, data_redresse_code_promo_2$CSP_VE)

mytest$renseignement_credit<-ifelse(is.na(mytest$PRESCREDIT),"Crédit sécurisé par caution attendu",mytest$PRESCREDIT)
mytest$renseignement_credit<- ifelse(mytest$renseignement_credit=="O","Crédit hypothécaire",
                                             ifelse(mytest$renseignement_credit=="0","Crédit hypothécaire",
                                                    ifelse(mytest$renseignement_credit=="N","Pas de crédit à l'achat",mytest$renseignement_credit)))

mytest$Acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Administration",
                           ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise",
                                  ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Marchands de biens",
                                         ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"SCI",
                                                       ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"HLM", "particulier_ou_na"))))))


mytest$Acquereurs<- ifelse(mytest$CSP_AC>=1 & mytest$CSP_AC<=69 &mytest$Acquereurs == "particulier_ou_na","Actifs",
                           ifelse(mytest$CSP_AC>=70 & mytest$CSP_AC<=90 & mytest$Acquereurs =="particulier_ou_na", "retraites_inactifs", 
                                  ifelse(is.na(mytest$CSP_AC) & mytest$Acquereurs=="particulier_ou_na",NA,mytest$Acquereurs)))

table(mytest$Acquereurs, useNA = "ifany")
# 
# mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"Administration",
#                          ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"Entreprise",
#                                 ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"Marchands de biens",
#                                        ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"SAFER",
#                                               ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"SCI",
#                                                      ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"HLM", "particulier_ou_na"))))))
# 
# 
# mytest$Vendeurs<- ifelse(mytest$CSP_VE>=1 & mytest$CSP_VE<=69& mytest$Vendeurs == "particulier_ou_na","Actifs",
#                          ifelse(mytest$CSP_VE>=70 & mytest$CSP_VE<=90 & mytest$Vendeurs =="particulier_ou_na", "retraites_inactifs", 
#                                 ifelse(is.na(mytest$CSP_VE) & mytest$Vendeurs=="particulier_ou_na",NA,mytest$Vendeurs)))
# 
# mytest$Vendeurs<- ifelse(mytest$cluster.y>=1 & !is.na(mytest$cluster.y),"Promoteurs", mytest$Vendeurs)
# table(mytest$Vendeurs, useNA = "ifany")

mytest<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(CSP_AC) )


####Fourchette age
mytest$Age_acq = mytest$annee.x-mytest$ANNAIS_AC
# mytest$Age_ve = mytest$annee.x-mytest$ANNAIS_VE

mytest$fourchette_age_acq<-mytest$Age_acq 
# mytest$fourchette_age_acq<- ifelse(mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<30, "[18,30[",
#                                         ifelse(mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<40, "[30,40[",
#                                                ifelse(mytest$fourchette_age_acq>=40 & mytest$fourchette_age_acq<50, "[40,50[",
#                                                       ifelse(mytest$fourchette_age_acq>=50 & mytest$fourchette_age_acq<60, "[50,60[",
#                                                              ifelse(mytest$fourchette_age_acq>=60 , "[60+",mytest$Age_acq)))))
# 


mytest$fourchette_age_acq<- ifelse(mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<25, "[18,25[",
                                   ifelse(mytest$fourchette_age_acq>=25 & mytest$fourchette_age_acq<30, "[25,30[",
                                   ifelse(mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<35, "[30,35[",
                                          ifelse(mytest$fourchette_age_acq>=35 & mytest$fourchette_age_acq<40, "[35,40[",
                                                 ifelse(mytest$fourchette_age_acq>=40 & mytest$fourchette_age_acq<45, "[40,45[",
                                          ifelse(mytest$fourchette_age_acq>=45 & mytest$fourchette_age_acq<50, "[45,50[",
                                                 ifelse(mytest$fourchette_age_acq>=50 & mytest$fourchette_age_acq<55, "[50,55[",
                                                        ifelse(mytest$fourchette_age_acq>=55 & mytest$fourchette_age_acq<60, "[55,60[",
                                                        ifelse(mytest$fourchette_age_acq>=60 , "[60+",mytest$Age_acq)))))))))

# 
# 
# mytest$fourchette_age_ve<-mytest$Age_ve 
# mytest$fourchette_age_ve<- ifelse(mytest$fourchette_age_ve>=18 & mytest$fourchette_age_ve<30, "[18,30[",
#                                        ifelse(mytest$fourchette_age_ve>=30 & mytest$fourchette_age_ve<40, "[30,40[",
#                                               ifelse(mytest$fourchette_age_ve>=40 & mytest$fourchette_age_ve<50, "[40,50[",
#                                                      ifelse(mytest$fourchette_age_ve>=50 & mytest$fourchette_age_ve<60, "[50,60[",
#                                                             ifelse(mytest$fourchette_age_ve>=60 , "[60+",mytest$Age_ve)))))


mytest<-mytest %>%
 filter(Acquereurs=="Actifs") %>%
  group_by(annee.x,fourchette_age_acq)  %>%
  summarise(n_acq=n()) %>%
  spread(fourchette_age_acq,n_acq,fill = 0)%>%
  select(c(1:11))%>%
  gather(key = "Age_acquereur", value= "Value_Ageacq", c(3:11))
         # | Acquereurs=="retraites_inactifs" )
pyramide_age_2<-pyramide_age_2%>% gather(key = "Age_acquereur", value= "Value_Pyramide", c(2:10))
mytest_2<-left_join(mytest,pyramide_age_2,by= c("annee.x"="date","Age_acquereur" ) )

mytest_2$rapport<- mytest_2$Value_Ageacq/mytest_2$Value_Pyramide *1000


mytest<-as.data.frame(mytest)
mytest_2<-as.data.frame(mytest_2)
sum(mytest$Value_Ageacq)
library(ggplot2)
fourchette_age_acquereurs<-ggplot(mytest, aes(x=Age_acquereur, y=Value_Ageacq)) +
  geom_bar(stat = "identity", aes(fill=Acquereurs))+
  theme_tmd() +
  facet_wrap(~annee.x)+
  labs(title = "Volume des achats selon l'âge des ménages",
       subtitle = "Calcul réalisé par année sur les effectifs redressés des transactions de type ménage. n= 1 390 728", x= "Fourchettes d'âge", y= "Effectifs")+
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")


# fourchette_age_acquereurs_credit<-ggplot(mytest, aes(x=Age_acquereur, y=Value_Ageacq)) +
#   geom_bar(stat = "identity", aes(fill=renseignement_credit ))+
#   theme_tmd() +
#   facet_wrap(~annee.x)+
#   labs(title = "Volume des achats selon l'âge des ménages", 
#        subtitle = "Calcul réalisé par année sur les effectifs redressés des transactions de type ménage. n= 1 390 728", x= "Fourchettes d'âge", y= "Effectifs")+ 
#   labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")


rapport_fourchette_age_1000hba<-ggplot(mytest_2, aes(x=Age_acquereur, y=rapport)) +
  geom_bar(stat = "identity")+
  theme_tmd() +
  facet_wrap(~annee.x)+
  labs(title = "Nombre d'achats selon l'âge des ménages pour 1000 habitants en Île-de-France",
       subtitle = "Calcul réalisé par année sur les effectifs redressés des transactions de type ménage. n= 1 390 728\nLecture : En 2005, le volume des biens achetés par un ménage avec comme acquéreur un individu entre 30 et 35 ans\nreprésente environ 30 acquisitions pour 1000 habitants de la population résidente en Île-de-France pour cette même classe d'âge", x= "Fourchettes d'âge", y= "Nombre d'achats pour 1000 habitants")+
  labs(caption = "Sources : échantillon BD BIEN, Insee ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")


setwd("~/Sauvegarde_figures")
library(extrafont)
loadfonts()
fonts() 
ggsave("rapport_fourchette_age_1000hba.pdf",plot= rapport_fourchette_age_1000hba,setwd("~/Sauvegarde_figures"), device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)


#####Situation matrimonaiel
mytest$Couple<- ifelse(mytest$SITMAT_AC == "M" | mytest$SITMAT_AC == "P" | mytest$SITMAT_AC == "R"| mytest$SITMAT_AC == "V", "en_couple_dont_veuvage",
                            ifelse(mytest$SITMAT_AC == "D", "Divorce", 
                                   ifelse(mytest$SITMAT_AC == "C", "Celibataire", mytest$SITMAT_AC )))