library(dplyr)
library(tidyr)
library(ggplot2)
####### pour le renseignement sur les acquéreurs#
#objectif connaitre le taux de renseignement tout acquéreur confondu, faire pareil pour le svenudeurs, puis faire couple vendeurs
mytest<-data_redresse[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE")]
table( mytest$CSP_AC, mytest$QUALITE_AC, useNA = "ifany", deparse.level = 0)

mytest$PCS_rg<- ifelse(mytest$CSP_AC>=1 &!is.na(mytest$CSP_AC),"PCS_connue","PCS_nonconnue")
table(mytest$PCS_rg, useNA = "ifany")

mytest$personne_morale_rg<- ifelse(mytest$QUALITE_AC == "AD"&!is.na(mytest$QUALITE_AC),"morale",
                                   ifelse(mytest$QUALITE_AC== "EN"&!is.na(mytest$QUALITE_AC),"morale",
                                          ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"morale",
                                                 ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"morale",
                                                        ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"morale",
                                                               ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"morale","particulier_ou_na"))))))

table(mytest$personne_morale_rg, mytest$QUALITE_AC)
mytest$acquereur_rg<- ifelse(mytest$personne_morale_rg == "morale","acquereur_morale",
                             ifelse(mytest$PCS_rg!= "PCS_connue" & mytest$personne_morale_rg == "particulier_ou_na","sans_info","acquereur_particulier"))
# verif :
table(mytest$personne_morale_rg, useNA = "ifany")
####### pour le renseignement sur les vendeurs#

table (mytest$PCS_ve_rg, useNA = "ifany")
mytest$PCS_ve_rg<- ifelse(mytest$CSP_VE>= 1 &!is.na(mytest$CSP_VE),"PCS_connue","PCS_nonconnue")

mytest$personne_morale_ve_rg<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"morale",
                                      ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"morale",
                                             ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"morale",
                                                    ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"morale",
                                                           ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"morale",
                                                                  ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"morale", "particulier_ou_na"))))))


mytest$vendeur_rg<- ifelse(mytest$personne_morale_ve_rg == "morale","vendeur_morale",
                           ifelse(mytest$PCS_ve_rg!= "PCS_connue" & mytest$personne_morale_ve_rg == "particulier_ou_na", "PCS_non_connue", "vendeur_particulier"))



table (mytest$vendeur_rg,  useNA = "ifany", deparse.level = 0)

#effectif par année#
vendeur_by_year<- as.data.frame.matrix(table(mytest$annee, mytest$vendeur_rg,useNA= "ifany",deparse.level = 0))
acq_by_year<-as.data.frame.matrix(table(mytest$annee, mytest$acquereur_rg,useNA= "ifany",deparse.level = 0))
acq_by_year$annee<- row.names(acq_by_year)
vendeur_by_year$annee<- row.names(vendeur_by_year)



#preprarer un tableau à partir dun data frmae commun#
# vendeur
colnames(acq_by_year)<- c("acquereur_morale","acquereur_particulier","acquereur_sans_info","annee")

acq_by_year [, c(1,2,3,4)]<- acq_by_year [, c(4,1,2,3)]
colnames(acq_by_year)<- c("annee","acquereur_morale","acquereur_particulier","acquereur_sans_info")


vendeur_by_year [, c(1,2,3,4)]<- vendeur_by_year [, c(4,1,2,3)]
colnames(vendeur_by_year)<- c("annee","vendeur_sans_info","vendeur_morale","vendeur_particulier")

# jointure
acq_and_ve_by_year<- left_join(acq_by_year,vendeur_by_year, by="annee")
# particulier



acq_and_ve_by_year<-gather(acq_and_ve_by_year, "variables", "Values", 2:7)

acq_and_ve_by_year$typeVar <- substr(acq_and_ve_by_year$variables, start = 0, stop = 4)
acq_and_ve_by_year$typeVar<- ifelse (acq_and_ve_by_year$typeVar=="acqu", "Acquéreurs",
                                     ifelse (acq_and_ve_by_year$typeVar=="vend", "Vendeurs",acq_and_ve_by_year$typeVar ))
acq_and_ve_by_year$blob <- ifelse(grepl(x = acq_and_ve_by_year$variables, pattern = "morale"), "Personne morale", 
                                  ifelse(grepl(x = acq_and_ve_by_year$variables, pattern = "particulier"),"Ménage", "Sans renseignement"))


acq_and_ve_by_year$annee <- as.numeric(acq_and_ve_by_year$annee)

acq_and_ve_by_year.plot<- ggplot(acq_and_ve_by_year, aes(annee, Values, group=blob)) +
  geom_line(aes(color = blob))+
  geom_line(aes(color = blob),size=2,alpha=0.2,show.legend =F)+
  geom_line(aes(color = blob,linetype = "dashed"), show.legend =F) +  geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  facet_wrap(~typeVar, nrow = 1) +
  theme_tmd() +
  labs(title = "Evolution des types de vendeurs et acquéreurs", x= NULL, y= NULL)+ 
  labs(subtitle = "Effectif par année de l'effectif total redressé des transactions", x= NULL, y= NULL)+
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

setwd("~/Sauvegarde_figures")
ggsave("acq_and_ve_by_year.plot.pdf",plot= acq_and_ve_by_year.plot, "~/Sauvegarde_figures", device = "pdf", width = 220, height = 120, units = "mm", dpi = 330)

###################################################################################################

mytest<-data_redresse[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","REQ_AF_AC", "REQ_AF_VE", "REQ_ANC")]
table(mytest$REQ_AF_AC,mytest$QUALITE_AC, useNA = "ifany", deparse.level = 0)

mytest$PCS_rg<- ifelse(mytest$CSP_AC>=1 &!is.na(mytest$CSP_AC),"PCS_connue","PCS_nonconnue")
table(mytest$PCS_rg, useNA = "ifany")

mytest$personne_morale_rg<- ifelse(mytest$QUALITE_AC == "AD"&!is.na(mytest$QUALITE_AC),"morale",
                                   ifelse(mytest$QUALITE_AC== "EN"&!is.na(mytest$QUALITE_AC),"morale",
                                          ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"morale",
                                                 ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"morale",
                                                        ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"morale",
                                                               ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"morale","particulier_ou_na"))))))

table(mytest$personne_morale_rg, mytest$QUALITE_AC)
mytest$acquereur_rg<- ifelse(mytest$personne_morale_rg == "morale","acquereur_morale",
                             ifelse(mytest$PCS_rg!= "PCS_connue" & mytest$personne_morale_rg == "particulier_ou_na","sans_info","acquereur_particulier"))
# verif :
table(mytest$personne_morale_rg, useNA = "ifany")
####### pour le renseignement sur les vendeurs#

table (mytest$PCS_ve_rg, useNA = "ifany")
mytest$PCS_ve_rg<- ifelse(mytest$CSP_VE>= 1 &!is.na(mytest$CSP_VE),"PCS_connue","PCS_nonconnue")

mytest$personne_morale_ve_rg<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"morale",
                                      ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"morale",
                                             ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"morale",
                                                    ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"morale",
                                                           ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"morale",
                                                                  ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"morale", "particulier_ou_na"))))))


mytest$vendeur_rg<- ifelse(mytest$personne_morale_ve_rg == "morale","vendeur_morale",
                           ifelse(mytest$PCS_ve_rg!= "PCS_connue" & mytest$personne_morale_ve_rg == "particulier_ou_na", "PCS_non_connue", "vendeur_particulier"))



table (mytest$vendeur_rg,  useNA = "ifany", deparse.level = 0)

#effectif par année#
vendeur_by_year<- as.data.frame.matrix(table(mytest$annee, mytest$vendeur_rg,useNA= "ifany",deparse.level = 0))
acq_by_year<-as.data.frame.matrix(table(mytest$annee, mytest$acquereur_rg,useNA= "ifany",deparse.level = 0))
acq_by_year$annee<- row.names(acq_by_year)
vendeur_by_year$annee<- row.names(vendeur_by_year)



#preprarer un tableau à partir dun data frmae commun#
# vendeur
colnames(acq_by_year)<- c("acquereur_morale","acquereur_particulier","acquereur_sans_info","annee")

acq_by_year [, c(1,2,3,4)]<- acq_by_year [, c(4,1,2,3)]
colnames(acq_by_year)<- c("annee","acquereur_morale","acquereur_particulier","acquereur_sans_info")


vendeur_by_year [, c(1,2,3,4)]<- vendeur_by_year [, c(4,1,2,3)]
colnames(vendeur_by_year)<- c("annee","vendeur_sans_info","vendeur_morale","vendeur_particulier")

# jointure
acq_and_ve_by_year<- left_join(acq_by_year,vendeur_by_year, by="annee")
# particulier



acq_and_ve_by_year<-gather(acq_and_ve_by_year, "variables", "Values", 2:7)

acq_and_ve_by_year$typeVar <- substr(acq_and_ve_by_year$variables, start = 0, stop = 4)
acq_and_ve_by_year$typeVar<- ifelse (acq_and_ve_by_year$typeVar=="acqu", "Acquéreurs",
                                     ifelse (acq_and_ve_by_year$typeVar=="vend", "Vendeurs",acq_and_ve_by_year$typeVar ))
acq_and_ve_by_year$blob <- ifelse(grepl(x = acq_and_ve_by_year$variables, pattern = "morale"), "Personne morale", 
                                  ifelse(grepl(x = acq_and_ve_by_year$variables, pattern = "particulier"),"Ménage", "Sans renseignement"))


acq_and_ve_by_year$annee <- as.numeric(acq_and_ve_by_year$annee)

acq_and_ve_by_year.plot<- ggplot(acq_and_ve_by_year, aes(annee, Values, group=blob)) +
  geom_line(aes(color = blob))+
  geom_line(aes(color = blob),size=2,alpha=0.2,show.legend =F)+
  geom_line(aes(color = blob,linetype = "dashed"), show.legend =F) +  geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  facet_wrap(~typeVar, nrow = 1) +
  theme_tmd() +
  labs(title = "Evolution des types de vendeurs et acquéreurs", x= NULL, y= NULL)+ 
  labs(subtitle = "Effectif par année de l'effectif total redressé des transactions", x= NULL, y= NULL)+
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

setwd("~/Sauvegarde_figures")
ggsave("acq_and_ve_by_year.plot.pdf",plot= acq_and_ve_by_year.plot, "~/Sauvegarde_figures", device = "pdf", width = 220, height = 120, units = "mm", dpi = 330)

############