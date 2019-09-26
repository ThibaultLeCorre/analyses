library(dplyr)
library(tidyr)
library( ggplot2)

library(extrafont)
loadfonts(device="win")
#taux d'enregistrement des variables#
#travailler à partir de la BD bien filtrée aux XY#
#détemriner le seuil de renseignement en % des variables par group : donénes financieres, descriptives sur le bien, temporelle, et su le spartie#
#méthode :créer des varibles d'indications de présence pour chaque variable où il y a un renseignement atribué une vlaeur "1", sinon na#
#faire la somme des "1" par année et diviser cela par le total des lignes d ela base à ce stade, soit 921075#
#regarder spécificite par variables, notamment au niveau acquéreurs#

####explortation variables avec table

mytest<- BIEN_fil_XY



##varaibels fiscales : 
#taux TVA : concerne les biens neufs, si = 0 ("A" dasn la base) c'est un bien ancien#
table(mytest$TAUXTVA)
#droit de mutation = droit d'enregistrements (perçu par l'etat et collectivités): pour les biens anciens, sinon = 0;
table(mytest$TXDRMUT1)
# taxe publicité foncière : si présence = neuf, sinon ancien ppiur verif : table entre varialbles
table (mytest$TAXPF)
table (mytest$TAXPF, mytest$TAUXTVA)
#rectification variable TXDRMUT1 dans la base originale 36 et 38 doit correspondre à 3.6 et 3.8#
mytest$TXDRMUT1 <- ifelse(mytest$TXDRMUT1 == "36","3.6",ifelse( mytest$TXDRMUT1 == "38","3.8", mytest$TXDRMUT1))

###variables financières#
#nouvelle variabel pour les variables quantitatives, si na = na, sinon = 1#
#prix#
mytest$PRIX_rg <- ifelse(mytest$REQ_PRIX  == "na","na",ifelse( mytest$REQ_PRIX != "na","info_prix", "na"))
PRIX_rg<-as.data.frame.matrix(table (mytest$annee, mytest$PRIX_rg,useNA= "always",deparse.level = 0))
colnames(PRIX_rg)<- c(1,2,3)
#prix m²
mytest$PRIX_sqmet_rg <- ifelse(mytest$REQ_PM2  == "na","na",ifelse( mytest$REQ_PM2 != "na","info_prixm²", "na"))
PRIX_sqmet<- as.data.frame.matrix(table(mytest$annee, mytest$PRIX_sqmet_rg,useNA= "always",deparse.level = 0))
#presence credit, attention variable qualitative#*
mytest$PRESCREDIT_rg <-ifelse(mytest$PRESCREDIT== "O","info_credit", 
                            ifelse(mytest$PRESCREDIT== "N", "info_pas_de_credit", "credit securise espere"))

PRESCREDIT_rg<- as.data.frame.matrix( table(mytest$annee, mytest$PRESCREDIT_rg,useNA= "ifany",deparse.level = 0))
#montant du crédit si presence crédit#

#plus ou moins value#
mytest$REQ_VALUE_rg <- ifelse(mytest$REQ_VALUE  == "na","na",ifelse( mytest$REQ_VALUE != "na","info_perte_benefice", "na"))
REQ_VALUE_rg <- as.data.frame.matrix(table(mytest$annee, mytest$REQ_VALUE_rg,useNA= "always",deparse.level = 0))
#montant mutation précédente#
mytest$PXMUTPREC_rg<- ifelse(mytest$PXMUTPREC== "","na",
                             ifelse(mytest$PXMUTPREC== " ","na",
                             ifelse(mytest$PXMUTPREC!= "na","inf_prix_mutation_precedente", "na")))

PXMUTPREC_rg<- as.data.frame.matrix (table( mytest$annee, mytest$PXMUTPREC_rg,useNA= "always"))


#créer un data frame par année
#creer la variable commune
PRESCREDIT_rg$annee<- row.names(PRESCREDIT_rg)
REQ_VALUE_rg$annee<- row.names(REQ_VALUE_rg)
PXMUTPREC_rg$annee<- row.names(PXMUTPREC_rg)
#preprarer un tableau à partir dun data frmae commun#
colnames(PRIX_rg)<- c("Prix des transactions","2","annee")
tableau_renseignement_fin<- PRIX_rg [1:12, c(1,3)]
tableau_renseignement_fin [, c(1,2)]<- tableau_renseignement_fin [, c(2,1)]
colnames(tableau_renseignement_fin)<- c("annee","Prix")

#jointure
tableau_renseignement_fin<-left_join(tableau_renseignement_fin,PRIX_sqmet[,c(1,3)], by="annee")
tableau_renseignement_fin<-left_join(tableau_renseignement_fin,PRESCREDIT_rg[,c(1:4)], by="annee")
tableau_renseignement_fin<-left_join(tableau_renseignement_fin,REQ_VALUE_rg[,c(1,3)], by="annee")
tableau_renseignement_fin<-left_join(tableau_renseignement_fin,PXMUTPREC_rg[,c(1,4)], by="annee")

transac_total<-as.data.frame(table (mytest$annee,deparse.level = 0))
colnames(transac_total)<- c("annee","total")
tableau_renseignement_fin<-left_join(tableau_renseignement_fin,transac_total, by="annee")
# tableau_renseignement_fin[,c(2:6)]<- tableau_renseignement_fin[1:12,c(2:6)]/ tableau_renseignement_fin[1:12,c(7)]
# colnames(tableau_renseignement_fin)<- c("annee","Prix", "Prix au m²", "Présence d'un crédit", "Plus ou moins-value", "Prix de la mutation précédente")
# ##plotage#
tableau_renseignement_fin<-tableau_renseignement_fin[,c(1:6)]
tableau_renseignement_fin_long<-gather(tableau_renseignement_fin, "variables", "Values", 2:6)

tableau_renseignement_fin_long$annee <- as.numeric(tableau_renseignement_fin_long$annee)
# 
# 
evol_taux_rg<- ggplot(tableau_renseignement_long, aes(annee, jitter(Values , amount = 0.01), group=variables)) +
  geom_line(aes(color = variables))+
              geom_line(aes(color = variables),size=2,alpha=0.2,show.legend =F)+
              geom_line(aes(color = variables,linetype = "dashed"), show.legend =F) +  geom_point(size=0.5)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  labs(title = "Evolution du taux de renseignement",  x = NULL, y = NULL)+
  theme_tmd() +
#   # theme(axis.line = element_line(size = 5), 
#   #       axis.ticks = element_blank(), 
#   #       axis.title = element_blank(), 
#   #       axis.text.x = element_text(size = 6, angle=45), 
#   #       axis.text.y = element_text(size = 8), 
#   #       plot.title = element_text(vjust = 1), 
#   #       legend.title = element_text(size = 0, 
#   #                                   colour = NA),
#   #       legend.position = 'bottom',
#   #       legend.background = element_rect(fill = NA)) +  theme(legend.text = element_text(size = 7)) + theme(plot.title = element_text(size = 10, face = "italic")) 
 labs(title = "Evolution du taux de renseignement",
    x = NULL)+
  labs(subtitle = "Fréquence par année de l'effectif total des transactions", x= NULL, y= NULL)+
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")




ggsave("evol_taux_rg.png",plot= evol_taux_rg, "C:/Users/Utilisateur/Documents/Thibault__4eme/thèse_redaction/Image_BIEN", device = "png", width = 220, height = 120, units = "mm", dpi = 330)


###faire la même chose avec les données description du bien#
##dissocier directement par type appartmeent maison#
mytest$REQTYPBIEN
typbien<-as.data.frame.matrix(table(mytest$annee, mytest$REQTYPBIEN,useNA= "ifany",deparse.level = 0))

typbien$annee<- row.names(typbien)
#preprarer un tableau à partir dun data frmae commun#
colnames(typbien)<- c("Appartements","Maisons", "annee")

typbien [, c(1,2,3)]<- typbien [, c(3,2,1)]
colnames(typbien)<- c("annee","Maisons","Appartements")


typbien<-gather(typbien, "variables", "Values", 2:3)

typbien$annee <- as.numeric(typbien$annee)


typbien.plot<- ggplot(typbien, aes(annee, Values, group=variables)) +
  geom_line(aes(color = variables))+
  geom_line(aes(color = variables),size=2,alpha=0.2,show.legend =F)+
  geom_line(aes(color = variables,linetype = "dashed"), show.legend =F) +  geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  labs(title = "Evolution des transactions maisons et appartements", x= NULL, y= NULL)+ 
  labs(subtitle = "Effectif par année de l'effectif total des transactions", x= NULL, y= NULL)+
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

ggsave("typbien.plot.png",plot= typbien.plot, "C:/Users/Utilisateur/Documents/Thibault__4eme/thèse_redaction/Image_BIEN", device = "png", width = 220, height = 120, units = "mm", dpi = 330)

###faire 2 tableaux : un pour les appartements, un autre pour les maisons#
mytest_AP<- mytest%>% filter (REQTYPBIEN=="AP")
mytest_MA<- mytest%>% filter (REQTYPBIEN=="MA")
##sur les sous types
#rg type appartements
table (mytest_AP$TYPAP)
mytest_AP$TYPAP_rg<- ifelse(mytest_AP$TYPAP  == "na","na",
                            ifelse( mytest_AP$TYPAP == "","na", 
                                    ifelse( mytest_AP$TYPAP == " ","na","info_typap")))
TYPAP_rg<-as.data.frame.matrix(table (mytest_AP$annee,mytest_AP$TYPAP_rg,useNA= "ifany",deparse.level = 0))
#rg type maisonsmaisons
mytest_MA$TYPMA_rg<- ifelse(mytest_MA$TYPMA  == "na","na",
                            ifelse( mytest_MA$TYPMA == "","na", 
                                    ifelse( mytest_MA$TYPMA == " ","na","info_typMA")))
TYPMA_rg<-as.data.frame.matrix(table (mytest_MA$annee,mytest_MA$TYPMA_rg,useNA= "ifany",deparse.level = 0))

#epoque de construction#

#appart#
table (mytest_AP$REQ_EPOQU)
mytest_AP$REQ_EPOQU_rg<- ifelse(mytest_AP$REQ_EPOQU  == "na","na",
                            ifelse( mytest_AP$REQ_EPOQU == "","na", 
                                    ifelse( mytest_AP$REQ_EPOQU == " ","na","info_REQ_EPOQU")))
REQ_EPOQU_AP_rg<-as.data.frame.matrix(table (mytest_AP$annee,mytest_AP$REQ_EPOQU_rg,useNA= "ifany",deparse.level = 0))
# maisonsmaisons#
mytest_MA$REQ_EPOQU_rg<- ifelse(mytest_MA$REQ_EPOQU  == "na","na",
                            ifelse( mytest_MA$REQ_EPOQU == "","na", 
                                    ifelse( mytest_MA$REQ_EPOQU == " ","na","info_REQ_EPOQU")))
REQ_EPOQU_MA_rg<-as.data.frame.matrix(table (mytest_MA$annee,mytest_MA$REQ_EPOQU_rg,useNA= "ifany",deparse.level = 0))

#surface habitable déclarée en m²#

mytest_AP$SURFHABDEC_rg<- ifelse(mytest_AP$SURFHABDEC  == "na","na",
                                ifelse( mytest_AP$SURFHABDEC == "","na", 
                                        ifelse( mytest_AP$SURFHABDEC == " ","na","info_SURFHABDECU_AP")))
SURFHABDEC_AP_rg<-as.data.frame.matrix(table (mytest_AP$annee,mytest_AP$SURFHABDEC_rg,useNA= "ifany",deparse.level = 0))
#rg type maisonsmaisons#
mytest_MA$SURFHABDEC_rg<- ifelse(mytest_MA$SURFHABDEC  == "na","na",
                                ifelse( mytest_MA$SURFHABDEC == "","na", 
                                        ifelse( mytest_MA$SURFHABDEC == " ","na","info_SURFHABDECU_MA")))
SURFHABDEC_MA_rg<-as.data.frame.matrix(table (mytest_MA$annee,mytest_MA$SURFHABDEC_rg,useNA= "ifany",deparse.level = 0))
#seulement pour les maisons surface totale du terrain#
mytest_MA$REQ_SURFT_rg<- ifelse(mytest_MA$REQ_SURFT  == "na","na",
                                 ifelse( mytest_MA$REQ_SURFT == "","na", 
                                         ifelse( mytest_MA$REQ_SURFT == " ","na","info_REQ_SURFTU_MA")))
REQ_SURFT_MA_rg<-as.data.frame.matrix(table (mytest_MA$annee,mytest_MA$REQ_SURFT_rg,useNA= "ifany",deparse.level = 0))



#nombre de piece principales#
table (mytest_AP$NBRPIECE,useNA= "ifany" )
mytest_AP$NBRPIECE_rg<- ifelse(mytest_AP$NBRPIECE == "na","na","info_NBRPIECEU_AP")

NBRPIECE_AP_rg<-as.data.frame.matrix(table (mytest_AP$annee,mytest_AP$NBRPIECE_rg,useNA= "ifany",deparse.level = 0))
#rg type maisonsmaisons#
mytest_MA$NBRPIECE_rg<- ifelse(mytest_MA$NBRPIECE  == "na","na",
                                 ifelse( mytest_MA$NBRPIECE == "","na", 
                                         ifelse( mytest_MA$NBRPIECE == " ","na","info_NBRPIECEU_MA")))
NBRPIECE_MA_rg<-as.data.frame.matrix(table (mytest_MA$annee,mytest_MA$NBRPIECE_rg,useNA= "ifany",deparse.level = 0))







#créer un data frame par année et par type de biens, pour les maisons pas oubleir variable en plus
#creer la variable commune
TYPAP_rg$annee<- row.names(TYPAP_rg)
REQ_EPOQU_AP_rg$annee<- row.names(REQ_EPOQU_AP_rg)
SURFHABDEC_AP_rg$annee<- row.names(SURFHABDEC_AP_rg)
NBRPIECE_AP_rg$annee<- row.names(NBRPIECE_AP_rg)
#preprarer un tableau à partir dun data frmae commun#
colnames(TYPAP_rg)<- c("type d'appartement","2","annee")
tableau_renseignement<- TYPAP_rg[1:12, c(1,3)]
tableau_renseignement [, c(1,2)]<- tableau_renseignement [, c(2,1)]
colnames(tableau_renseignement)<- c("annee","type d'appartement")

#jointure
tableau_renseignement<-left_join(tableau_renseignement,REQ_EPOQU_AP_rg[,c(1,3)], by="annee")
tableau_renseignement<-left_join(tableau_renseignement,SURFHABDEC_AP_rg[,c(1,3)], by="annee")
tableau_renseignement<-left_join(tableau_renseignement,NBRPIECE_AP_rg[,c(1,3)], by="annee")

#calcul d taux sur type appartement#

transac_total_ap<-as.data.frame(table (mytest_AP$annee,deparse.level = 0))
colnames(transac_total_ap)<- c("annee","total")
tableau_renseignement<-left_join(tableau_renseignement,transac_total_ap, by="annee")
tableau_renseignement[,c(2:5)]<- tableau_renseignement[1:12,c(2:5)]/ tableau_renseignement[1:12,c(6)]

#meme chos epour le smaisons : attention calculer le taux sur le totla des maisosn
#creer la variable commune
TYPMA_rg$annee<- row.names(TYPMA_rg)
REQ_EPOQU_MA_rg$annee<- row.names(REQ_EPOQU_MA_rg)
SURFHABDEC_MA_rg$annee<- row.names(SURFHABDEC_MA_rg)
REQ_SURFT_MA_rg$annee<- row.names(REQ_SURFT_MA_rg)
NBRPIECE_MA_rg$annee<- row.names(NBRPIECE_MA_rg)

#preprarer un tableau à partir dun data frmae commun#
colnames(TYPMA_rg)<- c("type de maison","2","annee")
tableau_renseignement_MA<- TYPMA_rg[1:12, c(1,3)]
tableau_renseignement_MA [, c(1,2)]<- tableau_renseignement_MA [, c(2,1)]
colnames(tableau_renseignement_MA)<- c("annee","type de maison")

#jointure
tableau_renseignement_MA<-left_join(tableau_renseignement_MA,REQ_EPOQU_MA_rg[,c(1,3)], by="annee")
tableau_renseignement_MA<-left_join(tableau_renseignement_MA,SURFHABDEC_MA_rg[,c(1,3)], by="annee")
tableau_renseignement_MA<-left_join(tableau_renseignement_MA,REQ_SURFT_MA_rg[,c(1,3)], by="annee")
tableau_renseignement_MA<-left_join(tableau_renseignement_MA,NBRPIECE_MA_rg[,c(1,3)], by="annee")

#calcul d taux sur type MApartement#

transac_total_MA<-as.data.frame(table (mytest_MA$annee,deparse.level = 0))
colnames(transac_total_MA)<- c("annee","total")
tableau_renseignement_MA<-left_join(tableau_renseignement_MA,transac_total_MA, by="annee")
tableau_renseignement_MA[,c(2:6)]<- tableau_renseignement_MA[1:12,c(2:6)]/ tableau_renseignement_MA[1:12,c(7)]


#joindre le tableau maisons et appartement, mais renommer avant les variables#

colnames(tableau_renseignement)<- c("annee","type d'appartement", "Epoque de construction appartement", "Surface habitable déclarée appartement", "Nombre de pièces appartement", "total")
tableau_renseignement<-tableau_renseignement[,c(1:5)]
colnames(tableau_renseignement_MA)<- c("annee","type de maison", "Epoque de construction maison", "Surface habitable déclarée maison", "Surface totale du terrain","Nombre de pièces maison", "total")
tableau_renseignement<-left_join(tableau_renseignement,tableau_renseignement_MA[,c(1:6)], by="annee")
##plotage#

tableau_renseignement_long<-gather(tableau_renseignement, "variables", "Values", 2:10)

tableau_renseignement_long$annee <- as.numeric(tableau_renseignement_long$annee)
tableau_renseignement_long$typeVar <- substr(tableau_renseignement_long$variables, start = 0, stop = 4)
tableau_renseignement_long$typeVar<- ifelse (tableau_renseignement_long$typeVar=="Epoq", "Epoque de construction",
                                             ifelse (tableau_renseignement_long$typeVar=="Nomb", "Nombre de pièces",
                                                     ifelse (tableau_renseignement_long$typeVar=="Surf", "Surfaces",
                                                             ifelse (tableau_renseignement_long$typeVar== "type", "Type de logement", tableau_renseignement_long$typeVar ))))
tableau_renseignement_long$blob <- ifelse(grepl(x = tableau_renseignement_long$variables, pattern = "maison"), "Maison", 
                                          ifelse(grepl(x = tableau_renseignement_long$variables, pattern = "terrain"),"Surface du terrain", "Appartement"))

testplot<- ggplot(tableau_renseignement_long, aes(annee, Values , group=blob)) +
  geom_line(aes(color = blob))+
  geom_line(aes(color = blob),size=2,alpha=0.2,show.legend =F)+
  geom_line(aes(color = blob,linetype = "dashed"), show.legend =F) +  geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  labs(title = "Evolution du taux de renseignement des maisons et appartements",  x = "Année", y = NULL)+
  facet_wrap(~typeVar, nrow = 1) +
  theme_tmd() +
  labs(title = "Evolution du taux de renseignement pour des maisons et appartements", x = NULL)+
  labs(subtitle = "Fréquence par année, strictement sur l'effectif des maisons et appartements\n ex : L'information par année du 'type de logement' des appartements est calculé sur l'effectif des appartements par année", x= NULL, y= NULL)+
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")


ggsave("testplot.png",plot= testplot, "C:/Users/Utilisateur/Documents/Thibault__4eme/thèse_redaction/Image_BIEN", device = "png", width = 220, height = 120, units = "mm", dpi = 330)


#####On n'a pas fait directement les test sur la varaible anciennete car on sait que cest renseigne a 100%#
# on le fait ici en dissocsinat bien neuf maisons, appart et ancien maisons, appart


mytest_AP$REQ_ANC
bien_ancien_neuf_ap<-as.data.frame.matrix(table(mytest_AP$annee, mytest_AP$REQ_ANC,useNA= "ifany",deparse.level = 0))

mytest_MA$REQ_ANC
bien_ancien_neuf_ma<-as.data.frame.matrix(table(mytest_MA$annee, mytest_MA$REQ_ANC,useNA= "ifany",deparse.level = 0))
#appart
bien_ancien_neuf_ap$annee<- row.names(bien_ancien_neuf_ap)
transac_total_ap<-as.data.frame(table (mytest_AP$annee,deparse.level = 0))
colnames(transac_total_ap)<- c("annee","total")
bien_ancien_neuf_ap<-left_join(bien_ancien_neuf_ap,transac_total_ap, by="annee")
bien_ancien_neuf_ap [, c(1,2)]<- tableau_renseignement [, c(2,1)]
colnames(bien_ancien_neuf_ap)<- c("annee","type d'appartement")
bien_ancien_neuf_ap[,c(2:5)]<- bien_ancien_neuf_ap[1:12,c(2:5)]/ bien_ancien_neuf_ap[1:12,c(6)]



###########################test sur les parties de la vente#####################################


####### pour le renseignement sur les acquéreurs#
#objectif connaitre le taux de renseignement tout acquéreur confondu, faire pareil pour le svenudeurs, puis faire couple vendeurs
table( mytest$CSP_AC, mytest$QUALITE_AC, useNA = "ifany", deparse.level = 0)

mytest$PCS_rg<- ifelse(mytest$CSP_AC>= 1,"PCS_connue", "PCS_nonconnue")

mytest$personne_morale_rg<- ifelse(mytest$QUALITE_AC == "AD","morale",
                                   ifelse(mytest$QUALITE_AC== "EN","morale",
                                          ifelse(mytest$QUALITE_AC== "PR","morale",
                                                 ifelse(mytest$QUALITE_AC== "SA","morale",
                                                        ifelse(mytest$QUALITE_AC== "SC","morale",
                                                               ifelse(mytest$QUALITE_AC== "SO","morale", "particulier_ou_na"))))))


mytest$acquereur_rg<- ifelse(mytest$personne_morale_rg == "morale","acquereur_morale",
                           ifelse(mytest$PCS_rg!= "PCS_connue" & mytest$personne_morale_rg == "particulier_ou_na", "sans_info", "acquereur_particulier"))
# verif :
  table(mytest$personne_morale_rg, useNA = "ifany")
####### pour le renseignement sur les vendeurs#

table (mytest$PCS_ve_rg, useNA = "ifany")
mytest$PCS_ve_rg<- ifelse(mytest$CSP_VE>= 1,"PCS_connue","PCS_nonconnue")

mytest$personne_morale_ve_rg<- ifelse(mytest$QUALITE_VE == "AD","morale",
                                   ifelse(mytest$QUALITE_VE== "EN","morale",
                                          ifelse(mytest$QUALITE_VE== "PR","morale",
                                                 ifelse(mytest$QUALITE_VE== "SA","morale",
                                                        ifelse(mytest$QUALITE_VE== "SC","morale",
                                                               ifelse(mytest$QUALITE_VE== "SO","morale", "particulier_ou_na"))))))


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

# jointure
acq_and_ve_by_year<- left_join(acq_by_year,vendeur_by_year, by="annee")
# particulier



acq_and_ve_by_year<-gather(acq_and_ve_by_year, "variables", "Values", 2:7)

acq_and_ve_by_year$typeVar <- substr(acq_and_ve_by_year$variables, start = 0, stop = 4)
acq_and_ve_by_year$typeVar<- ifelse (acq_and_ve_by_year$typeVar=="acqu", "Acquéreurs",
                                             ifelse (acq_and_ve_by_year$typeVar=="vend", "Vendeurs",acq_and_ve_by_year$typeVar ))
acq_and_ve_by_year$blob <- ifelse(grepl(x = acq_and_ve_by_year$variables, pattern = "morale"), "Personne morale", 
                                          ifelse(grepl(x = acq_and_ve_by_year$variables, pattern = "particulier"),"Ménage", "sans renseignement"))


acq_and_ve_by_year$annee <- as.numeric(acq_and_ve_by_year$annee)

acq_and_ve_by_year.plot<- ggplot(acq_and_ve_by_year, aes(annee, Values, group=blob)) +
  geom_line(aes(color = blob))+
  geom_line(aes(color = blob),size=2,alpha=0.2,show.legend =F)+
  geom_line(aes(color = blob,linetype = "dashed"), show.legend =F) +  geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  facet_wrap(~typeVar, nrow = 1) +
  theme_tmd() +
  labs(title = "Evolution des types de vendeurs et acquéreurs", x= NULL, y= NULL)+ 
  labs(subtitle = "Effectif par année de l'effectif total des transactions", x= NULL, y= NULL)+
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

ggsave("acq_and_ve_by_year.plot.png",plot= acq_and_ve_by_year.plot, "C:/Users/Utilisateur/Documents/Thibault__4eme/thèse_redaction/Image_BIEN", device = "png", width = 220, height = 120, units = "mm", dpi = 330)


#########################################################################################################

mytest$couple_ve_acq<- ifelse(grepl(x = mytest$acquereur_rg, pattern = "acq") & grepl(x = mytest$vendeur_rg, pattern = "ve"), "couple_acquereur_vendeur", "couple_inconnu")

table (mytest$couple_ve_acq,  useNA = "ifany", deparse.level = 0)

#effectif par année#
couple_ve_acq<- as.data.frame.matrix(table(mytest$annee, mytest$couple_ve_acq,useNA= "ifany",deparse.level = 0))

couple_ve_acq$annee<- row.names(couple_ve_acq)


#preprarer un tableau à partir dun data frmae commun#
# vendeur
colnames(couple_ve_acq)<- c("couple_acquereur_vendeur","couple_inconnu","annee")

couple_ve_acq [, c(1,2,3)]<- couple_ve_acq [, c(3,1,2)]
colnames(couple_ve_acq)<- c("annee","couple_acquereur_vendeur","couple_inconnu")
couple_ve_acq<- couple_ve_acq [,c(1,2)]
couple_ve_acq$annee <- as.numeric(as.character(couple_ve_acq$annee))
transac_total$annee <- as.numeric(as.character(transac_total$annee))


##mettre une variabel acquereur et vendeur seul#
couple_ve_acq$acquereur<-acq_by_year[1:12,c(2)] + acq_by_year[1:12,c(3)]
couple_ve_acq$vendeur<-vendeur_by_year[1:12,c(2)] + vendeur_by_year[1:12,c(3)]
couple_ve_acq<-left_join(couple_ve_acq,transac_total, by="annee")

couple_ve_acq[,c(2:4)]<- couple_ve_acq[1:12,c(2:4)]/ couple_ve_acq[1:12,c(5)]
######
couple_ve_acq<-gather(couple_ve_acq, "variables", "Values", 2:4)

couple_ve_acq$typeVar <- substr(couple_ve_acq$variables, start = 0, stop = 4)
couple_ve_acq$typeVar<- ifelse (couple_ve_acq$typeVar=="acqu", "Acquéreur",
                                     ifelse (couple_ve_acq$typeVar=="vend", "Vendeur",
                                             ifelse ( couple_ve_acq$typeVar=="coup", "Couple acquéreur et vendeur",couple_ve_acq$typeVar )))




rg_couple_acq_vendeur.plot<- ggplot(couple_ve_acq, aes(annee, Values, group=typeVar)) +
  geom_line(aes(color = typeVar))+
  geom_line(aes(color = typeVar),size=2,alpha=0.2,show.legend =F)+
  geom_line(aes(color = typeVar,linetype = "dashed"), show.legend =F) +  geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  labs(title = "Evolution du taux de renseignement des vendeurs,acquéreurs et couples acquéreurs vendeurs", x= NULL, y= NULL)+ 
  labs( subtitle = "Fréquence par année sur l'effectif total des transactions", x= NULL, y= NULL)+ 
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

ggsave("rg_couple_acq_vendeur.plot.png",plot= rg_couple_acq_vendeur.plot, "C:/Users/Utilisateur/Documents/Thibault__4eme/thèse_redaction/Image_BIEN", device = "png", width = 220, height = 120, units = "mm", dpi = 330)

parties_transac_by_year<-left_join(acq_and_ve_by_year, couple_ve_acq, by ="annee")
total_parties_transac<- colSums(parties_transac_by_year[,c(2:ncol(parties_transac_by_year))])
total_parties_transac$acquereur_total_rg<- 872719
total_parties_transac$vendeur_total_rg<- 884222
total_parties_transac$acquereur_total_rg<- 872719
total_parties_transac<- as.data.frame(total_parties_transac)
#############informations sur les variabels descripitives des ménages#
# "sexe"
mytest$sexe_ac_rg<- ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$SEXE_AC == "F" ,"sexe_acq_connu",
                            ifelse (mytest$acquereur_rg == "acquereur_particulier"& mytest$SEXE_AC == "M", "sexe_acq_connu",  "sexe_acq_non_conu"))
table(mytest$sexe_ac_rg)

mytest$sexe_ve_rg<- ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$SEXE_VE == "F" ,"sexe_ve_connu",
                            ifelse (mytest$vendeur_rg == "vendeur_particulier"& mytest$SEXE_VE == "M", "sexe_ve_connu",  "sexe_ve_non_conu"))
mytest$couple_sexe_ac_ve_rg<- ifelse (mytest$couple_menage_ou_autres=="couple_particulier" & mytest$sexe_ac_rg=="sexe_acq_connu" & mytest$sexe_ve_rg == "sexe_ve_connu", "couple_sexe_ac_ve_connu", "couple_sexe_ac_ve_NON_connu")

table(mytest$couple_menage_ou_autres)
rg_sexe_ac<- mytest%>% 
  filter(sexe_ac_rg=="sexe_acq_connu")%>%
  select (annee,sexe_ac_rg)%>% 
  group_by(annee)%>% 
  count(sexe_ac_rg)%>%
  arrange (n,annee)%>%
  spread(sexe_ac_rg,n)

rg_sexe_ve<- mytest%>% 
  filter(sexe_ve_rg=="sexe_ve_connu")%>%
  select (annee,sexe_ve_rg)%>% 
  group_by(annee)%>% 
  count(sexe_ve_rg)%>%
  arrange (n,annee)%>%
  spread(sexe_ve_rg,n)

rg_couple_sexe_ac_ve<- mytest%>% 
  filter(couple_sexe_ac_ve_rg=="couple_sexe_ac_ve_connu")%>%
  select (annee,couple_sexe_ac_ve_rg)%>% 
  group_by(annee)%>% 
  count(couple_sexe_ac_ve_rg)%>%
  arrange (n,annee)%>%
  spread(couple_sexe_ac_ve_rg,n)

info_sexe<- left_join( rg_sexe_ac,rg_sexe_ve)
info_sexe<- left_join( info_sexe,rg_couple_sexe_ac_ve)




# age
mytest$age_ac_rg<- ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$ANNAIS_AC >= 1,"age_acq_connu", "age_acq_non_conu")
table(mytest$couple_age_ac_ve_rg)

mytest$age_ve_rg<- ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$ANNAIS_VE >=1 ,"age_ve_connu",  "age_ve_non_conu")

mytest$couple_age_ac_ve_rg<- ifelse (mytest$couple_menage_ou_autres=="couple_particulier" & mytest$age_ve_rg=="age_ve_connu" & mytest$age_ac_rg =="age_acq_connu" ,"couple_age_ac_ve_connu",  "couple_age_ac_ve_NON_connu")


rg_age_ac<- mytest%>% 
  filter(age_ac_rg=="age_acq_connu")%>%
  select (annee,age_ac_rg)%>% 
  group_by(annee)%>% 
  count(age_ac_rg)%>%
  arrange (n,annee)%>%
  spread(age_ac_rg,n)

rg_age_ve<- mytest%>% 
  filter(age_ve_rg=="age_ve_connu")%>%
  select (annee,age_ve_rg)%>% 
  group_by(annee)%>% 
  count(age_ve_rg)%>%
  arrange (n,annee)%>%
  spread(age_ve_rg,n)

rg_couple_age_ac_ve<- mytest%>% 
  filter(couple_age_ac_ve_rg=="couple_age_ac_ve_connu")%>%
  select (annee,couple_age_ac_ve_rg)%>% 
  group_by(annee)%>% 
  count(couple_age_ac_ve_rg)%>%
  arrange (n,annee)%>%
  spread(couple_age_ac_ve_rg,n)

info_age<- left_join( rg_age_ac,rg_age_ve)
info_age<- left_join( info_age,rg_couple_age_ac_ve)












# situation matrimo
table(mytest$couple_sitmatri_ac_ve_rg)
mytest$sitmatri_ac_rg<- ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$SITMAT_AC =="C" ,"sitmatri_acq_connu",
                                ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$SITMAT_AC =="D" ,"sitmatri_acq_connu",
                                        ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$SITMAT_AC =="M" ,"sitmatri_acq_connu",
                                                ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$SITMAT_AC =="P" ,"sitmatri_acq_connu",
                                                        ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$SITMAT_AC =="R" ,"sitmatri_acq_connu",
                                                                ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$SITMAT_AC =="V" ,"sitmatri_acq_connu",  "sitmatri_acq_non_conu"))))))

mytest$sitmatri_ve_rg<- ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$SITMAT_VE =="C" ,"sitmatri_ve_connu",
                                ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$SITMAT_VE =="D" ,"sitmatri_ve_connu",
                                        ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$SITMAT_VE =="M" ,"sitmatri_ve_connu",
                                                ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$SITMAT_VE =="P" ,"sitmatri_ve_connu",
                                                        ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$SITMAT_VE =="R" ,"sitmatri_ve_connu",
                                                                ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$SITMAT_VE =="V" ,"sitmatri_ve_connu",  "sitmatri_ve_non_conu"))))))
mytest$couple_sitmatri_ac_ve_rg<- ifelse(mytest$couple_menage_ou_autres=="couple_particulier" & mytest$sitmatri_ac_rg=="sitmatri_acq_connu" & mytest$sitmatri_ve_rg == "sitmatri_ve_connu", "couple_sitmatri_connu", "couple_sitmatri_NON_connu")
table(mytest$couple_sitmatri_ac_ve_rg, useNA = "ifany")

rg_situmatri_ac<- mytest%>% 
  filter(sitmatri_ac_rg=="sitmatri_acq_connu")%>%
  select (annee,sitmatri_ac_rg)%>% 
    group_by(annee)%>% 
  count(sitmatri_ac_rg)%>%
  arrange (n,annee)%>%
 spread(sitmatri_ac_rg,n)
  
rg_situmatri_ve<- mytest%>% 
  filter(sitmatri_ve_rg=="sitmatri_ve_connu")%>%
  select (annee,sitmatri_ve_rg)%>% 
  group_by(annee)%>% 
  count(sitmatri_ve_rg)%>%
  arrange (n,annee)%>%
  spread(sitmatri_ve_rg,n)

rg_couple_situmatri_ac_ve<- mytest%>% 
  filter(couple_sitmatri_ac_ve_rg=="couple_sitmatri_connu")%>%
  select (annee,couple_sitmatri_ac_ve_rg)%>% 
  group_by(annee)%>% 
  count(couple_sitmatri_ac_ve_rg)%>%
  arrange (n,annee)%>%
  spread(couple_sitmatri_ac_ve_rg,n)

info_sitmatri<- left_join( rg_situmatri_ac,rg_situmatri_ve)
info_sitmatri<- left_join( info_sitmatri,rg_couple_situmatri_ac_ve)


######
menage_socio_demo<- left_join(info_age,info_sexe)
menage_socio_demo<- left_join(menage_socio_demo,info_sitmatri)
menage_socio_demo<- left_join(menage_socio_demo,acq_and_ve_by_year[,c(1,3,6)])

#coupke total menage et personne morale#

mytest$couple_menage_ou_autres<-ifelse(mytest$couple_ve_acq == "couple_acquereur_vendeur" & mytest$vendeur_rg == "vendeur_particulier" & mytest$acquereur_rg == "acquereur_particulier", "couple_particulier",
                                       ifelse(mytest$couple_ve_acq == "couple_acquereur_vendeur" & mytest$vendeur_rg == "vendeur_morale" & mytest$acquereur_rg == "acquereur_morale", "couple_morale",
                                       "couple_autre"))

table(mytest$couple_menage_ou_autres, useNA = "ifany")
rg_couple_menage<- mytest%>% 
  filter(couple_menage_ou_autres=="couple_particulier")%>%
  select (annee,couple_menage_ou_autres)%>% 
  group_by(annee)%>% 
  count(couple_menage_ou_autres)%>%
  arrange (n,annee)%>%
  spread(couple_menage_ou_autres,n)

####
menage_socio_demo<- left_join(menage_socio_demo,rg_couple_menage)
menage_socio_demo<-as.data.frame(menage_socio_demo)
#####
menage_socio_demo[,c(2,5,8)]<- menage_socio_demo[1:12,c(2,5,8)]/menage_socio_demo[1:12,c(11)]
menage_socio_demo[,c(3,6,9)]<- menage_socio_demo[1:12,c(3,6,9)]/menage_socio_demo[1:12,c(12)]
menage_socio_demo[,c(4,7,10)]<- menage_socio_demo[1:12,c(4,7,10)]/menage_socio_demo[1:12,c(13)]

menage_socio_demo<-gather(menage_socio_demo, "variables", "Values", 2:10)

menage_socio_demo$typeVar <- substr(menage_socio_demo$variables, start = 0, stop = 4)
menage_socio_demo$typeVar_2 <- substr(menage_socio_demo$variables, start = 0, stop = 9)



menage_socio_demo$typeVar<- ifelse (menage_socio_demo$typeVar=="age_" | menage_socio_demo$typeVar_2=="couple_ag", "Age",
                                   ifelse (menage_socio_demo$typeVar=="sexe"| menage_socio_demo$typeVar_2=="couple_se", "Sexe",
                                        ifelse (menage_socio_demo$typeVar=="sitm" | menage_socio_demo$typeVar_2=="couple_si", "Situation matrimoniale", menage_socio_demo$typeVar )))
menage_socio_demo$blob <- ifelse(grepl(x = menage_socio_demo$variables, pattern = "couple"), "couple acquéreur vendeur", 
                                          ifelse(grepl(x = menage_socio_demo$variables, pattern = "acq"),"acquéreur", "vendeur"))


description_menages_acq_ve.plot<- ggplot(menage_socio_demo, aes(annee, Values, group=blob)) +
  geom_line(aes(color = blob))+
  geom_line(aes(color = blob),size=2,alpha=0.2,show.legend =F)+
  geom_line(aes(color = blob,linetype = "dashed"), show.legend =F) +  geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  facet_wrap(~typeVar, nrow = 1) +
  theme_tmd() +
  labs(title = "Evolution du taux de renseignement des parties de la vente de type ménage", 
       subtitle = "Fréquence par année, strictement sur l'effectif des ménages acquéreurs, vendeurs et couple acquéreurs-vendeurs \n ex : L'information sur le couple est calculée par rapport à l'effectif par année de couples renseignés comme ménages  ", x= NULL, y= NULL)+ 
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

ggsave("description_menages_acq_ve.plot.png",plot= description_menages_acq_ve.plot, "C:/Users/Utilisateur/Documents/Thibault__4eme/thèse_redaction/Image_BIEN", device = "png", width = 220, height = 120, units = "mm", dpi = 330)


###################################################################################################################

# description géographiques : insee et nationalite
mytest$origin_nation_ac_rg<-ifelse (mytest$CODNAT_AC == "" | mytest$CODNAT_AC ==" " |  mytest$CODNAT_AC == "NA", "code_nat_non_connue",mytest$CODNAT_AC)
mytest$origin_nation_ac_rg<- ifelse (mytest$origin_nation_ac_rg!="code_nat_non_connue", "code_nat_connu",mytest$origin_nation_ac_rg)

mytest$origin_nation_ve_rg<-ifelse (mytest$CODNAT_VE == "" | mytest$CODNAT_VE ==" " |  mytest$CODNAT_VE == "NA", "code_nat_non_connue",mytest$CODNAT_VE)
mytest$origin_nation_ve_rg<- ifelse (mytest$origin_nation_ve_rg!="code_nat_non_connue", "code_nat_connu",mytest$origin_nation_ve_rg)

mytest$origin_nation_couple_ve_ac_rg <- ifelse (mytest$couple_ve_acq == "couple_acquereur_vendeur" & mytest$origin_nation_ve_rg == "code_nat_connu" & mytest$origin_nation_ac_rg== "code_nat_connu", "code_nat_couple_ac_ve_connu", "code_nat_couple_ac_ve_NON_connu"  )



mytest$origin_nation_ac_rg<- ifelse (mytest$acquereur_rg=="acquereur_morale" & mytest$origin_nation_ac_rg == "code_nat_connu","code_nat_morale_ac_connu",
                                     ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$origin_nation_ac_rg == "code_nat_connu","code_nat_particulier_ac_connu",
                                            "No_informations_origine_ou_type_acquereur"))




mytest$origin_nation_ve_rg<- ifelse (mytest$vendeur_rg=="vendeur_morale" & mytest$origin_nation_ve_rg == "code_nat_connu","code_nat_morale_ve_connu",
                                     ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$origin_nation_ve_rg == "code_nat_connu","code_nat_particulier_ve_connu",
                                             "No_informations_origine_ou_type_vendeur"))




origin_nation_ac<- mytest%>% 
  filter(origin_nation_ac_rg=="code_nat_particulier_ac_connu")%>%
  select (annee,origin_nation_ac_rg)%>% 
  group_by(annee)%>% 
  count(origin_nation_ac_rg)%>%
  arrange (n,annee)%>%
  spread(origin_nation_ac_rg,n)

origin_nation_morale_ac<- mytest%>% 
  filter(origin_nation_ac_rg=="code_nat_morale_ac_connu")%>%
  select (annee,origin_nation_ac_rg)%>% 
  group_by(annee)%>% 
  count(origin_nation_ac_rg)%>%
  arrange (n,annee)%>%
  spread(origin_nation_ac_rg,n)



origin_nation_ve<- mytest%>% 
  filter(origin_nation_ve_rg=="code_nat_particulier_ve_connu")%>%
  select (annee,origin_nation_ve_rg)%>% 
  group_by(annee)%>% 
  count(origin_nation_ve_rg)%>%
  arrange (n,annee)%>%
  spread(origin_nation_ve_rg,n)

origin_nation_morale_ve<- mytest%>% 
  filter(origin_nation_ve_rg=="code_nat_morale_ve_connu")%>%
  select (annee,origin_nation_ve_rg)%>% 
  group_by(annee)%>% 
  count(origin_nation_ve_rg)%>%
  arrange (n,annee)%>%
  spread(origin_nation_ve_rg,n)



origin_nation_couple<-  mytest%>% 
  filter(origin_nation_couple_ve_ac_rg=="couple_origin_nat_ac_ve_connu")%>%
  select (annee,origin_nation_couple_ve_ac_rg)%>% 
  group_by(annee)%>% 
  count(origin_nation_couple_ve_ac_rg)%>%
  arrange (n,annee)%>%
  spread(origin_nation_couple_ve_ac_rg,n)

info_cod_nat<- left_join( origin_nation_ac,origin_nation_morale_ac, by="annee")
info_cod_nat<- left_join( info_cod_nat,origin_nation_ve, by="annee")
info_cod_nat<- left_join( info_cod_nat,origin_nation_morale_ve, by="annee")
info_cod_nat<- left_join( info_cod_nat,origin_nation_couple, by="annee")


#code_insee#
mytest$commune_origine_ac<-ifelse (mytest$NUMCOM_AC == "" | mytest$NUMCOM_AC ==" " |  mytest$NUMCOM_AC == "NA", "orig_commune_non_connue",mytest$NUMCOM_AC)
mytest$commune_origine_ac<- ifelse (mytest$commune_origine_ac!="orig_commune_non_connue", "code_commune_connu",mytest$commune_origine_ac)



mytest$commune_origine_ve<-ifelse (mytest$NUMCOM_VE == "" | mytest$NUMCOM_VE ==" " |  mytest$NUMCOM_VE == "NA", "orig_commune_non_connue",mytest$NUMCOM_VE)
mytest$commune_origine_ve<- ifelse (mytest$commune_origine_ve!="orig_commune_non_connue", "code_commune_connu",mytest$commune_origine_ve)


mytest$origin_commune_couple_ve_ac_rg <- ifelse (mytest$couple_ve_acq == "couple_acquereur_vendeur" & mytest$commune_origine_ac == "code_commune_connu" & mytest$commune_origine_ve== "code_commune_connu", "code_commune_couple_ac_ve_connu", "code_commune_couple_ac_ve_NON_connu"  )



mytest$commune_origine_ac<- ifelse (mytest$acquereur_rg=="acquereur_morale" & mytest$commune_origine_ac == "code_commune_connu","code_commune_morale_ac_connu",
                                    ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$commune_origine_ac == "code_commune_connu","code_commune_particulier_ac_connu",
                                            "No_informations_originecommune_ou_type_acquereur"))


mytest$commune_origine_ve<- ifelse (mytest$vendeur_rg=="vendeur_morale" & mytest$commune_origine_ve == "code_commune_connu","code_commune_morale_ve_connu",
                                    ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$commune_origine_ve == "code_commune_connu","code_commune_particulier_ve_connu",
                                            "No_informations_originecommune_ou_type_vequereur"))

table(mytest$commune_origine_ac, useNA = "ifany")




origin_commune_ac<- mytest%>% 
  filter(commune_origine_ac=="code_commune_particulier_ac_connu")%>%
  select (annee,commune_origine_ac)%>% 
  group_by(annee)%>% 
  count(commune_origine_ac)%>%
  arrange (n,annee)%>%
  spread(commune_origine_ac,n)

origin_commune_morale_ac<- mytest%>% 
  filter(commune_origine_ac=="code_commune_morale_ac_connu")%>%
  select (annee,commune_origine_ac)%>% 
  group_by(annee)%>% 
  count(commune_origine_ac)%>%
  arrange (n,annee)%>%
  spread(commune_origine_ac,n)



origin_commune_ve<- mytest%>% 
  filter(commune_origine_ve=="code_commune_particulier_ve_connu")%>%
  select (annee,commune_origine_ve)%>% 
  group_by(annee)%>% 
  count(commune_origine_ve)%>%
  arrange (n,annee)%>%
  spread(commune_origine_ve,n)

origin_commune_morale_ve<- mytest%>% 
  filter(commune_origine_ve=="code_commune_morale_ve_connu")%>%
  select (annee,commune_origine_ve)%>% 
  group_by(annee)%>% 
  count(commune_origine_ve)%>%
  arrange (n,annee)%>%
  spread(commune_origine_ve,n)



origin_commune_couple<-  mytest%>% 
  filter(origin_commune_couple_ve_ac_rg=="couple_origin_commune_ac_ve_connu")%>%
  select (annee,origin_commune_couple_ve_ac_rg)%>% 
  group_by(annee)%>% 
  count(origin_commune_couple_ve_ac_rg)%>%
  arrange (n,annee)%>%
  spread(origin_commune_couple_ve_ac_rg,n)

info_cod_com<- left_join( origin_commune_ac,origin_commune_morale_ac, by="annee")
info_cod_com<- left_join( info_cod_com,origin_commune_ve, by="annee")
info_cod_com<- left_join( info_cod_com,origin_commune_morale_ve, by="annee")
info_cod_com<- left_join( info_cod_com,origin_commune_couple, by="annee")






#code dept#

mytest$departement_origine_ac<-ifelse (mytest$PADEPT_AC == "" | mytest$PADEPT_AC ==" " |  mytest$PADEPT_AC == "NA", "orig_departement_non_connue",mytest$PADEPT_AC)
mytest$departement_origine_ac<- ifelse (mytest$departement_origine_ac!="orig_departement_non_connue", "code_departement_connu",mytest$departement_origine_ac)



mytest$departement_origine_ve<-ifelse (mytest$PADEPT_VE == "" | mytest$PADEPT_VE ==" " |  mytest$PADEPT_VE == "NA", "orig_departement_non_connue",mytest$PADEPT_VE)
mytest$departement_origine_ve<- ifelse (mytest$departement_origine_ve!="orig_departement_non_connue", "code_departement_connu",mytest$departement_origine_ve)


mytest$origin_departement_couple_ve_ac_rg <- ifelse (mytest$couple_ve_acq == "couple_acquereur_vendeur" & mytest$departement_origine_ac == "code_departement_connu" & mytest$departement_origine_ve== "code_departement_connu", "code_departement_couple_ac_ve_connu", "code_departement_couple_ac_ve_NON_connu"  )



mytest$departement_origine_ac<- ifelse (mytest$acquereur_rg=="acquereur_morale" & mytest$departement_origine_ac == "code_departement_connu","code_departement_morale_ac_connu",
                                    ifelse (mytest$acquereur_rg=="acquereur_particulier" & mytest$departement_origine_ac == "code_departement_connu","code_departement_particulier_ac_connu",
                                            "No_informations_originedepartement_ou_type_acquereur"))


mytest$departement_origine_ve<- ifelse (mytest$vendeur_rg=="vendeur_morale" & mytest$departement_origine_ve == "code_departement_connu","code_departement_morale_ve_connu",
                                    ifelse (mytest$vendeur_rg=="vendeur_particulier" & mytest$departement_origine_ve == "code_departement_connu","code_departement_particulier_ve_connu",
                                            "No_informations_originedepartement_ou_type_vequereur"))


table(mytest$departement_origine_ac, useNA = "ifany")


origin_departement_ac<- mytest%>% 
  filter(departement_origine_ac=="code_departement_particulier_ac_connu")%>%
  select (annee,departement_origine_ac)%>% 
  group_by(annee)%>% 
  count(departement_origine_ac)%>%
  arrange (n,annee)%>%
  spread(departement_origine_ac,n)

origin_departement_morale_ac<- mytest%>% 
  filter(departement_origine_ac=="code_departement_morale_ac_connu")%>%
  select (annee,departement_origine_ac)%>% 
  group_by(annee)%>% 
  count(departement_origine_ac)%>%
  arrange (n,annee)%>%
  spread(departement_origine_ac,n)



origin_departement_ve<- mytest%>% 
  filter(departement_origine_ve=="code_departement_particulier_ve_connu")%>%
  select (annee,departement_origine_ve)%>% 
  group_by(annee)%>% 
  count(departement_origine_ve)%>%
  arrange (n,annee)%>%
  spread(departement_origine_ve,n)

origin_departement_morale_ve<- mytest%>% 
  filter(departement_origine_ve=="code_departement_morale_ve_connu")%>%
  select (annee,departement_origine_ve)%>% 
  group_by(annee)%>% 
  count(departement_origine_ve)%>%
  arrange (n,annee)%>%
  spread(departement_origine_ve,n)



origin_departement_couple<-  mytest%>% 
  filter(origin_departement_couple_ve_ac_rg=="couple_origin_departement_ac_ve_connu")%>%
  select (annee,origin_departement_couple_ve_ac_rg)%>% 
  group_by(annee)%>% 
  count(origin_departement_couple_ve_ac_rg)%>%
  arrange (n,annee)%>%
  spread(origin_departement_couple_ve_ac_rg,n)

info_cod_dpt<- left_join( origin_departement_ac,origin_departement_morale_ac, by="annee")
info_cod_dpt<- left_join( info_cod_dpt,origin_departement_ve, by="annee")
info_cod_dpt<- left_join( info_cod_dpt,origin_departement_morale_ve, by="annee")
info_cod_dpt<- left_join( info_cod_dpt,origin_departement_couple, by="annee")

###########

Info_geo<- left_join(info_cod_nat,info_cod_com, by="annee")
Info_geo<- left_join(Info_geo,info_cod_dpt, by="annee")
#####
acq_and_ve_by_year$annee <- as.numeric(acq_and_ve_by_year$annee)
couple_ve_acq$annee <- as.numeric(couple_ve_acq$annee)
Info_geo<- left_join(Info_geo,acq_and_ve_by_year[,c(1:3,5:6)], by="annee")
Info_geo<- left_join(Info_geo,couple_ve_acq[,c(1:2)], by="annee")
Info_geo<-as.data.frame(Info_geo)

#######
Info_geo[,c(2,7,12)]<- Info_geo[1:12,c(2,7,12)]/Info_geo[1:12,c(18)]
Info_geo[,c(3,8,13)]<- Info_geo[1:12,c(3,8,13)]/Info_geo[1:12,c(17)]
Info_geo[,c(4,9,14)]<- Info_geo[1:12,c(4,9,14)]/Info_geo[1:12,c(20)]
Info_geo[,c(5,10,15)]<- Info_geo[1:12,c(5,10,15)]/Info_geo[1:12,c(19)]
Info_geo[,c(6,11,16)]<- Info_geo[1:12,c(6,11,16)]/Info_geo[1:12,c(21)]

Info_geo<-gather(Info_geo, "variables", "Values", 2:16)


Info_geo$variables<- ifelse (Info_geo$variables=="couple_origin_departement_ac_ve_connu", "code_departement_couple_ac_ve_connu",
                           ifelse (Info_geo$variables=="couple_origin_commune_ac_ve_connu", "code_commune_couple_ac_ve_connu",
                                   ifelse ( Info_geo$variables=="couple_origin_nat_ac_ve_connu" , "code_nat_couple_ac_ve_connu",Info_geo$variables)))
Info_geo$typeVar <- substr(Info_geo$variables, start = 0, stop = 6)
Info_geo$blob<- ifelse (Info_geo$typeVar=="code_n", "Nationalité d'origine",
                                ifelse (Info_geo$typeVar=="code_c", "Commune d'origine",
                                        ifelse ( Info_geo$typeVar=="code_d", "Département d'origine",Info_geo$typeVar )))

Info_geo$typeVar <- ifelse(grepl(x = Info_geo$variables, pattern = "couple"), "Couple acquéreur vendeur", 
                                 ifelse(grepl(x = Info_geo$variables, pattern = "morale_ac"),"Acquéreur personne morale", 
                                        ifelse(grepl(x = Info_geo$variables, pattern = "morale_ve"),"Vendeur personne morale",
                                               ifelse(grepl(x = Info_geo$variables, pattern = "particulier_ac"),"Acquéreur ménage",
                                                      ifelse(grepl(x = Info_geo$variables, pattern = "particulier_ve"),"Vendeur ménage",
                                                             Info_geo$variables)))))


Info_geo.plot<-ggplot(Info_geo, aes(annee, Values, group=blob)) +
  geom_line(aes(color = blob))+
  geom_line(aes(color =blob),size=2,alpha=0.2,show.legend =F)+
  geom_line(aes(color = blob,linetype = "dashed"), show.legend =F) +  geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  theme_tmd() +
  facet_wrap(~typeVar, nrow = 2)+
  labs(title = "Evolution des renseignements sur l'origine géographique vendeurs et acquéreurs", x= NULL, y= NULL)+ 
  labs(subtitle = "Fréquence par année, strictement sur l'effectif des ménages et personnes morales acquéreurs, vendeurs et couples renseignés \n ex : L'information par année de la 'commune d'origine' de 'acquéreur personne morale' est calculée sur l'effectif par année d'acquéreurs\n renseignés comme personne morale", x= NULL, y= NULL)+
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

ggsave("Info_geo.plot.png",plot= Info_geo.plot, "C:/Users/Utilisateur/Documents/Thibault__4eme/thèse_redaction/Image_BIEN", device = "png", width = 220, height = 150, units = "mm", dpi = 330)

#####################                       ################################
#######################variables financières#################################
###certaines variable demandent une entrée par type de biens, d'autres par type d'acquéreurs

#affectation du logement  sur l'ensemble des transactions : à faire après.
table (mytest$REQ_AF_VE)
# variable  mal renseignée et qui sous estime certaines dimensions notamment IL et RS. Difficile de s'y frotter pour une analyse systématique

####prix et crédit#

tableau_renseignement_prix_credit<- tableau_renseignement_fin[1:12,c(2,4)]/ tableau_renseignement_fin[1:12,c(5)]
tableau_renseignement_prix_credit$annee<-tableau_renseignement_fin$annee
tableau_renseignement_prix_credit[, c(1,2,3)]<- tableau_renseignement_prix_credit [, c(3,2,1)]
colnames(tableau_renseignement_prix_credit)<- c("annee","credit","Prix")

#rg du montant credit si presence credit
mytest$montant_credit_rg<- ifelse(mytest$PRESCREDIT=="O" & mytest$MTCRED>=1, "credit_montant_connu",
                                  ifelse(mytest$PRESCREDIT=="O" & mytest$MTCRED<=1 | mytest$PRESCREDIT=="O" & mytest$MTCRED==" " , "presence_credit_montant_Inconnu",
                                  ifelse(mytest$PRESCREDIT=="N" , "pas de credit", 
                                         ifelse(  mytest$PRESCREDIT=="N" & mytest$MTCRED>=1, "pas de credit mais montant indique",
                                      ifelse(mytest$PRESCREDIT!="O" | mytest$PRESCREDIT!="N", "credit securise espere", 
                                             ifelse(mytest$PRESCREDIT!="O" | mytest$PRESCREDIT!="N"  & mytest$MTCRED>=1, "pas d'information presence mais montant",   mytest$PRESCREDIT  ))))))
  

table (mytest$annee,mytest$PRESCREDIT_rg, useNA = "ifany")  
Avec_credit_rg<- mytest%>% 
  filter(PRESCREDIT_rg=="info_credit")%>%
  select (annee,PRESCREDIT_rg)%>% 
  group_by(annee)%>% 
  count(PRESCREDIT_rg)%>%
  arrange (n,annee)%>%
  spread(PRESCREDIT_rg,n)

montant_credit_rg<- mytest%>% 
  filter(montant_credit_rg=="credit_montant_connu")%>%
  select (annee,montant_credit_rg)%>% 
  group_by(annee)%>% 
  count(montant_credit_rg)%>%
  arrange (n,annee)%>%
  spread(montant_credit_rg,n)

transac_avec_credit<- left_join(Avec_credit_rg,montant_credit_rg, by="annee")
transac_avec_credit<- as.data.frame(transac_avec_credit)
transac_avec_credit$credit_montant_connu<- transac_avec_credit[1:12,c(3)]/ transac_avec_credit[1:12,c(2)]

#######
tableau_renseignement_fin$annee<-as.numeric(as.character(tableau_renseignement_fin$annee))
tableau_renseignement_prix_credit$annee<-as.numeric(as.character(tableau_renseignement_prix_credit$annee))
tableau_renseignement_prix_credit<- left_join(tableau_renseignement_fin,transac_avec_credit[,c(1,3)], by="annee")
###
mytest$SURFHABDEC_rg<- ifelse(mytest$SURFHABDEC  == "na","na",
                                 ifelse( mytest$SURFHABDEC == "","na", 
                                         ifelse( mytest$SURFHABDEC == " ","na","info_SURFHABDECU")))
SURFHABDEC_rg<-as.data.frame.matrix(table (mytest$annee,mytest$SURFHABDEC_rg,useNA= "ifany",deparse.level = 0))
SURFHABDEC_rg$annee<- row.names(SURFHABDEC_rg)
SURFHABDEC_rg$annee<-as.numeric(as.character(SURFHABDEC_rg$annee))
tableau_renseignement_prix_credit<- left_join(tableau_renseignement_prix_credit,SURFHABDEC_rg[,c(1,3)], by="annee")
####
tableau_renseignement_prix_credit$pm_par_info_surf<- tableau_renseignement_prix_credit[1:12,c(3)]/tableau_renseignement_prix_credit[1:12,c(9)]
tableau_renseignement_prix_credit[1:12,c(2:6)]<- tableau_renseignement_prix_credit[1:12,c(2:6)]/tableau_renseignement_prix_credit[1:12,c(7)]
tableau_renseignement_prix_credit$credit_presence_ou_non<- tableau_renseignement_prix_credit[1:12,c(5)]+tableau_renseignement_prix_credit[1:12,c(6)]


table_prix_credit<-gather(tableau_renseignement_prix_credit, "variables", "Values", c(2,3,8,10,11))

table_prix_credit$typeVar <- substr(table_prix_credit$variables, start = 0, stop = 8)




table_prix_credit$typeVar<- ifelse (table_prix_credit$typeVar=="pm_par_i",  "Prix au m² si surface habitable déclarée",
                             ifelse (table_prix_credit$typeVar=="credit_m" , "Montant du crédit si présence d'un crédit",
                            ifelse (table_prix_credit$typeVar=="Prix"|table_prix_credit$typeVar=="credit_p"| table_prix_credit$typeVar=="info_pri", "Prix, prix au m² et crédit", table_prix_credit$typeVar )))

table_prix_credit$blob <- ifelse(grepl(x = table_prix_credit$variables, pattern = "credit"), "Crédit",
                                 ifelse(grepl(x = table_prix_credit$variables, pattern = "info"),"Prix au m²", "Prix" ))


table_prix_credit.plot<- ggplot(table_prix_credit, aes(annee, Values, group=blob)) +
  geom_line(aes(color = blob))+
  geom_line(aes(color = blob),size=2,alpha=0.2,show.legend =F)+
  geom_line(aes(color = blob,linetype = "dashed"), show.legend =F) +  geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  facet_wrap(~typeVar, nrow = 2)+
  theme_tmd() +
  labs(title = "Evolution du taux de renseignement des variables financières", 
       subtitle = "Fréquence par année sur l'ensemble des transactions ou effectif de transactions avec contraintes \n ex : L'information du 'prix au m² si surface habitable déclarée' est calculée par rapport à l'effectif par année des transactions avec renseignement \n de la surface habitable déclarée", x= NULL, y= NULL)+ 
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

ggsave("table_prix_credit.plot.png",plot= table_prix_credit.plot, "C:/Users/Utilisateur/Documents/Thibault__4eme/thèse_redaction/Image_BIEN", device = "png", width = 220, height = 150, units = "mm", dpi = 330)

####inside le crédit##
tableau_credit<- tableau_renseignement_prix_credit[,c(1,4,5,6)]



table_type_credit<-gather(tableau_credit, "variables", "Values", 2:4)

table_type_credit$typeVar <- substr(table_type_credit$variables, start = 0, stop = 8)




table_type_credit$typeVar<- ifelse (table_type_credit$typeVar=="credit s",  "Crédit sécurisé attendu",
                                    ifelse (table_type_credit$typeVar=="info_cre" , "Crédit hypothécaire",
                                            ifelse (table_type_credit$typeVar=="info_pas", "Pas de crédit à l'achat", table_type_credit$typeVar )))

# table_type_credit$blob <- ifelse(grepl(x = table_type_credit$variables, pattern = "credit"), "Crédit",
#                                  ifelse(grepl(x = table_type_credit$variables, pattern = "info"),"Prix au m²", "Prix" ))


table_type_credit.plot<- ggplot(table_type_credit, aes(annee, Values, group=typeVar)) +
  geom_line(aes(color = typeVar))+
  geom_line(aes(color = typeVar),size=2,alpha=0.2,show.legend =F)+
  geom_line(aes(color = typeVar,linetype = "dashed"), show.legend =F) +  geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  theme_tmd() +
  labs(title = "Evolution de la présence d'un crédit à l'achat", 
       subtitle = "Fréquence par année sur l'ensemble des transactions", x= NULL, y= NULL)+ 
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

ggsave("table_type_credit.plot.png",plot= table_type_credit.plot, "C:/Users/Utilisateur/Documents/Thibault__4eme/thèse_redaction/Image_BIEN", device = "png", width = 220, height = 120, units = "mm", dpi = 330)

