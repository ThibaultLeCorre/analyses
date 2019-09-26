mytest<-data_redresse1570533transacs[,c("ID","annee.x","REQ_PRIX","QUALITE_AC","CSP_AC", "QUALITE_VE","CSP_VE","REQTYPBIEN", "REQ_VALUE","DATMUTPREC", "TYPMUTPREC", "PXMUTPREC", "ANNAIS_AC", "ANNAIS_VE","PRESCREDIT", "MTCRED")]
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

###
mytest<-mytest%>%
  filter(Vendeurs!= "Promoteurs" & Vendeurs!= "Agriculteurs",!is.na(DATMUTPREC))
mytest$Annee_MUTPREC<- as.numeric(substr(mytest$DATMUTPREC, start = 7, stop = 10))

mytest$Annee_MUTPREC<- ifelse(mytest$Annee_MUTPREC<1000,as.numeric(substr(mytest$DATMUTPREC, start = 6, stop = 10)),mytest$Annee_MUTPREC)
str(mytest)

mytest$Temps_Pocession<- mytest$annee.x - mytest$Annee_MUTPREC
mytest<-mytest%>%
  filter(Temps_Pocession>=1 & Temps_Pocession<150)


mytest$Age_acq = mytest$annee.x-mytest$ANNAIS_AC

mytest$fourchette_age_acq<-mytest$Age_acq
mytest$fourchette_age_acq<- ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<30 & mytest$acquereurs!="retraites", "[18,30[",
                                   ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<50 & mytest$acquereurs!="retraites", "[30,50[",
                                          ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=50 | !is.na(mytest$CSP_AC) & mytest$acquereurs=="retraites", "[50+",NA)))

mytest$Age_ve = mytest$annee.x-mytest$ANNAIS_VE
mytest$REQTYPBIEN <- ifelse(mytest$REQTYPBIEN=="AP","Appartements", "Maisons")


mytest$fourchette_age_VE<-mytest$Age_ve
mytest$fourchette_age_VE<- ifelse(!is.na(mytest$CSP_VE) & mytest$fourchette_age_VE>=18 & mytest$fourchette_age_VE<30 & mytest$Vendeurs!="retraites", "[18,30[",
                                   ifelse(!is.na(mytest$CSP_VE) & mytest$fourchette_age_VE>=30 & mytest$fourchette_age_VE<50 & mytest$Vendeurs!="retraites", "[30,50[",
                                          ifelse(!is.na(mytest$CSP_VE) & mytest$fourchette_age_VE>=50 | !is.na(mytest$CSP_VE) & mytest$Vendeurs=="retraites", "[50+",NA)))


mytest$TYPMUTPREC_2<- ifelse(mytest$TYPMUTPREC == "A","Acquisition",
                             ifelse(mytest$TYPMUTPREC == "D" | mytest$TYPMUTPREC == "S" ,"Donation ou héritage", NA))

# mytest_PV<-mytest%>%
#   filter(annee.x==1999| annee.x==2008|annee.x==2012, !is.na(REQ_VALUE), TYPMUTPREC_2=="Acquisition",
#          Vendeurs!="Entreprise_Marchands_SCI" &  Vendeurs!="Biens_publics_et_HLM" &  Vendeurs!="Liberales")%>%
#   group_by(annee.x,Vendeurs,Temps_Pocession, REQTYPBIEN)%>%
#   summarise(Moyenne_PV=mean(REQ_VALUE),
#             Moyenne_Prix_achat=mean(PXMUTPREC),
#             Median_PV=median(REQ_VALUE),
#             First_quartile = quantile(REQ_VALUE, probs=0.25),
#             Last_quartile = quantile(REQ_VALUE, probs=0.75),
#             nombre=n())%>%
#   filter(Temps_Pocession<=30 )%>%
#   gather("Type","Value", 5:8)
# 
# Montant_PV_plot_Maison<- mytest_PV%>%
#   filter(Vendeurs!= "autres_inactifs", REQTYPBIEN=="Maisons")%>%
#   ggplot(., aes(Temps_Pocession, Value, fill=Type)) +
#   geom_line()+
#   geom_path(aes(color = Type))+
#   scale_x_continuous(expand = c(0.01, 0),breaks = c(0,5,10,15,20,25,30)) +
#   theme_tmd() +
#   facet_grid(annee.x~Vendeurs,scale="free")+
#   labs(title = "Montants des plus-values sur les maisons par catégories sociales en fonction de la durée de détention des biens", x= "Nombre d'années de détention du bien" , y= "Montant de la plus- value")+ 
#   labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")
# 
# setwd("~/Projets/Plus_value/Plus_value")
# ggsave("Montant_PV_Maisons_plot.png",plot= Montant_PV_plot_Maison,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)
# ggsave("Montant_PV_Maisons_plot.pdf",plot= Montant_PV_plot_Maison,  device = "pdf", width = 310, height = 200, units = "mm", dpi = 330)
# 
# 
# 
# Montant_PV_plot_Appartements<- mytest_PV%>%
#   filter(Vendeurs!= "autres_inactifs", REQTYPBIEN=="Appartements")%>%
#   ggplot(., aes(Temps_Pocession, Value, fill=Type)) +
#   geom_line()+
#   geom_path(aes(color = Type))+
#   scale_x_continuous(expand = c(0.01, 0),breaks = c(0,5,10,15,20,25,30)) +
#   theme_tmd() +
#   facet_grid(annee.x~Vendeurs,scale="free")+
#   labs(title = "Montants des plus-values sur les appartements par catégories sociales en fonction de la durée de détention des biens", x= "Nombre d'années de détention du bien" , y= "Montant de la plus- value")+ 
#   labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")
# setwd("~/Projets/Plus_value/Plus_value")
# ggsave("Montant_PV_plot_Appartements.png",plot= Montant_PV_plot_Appartements,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)



mytest_PV<-mytest%>%
  filter(annee.x==2008|annee.x==2012, !is.na(REQ_VALUE), TYPMUTPREC_2=="Acquisition",
         Vendeurs!="Entreprise_Marchands_SCI" &  Vendeurs!="Biens_publics_et_HLM" &  Vendeurs!="Liberales")%>%
  group_by(annee.x,Vendeurs,Annee_MUTPREC)%>%
  summarise(Moyenne_PV=mean(REQ_VALUE))%>%
  filter(Annee_MUTPREC>=1996 )


mytest_Achats<-mytest%>%
  filter( acquereurs!="Entreprise_Marchands_SCI" &  acquereurs!="Biens_publics_et_HLM" &  acquereurs!="Liberales" & acquereurs!="Agriculteurs" & acquereurs!="SAFER", MTCRED>1000)%>%
  group_by(annee.x,acquereurs)%>%
  summarise(Moyenne_Prix=mean(REQ_PRIX),
            LTV=sum(MTCRED)/sum(REQ_PRIX),
            Capital_emprunté=mean(MTCRED),
            capitaux_propre= Moyenne_Prix-Capital_emprunté)



library(dplyr)
library(tidyr)
PTZ<- BASE_PTZ_2016%>% 
  select (cins,iden,ccsp,tegp, an, dept, dtpp, age)
PTZ<-PTZ %>% filter (tegp<16, 
                     ccsp!=10 & ccsp!=11 & ccsp!=12 & ccsp!=13 ,
                     dept==75 |dept==91 | dept==92| dept==93 | dept==95 | dept==95 | dept==77 | dept==78   )
unique(PTZ$an)

# Classification INSEE  
# 1 : Agriculteurs exploitants
# 2 : Artisans, commer?ants et chefs d'entreprise
# 3 : Cadres et professions intellectuelles sup?rieures
# 4 : Professions Interm?diaires
# 5 : Employ?s
# 6 : Ouvriers
# 7 : Retrait?s
# 8 : Autres personnes sans activit? professionnelle


PTZ$acquereurs<-ifelse(PTZ$ccsp >= 30 & PTZ$ccsp < 40, "CPIS",
                       ifelse(PTZ$ccsp >= 40 & PTZ$ccsp < 50, "Prof_intermediaires",
                              ifelse(PTZ$ccsp >= 50 & PTZ$ccsp < 60, "Employes",
                                     ifelse(PTZ$ccsp >= 60 & PTZ$ccsp < 70, "Ouvriers",
                                            ifelse(PTZ$ccsp >= 70 & PTZ$ccsp < 80, "retraites",NA)))))


PTZ$annee.x<-PTZ$an
PTZ<-PTZ%>%
  filter(!is.na(acquereurs))%>%
  group_by(acquereurs,annee.x)%>%
  summarise(Taux_moyen=median(tegp),
            Duree_moyen=mean(dtpp))



Table_test_modelPV<- left_join(mytest_Achats%>%filter(acquereurs!="autres_inactifs"), PTZ, by= c("acquereurs","annee.x"))

Table_test_modelPV<-left_join(Table_test_modelPV, mytest_PV, by= c("annee.x"= "Annee_MUTPREC", "acquereurs"= "Vendeurs"))
Table_test_modelPV<-Table_test_modelPV%>%
  filter(annee.x>1996, !is.na(Moment_vente))

Mensualites<- function (c,t,n){
  tt = (t /100)/12
  return(c*tt / (1 - 1 / ((1 + tt)^n)))
}

Mensualites_0<- function (c,t,n){
  tt = 0
  return(c /n)
}

Table_test_modelPV$mensu_theo<-Mensualites(Table_test_modelPV$Capital_emprunté,t = Table_test_modelPV$Taux_moyen,Table_test_modelPV$Duree_moyen)

Table_test_modelPV$mensu_0<-Mensualites_0(Table_test_modelPV$Capital_emprunté,t = Table_test_modelPV$Taux_moyen,Table_test_modelPV$Duree_moyen)



Table_test_modelPV$Moment_vente<-Table_test_modelPV$annee.x.y
Table_test_modelPV$Nombre_mois_detention<- (Table_test_modelPV$Moment_vente - Table_test_modelPV$annee.x)*12


Table_test_modelPV$Capital_Interetrembourse<- Table_test_modelPV$mensu_theo * Table_test_modelPV$Nombre_mois_detention

Table_test_modelPV$Capital_rembourse<- Table_test_modelPV$mensu_0 * Table_test_modelPV$Nombre_mois_detention

Table_test_modelPV$Cout_credit<- Table_test_modelPV$Capital_Interetrembourse - Table_test_modelPV$Capital_rembourse


Table_test_modelPV$Capital_restantDu<- Table_test_modelPV$Capital_emprunté - Table_test_modelPV$Capital_rembourse



Table_test_modelPV$PV_nette<-Table_test_modelPV$Moyenne_PV - Table_test_modelPV$Cout_credit
Table_test_modelPV$Benef_nette<-Table_test_modelPV$PV_nette - Table_test_modelPV$capitaux_propre


Table_test_modelPV$Cout_credit_rapporte_Emprunt<-Table_test_modelPV$Cout_credit / Table_test_modelPV$Capital_emprunté

Table_test_modelPV$Cout_credit_rapporte_PrixAch<-Table_test_modelPV$Cout_credit / Table_test_modelPV$Moyenne_Prix
# Table_test_modelPV$PV_nette2<- (Table_test_modelPV$Moyenne_PV + Table_test_modelPV$Capital_restantDu) - Table_test_modelPV$Moyenne_Prix
# 
# Table_test_modelPV$Depense_totale<- Table_test_modelPV$Capital_rembourse + Table_test_modelPV$capitaux_propre
# 
# Table_test_modelPV$Prix_vente<- Table_test_modelPV$Moyenne_Prix+Table_test_modelPV$Moyenne_PV
# 
# Table_test_modelPV$PV_nette<-Table_test_modelPV$Moyenne_PV - Table_test_modelPV$Capital_rembourse
# 
# 
# Table_test_modelPV$amortissement<-Table_test_modelPV$Depense_totale - Table_test_modelPV$Moyenne_Prix
# 
# Table_test_modelPV$PV_nette<-Table_test_modelPV$amortissement + Table_test_modelPV$Moyenne_PV

# Table_test_modelPV$PV_nette<- Table_test_modelPV$Moyenne_PV - Table_test_modelPV$Capital_rembourse



# Table_test_modelPV$cout_credti<- Table_test_modelPV$Duree_moyen * Table_test_modelPV$Capital_emprunté
# 
# Table_test_modelPV$Capital_Inter_rembourse<-  Table_test_modelPV$mensu_theo * Table_test_modelPV$Nombre_mois_detention
# 
# Table_test_modelPV$Depense_totale<- Table_test_modelPV$Capital_Inter_rembourse + Table_test_modelPV$capitaux_propre
# 
# Table_test_modelPV$PV_nette<- Table_test_modelPV$Moyenne_PV - Table_test_modelPV$Depense_totale
# 
# Table_test_modelPV$Restant_du<- Table_test_modelPV$Capital_emprunté - Table_test_modelPV$Capital_Inter_rembourse
# Table_test_modelPV$Restant_du<- ifelse(Table_test_modelPV$Restant_du<=0, 0, Table_test_modelPV$Restant_du)
# Table_test_modelPV$PV_nette<-Table_test_modelPV$Moyenne_PV - (Table_test_modelPV$Restant_du +  Table_test_modelPV$capitaux_propre) 
# 
# 
# Table_test_modelPV$PrixdeVente<- Table_test_modelPV$Moyenne_Prix + Table_test_modelPV$Moyenne_PV
# Table_test_modelPV$PV_restantdurembourse<- Table_test_modelPV$PrixdeVente - Table_test_modelPV$Restant_du
# Table_test_modelPV$PV_nette<- Table_test_modelPV$PV_restantdurembourse - Table_test_modelPV$Capital_Inter_rembourse+Table_test_modelPV$capitaux_propre )
# Table_test_modelPV$PV_nette2<- Table_test_modelPV$PV_restantdurembourse - Table_test_modelPV$Capital_Inter_rembourse+Table_test_modelPV$capitaux_propre )
# 
# 
# Table_test_modelPV$PrixdeVente<- Table_test_modelPV$Moyenne_Prix + Table_test_modelPV$Moyenne_PV

Table_test_modelPV$Nombre_anne_detention<- Table_test_modelPV$Nombre_mois_detention/12

Table_test_modelPV$Moment_vente<-ifelse(Table_test_modelPV$Moment_vente==2008, "Ventes en 2008", "Ventes en 2012")

PV_nette<-Table_test_modelPV%>%
  ggplot(., aes(Nombre_anne_detention, Moyenne_PV, fill=acquereurs)) +
  geom_line()+
  geom_path(aes(color = acquereurs))+
  scale_color_manual(values = specificCol )+
  scale_y_continuous(expand = c(0.01, 0),breaks = c(-10000,0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000,140000,150000,160000,170000))+
  scale_x_continuous(expand = c(0.01, 0),breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  theme_tmd() +
  facet_wrap(~Moment_vente,scale="free")+
  labs(title = "Montants des plus-values moyennes nettes théoriques par catégories sociales en fonction de la durée de détention des biens", x= "Nombre d'années de détention du bien" , y= "Montant de la plus- value")+ 
  labs(subtitle = "Les plus-values moyennes nettes théoriques correspondent aux plus-values soustraites du coût des intérêts du crédit en fonction de la durée de détention du bien.\nElles sont théoriques car elles correspondent au profil d'un individu moyen qui vend en 2008 et 2012\npour un achat entre 1999 et 2011 selon les conditions moyennes d'octroi du crédit par CSP.\nL'emprunt moyen théorique a été calculé à partir des données de la base BIEN, en fonction du prix et du LTV moyen par CSP,\nles conditions d'octroi du crédit (taux d'intérêts moyens TEG et durée de l'emprunt) proviennent des données du SGFGAS.")+
  labs(caption = "Sources : Echantillon BD BIEN, SGFGAS ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")

setwd("~/Projets/Plus_value/Plus_value")
ggsave("PV_nette.pdf",plot= PV_nette, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
ggsave("PV_nette.png",plot= PV_nette, device = "png", width = 380, height = 180, units = "mm", dpi = 330)


Cout_credit_rapporte_PrixAch<-Table_test_modelPV%>%
  ggplot(., aes(Nombre_anne_detention, Cout_credit_rapporte_PrixAch, fill=acquereurs)) +
  geom_line()+
  geom_path(aes(color = acquereurs))+
  scale_color_manual(values = specificCol )+
  scale_y_continuous(expand = c(0.01, 0),breaks = c(0.1,0.15,0.2,0.25,0.3,0.35,0.4))+
  scale_x_continuous(expand = c(0.01, 0),breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  theme_tmd() +
  facet_wrap(~Moment_vente,scale="free")+
  labs(title = "Le coût moyen du crédit immobilier rapporté au prix d'achat moyen des biens", x= "Nombre d'années de détention du bien" , y= "Coût du crédit relatif au prix du bien")+ 
  labs(caption = "Sources : Echantillon BD BIEN, SGFGAS ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2019")

setwd("~/Projets/Plus_value/Plus_value")

ggsave("Cout_credit_rapporte_PrixAch.png",plot= Cout_credit_rapporte_PrixAch, device = "png", width = 380, height = 180, units = "mm", dpi = 330)



Cout_credit_net<-Table_test_modelPV%>%
  filter(Moment_vente=="Ventes en 2012")%>%
  ggplot(., aes(annee.x, Cout_credit, fill=acquereurs)) +
  geom_line()+
  geom_path(aes(color = acquereurs))+
  scale_color_manual(values = specificCol )+
  # scale_y_continuous(expand = c(0.01, 0),breaks = c(0.1,0.15,0.2,0.25,0.3,0.35,0.4))+
  # scale_x_continuous(expand = c(0.01, 0),breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  theme_tmd() +
  facet_wrap(~Moment_vente,scale="free")+
  labs(title = "Le coût net du crédit immobilier pour une vente en 2012", x= "Année de l'accession" , y= "Coût total du crédit (montant des intérêts remboursés)")+ 
  labs(caption = "Sources : Echantillon BD BIEN, SGFGAS ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2019")

setwd("~/Projets/Plus_value/Plus_value")

ggsave("Cout_credit_net.png",plot= Cout_credit_net, device = "png", width = 380, height = 180, units = "mm", dpi = 330)


Mensualites_credit<-Table_test_modelPV%>%
  filter(Moment_vente=="Ventes en 2012")%>%
  ggplot(., aes(annee.x, mensu_theo, fill=acquereurs)) +
  geom_line()+
  geom_path(aes(color = acquereurs))+
  scale_color_manual(values = specificCol )+
  # scale_y_continuous(expand = c(0.01, 0),breaks = c(0.1,0.15,0.2,0.25,0.3,0.35,0.4))+
  # scale_x_continuous(expand = c(0.01, 0),breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) +
  scale_x_continuous(breaks = c(1996,1999, 2003,2005,2007,2009,2012)) +
  theme_tmd() +
  # facet_wrap(~Moment_vente,scale="free")+
  labs(title = "Montant des mensualités théoriques par CSP", x= "Année" , y= "Montant des mensualités théoriques en euros")+ 
  labs(caption = "Sources : Echantillon BD BIEN, SGFGAS ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2019")
setwd("~/Projets/Plus_value/Plus_value")
ggsave("Mensualites_credit.png",plot= Mensualites_credit, device = "png", width = 380, height = 180, units = "mm", dpi = 330)




# 
# 
# capacite_emprunts<- function (m,t,n){
#   out1<-(1+t)^n - 1
#   out2<-t*((1+t)^n)
#   out<- out1/out2
#   c <- m*out
#   return(c)
# }
# 
# 
# 
# 
# 
#             Moyenne_Prix_achat=mean(REQ_PRIX),
#             Median_PV=median(REQ_VALUE),
#             ,
#             nombre=n())%>%
#   filter(Temps_Pocession<=30 )%>%
#   gather("Type","Value", 4:6)
# 
# mytest%>% 
#   filter(!is.na(acquereurs) & MTCRED>1000)%>% 
#   group_by(annee.x, acquereurs, fourchette_age_acq)%>% 
#   summarise()
# 
# Acquereurs_comby<-mytest%>% 
#   filter(!is.na(acquereurs))%>% 
#   group_by(annee.x, acquereurs, REQTYPBIEN)%>% 
#   summarise(Nombre=length(acquereurs),
#             Prix_median=mean(REQ_PRIX))