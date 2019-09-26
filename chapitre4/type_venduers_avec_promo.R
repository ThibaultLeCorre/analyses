mytest<-data_redresse[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE")]
table( mytest$CSP_AC, mytest$QUALITE_AC, useNA = "ifany", deparse.level = 0)

mytest$Profil_acquereur<- ifelse(mytest$CSP_AC>=1 & mytest$CSP_AC<=69 &!is.na(mytest$CSP_AC),"Actifs",
                                 ifelse(mytest$CSP_AC>=70 & mytest$CSP_AC<=90 &!is.na(mytest$CSP_AC), "Retraites_et_inactifs","PCS_nonconnue"))
table(mytest$Profil_acquereur, useNA = "ifany")

mytest$personne_morale_rg<- ifelse(mytest$QUALITE_AC == "AD"&!is.na(mytest$QUALITE_AC),"morale",
                                   ifelse(mytest$QUALITE_AC== "EN"&!is.na(mytest$QUALITE_AC),"morale",
                                          ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"morale",
                                                 ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"morale",
                                                        ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"morale",
                                                               ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"morale","particulier_ou_na"))))))

table(mytest$personne_morale_rg, mytest$QUALITE_AC)
mytest$acquereur_rg<- ifelse(mytest$personne_morale_rg == "morale","acquereur_morale",
                             ifelse(mytest$Profil_acquereur== "PCS_nonconnue" & mytest$personne_morale_rg == "particulier_ou_na","sans_info","acquereur_particulier"))
# verif :
table(mytest$acquereur_rg, useNA = "ifany")
####### pour le renseignement sur les vendeurs#

table (mytest$PCS_ve_rg, useNA = "ifany")
mytest$Profil_vendeur<- ifelse(mytest$CSP_VE>=1 & mytest$CSP_VE<=69 &!is.na(mytest$CSP_VE),"Actifs",
                          ifelse(mytest$CSP_VE>=70 & mytest$CSP_VE<=90 &!is.na(mytest$CSP_VE), "Retraites_et_inactifs","PCS_nonconnue"))

table(mytest$Profil_vendeur, useNA = "ifany")
mytest$personne_morale_ve_rg<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"morale",
                                      ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"morale",
                                             ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"morale",
                                                    ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"morale",
                                                           ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"morale",
                                                                  ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"morale", "particulier_ou_na"))))))


mytest$vendeur_rg<- ifelse(mytest$personne_morale_ve_rg == "morale","vendeur_morale",
                           ifelse(mytest$Profil_vendeur== "PCS_nonconnue" & mytest$personne_morale_ve_rg == "particulier_ou_na", "sans_info", "vendeur_particulier"))

table(mytest$vendeur_rg, useNA = "ifany")
