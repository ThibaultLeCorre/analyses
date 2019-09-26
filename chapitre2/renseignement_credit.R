tableau_credit<- data_redresse_code_promo_2[,c("ID","annee.x","PRESCREDIT","MTCRED","personne_morale_actifs_ac")]

table(tableau_credit$personne_morale_actifs_ac,tableau_credit$annee.x,useNA = "ifany")

# tableau_credit$renseignement_credit<- ifelse(tableau_credit$PRESCREDIT=="O" & tableau_credit$MTCRED>=1, "credit_montant_connu",
#                                   ifelse(tableau_credit$PRESCREDIT=="O" & tableau_credit$MTCRED<=1 | tableau_credit$PRESCREDIT=="O" & tableau_credit$MTCRED==" " , "presence_credit_montant_Inconnu",
#                                          ifelse(tableau_credit$PRESCREDIT=="N" , "pas de credit", 
#                                                 ifelse(  tableau_credit$PRESCREDIT=="N" & tableau_credit$MTCRED>=1, "pas de credit mais montant indique",
#                                                          ifelse(tableau_credit$PRESCREDIT!="O" | tableau_credit$PRESCREDIT!="N", "credit securise espere", 
#                                                                 ifelse(tableau_credit$PRESCREDIT!="O" | tableau_credit$PRESCREDIT!="N"  & tableau_credit$MTCRED>=1, "pas d'information presence mais montant",   tableau_credit$PRESCREDIT  ))))))
# 

tableau_credit$renseignement_credit<-ifelse(is.na(tableau_credit$PRESCREDIT),"Crédit sécurisé par caution attendu",tableau_credit$PRESCREDIT)
tableau_credit$renseignement_credit<- ifelse(tableau_credit$renseignement_credit=="O","Crédit hypothécaire",
                                             ifelse(tableau_credit$renseignement_credit=="0","Crédit hypothécaire",
                                             ifelse(tableau_credit$renseignement_credit=="N","Pas de crédit à l'achat",tableau_credit$renseignement_credit)))
                                                 

                                             

table(tableau_credit$renseignement_credit,useNA = "ifany")
#voir script table_acquereurs



# table_type_credit<-gather(tableau_credit, "variables", "Values", 2:4)

# table_type_credit$typeVar <- substr(table_type_credit$variables, start = 0, stop = 8)
# 
# table_type_credit$typeVar<- ifelse (table_type_credit$typeVar=="credit s",  "Crédit sécurisé attendu",
#                                     ifelse (table_type_credit$typeVar=="info_cre" , "Crédit hypothécaire",
#                                             ifelse (table_type_credit$typeVar=="info_pas", "Pas de crédit à l'achat", table_type_credit$typeVar )))

# table_type_credit$blob <- ifelse(grepl(x = table_type_credit$variables, pattern = "credit"), "Crédit",
#                                  ifelse(grepl(x = table_type_credit$variables, pattern = "info"),"Prix au m²", "Prix" ))

library(dplyr)
tableau_credit_table_effectifs<-as.data.frame(table(tableau_credit$annee.x,tableau_credit$renseignement_credit,useNA = "ifany"))
tableau_credit_table_effectifs$Type<-"Volume"
tableau_credit_table_pourcentage<-table(tableau_credit$annee.x,tableau_credit$renseignement_credit,useNA = "ifany")
tableau_credit_table_pourcentage<-as.data.frame(prop.table(tableau_credit_table_pourcentage,margin = 1)*100)
tableau_credit_table_pourcentage$Type<-"Pourcentage"
tableau_credit_table<-rbind(tableau_credit_table_effectifs,tableau_credit_table_pourcentage)
tableau_credit_table$Année<-factor(tableau_credit_table$Var1)
tableau_credit_table$Année<-as.character(tableau_credit_table$Année)
tableau_credit_table$Année<-as.numeric(tableau_credit_table$Année)
tableau_credit_table$Freq<-as.numeric(tableau_credit_table$Freq)
tableau_credit_table$Type_crédit<-tableau_credit_table$Var2

library(reshape2)
library(ggplot2)
 tableau_credit_plot<-
  ggplot(tableau_credit_table, aes(Année, Freq, group=Var2)) +
  geom_line(aes(color = Var2),size=1)+
  scale_x_continuous(breaks = c(1996,1999,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  facet_wrap(~Type, scales="free")+ 
  labs(title = "Evolution de la présence d'un crédit à l'achat", 
       subtitle = "Calcul réalisé par année sur les effectifs redressés des transactions", x= NULL, y= NULL)+ 
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

library(extrafont)
loadfonts()
fonts() 
ggsave("tableau_credit_plot.pdf",plot= tableau_credit_plot,setwd("~/Sauvegarde_figures"), device = "pdf", width = 320, height = 180, units = "mm", dpi = 330)


########Sur les actifs et les retraités##########


tableau_credit<- data_redresse_code_promo_2[,c("ID","annee.x","PRESCREDIT","MTCRED","personne_morale_actifs_ac")]
tableau_credit<-tableau_credit%>%filter(personne_morale_actifs_ac=="retraites_inactifs" | personne_morale_actifs_ac=="Actifs")

table(tableau_credit$PRESCREDIT,tableau_credit$personne_morale_actifs_ac,tableau_credit$annee.x,useNA = "ifany")

# tableau_credit$renseignement_credit<- ifelse(tableau_credit$PRESCREDIT=="O" & tableau_credit$MTCRED>=1, "credit_montant_connu",
#                                   ifelse(tableau_credit$PRESCREDIT=="O" & tableau_credit$MTCRED<=1 | tableau_credit$PRESCREDIT=="O" & tableau_credit$MTCRED==" " , "presence_credit_montant_Inconnu",
#                                          ifelse(tableau_credit$PRESCREDIT=="N" , "pas de credit", 
#                                                 ifelse(  tableau_credit$PRESCREDIT=="N" & tableau_credit$MTCRED>=1, "pas de credit mais montant indique",
#                                                          ifelse(tableau_credit$PRESCREDIT!="O" | tableau_credit$PRESCREDIT!="N", "credit securise espere", 
#                                                                 ifelse(tableau_credit$PRESCREDIT!="O" | tableau_credit$PRESCREDIT!="N"  & tableau_credit$MTCRED>=1, "pas d'information presence mais montant",   tableau_credit$PRESCREDIT  ))))))
# 

tableau_credit$renseignement_credit<-ifelse(is.na(tableau_credit$PRESCREDIT),"Crédit sécurisé par caution supposé",tableau_credit$PRESCREDIT)
tableau_credit$renseignement_credit<- ifelse(tableau_credit$renseignement_credit=="O","Crédit hypothécaire",
                                             ifelse(tableau_credit$renseignement_credit=="0","Crédit hypothécaire",
                                                    ifelse(tableau_credit$renseignement_credit=="N","Pas de crédit à l'achat",tableau_credit$renseignement_credit)))




table(tableau_credit$renseignement_credit,useNA = "ifany")
#voir script table_acquereurs



# table_type_credit<-gather(tableau_credit, "variables", "Values", 2:4)

# table_type_credit$typeVar <- substr(table_type_credit$variables, start = 0, stop = 8)
# 
# table_type_credit$typeVar<- ifelse (table_type_credit$typeVar=="credit s",  "Crédit sécurisé attendu",
#                                     ifelse (table_type_credit$typeVar=="info_cre" , "Crédit hypothécaire",
#                                             ifelse (table_type_credit$typeVar=="info_pas", "Pas de crédit à l'achat", table_type_credit$typeVar )))

# table_type_credit$blob <- ifelse(grepl(x = table_type_credit$variables, pattern = "credit"), "Crédit",
#                                  ifelse(grepl(x = table_type_credit$variables, pattern = "info"),"Prix au m²", "Prix" ))

library(dplyr)
tableau_credit_table<-as.data.frame(table(tableau_credit$annee.x,tableau_credit$renseignement_credit,tableau_credit$personne_morale_actifs_ac,useNA = "ifany"))

tableau_credit_table$Année<-factor(tableau_credit_table$Var1)
tableau_credit_table$Année<-as.character(tableau_credit_table$Année)
tableau_credit_table$Année<-as.numeric(tableau_credit_table$Année)
tableau_credit_table$Freq<-as.numeric(tableau_credit_table$Freq)
tableau_credit_table$Type_crédit<-tableau_credit_table$Var2

library(reshape2)
library(ggplot2)
tableau_credit_plot_categorie_actifs_retraites<-ggplot(tableau_credit_table, aes(Année, Freq, group=Var2)) +
  geom_line(aes(color = Var2),size=1)+
  scale_x_continuous(breaks = c(1996,1999,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  facet_wrap(~Var3, scales="free")+ 
  labs(title = "Comment achète les ménages?", 
       subtitle = "Calcul réalisé par année sur les effectifs redressés des transactions", x= NULL, y= NULL)+ 
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

library(extrafont)
loadfonts()
fonts() 
ggsave("tableau_credit_plot_categorie_actifs_retraites.pdf",plot= tableau_credit_plot_categorie_actifs_retraites,setwd("~/Sauvegarde_figures"), device = "pdf", width = 320, height = 180, units = "mm", dpi = 330)


