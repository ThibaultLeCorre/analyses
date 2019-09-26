library(dplyr)
DBSCAN_results_table_Promoteurs <- read.csv("~/Projets/DBSCAN/DBSAN/DBSCAN_results_table_Promoteurs.csv", sep="", stringsAsFactors=FALSE)
data_redresse_code_promo_2<-left_join(data_redresse,DBSCAN_results_table_Promoteurs, by = "ID")

data_redresse_code_promo_2$personne_morale_AC_rg<- ifelse(data_redresse_code_promo_2$QUALITE_AC == "AD"  &!is.na(data_redresse_code_promo_2$QUALITE_AC),"ADM",
                                                        ifelse(data_redresse_code_promo_2$QUALITE_AC== "EN"& !is.na(data_redresse_code_promo_2$QUALITE_AC),"Entreprise",
                                                               ifelse(data_redresse_code_promo_2$QUALITE_AC== "PR"&!is.na(data_redresse_code_promo_2$QUALITE_AC),"Marchand",
                                                                      ifelse(data_redresse_code_promo_2$QUALITE_AC== "SA"&!is.na(data_redresse_code_promo_2$QUALITE_AC),"SAFER",
                                                                             ifelse(data_redresse_code_promo_2$QUALITE_AC== "SC"&!is.na(data_redresse_code_promo_2$QUALITE_AC),"SCI",
                                                                                    ifelse(data_redresse_code_promo_2$QUALITE_AC== "SO"&!is.na(data_redresse_code_promo_2$QUALITE_AC),"HLM", "particulier_ou_na"))))))



table(data_redresse_code_promo_2$personne_morale_AC_rg, useNA = "ifany")
table(data_redresse_code_promo_2$typeVar)

# data_redresse_code_promo_2$personne_morale_AC_rg<- ifelse(data_redresse_code_promo_2$cluster.y>=1 & !is.na(data_redresse_code_promo_2$cluster.y), "promo", data_redresse_code_promo_2$personne_morale_AC_rg)

table(data_redresse_code_promo_2$REQ_ANC,data_redresse_code_promo_2$personne_morale_AC_rg)

data_redresse_code_promo_2$personne_morale_actifs_ac<- ifelse(data_redresse_code_promo_2$CSP_AC>=1 & data_redresse_code_promo_2$CSP_AC<=69  &data_redresse_code_promo_2$personne_morale_AC_rg == "particulier_ou_na","Actifs",
                                                         ifelse(data_redresse_code_promo_2$CSP_AC>=70 & data_redresse_code_promo_2$CSP_AC<=90  & data_redresse_code_promo_2$personne_morale_AC_rg =="particulier_ou_na", "retraites_inactifs", 
                                                                ifelse(is.na(data_redresse_code_promo_2$CSP_AC) & data_redresse_code_promo_2$personne_morale_AC_rg=="particulier_ou_na",NA,data_redresse_code_promo_2$personne_morale_AC_rg)))


table(data_redresse_code_promo_2$personne_morale_actifs_ac,data_redresse_code_promo_2$REQ_ANC, useNA = "ifany")


test_table_acq_vendeur<-left_join(data_redresse_code_promo[,c("ID","personne_morale_actifs")],data_redresse_code_promo_2[,c("ID","personne_morale_actifs_ac")], by="ID")
test_table_acq_vendeur<-test_table_acq_vendeur%>% filter(personne_morale_actifs!="SAFER" & personne_morale_actifs_ac!="SAFER")
 Contigence_ac_vendeur<- (table (test_table_acq_vendeur$personne_morale_actifs,test_table_acq_vendeur$personne_morale_actifs_ac, deparse.level = 1))/2
 ?table
 options(digits=0)
 Khi2test<-chisq.test(Contigence_ac_vendeur)
residus_test_Khi2<-Khi2test$residuals
# Khi2test$statistic
# Khi2test$parameter
# Khi2test$p.value
# Khi2test$residuals
# Khi2test$stdres
# 
# 
# Khi2test$observed
# Khi2test$expected
#  options(scipen=999)
#  prop.table(Contigence_ac_vendeur, margin = 1)*100
#margin = 0 tableau en pourcentage ensemblz effectifs ; margin 2 pourcentage en colonnes
 
 
 #Heatmap sur le test des résidu du KHi 2
 library(ggplot2)
 library(reshape2)
 residus_test_Khi2<-melt(residus_test_Khi2, varnames = c("vendeurs", "acquéreurs"),value.name = "residus")
 Contigence_ac_vendeur<-melt(Contigence_ac_vendeur, varnames = c("vendeurs", "acquéreurs"), value.name = "Effectifs")
 tableau_resid_effectifs<-left_join(residus_test_Khi2, Contigence_ac_vendeur, by = c("vendeurs", "acquéreurs"))
 #plot
 tableau_resid_effectifs_plot<- ggplot(tableau_resid_effectifs, aes(acquéreurs,vendeurs, fill= residus)) +
   geom_tile()+
   geom_text(aes(label=round(Effectifs, digits = 0))) +
   theme_tmd() +
   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                        midpoint = 0, limit = c(-50,50), space = "Lab",
                        name= "Variation des résidus" )+
   labs(title = "Répartition des transactions en fonction des couples acquéreurs-vendeurs", x= NULL, y= NULL)+ 
   labs(subtitle = "Heat map sur les résidus du test du Khi2 de Pearson.\nLes valeurs répertorient le nombre d'échanges entre parties de la vente. Les couleurs représentent les résidus du Khi² (différences entre valeurs observées et valeurs théoriques).\nlecture: Dans la période étudiée, la population retraitée et inactive a vendu 286 718 logements à la population active.\nCette valeur est toutefois en deçà de la valeur théorique en cas d'indépendance statistique des relations.",
        x= "Acquéreurs", y= "Vendeurs")+
   labs(caption = "Sources : échantillon BD BIEN sur effectifs redressés ; Réalisation : Thibault Le Corre, Géographie-Cités, 2018")

 library(extrafont)
 loadfonts()
 fonts() 
 ggsave("tableau_resid_effectifs_plot.pdf",plot= tableau_resid_effectifs_plot,setwd("~/Sauvegarde_figures"), device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
 