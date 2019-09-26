DBSCAN_results_table_Promoteurs <- read.csv("~/Projets/DBSCAN/DBSAN/DBSCAN_results_table_Promoteurs.csv", sep="", stringsAsFactors=FALSE)
data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)

data_redresse_code_promo_2<-left_join(data_redresse1570533transacs,DBSCAN_results_table_Promoteurs, by = "ID")
mytest<-data_redresse_code_promo_2[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","cluster.y")]


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

mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"Administration",
                         ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"Entreprise",
                                ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"Marchands de biens",
                                       ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"SAFER",
                                              ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"SCI",
                                                     ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"HLM", "particulier_ou_na"))))))


mytest$Vendeurs<- ifelse(mytest$CSP_VE>=1 & mytest$CSP_VE<=69& mytest$Vendeurs == "particulier_ou_na","Actifs",
                         ifelse(mytest$CSP_VE>=70 & mytest$CSP_VE<=90 & mytest$Vendeurs =="particulier_ou_na", "retraites_inactifs", 
                                ifelse(is.na(mytest$CSP_VE) & mytest$Vendeurs=="particulier_ou_na",NA,mytest$Vendeurs)))

mytest$Vendeurs<- ifelse(mytest$cluster.y>=1 & !is.na(mytest$cluster.y),"Promoteurs", mytest$Vendeurs)
table(mytest$Vendeurs, useNA = "ifany")


mytest<- mytest%>% filter(!is.na(Vendeurs),!is.na(Acquereurs),Acquereurs!="SAFER", Vendeurs!="SAFER")


Contigence_ac_vendeur<- (table (mytest$Vendeurs,mytest$Acquereurs, deparse.level = 1))
?table
options(digits=0)
Khi2test<-chisq.test(Contigence_ac_vendeur)
residus_test_Khi2<-Khi2test$residuals



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
                       midpoint = 0, limit = c(-60,60), space = "Lab",
                       name= "Variation des résidus" )+
  labs(title = "Répartition des transactions en fonction des couples acquéreurs-vendeurs", x= NULL, y= NULL)+ 
  labs(subtitle = "Heat map sur les résidus du test du Khi2 de Pearson.\nLes valeurs répertorient le nombre d'échanges entre parties de la vente. Les couleurs représentent les résidus du Khi² (différences entre valeurs observées et valeurs théoriques).\nlecture: Dans la période étudiée, la population retraitée et inactive a vendu 308 820 logements à la population active.\nCette valeur est toutefois en deçà de la valeur théorique en cas d'indépendance statistique des relations.",
       x= "Acquéreurs", y= "Vendeurs")+
  labs(caption = "Sources : échantillon BD BIEN sur effectifs redressés ; Réalisation : Thibault Le Corre, Géographie-Cités, 2018")

library(extrafont)
loadfonts()
fonts() 
ggsave("tableau_Khi2resid_effectifs_plot.pdf",plot= tableau_resid_effectifs_plot,setwd("~/Sauvegarde_figures"), device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
ggsave("tableau_Khi2resid_effectifs_plot.png",plot= tableau_resid_effectifs_plot,setwd("~/Sauvegarde_figures"), device = "png", width = 380, height = 180, units = "mm", dpi = 330)
