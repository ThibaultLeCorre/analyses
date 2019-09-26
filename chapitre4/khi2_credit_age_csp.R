library(dplyr)
library(SpatialPosition)
library(cartography)
library(tidyr)
mytest<-data_redresse[,c("ID","annee.x","PRESCREDIT","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x","ANNAIS_AC","ANNAIS_VE", "NBRPIECE","REQTYPBIEN","BIDEPT", "REQ_ANC")]
mytest$Credit <- ifelse(mytest$PRESCREDIT=="N", "Sans credit", "Avec credit")
mytest$Credit <- ifelse(is.na(mytest$PRESCREDIT),"Avec credit",mytest$Credit)
length(which(mytest$Credit=="Avec credit"))
# mytest<-mytest%>% filter(BIDEPT==75)

####Fourchette age
mytest$Age_acq = mytest$annee.x-mytest$ANNAIS_AC
mytest$fourchette_age_acq<-mytest$Age_acq 
mytest<-mytest%>%filter(fourchette_age_acq>=18 & !is.na(CSP_AC) & CSP_AC!=10)

mytest$fourchette_age_acq<- ifelse(mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<30, "Jeunes",
                                   ifelse(mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<50, "Age moyen",
                                          ifelse(mytest$fourchette_age_acq>=50 , "Vieux",NA)))


mytest$Type_acheteurs<-ifelse(mytest$CSP_AC >= 20 & mytest$CSP_AC < 40, "com/art/ets et Cadres",
                ifelse(mytest$CSP_AC >= 40 & mytest$CSP_AC < 50, "profession intermédiaire",
                  ifelse(mytest$CSP_AC >= 50 & mytest$CSP_AC < 70,"Employé et ouvriers",
                     ifelse(mytest$CSP_AC >= 70 & mytest$CSP_AC < 80, "retraités",
                          ifelse(mytest$CSP_AC == 80, "Inactifs", "non_rs_AC" )))))

mytest$Type_acheteurs<-ifelse(mytest$Type_acheteurs!="retraités",paste0(mytest$Type_acheteurs,"_",mytest$fourchette_age_acq),mytest$Type_acheteurs)

unique(mytest$Type_acheteurs)


# mytest<-mytest%>% filter(REQTYPBIEN!="MA")
# mytest$Type_log<- ifelse(mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>=0 & mytest$NBRPIECE<=3,"Petite maison",
#                          ifelse(mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>3 & mytest$NBRPIECE<=5,"Maison taille moyenne",
#                                 ifelse( mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>7,"Maison exceptionnelle",
#                                         ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=0 & mytest$NBRPIECE<=1, "studio et 1 pièce",
#                                                ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE==2, "2 pièces",
#                                                       ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=3 & mytest$NBRPIECE<=5, "Appartement taille moyenne",
#                                                              ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=6, "Appartement exceptionnel", NA)))))))

# mytest$Type_log<- ifelse( mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>=0 & mytest$NBRPIECE<=3,"Petite maison",
#                           ifelse(mytest$Promo=="Autre_vendeur" &mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>3 & mytest$NBRPIECE<=5,"Maison taille moyenne",
#                                  ifelse( mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>7,"Maison exceptionnelle",
#                                          ifelse(mytest$Promo=="Autre_vendeur" &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=0 & mytest$NBRPIECE<=1, "studio et 1 pièce",
#                                                 ifelse(mytest$Promo=="Autre_vendeur" &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE==2, "Appartement 2 pièces",
#                                                        ifelse(mytest$Promo=="Autre_vendeur" &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=3 & mytest$NBRPIECE<=5, "Appartement taille moyenne",
#                                                               ifelse(mytest$Promo=="Autre_vendeur" &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=6, "Appartement exceptionnel",
#                                                                      ifelse(mytest$Promo=="promo"&mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>3 & mytest$NBRPIECE<=5,"Maison taille moyenne promotion",
#                                                                             ifelse(mytest$Promo=="promo"&mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=0 & mytest$NBRPIECE<=1, "studio et 1 pièce promotion",
#                                                                                    ifelse(mytest$Promo=="promo"&mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE==2, "Appartement 2 pièces promotion",
#                                                                                           ifelse(mytest$Promo=="promo" &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=3 & mytest$NBRPIECE<=5, "Appartement taille moyenne promotion",NA)))))))))))
# 
# table(mytest$Type_log, useNA = "ifany")
# mytest<-mytest%>% filter(Type_acheteurs!="SAFER"&Type_acheteurs!= "Actifs_NA"&Type_acheteurs!="inactifs_NA" & Type_acheteurs!="HLM" & Type_acheteurs!="ADM" &!is.na(Type_log))  
mytest$Periode<-ifelse(mytest$annee.x==1996|mytest$annee.x==1999|mytest$annee.x==2003, "Periode_96_2003",
                        ifelse(mytest$annee.x>=2004&mytest$annee.x<=2007, "Periode_04_2007", 
                               ifelse(mytest$annee.x>=2008&mytest$annee.x<=2012, "Periode_08_2012", NA)))
Periode <- unique(mytest$Periode)
Periode <- sort(Periode[!is.na(Periode)])


allBreaks_ratio <- list()
for (cettePeriodeLa in Periode){
  
mytest_2<- mytest%>% filter(Periode == cettePeriodeLa)
Contigence_acquereru_Credit<- (table (mytest_2$Type_acheteurs,mytest_2$Credit, deparse.level = 1))/2
 # options(digits=5)
Khi2test<-chisq.test(Contigence_acquereru_Credit)
residus_test_Khi2<-Khi2test$residuals

#Heatmap sur le test des résidu du KHi 2

residus_test_Khi2<-melt(residus_test_Khi2, varnames = c("Acquereurs", "Credit"),value.name = "residus")
residus_test_Khi2$periode<-cettePeriodeLa
Contigence_acquereru_Credit<-melt(Contigence_acquereru_Credit, varnames = c("Acquereurs", "Credit"), value.name = "Effectifs")
tableau_resid_effectifs<-left_join(residus_test_Khi2, Contigence_acquereru_Credit, by = c("Credit", "Acquereurs"))
allBreaks_ratio[[as.character(cettePeriodeLa)]] <- tableau_resid_effectifs
}
allBreaksDF_ratio <- purrr::map(allBreaks_ratio, ~bind_rows(.x)) %>% bind_rows()
#plot
library(ggplot2)
library(reshape2)
ggplot(allBreaksDF_ratio, aes(Acquereurs,Credit, fill= residus)) +
  geom_tile()+
  geom_text(aes(label=round(Effectifs, digits = 0))) +
  theme_tmd() +
  facet_wrap(~periode, c(3,1) )+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-100,100), space = "Lab",
                       name= "Variation des résidus" )+
  labs(title = "Répartition des acquisitions en fonction du type de bien et du profil d'acquéreur", x= NULL, y= NULL)+
  labs(subtitle = "Heat map sur les résidus du test du Khi2 de Pearson.\nLes valeurs répertorient le nombre d'acquisition pour les principales catégoires d'acquéreurs. Les couleurs représentent les résidus du Khi² (différences entre valeurs observées et valeurs théoriques).\nlecture: Dans la période étudiée, la population active jeune, entre 18 et 30 ans, a acheté 11 520 maisons de taille moyenne en IDF\nCette valeur est en deçà de la valeur théorique en cas d'indépendance statistique des relations.",
       x= "Acquéreurs", y= "Types de biens")+
  labs(caption = "Sources : échantillon BD BIEN sur effectifs redressés ; Réalisation : Thibault Le Corre, Géographie-Cités, 2018")

library(extrafont)
loadfonts()
fonts()
ggsave("Khi2_type_biens_acquereurs_IDF.pdf",plot= tableau_resid_effectifs_plot,setwd("~/Sauvegarde_figures"), device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)

# tableau_resid_effectifs_plot_Paris<-ggplot(tableau_resid_effectifs, aes(Acquereurs,Logements, fill= residus)) +
#   geom_tile()+
#   geom_text(aes(label=round(Effectifs, digits = 0))) +
#   theme_tmd() +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white",
#                        midpoint = 0, limit = c(-100,100), space = "Lab",
#                        name= "Variation des résidus" )+
#   labs(title = "Répartition des acquisitions en fonction du type de bien et du profil d'acquéreur", x= NULL, y= NULL)+
#   labs(subtitle = "Heat map sur les résidus du test du Khi2 de Pearson.\nLes valeurs répertorient le nombre d'acquisition pour les principales catégoires d'acquéreurs. Les couleurs représentent les résidus du Khi² (différences entre valeurs observées et valeurs théoriques).\nlecture: Dans la période étudiée, la population active jeune, entre 18 et 30 ans, a acheté 9700 appartements 2 pièces à Paris\nCette valeur est au-dessus de la valeur théorique en cas d'indépendance statistique des relations.",
#        x= "Acquéreurs", y= "Types de biens")+
#   labs(caption = "Sources : échantillon BD BIEN sur effectifs redressés ; Réalisation : Thibault Le Corre, Géographie-Cités, 2018")
# 
#  library(extrafont)
#  loadfonts()
#  fonts()
#  ggsave("Khi2_type_biens_acquereurs_Paris.pdf",plot= tableau_resid_effectifs_plot,setwd("~/Sauvegarde_figures"), device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
# 
# 

