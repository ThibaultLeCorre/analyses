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


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

line_df<-
  mytest%>%
  group_by(annee.x, TYPMUTPREC_2)%>%
  filter(!is.na(TYPMUTPREC_2))%>%
  summarise(Temps_moyen=mean(Temps_Pocession),
            Temps_median=median(Temps_Pocession),
            modal = Mode(Temps_Pocession))
  
mytest%>%
  group_by(annee.x, TYPMUTPREC_2, Temps_Pocession)%>%
  filter(!is.na(TYPMUTPREC_2))%>%
  summarise(Nombre=length(TYPMUTPREC_2))%>%
  ggplot(., aes(Temps_Pocession, annee.x, fill=TYPMUTPREC_2)) +
  geom_density_ridges()+
  geom_vline(data=line_df,  mapping = aes(xintercept=modal))+
  theme_tmd() +
  facet_grid(annee.x ~ TYPMUTPREC_2,scale="free")+
  geom_text(check_overlap = TRUE) +
geom_label(data = line_df, aes(x = 50, y = annee.x, 
    label = paste("Mean: ", round(Temps_moyen, 1), "\nMedian: ", Temps_median),label.padding = unit(0.25, "lines")))


Densite_detention_plot<-mytest%>%
  group_by(annee.x, TYPMUTPREC_2, Temps_Pocession,REQTYPBIEN)%>%
  filter(!is.na(TYPMUTPREC_2),Temps_Pocession<40 )%>%
  ggplot(., aes(Temps_Pocession, factor(annee.x), fill=TYPMUTPREC_2)) +
  geom_density_ridges_gradient(aes(fill =..x..),scale = 2, calc_ecdf = TRUE,
                      quantiles = 4, quantile_lines = TRUE)+
  scale_x_continuous(expand = c(0.01, 0),breaks = c(0,5,10,15,20,25,30,35,40)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(discrete = FALSE, name = "Quartiles")+
  theme_tmd() +
  facet_grid(REQTYPBIEN ~ TYPMUTPREC_2,scale="free")+
  labs(title = "Densité de la durée de détention des biens vendus sur le marché selon les types de mutation précédente des biens", x= "Nombre d'années de détention du bien" , y= "Année") + 
  labs(subtitle = "Les barres noires représentent les quartiles")+
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")


setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
ggsave("Densite_detention_plot.png",plot= Densite_detention_plot,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)



unique(mytest$Vendeurs)
geom_label(aes(x=., y=Mean, label = paste("Mean: ", round(Mean,1))))

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

blobTest <- mytest%>%
  filter(annee.x==1999| annee.x==2008|annee.x==2012, !is.na(REQ_VALUE), TYPMUTPREC_2=="Acquisition",
         Vendeurs!="Entreprise_Marchands_SCI" &  Vendeurs!="Biens_publics_et_HLM" &  Vendeurs!="Liberales" & Vendeurs!="autres_inactifs") %>%
  select(annee.x, REQ_VALUE, Temps_Pocession, Vendeurs, REQTYPBIEN,REQ_PRIX, PXMUTPREC) %>%
  filter(Temps_Pocession<=30 )
blobTest$RapportPV = blobTest$REQ_VALUE / blobTest$REQ_PRIX
blobTest$RapportPV_2 = blobTest$REQ_VALUE / blobTest$PXMUTPREC
Rapport_PV_plot <- ggplot(blobTest) + 
  geom_boxplot(aes(factor(Temps_Pocession), RapportPV_2), outlier.shape = NA) +
  scale_x_discrete(expand = c(0.01, 0),breaks = c(0,5,10,15,20,25,30)) +
  scale_y_continuous(limits = c(-1, 10)) +
  facet_grid(annee.x~Vendeurs) +
  theme_tmd()+
  labs(title = "Les plus-values réalisées en fonction du prix d'achat et de la durée de détention des biens par catégories sociales", x= "Nombre d'années de détention du bien" , y= "Rapport Plus-value / Prix") + 
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")

  
  setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
  ggsave("Rapport_PV_plot.png",plot= Rapport_PV_plot,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)
  



# c(-30E3, 500E3))


specificCol <- c("Liberales" = "#861388",
                 "CPIS"= "#067BC2" ,
                 "Prof_intermediaires"="#5FAD41",
                 "Employes"= "#FF8811",
                 "Ouvriers"="#FF1D15",
                 "retraites"= "#77878B",
                 "autres_inactifs"="#8D775F",
                 "Entreprise_Marchands_SCI"= "#E9DF00",
                 "Biens_publics_et_HLM"= "black",
                 "Promoteurs"="#503D3F")

options(scipen=999)
Montant_PV_plot<- mytest_PV%>%
  filter(Vendeurs!= "autres_inactifs")%>%
ggplot(., aes(Temps_Pocession, Value, fill=Type)) +
  geom_line()+
  geom_path(aes(color = Type))+
  scale_x_continuous(expand = c(0.01, 0),breaks = c(0,5,10,15,20,25,30)) +
  theme_tmd() +
  facet_grid(annee.x~Vendeurs,scale="fix")+
  labs(title = "Montants des plus-values par catégories sociales en fonction de la durée de détention des biens", x= "Nombre d'années de détention du bien" , y= "Montant de la plus- value")+ 
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")


setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
ggsave("Montant_PV_plot.png",plot= Montant_PV_plot,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)



# ggplot(mytest_PV, aes(Temps_Pocession, Value, fill=Type)) +
#   geom_boxplot(aes(factor(Temps_Pocession), Value), outlier.shape = NA)+
#   scale_x_continuous(expand = c(0.01, 0),breaks = c(0,5,10,15,20,25,30)) +
#   theme_tmd() +
#   facet_grid(annee.x~Vendeurs,scale="fix")
# hist(mytest$Temps_Pocession, breaks = 100)
table(mytest$Temps_Pocession, mytest$TYPMUTPREC)
