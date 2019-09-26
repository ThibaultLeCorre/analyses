library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
##courbe de comby


data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","REQ_PRIX","QUALITE_AC","CSP_AC", "QUALITE_VE","CSP_VE","REQTYPBIEN")]
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
                           ifelse(mytest$CSP_AC >= 20 & mytest$CSP_AC < 30 & mytest$acquereurs == "particulier_ou_na", "Com_art_Chef_entreprises",
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
                         ifelse(mytest$CSP_VE >= 20 & mytest$CSP_VE < 30 & mytest$Vendeurs == "particulier_ou_na", "Com_art_Chef_entreprises",
                                ifelse(mytest$CSP_VE >= 30 & mytest$CSP_VE < 40 & mytest$Vendeurs == "particulier_ou_na", "CPIS",
                                       ifelse(mytest$CSP_VE >= 40 & mytest$CSP_VE < 50 & mytest$Vendeurs == "particulier_ou_na", "Prof_intermediaires",
                                              ifelse(mytest$CSP_VE >= 50 & mytest$CSP_VE < 60 & mytest$Vendeurs == "particulier_ou_na", "Employes",
                                                     ifelse(mytest$CSP_VE >= 60 & mytest$CSP_VE < 70 & mytest$Vendeurs == "particulier_ou_na", "Ouvriers",
                                                            ifelse(mytest$CSP_VE >= 70 & mytest$CSP_VE < 80 & mytest$Vendeurs == "particulier_ou_na", "retraites",
                                                                   ifelse(mytest$CSP_VE == 80 & mytest$Vendeurs == "particulier_ou_na", "autres_inactifs", mytest$Vendeurs))))))))

mytest$Vendeurs<- ifelse(mytest$cluster>=1 & !is.na(mytest$cluster),"Promoteurs", mytest$Vendeurs)
table(mytest$Vendeurs, useNA = "ifany")

mytest$REQTYPBIEN <- ifelse(mytest$REQTYPBIEN=="AP","Appartements", "Maisons")

# mytest<- mytest%>%
# filter(Vendeurs != "Promoteurs" & Vendeurs != "retraites")
#courbe de comby

Acquereurs_comby<-mytest%>% 
  filter(!is.na(acquereurs))%>% 
  group_by(annee.x, acquereurs, REQTYPBIEN)%>% 
  summarise(Nombre=length(acquereurs),
            Prix_median=mean(REQ_PRIX))
Acquereurs_comby$Type<-"acquereurs"
names(Acquereurs_comby)[2]<-"categories"
Acquereurs_comby<-as.data.frame(Acquereurs_comby)

Vendeurs_comby<-mytest%>% 
  filter(!is.na(Vendeurs))%>% 
  group_by(annee.x, Vendeurs, REQTYPBIEN)%>% 
  summarise(Nombre=length(acquereurs),
            Prix_median=mean(REQ_PRIX))
Vendeurs_comby$Type<-"Vendeurs"
names(Vendeurs_comby)[2]<-"categories"
Vendeurs_comby<-as.data.frame(Vendeurs_comby)

Comby_df<- rbind(Acquereurs_comby,Vendeurs_comby)

Comby_df<-Comby_df%>%
  filter(categories!="Agriculteurs" & categories!="SAFER")
unique(Comby_df$categories)
options(scipen=999)

specificCol <- c("Com_art_Chef_entreprises" = "#861388",
                 "CPIS"= "#067BC2" ,
                 "Prof_intermediaires"="#5FAD41",
                 "Employes"= "#FF8811",
                 "Ouvriers"="#FF1D15",
                 "retraites"= "#77878B",
                 "autres_inactifs"="#8D775F",
                 "Entreprise_Marchands_SCI"= "#E9DF00",
                 "Biens_publics_et_HLM"= "black",
                 "Promoteurs"="#503D3F")

Comby_total_plot<-Comby_df%>%
  filter(categories!= "Biens_publics_et_HLM")%>%
ggplot(., aes(Nombre, Prix_median, group=categories)) +
  geom_path(aes(color = categories))+
  geom_path(aes(color =categories),size=2,alpha=0.2,show.legend =F)+
  geom_path(aes(color = categories,linetype = "dashed"), show.legend =F) +  
  geom_point(size=0) +geom_text(aes(label=annee.x), size=3, angle=45, family = "Century Gothic", hjust=0,vjust=1, check_overlap = TRUE)+
  # geom_point(size=0.75)+
  scale_color_manual(values = specificCol )+
  scale_x_continuous() +
  theme_tmd() +
  facet_grid(Type ~ REQTYPBIEN,scale="free")+
  labs(title = "Courbe de Comby des acquéreurs et vendeurs franciliens", x= "Nombre de transactions", y= "Prix nominal net vendeur")+ 
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")


setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
ggsave("Comby_total_plot.png",plot= Comby_total_plot,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)


library(extrafont)
loadfonts()
fonts() 
ggsave("Comby_total_plot.pdf",plot= Comby_total_plot,  device = "pdf", width = 310, height = 200, units = "mm", dpi = 330)
# 


AcquereursVendeurs_comby<-mytest%>% 
  filter(!is.na(acquereurs))%>% 
  group_by(annee.x, acquereurs,Vendeurs, REQTYPBIEN)%>% 
  summarise(Nombre=length(acquereurs),
            Prix_median=mean(REQ_PRIX))%>%
  filter(Nombre>=100)

AcquereursVendeurs_comby%>%
filter (acquereurs!="Agriculteurs", acquereurs!="Biens_publics_et_HLM", acquereurs!="autres_inactifs", acquereurs!="Com_art_Chef_entreprises",  
        Vendeurs!="Agriculteurs", Vendeurs!="Biens_publics_et_HLM", Vendeurs!="autres_inactifs", Vendeurs!="Com_art_Chef_entreprises" )%>%
  ggplot(., aes(annee.x, Prix_median,group= acquereurs)) +
  geom_line(aes(color= acquereurs))+
  scale_color_manual(values = specificCol )+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  # geom_text(aes(label=round(Prix_median, digits = 0))) +
  theme_tmd() +
  facet_grid(REQTYPBIEN~Vendeurs, scale="free")
  

scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-100,120), space = "Lab",
                       name= "Variation des résidus" )+
  labs(title = "Répartition des transactions en fonction des couples acquéreurs-vendeurs", x= NULL, y= NULL)+ 
  labs(subtitle = "Heat map sur les résidus du test du Khi2 de Pearson.\nLes valeurs répertorient le nombre d'échanges entre parties de la vente. Les couleurs représentent les résidus du Khi² (différences entre valeurs observées et valeurs théoriques).\nlecture: Dans la période étudiée, les retraités ont vendu 28498 logements à des ouvriers. Cette valeur est au-delà de la valeur théorique en cas d'indépendance statistique des relations.",
       x= "Acquéreurs", y= "Vendeurs")+
  labs(caption = "Sources : échantillon BD BIEN sur effectifs redressés ; Khi² et résidus de Pearson calculés sous RStudio avec le package stats ; Réalisation : Thibault Le Corre, Géographie-Cités, 2018")

