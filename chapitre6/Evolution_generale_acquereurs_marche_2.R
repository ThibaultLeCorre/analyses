mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","CSP_AC", "ANNAIS_AC")]



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


table(mytest$acquereurs, useNA = "ifany")
mytest_Acq_Pourc<-mytest%>% 
  filter(!is.na(acquereurs)| is.na(QUALITE_AC))%>% 
  group_by(annee.x)%>% 
  summarise(Agriculteurs=length(which(acquereurs=="Agriculteurs"))/length(acquereurs)*100,
            Com_art_Chef_entreprises=length(which(acquereurs=="Com_art_Chef_entreprises"))/length(acquereurs)*100,
            CPIS=length(which(acquereurs=="CPIS"))/length(acquereurs)*100,
            Prof_intermediaires=length(which(acquereurs=="Prof_intermediaires"))/length(acquereurs)*100,
            Employes=length(which(acquereurs=="Employes"))/length(acquereurs)*100,
            Ouvriers=length(which(acquereurs=="Ouvriers"))/length(acquereurs)*100,
            retraites=length(which(acquereurs=="retraites"))/length(acquereurs)*100,
            autres_inactifs=length(which(acquereurs=="autres_inactifs"))/length(acquereurs)*100,
            Biens_publics_et_HLM=length(which(acquereurs=="Biens_publics_et_HLM"))/length(acquereurs)*100,
            Entreprise_Marchands_SCI=length(which(acquereurs=="Entreprise_Marchands_SCI"))/length(acquereurs)*100,
            SAFER=length(which(acquereurs=="SAFER"))/length(acquereurs)*100)%>%
  gather("Acquereurs","Value", 2:12)
mytest_Acq_Pourc$Type= "Pourcentage"

mytest_Acq_Absolu<-mytest%>% 
  filter(!is.na(acquereurs)| is.na(QUALITE_AC))%>% 
  group_by(annee.x)%>% 
  summarise(Agriculteurs=length(which(acquereurs=="Agriculteurs")),
            Com_art_Chef_entreprises=length(which(acquereurs=="Com_art_Chef_entreprises")),
            CPIS=length(which(acquereurs=="CPIS")),
            Prof_intermediaires=length(which(acquereurs=="Prof_intermediaires")),
            Employes=length(which(acquereurs=="Employes")),
            Ouvriers=length(which(acquereurs=="Ouvriers")),
            retraites=length(which(acquereurs=="retraites")),
            autres_inactifs=length(which(acquereurs=="autres_inactifs")),
            Biens_publics_et_HLM=length(which(acquereurs=="Biens_publics_et_HLM")),
            Entreprise_Marchands_SCI=length(which(acquereurs=="Entreprise_Marchands_SCI")),
            SAFER=length(which(acquereurs=="SAFER")))%>%
  gather("Acquereurs","Value", 2:12)
mytest_Acq_Absolu$Type= "Absolu"

mytest_Acq_BIEN<- rbind(mytest_Acq_Pourc,mytest_Acq_Absolu)

specificCol <- c("Agriculteurs" = "#503D3F",
                 "Com_art_Chef_entreprises" = "#861388",
                 "CPIS"= "#067BC2" ,
                 "Prof_intermediaires"="#5FAD41",
                 "Employes"= "#FF8811",
                 "Ouvriers"="#FF1D15",
                 "retraites"= "#77878B",
                 "autres_inactifs"="#8D775F",
                 "Entreprise_Marchands_SCI"= "#E9DF00",
                 "Biens_publics_et_HLM"= "black",
                 "SAFER"="grey")

options(scipen = 999)
Nombre_transactions_CSP<-ggplot(mytest_Acq_BIEN, aes(annee.x, Value, group=Acquereurs)) +
  geom_line(aes(color = Acquereurs))+
  geom_line(aes(color =Acquereurs),size=1,show.legend =F)+
  geom_line(aes(color = Acquereurs,linetype = "dashed"), show.legend =F) + 
  scale_color_manual(values = specificCol ) +
  # geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  facet_wrap(~Type, ncol=2,scale="free")+
  labs(title = "Les acquisitions de logements en île-de-France selon les CSP et les catégories de personnes morales", x= NULL, y= NULL)+ 
  labs(subtitle = "lecture : En 2006 les cadres ont acquis environ 40 000 logements. Cela représente environ 28 % des acquisitions. \n84 445  transactions sont exclues (sur 1 570 533) en raison d'absence d'information" , x= NULL, y= NULL)+
  labs(caption = "Sources : Echantillon redressé de la base BIEN. Réalisation : Thibault Le Corre, 2017")


setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
ggsave("Nombre_transactions_CSP.png",plot= Nombre_transactions_CSP,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)
setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
ggsave("Nombre_transactions_CSP.pdf",plot= Nombre_transactions_CSP,  device = "pdf", width = 260, height = 150, units = "mm", dpi = 330)



