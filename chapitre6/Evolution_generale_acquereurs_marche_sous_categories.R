mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","CSP_AC", "ANNAIS_AC")]
table(mytest$CSP_AC)

mytest$acquereurs<-ifelse(mytest$CSP_AC == 31, "prof_liberale",
                      ifelse(mytest$CSP_AC == 33, "Cadre_fonction_publique",
                      ifelse(mytest$CSP_AC == 34, "Professeurs_universitaires_scientifiques",
                        ifelse(mytest$CSP_AC == 35, "Informations_arts_spectacles",
                        ifelse(mytest$CSP_AC == 37, "Cadre_administratif_commercial",
                         ifelse(mytest$CSP_AC == 38, "Ingenieur_cadre_technique",
                          ifelse(mytest$CSP_AC == 42 , "instituteur_et_assimile",
                           ifelse(mytest$CSP_AC == 43 , "intermed_sante_travail_social",
                           ifelse(mytest$CSP_AC == 44 , "Clerge_religieux",
                            ifelse(mytest$CSP_AC == 45 , "intermed.fonction_publique",
                           ifelse(mytest$CSP_AC == 46 , "inter_adm_et_commerciales_entreprises",
                          ifelse(mytest$CSP_AC == 47 , "technicien",
                          ifelse(mytest$CSP_AC == 48 , "Contremaitre_agent_de_maitrise",NA)))))))))))))

mytest$Type<- ifelse(mytest$CSP_AC >= 30 & mytest$CSP_AC < 40 , "CPIS",
                     ifelse(mytest$CSP_AC >= 40 & mytest$CSP_AC < 50, "Prof_intermediaires",NA))
  
  
  
table(mytest$acquereurs, useNA = "ifany")

mytest_Cadres_Pourc<-mytest%>% 
  filter(!is.na(acquereurs),Type=="CPIS")%>% 
  group_by(annee.x)%>% 
  summarise(prof_liberale=length(which(acquereurs=="prof_liberale"))/length(acquereurs)*100,
            Cadre_fonction_publique=length(which(acquereurs=="Cadre_fonction_publique"))/length(acquereurs)*100,
            Professeurs_universitaires_scientifiques=length(which(acquereurs=="Professeurs_universitaires_scientifiques"))/length(acquereurs)*100,
            Informations_arts_spectacles=length(which(acquereurs=="Informations_arts_spectacles"))/length(acquereurs)*100,
            Cadre_administratif_commercial=length(which(acquereurs=="Cadre_administratif_commercial"))/length(acquereurs)*100,
            Ingenieur_cadre_technique=length(which(acquereurs=="Ingenieur_cadre_technique"))/length(acquereurs)*100)%>%
  gather("Acquereurs","Value", 2:7)
mytest_Cadres_Pourc$Mode= "Pourcentage"


mytest_Cadres_Absolu<-mytest%>% 
  filter(!is.na(acquereurs),Type=="CPIS")%>% 
  group_by(annee.x)%>% 
  summarise(prof_liberale=length(which(acquereurs=="prof_liberale")),
            Cadre_fonction_publique=length(which(acquereurs=="Cadre_fonction_publique")),
            Professeurs_universitaires_scientifiques=length(which(acquereurs=="Professeurs_universitaires_scientifiques")),
            Informations_arts_spectacles=length(which(acquereurs=="Informations_arts_spectacles")),
            Cadre_administratif_commercial=length(which(acquereurs=="Cadre_administratif_commercial")),
            Ingenieur_cadre_technique=length(which(acquereurs=="Ingenieur_cadre_technique")))%>%
  gather("Acquereurs","Value", 2:7)
mytest_Cadres_Absolu$Mode= "Absolu"

mytest_cadres<- rbind (mytest_Cadres_Pourc,mytest_Cadres_Absolu)
mytest_cadres$Type<-"CPIS"


mytest_Inter_Pourc<-mytest%>% 
  filter(!is.na(acquereurs),Type=="Prof_intermediaires")%>% 
  group_by(annee.x)%>% 
  summarise(instituteur_et_assimile=length(which(acquereurs=="instituteur_et_assimile"))/length(acquereurs)*100,
            intermed_sante_travail_social=length(which(acquereurs=="intermed_sante_travail_social"))/length(acquereurs)*100,
            Clerge_religieux=length(which(acquereurs=="Clerge_religieux"))/length(acquereurs)*100,
            intermed.fonction_publique=length(which(acquereurs=="intermed.fonction_publique"))/length(acquereurs)*100,
            inter_adm_et_commerciales_entreprises=length(which(acquereurs=="inter_adm_et_commerciales_entreprises"))/length(acquereurs)*100,
            technicien=length(which(acquereurs=="technicien"))/length(acquereurs)*100,
            Contremaitre_agent_de_maitrise=length(which(acquereurs=="Contremaitre_agent_de_maitrise"))/length(acquereurs)*100,
            Cadre_administratif_commercial=length(which(acquereurs=="Cadre_administratif_commercial"))/length(acquereurs)*100)%>%
  gather("Acquereurs","Value", 2:9)
            
mytest_Inter_Pourc$Mode= "Pourcentage"


mytest_Inter_Absolu<-mytest%>% 
  filter(!is.na(acquereurs),Type=="Prof_intermediaires")%>% 
  group_by(annee.x)%>%   
  summarise(instituteur_et_assimile=length(which(acquereurs=="instituteur_et_assimile")),
                                   intermed_sante_travail_social=length(which(acquereurs=="intermed_sante_travail_social")),
                                   Clerge_religieux=length(which(acquereurs=="Clerge_religieux")),
                                   intermed.fonction_publique=length(which(acquereurs=="intermed.fonction_publique")),
                                   inter_adm_et_commerciales_entreprises=length(which(acquereurs=="inter_adm_et_commerciales_entreprises")),
                                   technicien=length(which(acquereurs=="technicien")),
                                   Contremaitre_agent_de_maitrise=length(which(acquereurs=="Contremaitre_agent_de_maitrise")),
                                   Cadre_administratif_commercial=length(which(acquereurs=="Cadre_administratif_commercial")))%>%
  gather("Acquereurs","Value", 2:9)
mytest_Inter_Absolu$Mode= "Absolu"

mytest_Inter<- rbind (mytest_Inter_Pourc,mytest_Inter_Absolu)
mytest_Inter$Type<-"Prof_intermediaires"

###
mytest_Inter_Cadre<- rbind(mytest_cadres,mytest_Inter)




options(scipen = 999)
ggplot(mytest_Inter_Cadre, aes(annee.x, Value, group=Acquereurs)) +
  geom_line(aes(color = Acquereurs))+
  geom_line(aes(color =Acquereurs),size=1,show.legend =F)+
  geom_line(aes(color = Acquereurs,linetype = "dashed"), show.legend =F) + 
  # scale_color_manual(values = specificCol ) +
  # geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2009,2010,2011,2012)) +
  theme_tmd() +
  facet_grid(Mode~Type, scale="free")+
  labs(title = "Les acquisitions de logements en île-de-France selon les CSP et les catégories de personnes morales", x= NULL, y= NULL)+ 
  labs(subtitle = "lecture : En 2006 les cadres ont acquis environ 40 000 logements. Cela représente environ 28 % des acquisitions. \n84 445  transactions sont exclues (sur 1 570 533) en raison d'absence d'information" , x= NULL, y= NULL)+
  labs(caption = "Sources : Echantillon redressé de la base BIEN. Réalisation : Thibault Le Corre, 2017")

