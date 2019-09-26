data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x",
                                        "ANNAIS_AC","SITMAT_AC", "ANNAIS_VE","SITMAT_VE","REQTYPBIEN")]
DBSCAN_results_table_Promoteurs <- read.csv("~/Projets/DBSCAN/DBSAN/DBSCAN_results_table_Promoteurs.csv", sep="", stringsAsFactors=FALSE)
mytest$ID<-as.numeric(mytest$ID)
mytest<-left_join(mytest,DBSCAN_results_table_Promoteurs, by="ID")


#Types acquéreurs et vendeurs: toutes transactions

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




mytest<-mytest%>%filter(!is.na(acquereurs), acquereurs!= "SAFER")
# 

# mytest$Age_acq = mytest$annee.x-mytest$ANNAIS_AC
# 
# mytest$fourchette_age_acq<-mytest$Age_acq
# mytest<-mytest %>%
#   filter(acquereurs=="CPIS" & fourchette_age_acq>=18)

# mytest$fourchette_age_acq<- ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<30 & mytest$acquereurs!="retraites", "[18,30[",
#                                    ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<50 & mytest$acquereurs!="retraites", "[30,50[",
#                                           ifelse(!is.na(mytest$CSP_AC) & mytest$fourchette_age_acq>=50 & mytest$acquereurs!="retraites", "[50+","")))
# 
# 
# 
# mytest$acquereurs<- paste0(mytest$acquereurs,mytest$fourchette_age_acq)

mytest$Periode<-ifelse(mytest$annee.x==1996|mytest$annee.x==1999|mytest$annee.x==2003, "Periode_96_2003",
                            ifelse(mytest$annee.x>=2004&mytest$annee.x<=2007, "Periode_04_2007", 
                                   ifelse(mytest$annee.x>=2008&mytest$annee.x<=2012, "Periode_08_2012", NA)))


library(sf)
test<- mytest
#Cr?ation du fichier SF (lambert II ?tendu, coord dorigine fichier BIEN)

spVente <- st_as_sf(test,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)



#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]   
spVente$Ylamb93<-coords[,2]


setwd("~/Shapes/datidf")
list.files()
carreauInsee200 <- st_read("car200m_idf_reparGeom.shp",
                           stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee200 <-  st_transform(carreauInsee200, crs = 2154)


carreauInsee200$Carreau_ID<-1:length(carreauInsee200$TARGET_FID)

Cadrillage_200<- carreauInsee200%>%
  st_sf(geometry = .)

Cadrillage_200<-Cadrillage_200%>% select(Carreau_ID,geometry)


mytest_2<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)
mytest_3<- as.data.frame(mytest_2)%>%filter(!is.na(acquereurs))%>%
  select(Carreau_ID,Periode,acquereurs)

# SpUnitt_with_cluster <- read.csv2("~/Projets/Realisation_ACP/SpUnitt_with_cluster.csv", stringsAsFactors=FALSE)
# SpUnitt_with_cluster<-SpUnitt_with_cluster[,c("Carreau_ID","cettePeriodeLa","cah.data.clust.clust")]
# SpUnitt_with_cluster$Periode<-SpUnitt_with_cluster$cettePeriodeLa

mytest_3<- left_join(mytest_3,SpUnitt_with_cluster)

mytest_3$cluster<-mytest_3$cah.data.clust.clust

mytest_3<-mytest_3%>% 
  mutate( cluster= case_when( cluster == 6 | cluster ==  7 | cluster == 3 ~ "Marchés banals avec changements de régime", 
                           
                           cluster == 1 | cluster ==  2  ~ "Marchés périphériques pavillonaires",   TRUE~ as.character(cluster)))%>%
  filter(!is.na(cluster))


carreaux_resumes <- mytest_3 %>%
  group_by(cluster,Periode, acquereurs) %>%
  summarise(Sum_ACQ=n())

carreaux_resumes <- carreaux_resumes %>%
  group_by(cluster) %>%
  mutate(Sum_Total_Unit=sum(Sum_ACQ ))

carreaux_resumes$Pourc_Acq_Unit<- (carreaux_resumes$Sum_ACQ / carreaux_resumes$Sum_Total_Unit)*100

Sum_Total_Region <- sum(carreaux_resumes$Sum_ACQ)


carreaux_resumes_2 <- carreaux_resumes_2 %>%
  group_by(acquereurs) %>%
  mutate(Sum_Acq_Region=sum(Sum_ACQ ))

carreaux_resumes_2$Intensite_Cluster<- (carreaux_resumes_2$Sum_Total_Unit / Sum_Total_Region)*100
carreaux_resumes_2$Moyenne_Region_Acq<- (carreaux_resumes_2$Sum_Acq_Region / Sum_Total_Region)*100

carreaux_resumes_2$QuotientLocal<-carreaux_resumes_2$Pourc_Acq_Unit / carreaux_resumes_2$Moyenne_Region_Acq

Test_Quotient_Local<- carreaux_resumes_2[,c("cluster","acquereurs","QuotientLocal", "Pourc_Acq_Unit")]


# SpUnitt_with_cluster <- read.csv2("~/Projets/Realisation_ACP/SpUnitt_with_cluster.csv", stringsAsFactors=FALSE)
# SpUnitt_with_cluster<-SpUnitt_with_cluster[,c("Carreau_ID","cettePeriodeLa","cah.data.clust.clust")]
# SpUnitt_with_cluster$Periode<-SpUnitt_with_cluster$cettePeriodeLa
# 
# Test_Quotient_Local<- left_join(carreaux_resumes,SpUnitt_with_cluster)
# 
# 
# Test_Quotient_Local$cluster<-Test_Quotient_Local$cah.data.clust.clust
# 
# Test_Quotient_Local<-as.data.frame(Test_Quotient_Local)




Test_Quotient_Regimes<-Test_Quotient_Local %>%
  group_by( acquereurs, cluster) %>%
  summarise( Moyenne_Quotient = mean (QuotientLocal, na.rm=T)) %>%
  mutate( cluster= case_when( cluster == 10  ~ "Marché de l'investissement et d'occupation\nrésidentielle dans les espaces centraux",
                              cluster == 11  ~ "Marché de l’hyper-luxe parisien",
                              cluster == 4  ~ "Marché familial pavillonnaire privilégié",
                              cluster == 5  ~ "Marché de la promotion immobilière",
                              cluster == 8  ~ "Marché des retraités",
                              cluster == 9  ~ "Marché privilégié sur des segments mixtes",TRUE~ as.character(cluster)))%>%
   filter(acquereurs=="CPIS" | acquereurs=="Prof_intermediaires" |  acquereurs=="Ouvriers" |  acquereurs=="Employes" )%>%
  filter(!is.na(cluster)) %>%
  ggplot() +
  geom_bar(aes(acquereurs,Moyenne_Quotient), stat = "identity") +
  ggtitle("") +
  labs(subtitle = "") +
  xlab("Quotients de localisation") +
  ylab("") +
   theme_tmd ()+
  # labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  # facet_grid(Periode~cluster ) +
   facet_wrap(~cluster)+
  labs(title = "Quotients de localisation des catégories sociales selon le régime local de marché", x= NULL, y= NULL)+ 
    labs(subtitle = "Les quotients ont été calculés sur l'ensemble des acquéreurs et pour toutes périodes confondues. Les quatres principales catégories sociales sont uniquement affichées",
         x= NULL, y= NULL)+
    labs(caption = "Sources : échantillon BD BIEN sur effectifs redressés ; Réalisé sous RStudio avec le package ggplot2 ; Réalisation : Thibault Le Corre, Géographie-Cités, 2018")
  
setwd("~/Projets/Chapitre7/Cartes_acquereurs")
  ggsave("Test_Quotient_Regimes.png",plot= Test_Quotient_Regimes,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)
  library(extrafont)
  font_import()
  
  loadfonts(device="win")
  loadfonts()
  fonts()  
  
  ggsave("Test_Quotient_Regimes.pdf",plot= Test_Quotient_Regimes,  device = "pdf", width = 310, height = 200, units = "mm", dpi = 330)
  
  
  
  Test_Pourcentage_Regimes<- Test_Quotient_Local %>%
    group_by( acquereurs, cluster) %>%
    summarise( Moyenne_Quotient = mean (Pourc_Acq_Unit, na.rm=T)) %>%
    mutate( cluster= case_when( cluster == 10  ~ "Marché de l'investissement et d'occupation\nrésidentielle dans les espaces centraux",
                                cluster == 11  ~ "Marché de l’hyper-luxe parisien",
                                cluster == 4  ~ "Marché familial pavillonnaire privilégié",
                                cluster == 5  ~ "Marché de la promotion immobilière",
                                cluster == 8  ~ "Marché des retraités",
                                cluster == 9  ~ "Marché privilégié sur des segments mixtes",TRUE~ as.character(cluster)))%>%
    filter(acquereurs=="CPIS" | acquereurs=="Prof_intermediaires" |  acquereurs=="Ouvriers" |  acquereurs=="Employes" )%>%
    filter( !is.na(cluster)) %>%
    ggplot() +
    geom_bar(aes(acquereurs,Moyenne_Quotient), stat = "identity") +
    ggtitle("") +
    labs(subtitle = "") +
    xlab("Pourcentage des acquéreurs") +
    ylab("") +
     theme_tmd ()+
    # labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
    # facet_grid(Periode~cluster ) +
    facet_wrap(~cluster)+
    labs(title = "Pourcentage des catégories sociales représentées selon le régime local de marché", x= NULL, y= NULL)+ 
    labs(subtitle = "Les pourcentages ont été calculés sur l'ensemble des acquéreurs et pour toutes périodes confondues. Les quatres principales catégories sociales sont uniquement affichées",
         x= NULL, y= NULL)+
    labs(caption = "Sources : échantillon BD BIEN sur effectifs redressés ; Réalisé sous RStudio avec le package ggplot2 ; Réalisation : Thibault Le Corre, Géographie-Cités, 2018")
  
  
  ggsave("Test_Pourcentage_Regimes.png",plot= Test_Pourcentage_Regimes,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)
  ggsave("Test_Pourcentage_Regimes.pdf",plot= Test_Pourcentage_Regimes,  device = "pdf", width = 310, height = 200, units = "mm", dpi = 330)
  

