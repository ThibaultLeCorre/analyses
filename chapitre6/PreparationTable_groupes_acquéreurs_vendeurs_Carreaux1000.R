
library(spdep)
library(ggplot2)
library(sf)
library(sp)
library(SpatialPosition)
library(dplyr)
library(tidyr)


Typo_1999_2012_carreaux1000_appartement<- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_carreaux1000_appartement_5030tiles.csv", stringsAsFactors=FALSE)
Typo_1999_2012_carreaux1000_Maison <- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_carreaux1000_Maison_5030tiles.csv", stringsAsFactors=FALSE)
tableauID<-full_join(Typo_1999_2012_carreaux1000_appartement,Typo_1999_2012_carreaux1000_Maison, by="Carreau_ID")
tableauID<-tableauID[,c(1,2)]


setwd("~/Shapes/shpIDF_dep_lamb93")
list.files()

lim_IDF.st<- st_read("ile-de-france.shp",
                     stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
lim_IDF.st  <-  st_transform(lim_IDF.st , crs = 2154)

setwd("~/Shapes/datidf")
list.files()
carreauInsee1000 <- st_read("car1000m_idf_reparGeom.shp",
                            stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee1000 <-  st_transform(carreauInsee1000, crs = 2154)

carreauInsee1000$Carreau_ID<-1:length(carreauInsee1000$id_c1000)

Cadrillage_1000<- carreauInsee1000%>%
  st_sf(geometry = .)

Cadrillage_1000<-Cadrillage_1000%>% select(Carreau_ID,geometry)

#Mask
#Mask
Mask_st_1000 <- st_union(carreauInsee1000)
Mask_st_1000<-st_sf(id = 1, geometry =Mask_st_1000 )

# Mask_simplified <-
Mask_st_1000 <- Mask_st_1000 %>%
  st_buffer(dist = 500) %>%
  st_buffer(dist = -1000) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 500) %>%
  st_buffer(500)


Cadrillage_1000$Carreau_ID<-as.numeric(Cadrillage_1000$Carreau_ID)
Cadrillage_1000<-right_join(Cadrillage_1000, tableauID, by ="Carreau_ID")
Cadrillage_1000<-Cadrillage_1000[,-(2)]

cadrillage_1000_spdf<-as(Cadrillage_1000, "Spatial")
proj4string(cadrillage_1000_spdf) <-CRS("+init=epsg:2154")
cadrillage_1000_spdf<- spTransform(cadrillage_1000_spdf, "+init=epsg:2154")


#######Table principal#####
data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x")]
DBSCAN_results_table_Promoteurs <- read.csv("~/Projets/DBSCAN/DBSAN/DBSCAN_results_table_Promoteurs.csv", sep="", stringsAsFactors=FALSE)
mytest$ID<-as.numeric(mytest$ID)
mytest<-left_join(mytest,DBSCAN_results_table_Promoteurs, by="ID")
# table(data_redresse_code_promo_2$typeVar, data_redresse_code_promo_2$CSP_VE)
mytest$X.x<-as.numeric(mytest$X.x)
mytest$Y.x<-as.numeric(mytest$Y.x)
mytest$CSP_AC<-as.numeric(mytest$CSP_AC)
mytest$CSP_VE<-as.numeric(mytest$CSP_VE)
mytest$annee.x<-as.numeric(mytest$annee.x)

str(mytest)
unique(mytest$QUALITE_AC)
# 1 import table

annee <- unique(mytest$annee.x)
annee <- sort(annee[!is.na(annee)])
# 2. Boucle d'alimentation

#Types acquéreurs et vendeurs: toutes transactions

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

table (mytest$acquereurs, useNA = "always")

mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM",
                         ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                       ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"SAFER",
                                              ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                                     ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM", "particulier_ou_na"))))))


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


table_acquereur_vendeur<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Vendeurs),!is.na(acquereurs),acquereurs!="SAFER",acquereurs!="particulier_ou_na", Vendeurs!="SAFER", Vendeurs!="particulier_ou_na")

Transac<-table_acquereur_vendeur%>% select(ID,X.x,Y.x,annee.x,acquereurs)
#
Montableau_Profil_acquereur<- data.frame(Carreau_ID=NA,cetteanneeLa=NA, cetacquereurLa=NA,Somme_Profil_acquereur=NA, Pourcentage_Profil_acquereur=NA,Nombre_totalPotentiel_transac=NA)
Montableau_Profil_acquereur<-Montableau_Profil_acquereur%>%filter(!is.na(cetteanneeLa))

for (cetteanneeLa in annee){
  print(cetteanneeLa)
  test<- Transac%>% filter(annee.x==cetteanneeLa)
  
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
  
  Cadrillage_1000_jointure<-st_join(Cadrillage_1000, spVente, join = st_contains, left=T)
  carreaux_total<- Cadrillage_1000_jointure %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs= length(which(!is.na(ID))))
  
  carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
  carreaux_total_spdf<-carreaux_total%>%
    as( "Spatial")
  proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
  # carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")
  
  stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                              unknownpts = cadrillage_1000_spdf,
                              varname = "Nombre_transacs",
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_1000, longlat = FALSE)
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)
  
  Profil_acquereur <- unique(spVente$acquereurs)
  Profil_acquereur <- sort(Profil_acquereur[!is.na(Profil_acquereur)])
  
  
  for (cetacquereurLa in Profil_acquereur){
    print(cetacquereurLa)
    spVente_2<-spVente%>%
      filter(acquereurs==cetacquereurLa)
    
    Cadrillage_1000_jointure<-st_join(Cadrillage_1000, spVente_2, join = st_contains, left=T)
    carreaux_Categories<- Cadrillage_1000_jointure %>%
      group_by(Carreau_ID)%>%
      summarise(Nbr_catego= length(which(!is.na(ID))))%>%
      filter(Nbr_catego>0)
    
    carreaux_Categories_spdf<- as(carreaux_Categories, "Spatial")
    proj4string(carreaux_Categories_spdf) <-CRS("+init=epsg:2154")
    stewartProfil_acquereur <- mcStewart(knownpts = carreaux_Categories_spdf,
                                         unknownpts = cadrillage_1000_spdf,
                                         varname = "Nbr_catego",
                                         typefct = "exponential", span = 500, beta = 2,
                                         mask = Mask_spdf_1000, longlat = FALSE)
    
    stewartProfil_acquereur_SF <- st_as_sf(stewartProfil_acquereur) %>%
      rename(Sum_Profil_acquereur = OUTPUT)
    stewartProfil_acquereur_SF <- left_join(stewartProfil_acquereur_SF %>%
                                              select(Sum_Profil_acquereur,Carreau_ID),
                                            stewartTransac_df%>%
                                              select(NbTransac,Carreau_ID),
                                            by="Carreau_ID") %>%
      mutate(Pourcentage_Profil_acquereur = (Sum_Profil_acquereur / NbTransac)*100,
             Somme_Profil_acquereur =Sum_Profil_acquereur,
             Nombre_totalPotentiel_transac = NbTransac)
    stewartProfil_acquereur_SF$cetacquereurLa<-cetacquereurLa
    stewartProfil_acquereur_SF$cetteanneeLa<- cetteanneeLa
    
    stewartProfil_acquereur_df<-as.data.frame(stewartProfil_acquereur_SF)
    stewartProfil_acquereur_df<-stewartProfil_acquereur_df%>%
      select(Carreau_ID,cetteanneeLa,cetacquereurLa,Somme_Profil_acquereur,Pourcentage_Profil_acquereur, Nombre_totalPotentiel_transac)
    
    
    Montableau_Profil_acquereur<-bind_rows(Montableau_Profil_acquereur, stewartProfil_acquereur_df)
  }
}



#########Vendeurs
Transac<-table_acquereur_vendeur%>% select(ID,X.x,Y.x,annee.x,Vendeurs)
unique(Transac$Vendeurs)
table(Transac$Vendeurs)
Montableau_Profil_vendeur<- data.frame(Carreau_ID=NA,cetteanneeLa=NA, ceVendeurLa=NA,Somme_Profil_vendeur=NA, Pourcentage_Profil_vendeur=NA,Nombre_totalPotentiel_transac=NA)
Montableau_Profil_vendeur<-Montableau_Profil_vendeur%>%filter(!is.na(cetteanneeLa))

for (cetteanneeLa in annee){
  print(cetteanneeLa)
  test<- Transac %>% filter(annee.x==cetteanneeLa)
  
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
  
  Cadrillage_1000_jointure<-st_join(Cadrillage_1000, spVente, join = st_contains, left=T)
  carreaux_total<- Cadrillage_1000_jointure %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs= length(which(!is.na(ID))))
  
  carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
  carreaux_total_spdf<-carreaux_total%>%
    as( "Spatial")
  proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
  # carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")
  
  stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                              unknownpts = cadrillage_1000_spdf,
                              varname = "Nombre_transacs",
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_1000, longlat = FALSE)
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)
  
  Profil_vendeur <- unique(spVente$Vendeurs)
  Profil_vendeur <- sort(Profil_vendeur[!is.na(Profil_vendeur)])
  
  
  for (ceVendeurLa in Profil_vendeur){
    print(ceVendeurLa)
    spVente_2<-spVente%>%
      filter(Vendeurs==ceVendeurLa)
    
    Cadrillage_1000_jointure<-st_join(Cadrillage_1000, spVente_2, join = st_contains, left=T)
    carreaux_Categories<- Cadrillage_1000_jointure %>%
      group_by(Carreau_ID) %>%
      summarise(Nbr_catego= length(which(!is.na(ID))))
    carreaux_Categories <- carreaux_Categories%>%filter(Nbr_catego>0)
    carreaux_Categories_spdf<- as(carreaux_Categories, "Spatial")
    proj4string(carreaux_Categories_spdf) <-CRS("+init=epsg:2154")
    
    stewartProfil_vendeur <- mcStewart(knownpts = carreaux_Categories_spdf,
                                       unknownpts = cadrillage_1000_spdf,
                                       varname = "Nbr_catego",
                                       typefct = "exponential", span = 500, beta = 2,
                                       mask = Mask_spdf_1000, longlat = FALSE)
    
    
    stewartProfil_vendeur_SF <- st_as_sf(stewartProfil_vendeur) %>%
      rename(Sum_Profil_vendeur  = OUTPUT)
    
    stewartProfil_vendeur_SF <- left_join(stewartProfil_vendeur_SF %>%
                                            select(Sum_Profil_vendeur,Carreau_ID),
                                          stewartTransac_df%>%
                                            select(NbTransac,Carreau_ID),
                                          by="Carreau_ID") %>%
      mutate(Pourcentage_Profil_vendeur = (Sum_Profil_vendeur / NbTransac)*100,
             Somme_Profil_vendeur =Sum_Profil_vendeur,
             Nombre_totalPotentiel_transac = NbTransac)
    stewartProfil_vendeur_SF$ceVendeurLa<-ceVendeurLa
    stewartProfil_vendeur_SF$cetteanneeLa<- cetteanneeLa
    
    stewartProfil_vendeur_df<-as.data.frame(stewartProfil_vendeur_SF)
    stewartProfil_vendeur_df<-stewartProfil_vendeur_df%>%
      select(Carreau_ID,cetteanneeLa,ceVendeurLa,Somme_Profil_vendeur,Pourcentage_Profil_vendeur, Nombre_totalPotentiel_transac)
    
    
    Montableau_Profil_vendeur<-bind_rows(Montableau_Profil_vendeur, stewartProfil_vendeur_df)
  }
}

# check<- Montableau_Profil_vendeur%>%
#   filter(ceVendeurLa=="CPIS",
#          cetteanneeLa=="2012")
# check<- Montableau_Profil_vendeur[!duplicated(Montableau_Profil_vendeur), ]
#Save
setwd("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux")
write.csv2(x=Montableau_Profil_acquereur,file = "Tableau_Acquereur_Carreaux1000.csv", row.names=FALSE, fileEncoding = "UTF-8")
setwd("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux")
write.csv2(x=Montableau_Profil_vendeur,file = "Tableau_Vendeur_Carreaux1000.csv", row.names=FALSE, fileEncoding = "UTF-8")
