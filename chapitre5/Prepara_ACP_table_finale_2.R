library(fasterize)
library(spdep)
library(ggplot2)
library(sf)
library(sp)
library(SpatialPosition)
library(dplyr)
library(tidyr)

# cl<-makeCluster(detectCores(),type="SOCK")
# registerDoSNOW(cl)
# id = mcparallel({ Sys.sleep(5); TRUE })
# mccollect(id)


Typo_1999_2012_carreaux200_appartement_37158tiles <- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_carreaux200_appartement_37158tiles.csv", stringsAsFactors=FALSE)
Typo_1999_2012_carreaux200_Maison_36693tiles <- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_carreaux200_Maison_36693tiles.csv", stringsAsFactors=FALSE)
tableauID<-full_join(Typo_1999_2012_carreaux200_appartement_37158tiles,Typo_1999_2012_carreaux200_Maison_36693tiles, by="Carreau_ID")
tableauID<-tableauID[,c(1,2)]
# 3changement en objet spatial

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
#
# Mask_st_200 <- st_union(carreauInsee200)
# Mask_st_200<-st_sf(id = 1, geometry =Mask_st_200 )
# 
# par(mfrow = c(1,2))
# # plot(Mask_st_200)
# # Mask_simplified <-
# Mask_st_200 <- Mask_st_200 %>%
#   st_buffer(dist = 200) %>%
#   st_buffer(dist = -400) %>%
#   st_simplify(preserveTopology = FALSE, dTolerance = 200) %>%
#   st_buffer(200)
# Mask_spdf_200<-as(Mask_st_200, "Spatial")
# proj4string(Mask_spdf_200) <-CRS("+init=epsg:2154")
# Mask_spdf_200<- spTransform(Mask_spdf_200, "+init=epsg:2154")

Cadrillage_200$Carreau_ID<-as.numeric(Cadrillage_200$Carreau_ID)
Cadrillage_200<-right_join(Cadrillage_200, tableauID, by ="Carreau_ID")
Cadrillage_200<-Cadrillage_200[,-(2)]

cadrillage_200_spdf<-as(Cadrillage_200, "Spatial")
proj4string(cadrillage_200_spdf) <-CRS("+init=epsg:2154")
cadrillage_200_spdf<- spTransform(cadrillage_200_spdf, "+init=epsg:2154")


#######Table principal#####
data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x",
                                      "ANNAIS_AC","PRESCREDIT","MTCRED","SITMAT_AC", "REQTYPBIEN", "NBRPIECE", "REQ_ANC", "REQ_PRIX")]
DBSCAN_results_table_Promoteurs <- read.csv("~/Projets/DBSCAN/DBSAN/DBSCAN_results_table_Promoteurs.csv", sep="", stringsAsFactors=FALSE)
mytest$ID<-as.numeric(mytest$ID)
mytest<-left_join(mytest,DBSCAN_results_table_Promoteurs, by="ID")
# table(data_redresse_code_promo_2$typeVar, data_redresse_code_promo_2$CSP_VE)
mytest$X.x<-as.numeric(mytest$X.x)
mytest$Y.x<-as.numeric(mytest$Y.x)
mytest$CSP_AC<-as.numeric(mytest$CSP_AC)
mytest$CSP_VE<-as.numeric(mytest$CSP_VE)
mytest$annee.x<-as.numeric(mytest$annee.x)
mytest$ANNAIS_AC<-as.numeric(mytest$ANNAIS_AC)
mytest$MTCRED<-as.numeric(mytest$MTCRED)
mytest$NBRPIECE<-as.numeric(mytest$NBRPIECE)
str(mytest)
unique(mytest$QUALITE_AC)
# 1 import table

mytest$Periode<-ifelse(mytest$annee.x==1996|mytest$annee.x==1999|mytest$annee.x==2003, "Periode_96_2003",
                       ifelse(mytest$annee.x>=2004&mytest$annee.x<=2007, "Periode_04_2007", 
                              ifelse(mytest$annee.x>=2008&mytest$annee.x<=2012, "Periode_08_2012", NA)))

Periode <- unique(mytest$Periode)
Periode <- sort(Periode[!is.na(Periode)])
# 2. Boucle d'alimentation

#Types acquéreurs et vendeurs: toutes transactions

mytest$acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM",
                           ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                  ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                         ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                                       ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM", "particulier_ou_na"))))))

mytest$acquereurs<- ifelse(mytest$CSP_AC>=1 & mytest$CSP_AC<=69&mytest$acquereurs == "particulier_ou_na","Actifs",
                           ifelse(mytest$CSP_AC>=70 & mytest$CSP_AC<=90 &  mytest$acquereurs =="particulier_ou_na", "retraites_inactifs",
                                         ifelse(is.na(mytest$CSP_AC) & mytest$acquereurs=="particulier_ou_na",NA,mytest$acquereurs)))

table(mytest$QUALITE_VE, useNA = "ifany")

mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM",
                         ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                       ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"SAFER",
                                              ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                                     ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM", "particulier_ou_na"))))))


mytest$Vendeurs<- ifelse(mytest$CSP_VE>=1 & mytest$CSP_VE<=69& mytest$Vendeurs == "particulier_ou_na","Actifs",
                         ifelse(mytest$CSP_VE>=70 & mytest$CSP_VE<=90 & mytest$Vendeurs =="particulier_ou_na", "retraites_inactifs", 
                                ifelse(is.na(mytest$CSP_VE) & mytest$Vendeurs=="particulier_ou_na",NA,mytest$Vendeurs)))

mytest$Vendeurs<- ifelse(mytest$cluster>=1 & !is.na(mytest$cluster),"Promoteurs", mytest$Vendeurs)
table(mytest$Vendeurs, useNA = "ifany")


table_acquereur_vendeur<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Vendeurs),!is.na(acquereurs),acquereurs!="SAFER",acquereurs!="particulier_ou_na", Vendeurs!="SAFER", Vendeurs!="particulier_ou_na")

Transac<-table_acquereur_vendeur%>% select(ID,X.x,Y.x,Periode,acquereurs)
#
Montableau_Profil_acquereur<- data.frame(Carreau_ID=NA,cettePeriodeLa=NA, cetacquereurLa=NA,Somme_Profil_acquereur=NA, Pourcentage_Profil_acquereur=NA,Nombre_totalPotentiel_transac=NA)
Montableau_Profil_acquereur<-Montableau_Profil_acquereur%>%filter(!is.na(cettePeriodeLa))

for (cettePeriodeLa in Periode){
  print(cettePeriodeLa)
  test<- Transac%>% filter(Periode==cettePeriodeLa)

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


  Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)
  carreaux_total<- Cadrillage_200_jointure %>%
    group_by(Carreau_ID) %>%
      summarise(Nombre_transacs= length(which(!is.na(ID))))

  carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
   carreaux_total_spdf<-carreaux_total%>%
    as( "Spatial")
  proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
  # carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")

  stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                              unknownpts = cadrillage_200_spdf,
                              varname = "Nombre_transacs",
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_200, longlat = FALSE)
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)

  Profil_acquereur <- unique(spVente$acquereurs)
  Profil_acquereur <- sort(Profil_acquereur[!is.na(Profil_acquereur)])


  for (cetacquereurLa in Profil_acquereur){
    print(cetacquereurLa)
    spVente_2<-spVente%>%
      filter(acquereurs==cetacquereurLa)

    Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente_2, join = st_contains, left=T)
 carreaux_Categories<- Cadrillage_200_jointure %>%
      group_by(Carreau_ID) %>%
      summarise(Nbr_catego= length(which(!is.na(ID))))
carreaux_Categories <- carreaux_Categories%>%filter(Nbr_catego>0)
carreaux_Categories_spdf<- as(carreaux_Categories, "Spatial")
     proj4string(carreaux_Categories_spdf) <-CRS("+init=epsg:2154")
 stewartProfil_acquereur <- mcStewart(knownpts = carreaux_Categories_spdf,
                                         unknownpts = cadrillage_200_spdf,
                                         varname = "Nbr_catego",
                                         typefct = "exponential", span = 500, beta = 2,
                                         mask = Mask_spdf_200, longlat = FALSE)

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
    stewartProfil_acquereur_SF$cettePeriodeLa<- cettePeriodeLa

    stewartProfil_acquereur_df<-as.data.frame(stewartProfil_acquereur_SF)
    stewartProfil_acquereur_df<-stewartProfil_acquereur_df%>%
      select(Carreau_ID,cettePeriodeLa,cetacquereurLa,Somme_Profil_acquereur,Pourcentage_Profil_acquereur, Nombre_totalPotentiel_transac)


    Montableau_Profil_acquereur<-bind_rows(Montableau_Profil_acquereur, stewartProfil_acquereur_df)
  }
}


#########Vendeurs
Transac<-table_acquereur_vendeur%>% select(ID,X.x,Y.x,Periode,Vendeurs)
Montableau_Profil_vendeur<- data.frame(Carreau_ID=NA,cettePeriodeLa=NA, ceVendeurLa=NA,Somme_Profil_vendeur=NA, Pourcentage_Profil_vendeur=NA,Nombre_totalPotentiel_transac=NA)
Montableau_Profil_vendeur<-Montableau_Profil_vendeur%>%filter(!is.na(cettePeriodeLa))

for (cettePeriodeLa in Periode){
  print(cettePeriodeLa)
  test<- Transac%>% filter(Periode==cettePeriodeLa)
  
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

Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)
carreaux_total<- Cadrillage_200_jointure %>%
  group_by(Carreau_ID) %>%
  summarise(Nombre_transacs= length(which(!is.na(ID))))

carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
carreaux_total_spdf<-carreaux_total%>%
  as( "Spatial")
proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
# carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")

stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                            unknownpts = cadrillage_200_spdf,
                            varname = "Nombre_transacs",
                            typefct = "exponential", span = 500, beta = 2,
                            mask = Mask_spdf_200, longlat = FALSE)
stewartTransac_SF <- st_as_sf(stewartTransac) %>%
  rename(NbTransac = OUTPUT)
stewartTransac_df<-as.data.frame(stewartTransac_SF)

  Profil_vendeur <- unique(spVente$Vendeurs)
  Profil_vendeur <- sort(Profil_vendeur[!is.na(Profil_vendeur)])


  for (ceVendeurLa in Profil_vendeur){
    print(ceVendeurLa)
    spVente_2<-spVente%>%
      filter(Vendeurs==ceVendeurLa)

     Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente_2, join = st_contains, left=T)
    carreaux_Categories<- Cadrillage_200_jointure %>%
      group_by(Carreau_ID) %>%
      summarise(Nbr_catego= length(which(!is.na(ID))))
    carreaux_Categories <- carreaux_Categories%>%filter(Nbr_catego>0)
    carreaux_Categories_spdf<- as(carreaux_Categories, "Spatial")
    proj4string(carreaux_Categories_spdf) <-CRS("+init=epsg:2154")
  
    stewartProfil_vendeur <- mcStewart(knownpts = carreaux_Categories_spdf,
                                         unknownpts = cadrillage_200_spdf,
                                         varname = "Nbr_catego",
                                         typefct = "exponential", span = 500, beta = 2,
                                         mask = Mask_spdf_200, longlat = FALSE)


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
    stewartProfil_vendeur_SF$cettePeriodeLa<- cettePeriodeLa

    stewartProfil_vendeur_df<-as.data.frame(stewartProfil_vendeur_SF)
    stewartProfil_vendeur_df<-stewartProfil_vendeur_df%>%
      select(Carreau_ID,cettePeriodeLa,ceVendeurLa,Somme_Profil_vendeur,Pourcentage_Profil_vendeur, Nombre_totalPotentiel_transac)


    Montableau_Profil_vendeur<-bind_rows(Montableau_Profil_vendeur, stewartProfil_vendeur_df)
  }
}

# 
# ####Profil age acquereur#
# 

# ####Fourchette age
mytest$Age_acq = mytest$annee.x-mytest$ANNAIS_AC

mytest$fourchette_age_acq<-mytest$Age_acq
mytest$fourchette_age_acq<- ifelse(mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<30, "[18,30[",
                                   ifelse(mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<50, "[30,50[",
                                          ifelse(mytest$fourchette_age_acq>=50, "[50+",mytest$Age_acq)))

table_age_acquereur<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(fourchette_age_acq), acquereurs=="Actifs" )
table_age_acquereur<-table_age_acquereur%>% filter(Age_acq>17)
Transac<-table_age_acquereur%>% select(ID,X.x,Y.x,Periode,fourchette_age_acq)

Montableau_Age_acquereur<- data.frame(Carreau_ID=NA,cettePeriodeLa=NA,cetAgeacquereurLa=NA,Somme_Profil_age_acquereur=NA,Pourcentage_age_acquereur=NA, Nombre_totalPotentiel_transac_age_acquereur=NA)

Montableau_Age_acquereur<-Montableau_Age_acquereur%>%filter(!is.na(cettePeriodeLa))

for (cettePeriodeLa in Periode){
 print(cettePeriodeLa)
  test<- Transac%>% filter(Periode==cettePeriodeLa)
  
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
  
  Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)
  carreaux_total<- Cadrillage_200_jointure %>%
    group_by(Carreau_ID) %>%
      summarise(Nombre_transacs= length(which(!is.na(ID))))
  
  carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
  carreaux_total_spdf<-carreaux_total%>%
    as( "Spatial")
  proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
  # carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")
  
  stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                              unknownpts = cadrillage_200_spdf,
                              varname = "Nombre_transacs",
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_200, longlat = FALSE)
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)
  
  Age_acquereur<- unique(spVente$fourchette_age_acq)
  Age_acquereur <- sort(Age_acquereur[!is.na(Age_acquereur)])
 
  for (cetAgeacquereurLa in Age_acquereur){
    print(cetAgeacquereurLa)
    spVente_2<-spVente%>%
      filter(fourchette_age_acq==cetAgeacquereurLa)
    
    Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente_2, join = st_contains, left=T)
    carreaux_Categories<- Cadrillage_200_jointure %>%
      group_by(Carreau_ID) %>%
      summarise(Nbr_catego= length(which(!is.na(ID))))
    carreaux_Categories <- carreaux_Categories%>%filter(Nbr_catego>0)
    carreaux_Categories_spdf<- as(carreaux_Categories, "Spatial")
    proj4string(carreaux_Categories_spdf) <-CRS("+init=epsg:2154")
    

    stewartAge_acquereur <- mcStewart(knownpts = carreaux_Categories_spdf,
                                      unknownpts = cadrillage_200_spdf,
                                      varname = "Nbr_catego",
                                      typefct = "exponential", span = 500, beta = 2,
                                      mask = Mask_spdf_200, longlat = FALSE)
    
    
    stewartAge_acquereur_SF <- st_as_sf(stewartAge_acquereur) %>%
      rename(Sum_Age_acquereur  = OUTPUT)
    
    
    stewartAge_acquereur_SF <- left_join(stewartAge_acquereur_SF %>%
                                           select(Sum_Age_acquereur,Carreau_ID),
                                         stewartTransac_df%>%
                                           select(NbTransac,Carreau_ID),
                                         by="Carreau_ID") %>%
      mutate(Pourcentage_age_acquereur = (Sum_Age_acquereur / NbTransac)*100,
             Somme_Profil_age_acquereur= Sum_Age_acquereur,
             Nombre_totalPotentiel_transac_age_acquereur=NbTransac)
    stewartAge_acquereur_SF$cetAgeacquereurLa<-cetAgeacquereurLa
    stewartAge_acquereur_SF$cettePeriodeLa<- cettePeriodeLa
    
    stewartAge_acquereur_df<-as.data.frame(stewartAge_acquereur_SF)
    stewartAge_acquereur_df<-stewartAge_acquereur_df%>%
      select(Carreau_ID,cettePeriodeLa,cetAgeacquereurLa,Somme_Profil_age_acquereur,Pourcentage_age_acquereur, Nombre_totalPotentiel_transac_age_acquereur)
    
    
    Montableau_Age_acquereur<-bind_rows(Montableau_Age_acquereur, stewartAge_acquereur_df)
  }
}




# ####Profil type logement_maisons_appartements#
# 
# 
mytest$Type_log<- ifelse( mytest$REQTYPBIEN=="MA" ,"Maisons",
                          ifelse(mytest$REQTYPBIEN=="AP", "Appartements",mytest$REQTYPBIEN))


table_Type_log<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Type_log))

Transac<-table_Type_log%>% select(ID,X.x,Y.x,Periode,Type_log)
Montableau_Type_logement<- data.frame(Carreau_ID=NA,cettePeriodeLa=NA,ceLogementLa, Somme_Profil_logement=NA, Pourcentage_Profil_logement=NA, Nombre_totalPotentiel_transac_logement=NA)
Montableau_Type_logement<-Montableau_Type_logement%>%filter(!is.na(cettePeriodeLa))

 for (cettePeriodeLa in Periode){
   print(cettePeriodeLa)
   test<- Transac%>% filter(Periode==cettePeriodeLa)
   
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
   
   Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)
   carreaux_total<- Cadrillage_200_jointure %>%
     group_by(Carreau_ID) %>%
       summarise(Nombre_transacs= length(which(!is.na(ID))))
   
   carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
   carreaux_total_spdf<-carreaux_total%>%
     as( "Spatial")
   proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
   # carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")
   
   stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                               unknownpts = cadrillage_200_spdf,
                               varname = "Nombre_transacs",
                               typefct = "exponential", span = 500, beta = 2,
                               mask = Mask_spdf_200, longlat = FALSE)
   stewartTransac_SF <- st_as_sf(stewartTransac) %>%
     rename(NbTransac = OUTPUT)
   stewartTransac_df<-as.data.frame(stewartTransac_SF)
  Type_log <- unique(spVente$Type_log)
  Type_log <- sort(Type_log[!is.na(Type_log)])

  for (ceLogementLa in Type_log){
    print(ceLogementLa)
     spVente_2<-spVente%>%
      filter(Type_log==ceLogementLa)
    
    Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente_2, join = st_contains, left=T)
    carreaux_Categories<- Cadrillage_200_jointure %>%
      group_by(Carreau_ID) %>%
      summarise(Nbr_catego= length(which(!is.na(ID))))
    carreaux_Categories <- carreaux_Categories%>%filter(Nbr_catego>0)
    carreaux_Categories_spdf<- as(carreaux_Categories, "Spatial")
    proj4string(carreaux_Categories_spdf) <-CRS("+init=epsg:2154")
    stewartType_BIEN <- mcStewart(knownpts = carreaux_Categories_spdf,
                                  unknownpts = cadrillage_200_spdf,
                                  varname = "Nbr_catego",
                                  typefct = "exponential", span = 500, beta = 2,
                                  mask = Mask_spdf_200, longlat = FALSE)


    stewartType_BIEN_SF <- st_as_sf(stewartType_BIEN) %>%
      rename(Sum_Type_BIEN = OUTPUT)


    stewartType_BIEN_SF <- left_join(stewartType_BIEN_SF %>%
                                       select(Sum_Type_BIEN,Carreau_ID),
                                     stewartTransac_df%>%
                                       select(NbTransac,Carreau_ID),
                                     by="Carreau_ID") %>%
      mutate(Pourcentage_Profil_logement = (Sum_Type_BIEN / NbTransac)*100,
             Somme_Profil_logement= Sum_Type_BIEN,
             Nombre_totalPotentiel_transac_logement=NbTransac)
    stewartType_BIEN_SF$ceLogementLa<-ceLogementLa
    stewartType_BIEN_SF$cettePeriodeLa<- cettePeriodeLa

    stewartType_BIEN_df<-as.data.frame(stewartType_BIEN_SF)
    stewartType_BIEN_df<-stewartType_BIEN_df%>%
      select(Carreau_ID,cettePeriodeLa,ceLogementLa,Somme_Profil_logement,Pourcentage_Profil_logement,Nombre_totalPotentiel_transac_logement)

    Montableau_Type_logement<-bind_rows(Montableau_Type_logement, stewartType_BIEN_df)
  }
}
# 

mytest$Profil_log<- ifelse( mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>7 | mytest$REQTYPBIEN=="AP" & mytest$NBRPIECE>6  ,"Bien_exceptionnel",
                          ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=0 & mytest$NBRPIECE<=1, "studio_et_1_pièce","Logement banal"))


table_Profil_log<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Profil_log))

Transac<-table_Profil_log%>% select(ID,X.x,Y.x,Periode,Profil_log)
Montableau_Profil_logement<- data.frame(Carreau_ID=NA,cettePeriodeLa=NA,ceProfilLogementLa=NA, Somme_Profil_logement=NA, Pourcentage_Profil_logement=NA, Nombre_totalPotentiel_transac_logementProfil=NA)
Montableau_Profil_logement<-Montableau_Profil_logement%>%filter(!is.na(cettePeriodeLa))

for (cettePeriodeLa in Periode){
  print(cettePeriodeLa)
  test<- Transac%>% filter(Periode==cettePeriodeLa)
  
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
  
  Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)
  carreaux_total<- Cadrillage_200_jointure %>%
    group_by(Carreau_ID) %>%
      summarise(Nombre_transacs= length(which(!is.na(ID))))
  
  carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
  carreaux_total_spdf<-carreaux_total%>%
    as( "Spatial")
  proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
  # carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")
  
  stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                              unknownpts = cadrillage_200_spdf,
                              varname = "Nombre_transacs",
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_200, longlat = FALSE)
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)
  Profil_log <- unique(spVente$Profil_log)
  Profil_log <- sort(Profil_log[!is.na(Profil_log)])

  for (ceProfilLogementLa in Profil_log){
    spVente_2<-spVente%>%
      filter(Profil_log==ceProfilLogementLa)
    print(ceProfilLogementLa)
    
    Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente_2, join = st_contains, left=T)
    carreaux_Categories<- Cadrillage_200_jointure %>%
      group_by(Carreau_ID) %>%
      summarise(Nbr_catego= length(which(!is.na(ID))))
    carreaux_Categories <- carreaux_Categories%>%filter(Nbr_catego>0)
    carreaux_Categories_spdf<- as(carreaux_Categories, "Spatial")
    proj4string(carreaux_Categories_spdf) <-CRS("+init=epsg:2154")
    stewartProfil_BIEN <- mcStewart(knownpts = carreaux_Categories_spdf,
                                  unknownpts = cadrillage_200_spdf,
                                  varname = "Nbr_catego",
                                  typefct = "exponential", span = 500, beta = 2,
                                  mask = Mask_spdf_200, longlat = FALSE)


    stewartProfil_BIEN_SF <- st_as_sf(stewartProfil_BIEN) %>%
      rename(Sum_Profil_BIEN = OUTPUT)

  stewartProfil_BIEN_SF <- left_join(stewartProfil_BIEN_SF %>%
                                       select(Sum_Profil_BIEN,Carreau_ID),
                                     stewartTransac_df%>%
                                       select(NbTransac,Carreau_ID),
                                     by="Carreau_ID") %>%
      mutate(Pourcentage_Profil_logement = (Sum_Profil_BIEN / NbTransac)*100,
             Somme_Profil_logement= Sum_Profil_BIEN,
             Nombre_totalPotentiel_transac_logementProfil=NbTransac)
    stewartProfil_BIEN_SF$ceProfilLogementLa<-ceProfilLogementLa
    stewartProfil_BIEN_SF$cettePeriodeLa<- cettePeriodeLa

    stewartProfil_BIEN_df<-as.data.frame(stewartProfil_BIEN_SF)
    stewartProfil_BIEN_df<-stewartProfil_BIEN_df%>%
      select(Carreau_ID,cettePeriodeLa,ceProfilLogementLa,Somme_Profil_logement,Pourcentage_Profil_logement,Nombre_totalPotentiel_transac_logementProfil)

    Montableau_Profil_logement<-bind_rows(Montableau_Profil_logement, stewartProfil_BIEN_df)
  }
}

# 

# ####################################
#
# ###ituation matri

mytest$Couple<- ifelse(mytest$SITMAT_AC == "M" | mytest$SITMAT_AC == "P" | mytest$SITMAT_AC == "R", "En_couple",
                            ifelse(mytest$SITMAT_AC == "D"| mytest$SITMAT_AC == "V", "Divorce_ou_veuvage",
                                   ifelse(mytest$SITMAT_AC == "C", "Celibataire", NA )))

table_situation_matri<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Couple))
Transac<-table_situation_matri%>% select(ID,X.x,Y.x,Periode,Couple)
Montableau_situation_matri<- data.frame(Carreau_ID=NA,cettePeriodeLa=NA,cetteSituMatriLa=NA,Somme_SituMatri=NA,Pourcentage_SituMatri=NA,Nombre_totalPotentiel_SituMatri=NA)
Montableau_situation_matri<-Montableau_situation_matri%>%filter(!is.na(cettePeriodeLa))

for (cettePeriodeLa in Periode){
  print(cettePeriodeLa)
  test<- Transac%>% filter(Periode==cettePeriodeLa)
  
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
  
  Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)
  carreaux_total<- Cadrillage_200_jointure %>%
    group_by(Carreau_ID) %>%
      summarise(Nombre_transacs= length(which(!is.na(ID))))
  
  carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
  carreaux_total_spdf<-carreaux_total%>%
    as( "Spatial")
  proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
  # carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")
  
  stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                              unknownpts = cadrillage_200_spdf,
                              varname = "Nombre_transacs",
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_200, longlat = FALSE)
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)
  situation_matri<- unique(spVente$Couple)
  situation_matri <- sort(situation_matri[!is.na(situation_matri)])

  for (cetteSituMatriLa in situation_matri){
    print(cetteSituMatriLa)
    spVente_2<-spVente%>%
      filter(Couple==cetteSituMatriLa)
    
    
    Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente_2, join = st_contains, left=T)
    carreaux_Categories<- Cadrillage_200_jointure %>%
      group_by(Carreau_ID) %>%
      summarise(Nbr_catego= length(which(!is.na(ID))))
    carreaux_Categories <- carreaux_Categories%>%filter(Nbr_catego>0)
    carreaux_Categories_spdf<- as(carreaux_Categories, "Spatial")
    proj4string(carreaux_Categories_spdf) <-CRS("+init=epsg:2154")
    stewartSituMatri<- mcStewart(knownpts = carreaux_Categories_spdf,
                                 unknownpts = cadrillage_200_spdf,
                                 varname = "Nbr_catego",
                                 typefct = "exponential", span = 500, beta = 2,
                                 mask = Mask_spdf_200, longlat = FALSE)


    stewartSituMatri_SF <- st_as_sf(stewartSituMatri) %>%
      rename(Sum_SituMatri  = OUTPUT)


    stewartSituMatri_SF <- left_join(stewartSituMatri_SF %>%
                                       select(Sum_SituMatri,Carreau_ID),
                                     stewartTransac_df%>%
                                       select(NbTransac,Carreau_ID),
                                     by="Carreau_ID") %>%
      mutate(Pourcentage_SituMatri = (Sum_SituMatri / NbTransac)*100,
             Somme_SituMatri=Sum_SituMatri,
             Nombre_totalPotentiel_SituMatri= NbTransac)
    stewartSituMatri_SF$cetteSituMatriLa<-cetteSituMatriLa
    stewartSituMatri_SF$cettePeriodeLa<- cettePeriodeLa

    stewartSituMatri_df<-as.data.frame(stewartSituMatri_SF)
    stewartSituMatri_df<-stewartSituMatri_df%>%
      select(Carreau_ID,cettePeriodeLa,cetteSituMatriLa,Somme_SituMatri,Pourcentage_SituMatri,Nombre_totalPotentiel_SituMatri)

    Montableau_situation_matri<-bind_rows(Montableau_situation_matri, stewartSituMatri_df)
  }
}

# 


# 
# ####Profil Regime marche#

mytest$Credit <- ifelse(mytest$PRESCREDIT!="N" & mytest$PRESCREDIT!="O" , "Credit_cautionne_suppose",
                        ifelse(mytest$PRESCREDIT=="O",  "Avec_credit_hypothécaire",
                               ifelse(mytest$PRESCREDIT=="N",  "Sans_credit",  mytest$Credit)))

table(mytest$Credit)
table_Credit<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Credit),acquereurs=="Actifs" | acquereurs=="retraites_inactifs")
table(table_Credit$Credit)
Transac<-table_Credit%>% select(ID,X.x,Y.x,Periode,Credit)

Montableau_Regime_achat<- data.frame(Carreau_ID=NA,cettePeriodeLa=NA,ceProfilCreditLa=NA,Somme_RegimeAchat=NA,Pourcentage_RegimeAchat=NA, Nombre_totalPotentiel_transac_RegimeAchatLa=NA)
Montableau_Regime_achat<-Montableau_Regime_achat%>%filter(!is.na(cettePeriodeLa))

for (cettePeriodeLa in Periode){
  print(cettePeriodeLa)
  test<- Transac%>% filter(Periode==cettePeriodeLa)
  
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
  
  Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)
  carreaux_total<- Cadrillage_200_jointure %>%
    group_by(Carreau_ID) %>%
      summarise(Nombre_transacs= length(which(!is.na(ID))))
  
  carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
  carreaux_total_spdf<-carreaux_total%>%
    as( "Spatial")
  proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
  # carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")
  
  stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                              unknownpts = cadrillage_200_spdf,
                              varname = "Nombre_transacs",
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_200, longlat = FALSE)
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)
  Regime_achat<- unique(spVente$Credit)
  Regime_achat <- sort(Regime_achat[!is.na(Regime_achat)])


  for (ceRegimeAchatLa in Regime_achat){
   print(ceRegimeAchatLa)
     spVente_2<-spVente%>%
      filter(Credit==ceRegimeAchatLa)

    Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente_2, join = st_contains, left=T)
    carreaux_Categories<- Cadrillage_200_jointure %>%
      group_by(Carreau_ID) %>%
      summarise(Nbr_catego= length(which(!is.na(ID))))
    carreaux_Categories <- carreaux_Categories%>%filter(Nbr_catego>0)
    carreaux_Categories_spdf<- as(carreaux_Categories, "Spatial")
    proj4string(carreaux_Categories_spdf) <-CRS("+init=epsg:2154")
    stewartRegime_achat<- mcStewart(knownpts = carreaux_Categories_spdf,
                                    unknownpts = cadrillage_200_spdf,
                                    varname = "Nbr_catego",
                                    typefct = "exponential", span = 500, beta = 2,
                                    mask = Mask_spdf_200, longlat = FALSE)


    stewartRegime_achat_SF <- st_as_sf(stewartRegime_achat) %>%
      rename(Sum_RegimeAchat  = OUTPUT)


    stewartRegime_achat_SF <- left_join(stewartRegime_achat_SF %>%
                                          select(Sum_RegimeAchat,Carreau_ID),
                                        stewartTransac_df%>%
                                          select(NbTransac,Carreau_ID),
                                        by="Carreau_ID") %>%
      mutate(Pourcentage_RegimeAchat = (Sum_RegimeAchat / NbTransac)*100,
             Somme_RegimeAchat=Sum_RegimeAchat,
             Nombre_totalPotentiel_transac_RegimeAchatLa=NbTransac)
    stewartRegime_achat_SF$ceProfilCreditLa<-ceRegimeAchatLa
    stewartRegime_achat_SF$cettePeriodeLa<- cettePeriodeLa

    stewartRegime_achat_df<-as.data.frame(stewartRegime_achat_SF)
    stewartRegime_achat_df<-stewartRegime_achat_df%>%
      select(Carreau_ID,cettePeriodeLa,ceProfilCreditLa,Somme_RegimeAchat,Pourcentage_RegimeAchat,Nombre_totalPotentiel_transac_RegimeAchatLa)


    Montableau_Regime_achat<-bind_rows(Montableau_Regime_achat, stewartRegime_achat_df)
  }
}


# 
# ###LTV


table_LTV<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, Credit=="Avec credit hypothécaire",acquereurs=="Actifs" | acquereurs=="retraites_inactifs", MTCRED>100,!is.na(REQ_PRIX))

Transac<-table_LTV%>% select(ID,X.x,Y.x,Periode,MTCRED,REQ_PRIX)

Montableau_LTV<- data.frame(Carreau_ID=NA,cettePeriodeLa=NA,Result_LTV=NA, Montant_moyen_credit=NA, Nombre_transacs_potentiel_LTV=NA)
Montableau_LTV<-Montableau_LTV%>%filter(!is.na(cettePeriodeLa))

for (cettePeriodeLa in Periode){
  print(cettePeriodeLa)
  test<- Transac%>% filter(Periode==cettePeriodeLa)
  
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
  
  Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)
  carreaux_total<- Cadrillage_200_jointure %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs=length(which(!is.na(ID))),
              MTCRED= sum(MTCRED,na.rm = TRUE),
              REQ_PRIX= sum(REQ_PRIX,na.rm = TRUE))
  
  carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
  carreaux_total_spdf<-carreaux_total%>%
    as( "Spatial")
  proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
  # carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")
  
  stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                              unknownpts = cadrillage_200_spdf,
                              varname = "Nombre_transacs",
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_200, longlat = FALSE)
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)
  # spVente_2$MTCRED
  
  stewartCred <- mcStewart(knownpts = carreaux_total_spdf,
                           unknownpts = cadrillage_200_spdf,
                           varname = "MTCRED",
                           typefct = "exponential", span = 500, beta = 2,
                           mask = Mask_spdf_200, longlat = FALSE)
  stewartCred_SF <- st_as_sf(stewartCred) %>%
    rename(MTCRED = OUTPUT)
  stewartCred_df<-as.data.frame(stewartCred_SF)
  
  
  stewartPrix<- mcStewart(knownpts = carreaux_total_spdf,
                          unknownpts = cadrillage_200_spdf,
                          varname = "REQ_PRIX",
                          typefct = "exponential", span = 500, beta = 2,
                          mask = Mask_spdf_200, longlat = FALSE)
  
  
  stewartPrix_SF <- st_as_sf(stewartPrix) %>%
    rename(Sum_PRIX  = OUTPUT)
  
  
  stewartLTV_SF <- left_join(stewartPrix_SF %>%
                               select(Sum_PRIX,Carreau_ID),
                             stewartCred_df%>%
                               select(MTCRED,Carreau_ID),
                             by="Carreau_ID") %>%
    mutate(Result_LTV = (MTCRED / Sum_PRIX)*100)
  
  stewartLTV_SF <- left_join(stewartLTV_SF, stewartTransac_df %>%
                               select(NbTransac,Carreau_ID),
                             by="Carreau_ID") %>%
    mutate(Montant_moyen_credit = (MTCRED / NbTransac),
           Nombre_transacs_potentiel_LTV=NbTransac)
  
  stewartLTV_SF$cettePeriodeLa<- cettePeriodeLa
  
  stewartLTV_df<-as.data.frame(stewartLTV_SF)
  stewartLTV_df<-stewartLTV_df%>%
    select(Carreau_ID,cettePeriodeLa,Result_LTV,Montant_moyen_credit,Nombre_transacs_potentiel_LTV)
  
  Montableau_LTV<-bind_rows(Montableau_LTV, stewartLTV_df)
}
########Communes d'originie, porte du marche#
Communes_origine_df <- read.csv("~/Projets/ACP_evolution_profil_marche_backup/ACP_projets_table_final/Communes_origine_df.csv", sep=";", stringsAsFactors=FALSE)
# 
mytest<-join(mytest, Communes_origine_df[,c("ID","origine")], by="ID", type="left", match="first")

table_origine_acquereur<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(origine))
Transac<-table_origine_acquereur%>% select(ID,X.x,Y.x,Periode,origine)
#
Montableau_Profil_origine<- data.frame(Carreau_ID=NA,cettePeriodeLa=NA, cetteOrigineLa=NA,Somme_Origine_acquereur=NA, Pourcentage_Origine_acquereur=NA,Nombre_totalPotentiel_Origine=NA)
Montableau_Profil_origine<-Montableau_Profil_origine%>%filter(!is.na(cettePeriodeLa))


for (cettePeriodeLa in Periode){
  print(cettePeriodeLa)
  test<- Transac%>% filter(Periode==cettePeriodeLa)
  
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
  
  
  Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)
  carreaux_total<- Cadrillage_200_jointure %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs= length(which(!is.na(ID))))
  
  carreaux_total <- carreaux_total%>%filter(Nombre_transacs>0)
  carreaux_total_spdf<-carreaux_total%>%
    as( "Spatial")
  proj4string(carreaux_total_spdf) <- CRS("+init=epsg:2154")
  # carreaux_total_spdf<- spTransform(carreaux_total_spdf, "+init=epsg:2154")
  
  stewartTransac <- mcStewart(knownpts = carreaux_total_spdf,
                              unknownpts = cadrillage_200_spdf,
                              varname = "Nombre_transacs",
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_200, longlat = FALSE)
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)
  
  Profil_origine <- unique(spVente$origine)
  Profil_origine <- sort(Profil_origine[!is.na(Profil_origine)])
  
  
  for (cetteOrigineLa in Profil_origine){
    print(cetteOrigineLa)
    spVente_2<-spVente%>%
      filter(origine==cetteOrigineLa)
    
    Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente_2, join = st_contains, left=T)
    carreaux_Categories<- Cadrillage_200_jointure %>%
      group_by(Carreau_ID) %>%
      summarise(Nbr_catego= length(which(!is.na(ID))))
    carreaux_Categories <- carreaux_Categories%>%filter(Nbr_catego>0)
    carreaux_Categories_spdf<- as(carreaux_Categories, "Spatial")
    proj4string(carreaux_Categories_spdf) <-CRS("+init=epsg:2154")
    stewartProfil_origine <- mcStewart(knownpts = carreaux_Categories_spdf,
                                         unknownpts = cadrillage_200_spdf,
                                         varname = "Nbr_catego",
                                         typefct = "exponential", span = 500, beta = 2,
                                         mask = Mask_spdf_200, longlat = FALSE)
    
    stewartProfil_origine_SF <- st_as_sf(stewartProfil_origine) %>%
      rename(Sum_Profil_origine = OUTPUT)
    stewartProfil_origine_SF <- left_join(stewartProfil_origine_SF %>%
                                              select(Sum_Profil_origine,Carreau_ID),
                                            stewartTransac_df%>%
                                              select(NbTransac,Carreau_ID),
                                            by="Carreau_ID") %>%
      mutate(Pourcentage_Origine_acquereur = (Sum_Profil_origine / NbTransac)*100,
             Somme_Origine_acquereur =Sum_Profil_origine,
             Nombre_totalPotentiel_Origine = NbTransac)
    stewartProfil_origine_SF$cetteOrigineLa<-cetteOrigineLa
    stewartProfil_origine_SF$cettePeriodeLa<- cettePeriodeLa
    
    stewartProfil_origine_df<-as.data.frame(stewartProfil_origine_SF)
    stewartProfil_origine_df<-stewartProfil_origine_df%>%
      select(Carreau_ID,cettePeriodeLa,cetteOrigineLa,Somme_Origine_acquereur,Pourcentage_Origine_acquereur, Nombre_totalPotentiel_Origine)
    
    
    Montableau_Profil_origine<-bind_rows(Montableau_Profil_origine, stewartProfil_origine_df)
  }
}








# ####################################

# Jointure Grand tableau de donnees




# select(-Somme_Profil_vendeur,Nombre_totalPotentiel_transac)%>% 
check<-Montableau_Profil_acquereur%>%
            gather(variable, value, -(Carreau_ID :cetacquereurLa)) %>%
            unite(cetacquereurLa, cetacquereurLa, variable) %>%
            spread(cetacquereurLa, value)
summary(check)

Tableau_ACP_Final<- left_join(Montableau_Profil_acquereur%>%
                                gather(variable, value, -(Carreau_ID :cetacquereurLa)) %>%
                                unite(cetacquereurLa, cetacquereurLa, variable) %>%
                                spread(cetacquereurLa, value),
                                Montableau_Profil_vendeur %>% 
                                gather(variable, value, -(Carreau_ID :ceVendeurLa)) %>%
                                unite(ceVendeurLa, ceVendeurLa, variable) %>%
                                spread(ceVendeurLa, value),
                                by= c("Carreau_ID", "cettePeriodeLa"))

Tableau_ACP_Final<- left_join(Tableau_ACP_Final, 
                              Montableau_Age_acquereur%>% 
                                gather(variable, value, -(Carreau_ID : cetAgeacquereurLa  )) %>%
                                unite(cetAgeacquereurLa  , cetAgeacquereurLa   , variable) %>%
                                spread(cetAgeacquereurLa  , value),
                              by= c("Carreau_ID", "cettePeriodeLa"))


Tableau_ACP_Final<- left_join(Tableau_ACP_Final,
                              Montableau_Type_logement %>% 
                                gather(variable, value, -(Carreau_ID :ceLogementLa )) %>%
                                unite(ceLogementLa, ceLogementLa , variable) %>%
                                spread(ceLogementLa, value),
                              by= c("Carreau_ID", "cettePeriodeLa"))


Tableau_ACP_Final<- left_join(Tableau_ACP_Final, 
                              Montableau_situation_matri%>% 
                                gather(variable, value, -(Carreau_ID : cetteSituMatriLa  )) %>%
                                unite(cetteSituMatriLa  , cetteSituMatriLa   , variable) %>%
                                spread(cetteSituMatriLa  , value),
                              by= c("Carreau_ID", "cettePeriodeLa"))


Tableau_ACP_Final<- left_join(Tableau_ACP_Final, 
                              Montableau_Profil_logement%>% 
                                gather(variable, value, -(Carreau_ID : ceProfilLogementLa )) %>%
                                unite(ceProfilLogementLa , ceProfilLogementLa  , variable) %>%
                                spread(ceProfilLogementLa , value),
                              by= c("Carreau_ID", "cettePeriodeLa"))



Tableau_ACP_Final<- left_join(Tableau_ACP_Final, 
                              Montableau_Regime_achat%>% 
                                gather(variable, value, -(Carreau_ID : ceProfilCreditLa   )) %>%
                                unite(ceProfilCreditLa ,ceProfilCreditLa ,variable) %>%
                                spread(ceProfilCreditLa,value),
                              by= c("Carreau_ID", "cettePeriodeLa"))

Tableau_ACP_Final<- left_join(Tableau_ACP_Final, 
                              Montableau_LTV,by= c("Carreau_ID", "cettePeriodeLa"))


Tableau_ACP_Final<- left_join(Tableau_ACP_Final, 
                              Montableau_Profil_origine%>% 
                                gather(variable, value, -(Carreau_ID : cetteOrigineLa   )) %>%
                                unite(cetteOrigineLa ,cetteOrigineLa ,variable) %>%
                                spread(cetteOrigineLa,value),
                              by= c("Carreau_ID", "cettePeriodeLa"))

summary(Tableau_ACP_Final)
colnames(Tableau_ACP_Final)
#Save
setwd("~/Projets/ACP_evolution_profil_marche_backup/ACP_projets_table_final")
write.csv2(x=Tableau_ACP_Final,file = "Tableau_ACP_Final.csv", row.names=FALSE, fileEncoding = "UTF-8")
