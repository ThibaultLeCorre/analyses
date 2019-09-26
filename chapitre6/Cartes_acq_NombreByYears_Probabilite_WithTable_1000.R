library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)
library(ggplot2)


### Importer Elements preparation carto
# setwd( "C:/Users/thibault/Documents/Sync/Work_on_DATA/shapes/limite_mask_idf")
setwd("~/Shapes/limite_mask_idf")
list.files()
Mask.st <- st_read("limite_carroyage_IDF_2.shp",
                   stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
#faire une couche unie avec contours

Mask.st <- st_union(Mask.st)
Mask.st <-st_sf(id = 1, geometry = Mask.st )
par(mfrow = c(1,2))


# Mask_simplified <-
Mask.st <- Mask.st %>%
  st_buffer(dist = 500) %>%
  st_buffer(dist = -1000) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 500) %>%
  st_buffer(500)

# par(mfrow = c(1,1))
#Importation carroyage Insee 1000*1000 et 1*1km
setwd("~/Shapes/datidf")
list.files()
carreauInsee1000 <- st_read("car1000m_idf_reparGeom.shp",
                            stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
carreauInsee1000 <-  st_transform(carreauInsee1000, crs = 2154)

# Passer de sfc a sf, trad : passer d'un objet type multi en uni

#Importer limite_region_departement
# setwd( "C:/Users/thibault/Documents/Sync/Work_on_DATA/shapes/shpIDF_dep_lamb93")
setwd("~/Shapes/shpIDF_dep_lamb93")
lim_IDF.st <- st_read("ile-de-france.shp",
                      stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
# union pour n'avoir que les limites
lim_IDF.st<- st_union(lim_IDF.st)
plot(lim_IDF.st)
# Passer de sfc a sf, trad : passer d'un objet type multi en uni
lim_IDF.st <- st_sf(id = 1, geometry = lim_IDF.st)

#reformatage de list à sf

carreauInsee1000_2<- carreauInsee1000%>%
  st_sf(geometry = .)

# cadrillage_jointure$carreau<- row.names(cadrillage_jointure)
plot(st_geometry(Mask.st), col = "grey90", lwd = 0.2)
plot(carreauInsee1000_2, border = "grey70", add = T)

# Faire l'intersection et les zones urbaines, peut prendre un certain temps
# verif st_crs()

st_crs(Mask.st)
st_crs(spVente)
st_crs(carreauInsee1000_2)


carreauInsee1000_2$Carreau_ID<-1:length(carreauInsee1000_2$id_c1000)


cadrillage_spdf<-as(carreauInsee1000_2, "Spatial")
Mask.spdf<-as(Mask.st, "Spatial")
lim_IDF.spdf<-as(lim_IDF.st, "Spatial")

proj4string(cadrillage_spdf) <-CRS("+init=epsg:2154")
cadrillage_spdf<- spTransform(cadrillage_spdf, "+init=epsg:2154")
proj4string(Mask.spdf) <-CRS("+init=epsg:2154")
Mask.spdf<- spTransform(Mask.spdf, "+init=epsg:2154")
proj4string(lim_IDF.spdf) <-CRS("+init=epsg:2154")
lim_IDF.spdf<- spTransform(lim_IDF.spdf, "+init=epsg:2154")

###Data###

# data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","CSP_AC","X.x","Y.x")]

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




###Periode
mytest$Periode<-ifelse(mytest$annee.x==1996|mytest$annee.x==1999|mytest$annee.x==2003, "Periode_96_2003",
                       ifelse(mytest$annee.x>=2004&mytest$annee.x<=2007, "Periode_04_2007", 
                              ifelse(mytest$annee.x>=2008&mytest$annee.x<=2012, "Periode_08_2012", NA)))

unique(mytest$acquereurs)
mytest <- mytest %>%
  filter(!is.na(acquereurs), acquereurs!="SAFER")

test<- mytest
#Cr?ation du fichier SF (lambert II ?tendu, coord dorigine fichier BIEN)

spVente <- st_as_sf(test,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)


#conversion WGS84 et stockage coords

spVente <-  st_transform(spVente, crs = 4326)
coords<- st_coordinates(spVente)
spVente$longWGS84<-coords[,1]   
spVente$latWGS84<-coords[,2]

#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]   
spVente$Ylamb93<-coords[,2]

## essai repr?sentation
osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)


##Jointure
carreauInsee1000_jointure<-st_join(carreauInsee1000_2, spVente, join = st_contains, left=T)


######################Define breaks
Periodes <- unique(carreauInsee1000_jointure$Periode)
Periodes <- sort(Periodes[!is.na(Periodes)])


###Discretisation
Tableau_Acquereur_Carreaux1000 <- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Carreaux1000.csv", stringsAsFactors=FALSE)
mytest<-Tableau_Acquereur_Carreaux1000
mytest$Periode<-ifelse(mytest$cetteanneeLa==1996|mytest$cetteanneeLa==1999|mytest$cetteanneeLa==2003, "Periode_96_2003",
                       ifelse(mytest$cetteanneeLa>=2004&mytest$cetteanneeLa<=2007, "Periode_04_2007",
                              ifelse(mytest$cetteanneeLa>=2008&mytest$cetteanneeLa<=2012, "Periode_08_2012", NA)))

mytest$cetacquereurLa<- ifelse(mytest$cetacquereurLa == "Liberales", "Com_art_Chef_entreprises",mytest$cetacquereurLa)
mytest_2 <- mytest %>%
  group_by(Carreau_ID,cetacquereurLa,Periode) %>%
  summarise(Total_type= sum(Somme_Profil_acquereur))
mytest_3 <- mytest %>%
  group_by(cetacquereurLa,Periode) %>%
  summarise(Total_acq = sum(Somme_Profil_acquereur))
mytest<- left_join(mytest_2, mytest_3)
mytest$Proba<- (mytest$Total_type / mytest$Total_acq)*100
breaksParCSP_ratio<- mytest %>%
  group_by(cetacquereurLa,Periode) %>%
  summarise(breaksMax = list(quantile(Proba, probs = seq(0, 1, 0.1))))



#etape 2
Probabilites_acquereur<- data.frame(Carreau_ID=NA,Potentiel_acquereur=NA, cetacquereurLa=NA, Periode=NA,Total=NA)
Probabilites_acquereur<-Probabilites_acquereur%>%
  filter(!is.na(Periode))

for (cettePeriodeLa in Periodes){
  
  carreaux_filtred <- carreauInsee1000_jointure %>%
    filter(Periode == cettePeriodeLa)
  
  # carreaux_filtred <-  carreaux_filtred[sample(1: nrow(carreaux_filtred), 500,replace=T), ]
  Total<- ifelse(cettePeriodeLa ==  "Periode_96_2003", 3,
                 ifelse(cettePeriodeLa == "Periode_04_2007", 4, 
                        ifelse(cettePeriodeLa == "Periode_08_2012",5,0)))
  
  
  carreaux_resumes <- carreaux_filtred %>%ungroup()%>%
    group_by(Carreau_ID, acquereurs) %>%
    dplyr::summarise(Nombre=length(acquereurs)/Total)%>%
    spread(acquereurs, Nombre, fill=0)
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  MesCSP <- colnames(carreaux_resumes_spdf@data)
  MesCSP <- MesCSP[-c(1,12)]
  
  for (cetteCSPLa in MesCSP){
    
    
    carreaux_resumes_2<- carreaux_resumes%>%
      gather("CSP", "Value", 2:11)%>%
      filter(CSP== cetteCSPLa)
    
    TotCSP <- sum(carreaux_resumes_2$Value, na.rm = TRUE)
    
    carreaux_resumes_spdf@data$TotCSP <- TotCSP/100
    
    mesBreaks_ratio <- breaksParCSP_ratio %>% 
      filter(cetacquereurLa == cetteCSPLa, Periode== cettePeriodeLa) %>% 
      pull() 
    mesBreaks_ratio <- unname(unlist(c(mesBreaks_ratio)))
    
    maPalette <- cspColors[[cetteCSPLa]]
    
    pdf(file=paste0("~/Projets/Chapitre7/Cartes_acquereurs/Cartes_acq_NombreByYears_Probabilite/",cetteCSPLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
    tilesLayer(osmTiles)
    
    
    smoothLayer(spdf =  carreaux_resumes_spdf, 
                spdfid ='Carreau_ID' ,
                df =  carreaux_resumes_spdf@data,
                dfid = 'Carreau_ID' ,
                var =cetteCSPLa, var2 = "TotCSP",
                span = 3000, beta = 2,
                col = maPalette,
                breaks =mesBreaks_ratio,
                mask = Mask.spdf,
                lwd=0.1,
                legend.title.txt = sprintf("Probabilite by year ", cetteCSPLa),
                legend.pos = "topright", legend.values.rnd = 5,add=TRUE)
    layoutLayer(title = paste("Nombre par annee", cetteCSPLa, cettePeriodeLa, sep = " - "))
    
    dev.off()
    
  
    stewartSomme <- mcStewart(knownpts = carreaux_resumes_spdf,
                                         unknownpts = carreaux_resumes_spdf,
                                         varname = cetteCSPLa,
                                         typefct = "exponential", span = 3000, beta = 2,
                                         mask = Mask.spdf, longlat = FALSE)
    
    stewartSomme_df<- as.data.frame(stewartSomme@data[,c("Carreau_ID","OUTPUT")])%>%
      rename(Potentiel_acquereur = OUTPUT)
    stewartSomme_df$cetacquereurLa<-cetteCSPLa
    stewartSomme_df$Periode<-cettePeriodeLa
    stewartSomme_df$Total<- TotCSP/100
    
    stewartProba_acquereur_df<-stewartSomme_df%>%
      select(Carreau_ID,Potentiel_acquereur,cetacquereurLa,Periode,Total)
    
    
    Probabilites_acquereur<-bind_rows(Probabilites_acquereur, stewartProba_acquereur_df)
  }
}


