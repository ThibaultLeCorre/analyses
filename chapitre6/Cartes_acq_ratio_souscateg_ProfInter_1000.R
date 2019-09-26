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
mytest<-data_redresse1570533transacs[,c("ID","annee.x","CSP_AC","X.x","Y.x")]

mytest$acquereurs<- ifelse(mytest$CSP_AC == 42 , "instituteur_et_assimile",
                           ifelse(mytest$CSP_AC == 43 , "intermed_sante_travail_social",
                                  ifelse(mytest$CSP_AC == 44 , "Clerge_religieux",
                                         ifelse(mytest$CSP_AC == 45 , "intermed.fonction_publique",
                                                ifelse(mytest$CSP_AC == 46 , "inter_adm_et_commerciales_entreprises",
                                                       ifelse(mytest$CSP_AC == 47 , "technicien",
                                                              ifelse(mytest$CSP_AC == 48 , "Contremaitre_agent_de_maitrise",NA)))))))
###Periode
mytest$Periode<-ifelse(mytest$annee.x==1996|mytest$annee.x==1999|mytest$annee.x==2003, "Periode_96_2003",
                       ifelse(mytest$annee.x>=2004&mytest$annee.x<=2007, "Periode_04_2007", 
                              ifelse(mytest$annee.x>=2008&mytest$annee.x<=2012, "Periode_08_2012", NA)))

unique(mytest$acquereurs)
mytest <- mytest %>%
  filter(!is.na(acquereurs))

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

allBreaks_ratio <- list()

for (cettePeriodeLa in Periodes){
  
  carreaux_filtred <- carreauInsee1000_jointure %>%
    filter(Periode == cettePeriodeLa)
  
  # carreaux_filtred <-  carreaux_filtred[sample(1: nrow(carreaux_filtred), 500,replace=T), ]
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID, acquereurs) %>%
    summarise(Nombre=length(acquereurs))%>%
    spread(acquereurs, Nombre, fill=0)
  
  carreaux_resumes$Total<- rowSums(as.data.frame(carreaux_resumes[,2:8,drop=TRUE]))
  
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  MesCSP <- colnames(carreaux_resumes_spdf@data)
  MesCSP <- MesCSP[-c(1,9)]
  
  for (cetteCSPLa in MesCSP){
    
    stewartBreaks_ratio <- quickStewart(spdf = carreaux_resumes_spdf, 
                                        df = carreaux_resumes_spdf@data, 
                                        var = cetteCSPLa, var2 = "Total",
                                        span = 3000, 
                                        beta = 2, mask = Mask.spdf)
    results_ratio <- stewartBreaks_ratio@data
    results_ratio$annee <- cettePeriodeLa
    results_ratio$CSP <- cetteCSPLa
    allBreaks_ratio[[as.character(cettePeriodeLa)]][[cetteCSPLa]] <- results_ratio
  }
  
}

allBreaksDF_ratio <- purrr::map(allBreaks_ratio, ~bind_rows(.x)) %>% bind_rows()
ggplot(allBreaksDF_ratio, aes(center, fill = as.factor(annee))) + geom_density(alpha = 0.1) + facet_wrap(~CSP, scales = "free")

breaksParCSP_ratio <- allBreaksDF_ratio %>%
  summarise(breaksMax = list(quantile(max, probs = seq(0, 1, 0.2))))
mesBreaks_ratio <- unname(unlist(c(0, breaksParCSP_ratio)))

opacity <- 85
maPalette <- paste0(carto.pal(pal1 = 'green.pal', n1=7), opacity)
#etape 2

dev.off()
for (cettePeriodeLa in Periodes){
  
  carreaux_filtred <- carreauInsee1000_jointure %>%
    filter(Periode == cettePeriodeLa)
  
  # carreaux_filtred <-  carreaux_filtred[sample(1: nrow(carreaux_filtred), 500,replace=T), ]
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID, acquereurs) %>%
    summarise(Nombre=length(acquereurs))%>%
    spread(acquereurs, Nombre, fill=0)
  
  carreaux_resumes$Total<- rowSums(as.data.frame(carreaux_resumes[,2:8,drop=TRUE]))
  
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  MesCSP <- colnames(carreaux_resumes_spdf@data)
  MesCSP <- MesCSP[-c(1,9)]
  
  for (cetteCSPLa in MesCSP){
    
    
    
    pdf(file=paste0("~/Projets/Chapitre7/Cartes_acquereurs/Cartes_acq_ratio_souscateg_ProfInter/",cetteCSPLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
    tilesLayer(osmTiles)
    
    
    smoothLayer(spdf =  carreaux_resumes_spdf, 
                spdfid ='Carreau_ID' ,
                df =  carreaux_resumes_spdf@data,
                dfid = 'Carreau_ID' ,
                var =cetteCSPLa, var2 ='Total',
                span = 3000, beta = 2,
                col = maPalette,
                breaks =mesBreaks_ratio,
                mask = Mask.spdf,
                lwd=0.1,
                legend.title.txt = sprintf("Potentiel de %s", cetteCSPLa),
                legend.pos = "topright", legend.values.rnd = 2,add=TRUE)
    layoutLayer(title = paste("Pourcentage", cetteCSPLa, cettePeriodeLa, sep = " - "))
    
    dev.off()
  }
  
}

