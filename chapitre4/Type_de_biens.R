library(dplyr)
library(SpatialPosition)
library(cartography)
library(tidyr)
library(sf)
mytest<-data_redresse[,c("ID","annee.x","PRESCREDIT","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x","ANNAIS_AC","ANNAIS_VE", "NBRPIECE","REQTYPBIEN","BIDEPT", "REQ_ANC")]

mytest$Type_log<- ifelse( mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>7 | mytest$REQTYPBIEN=="AP" & mytest$NBRPIECE>6  ,"Bien_exceptionnel",
                                 ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=0 & mytest$NBRPIECE<=1, "studio_et_1_pièce","Logement banal"))
table(mytest$Type_log)
mytest$Periode<-ifelse(mytest$annee.x==1996|mytest$annee.x==1999|mytest$annee.x==2003, "Periode_96_2003",
                       ifelse(mytest$annee.x>=2004&mytest$annee.x<=2007, "Periode_04_2007", 
                              ifelse(mytest$annee.x>=2008&mytest$annee.x<=2012, "Periode_08_2012", NA)))




Periode <- unique(mytest$Periode)
Periode <- sort(Periode[!is.na(Periode)])


Transac_sf<-st_as_sf(mytest,
                     
                     coords = c("X.x", "Y.x"),
                     
                     agr = "constant",
                     
                     crs = 27572,
                     
                     stringsAsFactors = FALSE)


Transac_sf <-  st_transform(Transac_sf, crs = 2154)
coords<- st_coordinates(Transac_sf)
Transac_sf$XLamb93<-coords[,1]   
Transac_sf$Ylamb93<-coords[,2]
#######################################################

setwd("~/Shapes/datidf")
list.files()
carreauInsee1000 <- st_read("car1000m_idf_reparGeom.shp",
                            stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee1000 <-  st_transform(carreauInsee1000, crs = 2154)
#Mask
Mask_st_1000 <- st_union(carreauInsee1000)
Mask_st_1000<-st_sf(id = 1, geometry =Mask_st_1000 )
par(mfrow = c(1,2))

Mask_st_1000 <- Mask_st_1000 %>%
  st_buffer(dist = 500) %>%
  st_buffer(dist = -1000) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 500) %>%
  st_buffer(500)

Cadrillage_1000<- carreauInsee1000%>%
  st_sf(geometry = .)
Mask_spdf_1000<-as(Mask_st_1000, "Spatial")
proj4string(Mask_spdf_1000) <-CRS("+init=epsg:2154")
Mask_spdf_1000<- spTransform(Mask_spdf_1000, "+init=epsg:2154")
st_crs(Mask_st_1000)

st_crs(Cadrillage_1000)

Cadrillage_1000$Carreau_ID<-1:length(Cadrillage_1000$id_c1000)
#########################################################
Jointure_sf<-st_join(Cadrillage_1000, Transac_sf, join = st_contains, left=T)
allBreaks_ratio <- list()

for (cettePeriodeLa in Periode){
  
  carreaux_filtred <- Jointure_sf %>%
    filter(Periode == cettePeriodeLa)
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs= n(),
              Bien_exceptionnel=length(which(Type_log=="Bien_exceptionnel")),
              studio_et_1_pièce=length(which(Type_log =="studio_et_1_pièce")),
              Logement_banal=length(which(Type_log!="Bien_exceptionnel"&Type_log !="studio_et_1_pièce" )))
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")

  
  Mescategories <- colnames(carreaux_resumes_spdf@data)
  Mescategories <- Mescategories[-c(1,2)]
  
  for (ceLogementLa in Mescategories){
    
    stewartBreaks_ratio <- quickStewart(spdf = carreaux_resumes_spdf, 
                                        df = carreaux_resumes_spdf@data, 
                                        var = ceLogementLa,var2 = "Nombre_transacs",
                                        span = 3000, 
                                        beta = 2, mask = Mask_spdf_1000)
    results_ratio <- stewartBreaks_ratio@data
    results_ratio$Periode <- cettePeriodeLa
    results_ratio$categories <- ceLogementLa
    allBreaks_ratio[[as.character(cettePeriodeLa)]][[ceLogementLa]] <- results_ratio
  }
  
}

allBreaksDF_ratio <- purrr::map(allBreaks_ratio, ~bind_rows(.x)) %>% bind_rows()

breaksParCategorie <- allBreaksDF_ratio %>%
  group_by(categories) %>%
  gather(key = id,value=bornes, c(2,3,4))%>%
  summarise(breaksMax = list(getBreaks(bornes, nclass = 10, method = "quantile")))
breaks_ratio<- unname(unlist(c(breaksParCategorie)))

cols=carto.pal(pal1 = 'grey.pal', n1 = 10)  


#etape 2
for (cettePeriodeLa in Periode){
  
  carreaux_filtred <- Jointure_sf %>%
    filter(Periode == cettePeriodeLa)
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs= n(),
              Bien_exceptionnel=length(which(Type_log=="Bien_exceptionnel")),
              studio_et_1_pièce=length(which(Type_log =="studio_et_1_pièce")),
              Logement_banal=length(which(Type_log!="Bien_exceptionnel"&Type_log !="studio_et_1_pièce" )))
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  Mescategories <- colnames(carreaux_resumes_spdf@data)
  Mescategories <- Mescategories[-c(1,2)]
  
  for (ceLogementLa in Mescategories){
    
    mesBreaks_ratio <- breaksParCategorie %>% filter(categories == ceLogementLa) %>% pull() 
    mesBreaks_ratio <- unname(unlist(c(mesBreaks_ratio)))
    
    setwd("~/Sauvegarde_figures/Types_logements_carto_1000_periode")
    pdf(file=paste0("~/Sauvegarde_figures/Types_logements_carto_1000_periode/",ceLogementLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
    
    plot(Mask_spdf_1000)
    smoothLayer(spdf= carreaux_resumes_spdf, df= carreaux_resumes_spdf@data,
                var =ceLogementLa, var2 = "Nombre_transacs",
                span = 3000, beta = 2,
                col = cols,
                breaks =mesBreaks_ratio,
                mask = Mask_spdf_1000,
                lwd=0.1,
                legend.title.txt = sprintf("Potentiel de %s", ceLogementLa),
                legend.pos = "topright", legend.values.rnd=2,add=T)
    
    plot(DepInsee_spdf,add=T)
    layoutLayer(title = paste(ceLogementLa, cettePeriodeLa, sep = " - "))
    
    dev.off()
  }
  
}
#############Maison et appartements
allBreaks_ratio <- list()

for (cettePeriodeLa in Periode){
  
  carreaux_filtred <- Jointure_sf %>%
    filter(Periode == cettePeriodeLa)
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs= n(),
              Maison=length(which(REQTYPBIEN=="MA")),
              Appartement=length(which(REQTYPBIEN =="AP")))
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  
  Mescategories <- colnames(carreaux_resumes_spdf@data)
  Mescategories <- Mescategories[-c(1,2)]
  
  for (ceLogementLa in Mescategories){
    
    stewartBreaks_ratio <- quickStewart(spdf = carreaux_resumes_spdf, 
                                        df = carreaux_resumes_spdf@data, 
                                        var = ceLogementLa,var2 = "Nombre_transacs",
                                        span = 3000, 
                                        beta = 2, mask = Mask_spdf_1000)
    results_ratio <- stewartBreaks_ratio@data
    results_ratio$Periode <- cettePeriodeLa
    results_ratio$categories <- ceLogementLa
    allBreaks_ratio[[as.character(cettePeriodeLa)]][[ceLogementLa]] <- results_ratio
  }
  
}

allBreaksDF_ratio <- purrr::map(allBreaks_ratio, ~bind_rows(.x)) %>% bind_rows()

breaksParCategorie <- allBreaksDF_ratio %>%
  group_by(categories) %>%
  gather(key = id,value=bornes, c(2,3,4))%>%
  summarise(breaksMax = list(getBreaks(bornes, nclass = 10, method = "quantile")))
breaks_ratio<- unname(unlist(c(breaksParCategorie)))

cols=carto.pal(pal1 = 'grey.pal', n1 = 10)  


#etape 2
for (cettePeriodeLa in Periode){
  
  carreaux_filtred <- Jointure_sf %>%
    filter(Periode == cettePeriodeLa)
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs= n(),
              Maison=length(which(REQTYPBIEN=="MA")),
              Appartement=length(which(REQTYPBIEN =="AP")))
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  Mescategories <- colnames(carreaux_resumes_spdf@data)
  Mescategories <- Mescategories[-c(1,2)]
  
  for (ceLogementLa in Mescategories){
    
    mesBreaks_ratio <- breaksParCategorie %>% filter(categories == ceLogementLa) %>% pull() 
    mesBreaks_ratio <- unname(unlist(c(mesBreaks_ratio)))
    
    setwd("~/Sauvegarde_figures/Maisons_appartements_geographie_1000_periode")
    pdf(file=paste0("~/Sauvegarde_figures/Maisons_appartements_geographie_1000_periode/",ceLogementLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
    
    plot(Mask_spdf_1000)
    smoothLayer(spdf= carreaux_resumes_spdf, df= carreaux_resumes_spdf@data,
                var =ceLogementLa, var2 = "Nombre_transacs",
                span = 3000, beta = 2,
                col = cols,
                breaks =mesBreaks_ratio,
                mask = Mask_spdf_1000,
                lwd=0.1,
                legend.title.txt = sprintf("Potentiel de %s", ceLogementLa),
                legend.pos = "topright", legend.values.rnd=2,add=T)
    
    plot(DepInsee_spdf,add=T)
    layoutLayer(title = paste(ceLogementLa, cettePeriodeLa, sep = " - "))
    
    dev.off()
  }
  
}
