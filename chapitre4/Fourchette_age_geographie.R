library(dplyr)
library(SpatialPosition)
library(cartography)
library(tidyr)
mytest<-data_redresse[,c("ID","annee.x","PRESCREDIT","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x","ANNAIS_AC","ANNAIS_VE")]

mytest<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(CSP_AC) )


####Fourchette age
mytest$Age_acq = mytest$annee.x-mytest$ANNAIS_AC
mytest$fourchette_age_acq<-mytest$Age_acq 
mytest<-mytest%>%filter(fourchette_age_acq>=18)

mytest$fourchette_age_acq<- ifelse(mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<25, "[18,25[",
                                   ifelse(mytest$fourchette_age_acq>=25 & mytest$fourchette_age_acq<30, "[25,30[",
                                          ifelse(mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<35, "[30,35[",
                                                 ifelse(mytest$fourchette_age_acq>=35 & mytest$fourchette_age_acq<40, "[35,40[",
                                                        ifelse(mytest$fourchette_age_acq>=40 & mytest$fourchette_age_acq<45, "[40,45[",
                                                               ifelse(mytest$fourchette_age_acq>=45 & mytest$fourchette_age_acq<50, "[45,50[",
                                                                      ifelse(mytest$fourchette_age_acq>=50 & mytest$fourchette_age_acq<55, "[50,55[",
                                                                             ifelse(mytest$fourchette_age_acq>=55 & mytest$fourchette_age_acq<60, "[55,60[",
                                                                                    ifelse(mytest$fourchette_age_acq>=60 , "[60+",mytest$Age_acq)))))))))


Transac<-mytest
###Periode
Transac$Periode<-ifelse(Transac$annee.x==1996|Transac$annee.x==1999|Transac$annee.x==2003, "Periode_96_2003",
                        ifelse(Transac$annee.x>=2004&Transac$annee.x<=2007, "Periode_04_2007", 
                               ifelse(Transac$annee.x>=2008&Transac$annee.x<=2012, "Periode_08_2012", NA)))




Periode <- unique(Transac$Periode)
Periode <- sort(Periode[!is.na(Periode)])


Transac_sf<-st_as_sf(Transac,
                     
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

st_crs(Mask_st_1000)

st_crs(Cadrillage_1000)

Cadrillage_1000$Carreau_ID<-1:length(Cadrillage_1000$id_c1000)
#########################################################
Jointure_sf<-st_join(Cadrillage_1000, Transac_sf, join = st_contains, left=T)
allBreaks_ratio <- list()
table(Jointure_sf$fourchette_age_acq)
for (cettePeriodeLa in Periode){
  
  carreaux_filtred <- Jointure_sf %>%
    filter(Periode == cettePeriodeLa)

  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs= n(),
      Les_18_25_ans=length(which(fourchette_age_acq =="[18,25["))/n(),
              Les_25_30_ans=length(which(fourchette_age_acq =="[25,30[")),
              Les_30_35_ans=length(which(fourchette_age_acq =="[30,35[")),
              Les_35_40_ans=length(which(fourchette_age_acq =="[35,40[")),
              Les_40_45_ans=length(which(fourchette_age_acq =="[40,45[")),
              Les_45_50_ans=length(which(fourchette_age_acq =="[45,50[")),
              Les_50_55_ans=length(which(fourchette_age_acq =="[50,55[")),
              Les_55_60_ans=length(which(fourchette_age_acq =="[55,60[")),
              Les_plus_60_ans=length(which(fourchette_age_acq =="[60+")))
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  Mask_spdf_1000<-as(Mask_st_1000, "Spatial")
  proj4string(Mask_spdf_1000) <-CRS("+init=epsg:2154")
  Mask_spdf_1000<- spTransform(Mask_spdf_1000, "+init=epsg:2154")
  
  Mescategories <- colnames(carreaux_resumes_spdf@data)
  Mescategories <- Mescategories[-c(1,2)]
  
  for (cetFourchette_ageLa in Mescategories){
    
    stewartBreaks_ratio <- quickStewart(spdf = carreaux_resumes_spdf, 
                                        df = carreaux_resumes_spdf@data, 
                                        var = cetFourchette_ageLa,var2 = "Nombre_transacs",
                                        span = 3000, 
                                        beta = 2, mask = Mask_spdf_1000)
    results_ratio <- stewartBreaks_ratio@data
    results_ratio$Periode <- cettePeriodeLa
    results_ratio$categories <- cetFourchette_ageLa
    allBreaks_ratio[[as.character(cettePeriodeLa)]][[cetFourchette_ageLa]] <- results_ratio
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
              Les_18_25_ans=length(which(fourchette_age_acq =="[18,25["))/n(),
              Les_25_30_ans=length(which(fourchette_age_acq =="[25,30[")),
              Les_30_35_ans=length(which(fourchette_age_acq =="[30,35[")),
              Les_35_40_ans=length(which(fourchette_age_acq =="[35,40[")),
              Les_40_45_ans=length(which(fourchette_age_acq =="[40,45[")),
              Les_45_50_ans=length(which(fourchette_age_acq =="[45,50[")),
              Les_50_55_ans=length(which(fourchette_age_acq =="[50,55[")),
              Les_55_60_ans=length(which(fourchette_age_acq =="[55,60[")),
              Les_plus_60_ans=length(which(fourchette_age_acq =="[60+")))
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")

  
  Mescategories <- colnames(carreaux_resumes_spdf@data)
  Mescategories <- Mescategories[-c(1,2)]
  
  for (cetFourchette_ageLa in Mescategories){
    
    mesBreaks_ratio <- breaksParCategorie %>% filter(categories == cetFourchette_ageLa) %>% pull() 
    mesBreaks_ratio <- unname(unlist(c(mesBreaks_ratio)))
    
    setwd("~/Sauvegarde_figures/Difference_type_foruchette_age")
    pdf(file=paste0("~/Sauvegarde_figures/Difference_type_foruchette_age/",cetFourchette_ageLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
    
    plot(Mask_spdf_1000)
    smoothLayer(spdf= carreaux_resumes_spdf, df= carreaux_resumes_spdf@data,
                var =cetFourchette_ageLa, var2 = "Nombre_transacs",
                span = 3000, beta = 2,
                col = cols,
                breaks =mesBreaks_ratio,
                mask = Mask_spdf_1000,
                lwd=0.1,
                legend.title.txt = sprintf("Potentiel de %s", cetFourchette_ageLa),
                legend.pos = "topright", legend.values.rnd=2,add=T)
    
    plot(DepInsee_spdf,add=T)
    layoutLayer(title = paste(cetFourchette_ageLa, cettePeriodeLa, sep = " - "))
    
    dev.off()
  }
  
}

DepInsee

DepInsee_spdf <- as(DepInsee, "Spatial")
proj4string(DepInsee_spdf) <-CRS("+init=epsg:2154")
DepInsee_spdf<- spTransform(DepInsee_spdf, "+init=epsg:2154")
