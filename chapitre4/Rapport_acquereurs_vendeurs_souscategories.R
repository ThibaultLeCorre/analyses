library(dplyr)
library(SpatialPosition)
library(cartography)
library(tidyr)
library(sf)

mytest<-data_redresse_avec_promo_2[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","cluster.y","X.x","Y.x")]
# table(data_redresse_code_promo_2$typeVar, data_redresse_code_promo_2$CSP_VE)
mytest$X.x<-as.numeric(mytest$X.x)
mytest$Y.x<-as.numeric(mytest$Y.x)
mytest$CSP_AC<-as.numeric(mytest$CSP_AC)
mytest$CSP_VE<-as.numeric(mytest$CSP_VE)
mytest$annee.x<-as.numeric(mytest$annee.x)

mytest$Acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Administration",
                           ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise",
                                  ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Marchands de biens",
                                         ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"SCI",
                                                       ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"HLM", "particulier_ou_na"))))))


mytest$Acquereurs<- ifelse(mytest$CSP_AC>=1 & mytest$CSP_AC<=69 &mytest$Acquereurs == "particulier_ou_na","Actifs",
                           ifelse(mytest$CSP_AC>=70 & mytest$CSP_AC<=90 & mytest$Acquereurs =="particulier_ou_na", "retraites_inactifs", 
                                  ifelse(is.na(mytest$CSP_AC) & mytest$Acquereurs=="particulier_ou_na",NA,mytest$Acquereurs)))

table(mytest$Acquereurs, useNA = "ifany")

mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"Administration",
                         ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"Entreprise",
                                ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"Marchands de biens",
                                       ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"SAFER",
                                              ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"SCI",
                                                     ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"HLM", "particulier_ou_na"))))))


mytest$Vendeurs<- ifelse(mytest$CSP_VE>=1 & mytest$CSP_VE<=69& mytest$Vendeurs == "particulier_ou_na","Actifs",
                         ifelse(mytest$CSP_VE>=70 & mytest$CSP_VE<=90 & mytest$Vendeurs =="particulier_ou_na", "retraites_inactifs", 
                                ifelse(is.na(mytest$CSP_VE) & mytest$Vendeurs=="particulier_ou_na",NA,mytest$Vendeurs)))

mytest$Vendeurs<- ifelse(mytest$cluster.y>=1 & !is.na(mytest$cluster.y),"Promoteurs", mytest$Vendeurs)
table(mytest$Vendeurs, useNA = "ifany")


mytest<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Vendeurs),!is.na(Acquereurs),Acquereurs!="SAFER", Vendeurs!="SAFER")


Transac<-mytest
str(Transac)

length(which(Transac$Y.x==NA))
###Periode
Transac$Periode<-ifelse(Transac$annee.x==1996|Transac$annee.x==1999|Transac$annee.x==2003, "Periode_96_2003",
                        ifelse(Transac$annee.x>=2004&Transac$annee.x<=2007, "Periode_04_2007", 
                               ifelse(Transac$annee.x>=2008&Transac$annee.x<=2012, "Periode_08_2012", NA)))
####
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
#########################################################
Jointure_sf<-st_join(Cadrillage_1000, Transac_sf, join = st_contains, left=T)
allBreaks_ratio <- list()

for (cettePeriodeLa in Periode){
  
  carreaux_filtred <- Jointure_sf %>%
    filter(Periode == cettePeriodeLa)
  
  carreaux_resumes_1<- carreaux_filtred %>%
    group_by(Carreau_ID,Acquereurs)  %>%
    summarise(n_acq=n()) %>%
    spread(Acquereurs,n_acq,fill = 0)%>%
    gather(key = "Type", value= "Value_acq", c(2:8))
  
  carreaux_resumes_2<- carreaux_filtred %>%
    group_by(Carreau_ID,Vendeurs)  %>%
    summarise(n_ve=n()) %>%
    spread(Vendeurs,n_ve,fill = 0)%>%
    gather(key = "Type", value= "Value_ve", c(2:9))
  

  carreaux_resumes_1<-as.data.frame(carreaux_resumes_1)
  carreaux_resumes<-full_join(carreaux_resumes_2,carreaux_resumes_1[,c(1:3)],by=c("Carreau_ID", "Type"))
  head(carreaux_resumes)
  
  carreaux_resumes$rapport<-carreaux_resumes$Value_ve/carreaux_resumes$Value_acq
 
  rm(carreaux_resumes_1,carreaux_resumes_2)
  
  Mescategories <- unique(carreaux_resumes$Type)
  #on garde actifs et retraités
  Mescategories <- Mescategories[c(1,7)]
  
  for (cetAcq_vendeurLa in Mescategories){
    
    carreaux_resumes_test<- carreaux_resumes%>% filter(Type==cetAcq_vendeurLa, rapport!="Inf",!is.na(rapport),rapport>0)
    
    carreaux_resumes_spdf <- as(carreaux_resumes_test, "Spatial")
    proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
    carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
    
    stewartBreaks_ratio <- quickStewart(spdf = carreaux_resumes_spdf, 
                                        df = carreaux_resumes_spdf@data, 
                                        var = "Value_ve",var2 ="Value_acq",
                                        span = 3000, 
                                        beta = 2, mask = Mask_spdf_1000,bypassctrl = TRUE)
    results_ratio <- stewartBreaks_ratio@data
    results_ratio$Periode <- cettePeriodeLa
    results_ratio$categories <- cetAcq_vendeurLa
    allBreaks_ratio[[as.character(cettePeriodeLa)]][[cetAcq_vendeurLa]] <- results_ratio
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
    
    carreaux_resumes_1<- carreaux_filtred %>%
      group_by(Carreau_ID,Acquereurs)  %>%
      summarise(n_acq=n()) %>%
      spread(Acquereurs,n_acq,fill = 0)%>%
      gather(key = "Type", value= "Value_acq", c(2:8))
    
    carreaux_resumes_2<- carreaux_filtred %>%
      group_by(Carreau_ID,Vendeurs)  %>%
      summarise(n_ve=n()) %>%
      spread(Vendeurs,n_ve,fill = 0)%>%
      gather(key = "Type", value= "Value_ve", c(2:9))
    
    
    carreaux_resumes_1<-as.data.frame(carreaux_resumes_1)
    carreaux_resumes<-full_join(carreaux_resumes_2,carreaux_resumes_1[,c(1:3)],by=c("Carreau_ID", "Type"))
    head(carreaux_resumes)
    
    carreaux_resumes$rapport<-carreaux_resumes$Value_ve/carreaux_resumes$Value_acq
    
    rm(carreaux_resumes_1,carreaux_resumes_2)
    
    Mescategories <- unique(carreaux_resumes$Type)
    #on garde actifs et retraités
    Mescategories <- Mescategories[c(1,7)]
  
  for (cetAcq_vendeurLa in Mescategories){
    
    carreaux_resumes_test<- carreaux_resumes%>% filter(Type==cetAcq_vendeurLa, rapport!="Inf",!is.na(rapport), rapport>0)
    carreaux_resumes_spdf <- as(carreaux_resumes_test, "Spatial")
    proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
    carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
    
    mesBreaks_ratio <- breaksParCategorie %>% filter(categories == cetAcq_vendeurLa) %>% pull() 
    mesBreaks_ratio <- unname(unlist(c(mesBreaks_ratio)))
    
    # mesBreaks_ratio <-getBreaks(carreaux_resumes_test$rapport, nclass = 10, method = "quantile")
    
    setwd("~/Sauvegarde_figures/Ratio_Categoires_acquereurs_vendeurs")
    pdf(file=paste0("~/Sauvegarde_figures/Ratio_Categoires_acquereurs_vendeurs/",cetAcq_vendeurLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
    
    plot(Mask_spdf_1000)
    smoothLayer(spdf = carreaux_resumes_spdf, 
                df = carreaux_resumes_spdf@data, 
                var = "Value_ve",var2 ="Value_acq", 
                span = 3000, beta = 2,
                col = cols,
                breaks =mesBreaks_ratio,
                mask = Mask_spdf_1000,
                lwd=0.1,
                legend.title.txt = sprintf("Ratio Potentiel vendeur-acquéreur des %s", cetAcq_vendeurLa),
                legend.pos = "topright", legend.values.rnd =2,add=T)
    
    plot(DepInsee_spdf,add=T)
    layoutLayer(title = paste(cetAcq_vendeurLa, cettePeriodeLa, sep = " - "))
    
    dev.off()
  }
  
}

DepInsee_spdf <- as(DepInsee, "Spatial")
proj4string(DepInsee_spdf) <-CRS("+init=epsg:2154")
DepInsee_spdf<- spTransform(DepInsee_spdf, "+init=epsg:2154")
######################################

poppot <- mcstewart(knownpts = carreaux_resumes_spdf,
                    unknownpts = carreaux_resumes_spdf,
                  varname = "Value_ve", 
                  typefct = "exponential", 
                  span = 3000, 
                  beta = 2, 
                  resolution = 50000, 
                  mask = nuts0.spdf)

# Compute the potentials of GDP on a regular grid (50km span)
# function = exponential, beta = 2, span = 75 km
gdppot <- stewart(knownpts = nuts3.spdf, 
                  varname = "gdppps2008", 
                  typefct = "exponential", 
                  span = 75000, 
                  beta = 2, 
                  resolution = 50000, 
                  mask = nuts0.spdf)

getBreaks(carreaux_resumes_test,v = carreaux_resumes_test$rapport)