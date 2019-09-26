library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)
library(RColorBrewer)

#Boucle_prix_immo_200
# orginals_data_BIEN<-orginals_data_BIEN%>%filter(cluster!=0, ID!=403773 & ID!=253617&ID!=570130&ID!=910347)
# data_redresse<- orginals_data_BIEN %>%
#   bind_rows(orginals_data_BIEN %>% filter(comurb2012=="1"))
data_redresse_avec_promo_2$MTCRED<-as.numeric(data_redresse_avec_promo_2$MTCRED)
str(data_redresse_avec_promo_2$REQ_PRIX)
Transac_acquereur<- data_redresse_avec_promo_2%>% 
  filter(`X.x`!= 0 & `Y.x`!=0, !is.na(REQ_PRIX), MTCRED>0)%>%
  select(ID,REQ_PRIX,MTCRED,annee.x,X.x, Y.x)

#LTV sur 392 548 transactions
test<- Transac_acquereur
test$X.x<-as.numeric(test$X.x)
test$Y.x<-as.numeric(test$Y.x)
test<-test%>%filter(`X.x`!= 0 & `Y.x`!=0)
#couleurs
# cols=carto.pal(pal1 = 'blue.pal',n1 = 5,pal2='red.pal', n2=6,  transparency = TRUE)
cols=carto.pal(pal1 = 'grey.pal', n1 = 10)
# cols=c("#23b28d","#5ba483","#769678","#89896d","#967b63","#9f6c57","#a75d4c","#ac4d40","#af3a32","#b12322")
# cols=c("#006837","#1a9850","#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61","#f46d43","#d73027","#a50026")
# opacity <- 80 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
# cols <- paste0(cols, opacity)
col_mask<-"#878787"
opacity_mask <-60
col_mask <- paste0(col_mask, opacity_mask)

#Shape

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

spVente$Periode<-ifelse(spVente$annee.x==1996|spVente$annee.x==1999|spVente$annee.x==2003, "Periode_96_2003",
                        ifelse(spVente$annee.x>=2004&spVente$annee.x<=2007, "Periode_04_2007", 
                               ifelse(spVente$annee.x>=2008&spVente$annee.x<=2012, "Periode_08_2012", NA)))
## essai repr?sentation

# osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)

#Importation carroyage Insee 200*200 et 1*1km
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

# Mask_simplified <-
Mask_st_1000 <- Mask_st_1000 %>%
  st_buffer(dist = 500) %>%
  st_buffer(dist = -1000) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 500) %>%
  st_buffer(500)

#pour carreau Insee
Cadrillage_1000<- carreauInsee1000%>%
  st_sf(geometry = .)

# pour carreaux Insee noramlement pas necessaire
# carreauInsee1000_3<- st_intersection(Mask.st, carreauInsee1000_2)
####

Cadrillage_1000$Carreau_ID<-1:length(Cadrillage_1000$id_c1000)
Cadrillage_1000_jointure<-st_join(Cadrillage_1000, spVente, join = st_contains, left=T)



#etape 1
#definir discretization


allBreaks_ratio <- list()
for (cettePeriodeLa in Periode){
  
  carreaux_resumes_test<- Cadrillage_1000_jointure%>% filter(Periode == cettePeriodeLa)
  
  carreaux_resumes_test <- carreaux_resumes_test %>%
    group_by(Carreau_ID) %>%
    summarise(Credit= sum(MTCRED),
              Prix = sum(REQ_PRIX),
              LTV = (sum(Credit)/sum(Prix))*100)
  carreaux_resumes <-carreaux_resumes_test %>% filter(!is.na(LTV) & LTV <150 & LTV>0)
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  stewartBreaks_ratio <- quickStewart(spdf = carreaux_resumes_spdf, 
                                      df = carreaux_resumes_spdf@data, 
                                      var = "Credit",var2 ="Prix",
                                      span = 3000, 
                                      beta = 2, mask = Mask_spdf_1000,bypassctrl = TRUE)
  results_ratio <- stewartBreaks_ratio@data
  results_ratio$Periode <- cettePeriodeLa
  allBreaks_ratio[[as.character(cettePeriodeLa)]] <- results_ratio
}


allBreaksDF_ratio <- purrr::map(allBreaks_ratio, ~bind_rows(.x)) %>% bind_rows()

breaks<- allBreaksDF_ratio %>%
  gather(key = id,value=bornes, c(2,3,4))%>%
  summarise(breaksMax = list(getBreaks(bornes, nclass = 10, method = "quantile")))
breaks_ratio<- unname(unlist(c(breaks)))

##############################################Etape 2
for (cettePeriodeLa in Periode){
  
  
  carreaux_resumes_test<- Cadrillage_1000_jointure%>% filter(Periode == cettePeriodeLa)
  
  carreaux_resumes_test <- carreaux_resumes_test %>%
    group_by(Carreau_ID) %>%
    summarise(Credit= sum(MTCRED),
              Prix = sum(REQ_PRIX),
              LTV = (sum(Credit)/sum(Prix))*100)
  carreaux_resumes <-carreaux_resumes_test %>% filter(!is.na(LTV) & LTV <150 & LTV>0)
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  
  setwd("~/Sauvegarde_figures/LTV_carreaux1000_periode")
  pdf(file=paste0("~/Sauvegarde_figures/LTV_carreaux1000_periode/", cettePeriodeLa,".pdf"),width = 10, height = 5)
  
  plot(Mask_spdf_1000)
  smoothLayer(spdf = carreaux_resumes_spdf, 
              df = carreaux_resumes_spdf@data, 
              var = "Credit",var2 ="Prix", 
              span = 3000, beta = 2,
              col = cols,
              breaks =breaks_ratio,
              mask = Mask_spdf_1000,
              lwd=0.1,
              legend.title.txt = sprintf("Potentiel LTV, quantile"),
              legend.pos = "topright", legend.values.rnd=2,add=T)
  
  plot(DepInsee_spdf,add=T)
  layoutLayer(title = cettePeriodeLa)
  
  dev.off()
}
