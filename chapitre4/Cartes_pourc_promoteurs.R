##############


Transac<- Transac_ACP%>% 
  filter(!is.na(Profil_acquereur) & !is.na(Profil_vendeur))%>%
  select(ID,annee.x,X.x,Y.x,fourchette_age_acq,fourchette_age_ve,Couple,Type_BIEN,Profil_acquereur,Profil_vendeur, MTCRED,REQ_PRIX)

###Periode
Transac$Periode<-ifelse(Transac$annee.x==1996|Transac$annee.x==1999|Transac$annee.x==2003, "Periode_96_2003",
                        ifelse(Transac$annee.x>=2004&Transac$annee.x<=2007, "Periode_04_2007", 
                               ifelse(Transac$annee.x>=2008&Transac$annee.x<=2012, "Periode_08_2012", NA)))
####
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

#######################################

Transac_PourPromo<-Jointure_sf%>% 
  group_by(Carreau_ID,Periode) %>% 
  filter(REQ_PRIX!="NA" )%>%
  summarise(Nombre_transacs=length(which(REQ_PRIX!="NA")),
            Promo_sum=length(which(Profil_vendeur=="100.Promoteur")),
            promoteur_pourc= (Promo_sum/Nombre_transacs)*100)



breaks_ratio<-getBreaks(Transac_PourPromo$promoteur_pourc,nclass =10,method="quantile")

Periode <- unique(Transac_PourPromo$Periode)
Periode <- sort(Periode[!is.na(Periode)])



cols=carto.pal(pal1 = 'grey.pal', n1 = 10)

allBreaks_ratio <- list()

#etape 1
for (cettePeriodeLa in Periode){
  
  Transac_PourPromo<- Jointure_sf%>% filter(Periode==cettePeriodeLa)
  

    test_PourPromo<-Transac_PourPromo%>% 
    group_by(Carreau_ID,Periode) %>% 
    filter(REQ_PRIX!="NA" )%>%
    summarise(Nombre_transacs=length(which(REQ_PRIX!="NA")),
              Promo_sum=length(which(Profil_vendeur=="100.Promoteur")),
              promoteur_pourc= (Promo_sum/Nombre_transacs)*100)
  
  
  
    test_PourPromo_spdf<-as(test_PourPromo, "Spatial")
  proj4string(test_PourPromo_spdf) <-CRS("+init=epsg:2154")
  test_PourPromo_spdf<- spTransform(test_PourPromo_spdf, "+init=epsg:2154")
  
  stewartBreaks <- quickStewart(spdf = test_PourPromo_spdf,spdfid = "Carreau_ID", 
                                df = test_PourPromo_spdf@data,dfid = "Carreau_ID",
                                var = "Promo_sum", var2 ="Nombre_transacs",
                                span = 3000, 
                                beta = 2, mask = Mask_spdf_1000,bypassctrl = TRUE)
  
  results_ratio <- stewartBreaks@data
  results_ratio$Periode <- cettePeriodeLa
  allBreaks_ratio[[as.character(cettePeriodeLa)]] <- results_ratio
}


allBreaksDF_ratio <- purrr::map(allBreaks_ratio, ~bind_rows(.x)) %>% 
  bind_rows()
# library(ggplot2)
# ggplot(allBreaksDF_ratio, aes(center, fill = as.factor(Periode))) + 
#   geom_density(alpha = 0.1)

breaks_ratio <- allBreaksDF_ratio %>%
  gather(key = id,value=bornes, c(2,3,4))%>%
  summarise(breaksMax = list(getBreaks(bornes, nclass = 10, method = "quantile")))
# breaks_ratio <-c(0.07142857, 0.1958655, 0.2983871, 0.3107418, 0.4153226, 0.4256182, 0.5322581, 0.5404946, 0.6517857, 0.7157968, 0.8863187)
# quantile(max, probs = seq(0, 1, 0.1),type=4)))
breaks_ratio<- unname(unlist(c(breaks_ratio)))


########
dev.off()
for (cettePeriodeLa in Periode){
  
  Transac_PourPromo<- Jointure_sf%>% filter(Periode==cettePeriodeLa)
  
  
  test_PourPromo<-Transac_PourPromo%>% 
    group_by(Carreau_ID,Periode) %>% 
    filter(REQ_PRIX!="NA" )%>%
    summarise(Nombre_transacs=length(which(REQ_PRIX!="NA")),
              Promo_sum=length(which(Profil_vendeur=="100.Promoteur")),
              promoteur_pourc= (Promo_sum/Nombre_transacs)*100)
  
  
  test_PourPromo_spdf<-as(test_PourPromo, "Spatial")
  proj4string(test_PourPromo_spdf) <-CRS("+init=epsg:2154")
  test_PourPromo_spdf<- spTransform(test_PourPromo_spdf, "+init=epsg:2154")
  
  # test__df<- as.data.frame(test__spdf@data)
  
  
  setwd("~/Projets/ACP_evolution_profil_marche_2/Cartes_pourc_promo/Cartes_promo_span3000")
  pdf(file=paste0(~"Cartes_promo_span3000",cettePeriodeLa ,".pdf"),width = 10, height = 5)
  
  
  smoothLayer(spdf = test_PourPromo_spdf, 
              df = test_PourPromo_spdf@data,
              var ="Promo_sum", var2 ="Nombre_transacs",
              span = 3000, beta = 2,
              breaks=breaks_ratio,
              col = cols,
              mask = Mask_spdf_1000,
              border = "grey",
              legend.title.txt = cettePeriodeLa,
              legend.values.rnd = 2,
              legend.pos = "topright",
              typefct = "exponential")
  
  plot(st_geometry(lim_IDF.st),add=T)
  
  dev.off()
}
