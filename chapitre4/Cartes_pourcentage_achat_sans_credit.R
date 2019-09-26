


##############


Transac<- Transac_ACP%>% 
  filter(!is.na(Profil_acquereur) & !is.na(Profil_vendeur))%>%
  select(ID,annee.x,X.x,Y.x,fourchette_age_acq,fourchette_age_ve,Couple,Credit,Type_BIEN,Profil_acquereur,Profil_vendeur, MTCRED,REQ_PRIX)
table(Transac_ACP$Profil_acquereur)
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

Transac_Pourcredit<-Jointure_sf%>% 
  group_by(Carreau_ID,Periode) %>% 
  filter(REQ_PRIX!="NA" )%>%
  summarise(Nombre_transacs=length(which(REQ_PRIX!="NA")),
            SansCredit_sum=length(which(Credit=="pas_de_credit")),
            Credit_pourc= (SansCredit_sum/Nombre_transacs)*100)

  

breaks_ratio<-getBreaks(Transac_Pourcredit$Credit_pourc,nclass =10,method="quantile")

Periode <- unique(Transac_Pourcredit$Periode)
Periode <- sort(Periode[!is.na(Periode)])



cols=carto.pal(pal1 = 'grey.pal', n1 = 10)

allBreaks_ratio <- list()

#etape 1
for (cettePeriodeLa in Periode){
  
  test_Pourcredit<- Jointure_sf%>% filter(Periode==cettePeriodeLa)
  
  test_Pourcredit<-test_Pourcredit%>% 
    group_by(Carreau_ID) %>% 
    filter(REQ_PRIX!="NA" )%>%
    summarise(Nombre_transacs=length(which(REQ_PRIX!="NA")),
              SansCredit_sum=length(which(Credit=="pas_de_credit")),
              Credit_pourc= (SansCredit_sum/Nombre_transacs)*100)%>%
    filter(Credit_pourc>1)
  
  
  test_credit_spdf<-as(test_Pourcredit, "Spatial")
  proj4string(test_credit_spdf) <-CRS("+init=epsg:2154")
  test_credit_spdf<- spTransform(test_credit_spdf, "+init=epsg:2154")
  
  stewartBreaks <- quickStewart(spdf = test_credit_spdf,spdfid = "Carreau_ID", 
                                df = test_credit_spdf@data,dfid = "Carreau_ID",
                                var = "SansCredit_sum", var2 ="Nombre_transacs",
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
  
  test_Pourcredit<- Jointure_sf%>% filter(Periode==cettePeriodeLa)
  
  test_Pourcredit<-test_Pourcredit%>% 
    group_by(Carreau_ID) %>% 
    filter(REQ_PRIX!="NA" )%>%
    summarise(Nombre_transacs=length(which(REQ_PRIX!="NA")),
              SansCredit_sum=length(which(Credit=="pas_de_credit")),
              Credit_pourc= (SansCredit_sum/Nombre_transacs)*100)%>%
    filter(Credit_pourc>1)
  
  test_credit_spdf<-as(test_Pourcredit, "Spatial")
  proj4string(test_credit_spdf) <-CRS("+init=epsg:2154")
  test_credit_spdf<- spTransform(test_credit_spdf, "+init=epsg:2154")
  
  # test_credit_df<- as.data.frame(test_credit_spdf@data)
  
  
   setwd("~/Projets/ACP_evolution_profil_marche_2/Cartes_Pourc_Credit/Cartes_PourCredit_span3000")
  pdf(file=paste0(~"Cartes_PourCredit_span3000",cettePeriodeLa ,".pdf"),width = 10, height = 5)
  
  
  smoothLayer(spdf = test_credit_spdf, 
              df = test_credit_spdf@data,
              var ="SansCredit_sum", var2 ="Nombre_transacs",
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
# 
# for (cettePeriodeLa in Periode){
#   
#   test_Pourcredit<- Transac_Pourcredit%>% filter(Periode==cettePeriodeLa)
#   
#   
#   test_credit_spdf<-as(test_Pourcredit, "Spatial")
#   test_credit_df<- as.data.frame(test_credit_spdf@data)
#   proj4string(test_credit_spdf) <-CRS("+init=epsg:2154")
#   test_credit_spdf<- spTransform(test_credit_spdf, "+init=epsg:2154")
#   
#   setwd("~/Projets/ACP_evolution_profil_marche_2/Cartes_Pourc_Credit/Cartes_PourCredit_span7000")
#   pdf(file=paste0(~"Cartes_PourCredit_span7000",cettePeriodeLa ,".pdf"),width = 10, height = 5)
#   
#   test_credit_df$dummy<-1
#   smoothLayer(spdf = test_credit_spdf, 
#               spdfid="Carreau_ID",
#               df = test_credit_df,
#               dfid="Carreau_ID",
#               var ="Credit_pourc", var2 ="dummy",
#               span = 7000, beta = 2,
#               breaks=breaks_ratio,
#               col = cols,
#               mask = Mask_spdf_1000,
#               border = "grey",
#               legend.title.txt = cettePeriodeLa,
#               legend.values.rnd = 2,
#               legend.pos = "topright",
#               typefct = "exponential")
#   
#   plot(st_geometry(lim_IDF.st),add=T)
#   
#   dev.off()
# }

#########carreaux 200


Jointure_sf<-st_join(Cadrillage_200, Transac_sf, join = st_contains, left=T)


for (cettePeriodeLa in Periode){
  
  test_Pourcredit<- Jointure_sf%>% filter(Periode==cettePeriodeLa)
  
  test_Pourcredit<-test_Pourcredit%>% 
    group_by(Carreau_ID) %>% 
    filter(REQ_PRIX!="NA" )%>%
    summarise(Nombre_transacs=length(which(REQ_PRIX!="NA")),
              SansCredit_sum=length(which(Credit=="pas_de_credit")),
              Credit_pourc= (SansCredit_sum/Nombre_transacs)*100)%>%
    filter(Credit_pourc>1)
  
  
  test_credit_spdf<-as(test_Pourcredit, "Spatial")
  proj4string(test_credit_spdf) <-CRS("+init=epsg:2154")
  test_credit_spdf<- spTransform(test_credit_spdf, "+init=epsg:2154")
  
  stewartBreaks <- quickStewart(spdf = test_credit_spdf,spdfid = "Carreau_ID", 
                                df = test_credit_spdf@data,dfid = "Carreau_ID",
                                var = "SansCredit_sum", var2 ="Nombre_transacs",
                                span = 500, 
                                beta = 2, mask = Mask_spdf_200,bypassctrl = TRUE)
  
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

#######################################



Periode <- unique(Transac_credit$Periode)
Periode <- sort(Periode[!is.na(Periode)])

dev.off()

cols=carto.pal(pal1 = 'grey.pal', n1 = 10)


for (cettePeriodeLa in Periode){
  
  
  test_Pourcredit<- Jointure_sf%>% filter(Periode==cettePeriodeLa)
  
  test_Pourcredit<-test_Pourcredit%>% 
    group_by(Carreau_ID) %>% 
    filter(REQ_PRIX!="NA" )%>%
    summarise(Nombre_transacs=length(which(REQ_PRIX!="NA")),
              SansCredit_sum=length(which(Credit=="pas_de_credit")),
              Credit_pourc= (SansCredit_sum/Nombre_transacs)*100)
  # %>%
  #   filter(Credit_pourc>1)
  
  test_credit_spdf<-as(test_Pourcredit, "Spatial")
  proj4string(test_credit_spdf) <-CRS("+init=epsg:2154")
  test_credit_spdf<- spTransform(test_credit_spdf, "+init=epsg:2154")
  
  
  
 setwd("~/Projets/ACP_evolution_profil_marche_2/Cartes_Pourc_Credit/Cartes_PourCredit_span500")  
 pdf(file=paste0(~"Cartes_PourCredit_span500",cettePeriodeLa ,".pdf"),width = 10, height = 5)
  
  
  smoothLayer(spdf = test_credit_spdf, 
              df = test_credit_spdf@data,
              var ="SansCredit_sum", var2 ="Nombre_transacs",
              span = 500, beta = 2,
              breaks=breaks_ratio,
              col = cols,
              mask = Mask_spdf_200,
              border = "grey",
              legend.title.txt = cettePeriodeLa,
              legend.values.rnd = 2,
              legend.pos = "topright",
              typefct = "exponential")
  
  
  plot(st_geometry(lim_IDF.st),add=T)
  
  dev.off()
}
