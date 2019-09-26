


##############


Transac<- Transac_ACP%>% 
  filter(!is.na(Profil_acquereur) & !is.na(Profil_vendeur))%>%
  select(ID,annee.x,X.x,Y.x,fourchette_age_acq,fourchette_age_ve,Couple,Credit,Type_BIEN,Profil_acquereur,Profil_vendeur, MTCRED,REQ_PRIX)

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

Jointure_sf<-st_join(Cadrillage_1000, Transac_sf, join = st_contains, left=T)

#######################################

Transac_credit<-Jointure_sf%>% 
  group_by(Carreau_ID,Periode) %>% 
  filter(MTCRED>=1000,REQ_PRIX!="NA" )%>%
  summarise(Nombre_transacs=length(which(REQ_PRIX!="NA")),
            Prix_sum=sum(REQ_PRIX, na.rm=T),
            Credit_sum=sum(MTCRED,na.rm=T),
            LTV=(Credit_sum/Prix_sum)*100)%>%
            filter(LTV<=140)

breaks_ratio<-getBreaks(Transac_credit$LTV,nclass =10,method="quantile")

Periode <- unique(Transac_credit$Periode)
Periode <- sort(Periode[!is.na(Periode)])

dev.off()

cols=carto.pal(pal1 = 'grey.pal', n1 = 10)


for (cettePeriodeLa in Periode){
  
  test_credit<- Transac_credit%>% filter(Periode==cettePeriodeLa)


test_credit_spdf<-as(test_credit, "Spatial")
test_credit_df<- as.data.frame(test_credit_spdf@data)
proj4string(test_credit_spdf) <-CRS("+init=epsg:2154")
test_credit_spdf<- spTransform(test_credit_spdf, "+init=epsg:2154")

setwd("~/Projets/ACP_evolution_profil_marche_2/Cartes_LTV/Cartes_LTV_span3000")
pdf(file=paste0(~"Cartes_LTV_span3000",cettePeriodeLa ,".pdf"),width = 10, height = 5)

test_credit_df$dummy<-1
smoothLayer(spdf = test_credit_spdf, 
            spdfid="Carreau_ID",
            df = test_credit_df,
            dfid="Carreau_ID",
            var ="LTV", var2 ="dummy",
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

for (cettePeriodeLa in Periode){
  
  test_credit<- Transac_credit%>% filter(Periode==cettePeriodeLa)
  
  
  test_credit_spdf<-as(test_credit, "Spatial")
  test_credit_df<- as.data.frame(test_credit_spdf@data)
  proj4string(test_credit_spdf) <-CRS("+init=epsg:2154")
  test_credit_spdf<- spTransform(test_credit_spdf, "+init=epsg:2154")
  
  setwd("~/Projets/ACP_evolution_profil_marche_2/Cartes_LTV/Cartes_LTV_span7000")
  pdf(file=paste0(~"Cartes_LTV_span7000",cettePeriodeLa ,".pdf"),width = 10, height = 5)
  
  test_credit_df$dummy<-1
  smoothLayer(spdf = test_credit_spdf, 
              spdfid="Carreau_ID",
              df = test_credit_df,
              dfid="Carreau_ID",
              var ="LTV", var2 ="dummy",
              span = 7000, beta = 2,
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

#########carreaux 200


Jointure_sf<-st_join(Cadrillage_200, Transac_sf, join = st_contains, left=T)

#######################################

Transac_credit<-Jointure_sf%>% 
  group_by(Carreau_ID,Periode) %>% 
  filter(MTCRED>=1000,REQ_PRIX!="NA" )%>%
  summarise(Nombre_transacs=length(which(REQ_PRIX!="NA")),
            Prix_sum=sum(REQ_PRIX, na.rm=T),
            Credit_sum=sum(MTCRED,na.rm=T),
            LTV=(Credit_sum/Prix_sum)*100)%>%
  filter(LTV<=140)

breaks_ratio<-getBreaks(Transac_credit$LTV,nclass =10,method="quantile")

Periode <- unique(Transac_credit$Periode)
Periode <- sort(Periode[!is.na(Periode)])

dev.off()

cols=carto.pal(pal1 = 'grey.pal', n1 = 10)


for (cettePeriodeLa in Periode){
  
  test_credit<- Transac_credit%>% filter(Periode==cettePeriodeLa)
  
  
  test_credit_spdf<-as(test_credit, "Spatial")
  test_credit_df<- as.data.frame(test_credit_spdf@data)
  proj4string(test_credit_spdf) <-CRS("+init=epsg:2154")
  test_credit_spdf<- spTransform(test_credit_spdf, "+init=epsg:2154")
  
  setwd("~/Projets/ACP_evolution_profil_marche_2/Cartes_LTV/Cartes_LTV_span500")
  pdf(file=paste0(~"Cartes_LTV_span500",cettePeriodeLa ,".pdf"),width = 10, height = 5)
  
  test_credit_df$dummy<-1
  test_credit_spdf$dummy<-1
  smoothLayer(spdf = test_credit_spdf,
              df = test_credit_spdf@data,
              var ="LTV", var2 ="dummy",
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
  
}
