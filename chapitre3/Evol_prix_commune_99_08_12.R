#Prix commune
library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)
Prix_commune<-orginals_data_BIEN%>%filter(REQTYPBIEN=="AP")
Prix_commune<- Prix_commune%>% filter(`X.x`!= 0 & `Y.x`!=  0,REQ_PM2>0)

#regler les modalites cartographqiues

med<-median(App_BIEN$REQ_PM2, na.rm=T)
cols=carto.pal(pal1 = 'green.pal', n1 = 5 ,pal2='red.pal', n2=5)
opacity <- 80 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)

#
setwd("~/Shapes/IRIS")
list.files()

IrisInsee<- st_read("IRIS_LAMB93_IDF_2008.shp",
                    stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
IrisInsee  <-  st_transform(IrisInsee , crs = 2154)
# "simplifier geometry"
IrisInsee  <- st_buffer(IrisInsee, 0)
plot(st_geometry(IrisInsee), col = "grey90", lwd = 0.2)

#obtenir shape commune par aggregation 
CommunesInsee<- IrisInsee%>% 
  group_by(DepCom)%>%
  summarize()
plot(st_geometry(CommunesInsee), col = "grey90", lwd = 0.2)


#departement
setwd("~/Shapes/shpIDF_dep_lamb93")
list.files()
DepInsee<- st_read("ile-de-france.shp",
                   stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
DepInsee <-  st_transform(DepInsee, crs = 2154)
plot(st_geometry(DepInsee), col = , lwd = 0.2, add=T)

##
spVente <- st_as_sf(Prix_commune,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)
#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]   
spVente$Ylamb93<-coords[,2]

spVente <-spVente%>% 
  filter(annee.x== 1999 |annee.x== 2008 | annee.x== 2012) 

Comjointure<-st_join(CommunesInsee, spVente, join = st_contains, left=T)

Comjointure_2<-Comjointure%>% group_by(DepCom, annee.x) %>% summarise(Nombre_transacs=length(which(REQ_PM2!="NA")),
                                                                                 Prix_mcarre_moyen=mean(REQ_PM2,na.rm=T))

Comjointure_2<-Comjointure_2%>%filter(Nombre_transacs>=5)
discret<-quantile(x = Comjointure_2$Prix_mcarre_moyen,seq(0, 1, 0.10), na.rm=T)
sum(Comjointure_2$Nombre_transacs)
#carto
Comjointure_3<- Comjointure_2%>% filter(annee.x==2012)
mean(Comjointure_3$Prix_mcarre_moyen)
max(Comjointure_3$Prix_mcarre_moyen)/min(Comjointure_3$Prix_mcarre_moyen)
dev.off()
setwd("~/Sauvegarde_figures/prix_commune_99_08_12")
pdf(file=paste0(~"Sauvegarde_figures/prix_commune_99_08_12","2012",".pdf"),width = 10, height = 5)
osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)
tilesLayer(osmTiles)
# length(unique(cadrillage_jointure_spdf@data$ID.carr))
#si raster values do not match break values verifier les valeurs du span par rapport à la discret
choroLayer(x=Comjointure_3,
           var="Prix_mcarre_moyen",
            col = cols,
            breaks =discret,
            border = "grey40",
           lwd= 0.3,
            legend.title.txt = "Prix moyen des appartements au m²",
            legend.pos = "topright", legend.values.rnd = -2,add=T)
# Layout plot
layoutLayer(title = "2012",
             sources = "RStudio Package Sf et Cartography - Données : BIEN", 
             author = "UMR Geographie-cites ®T.LE CORRE, 2017",
             north=T,
            col = "black",
            coltitle = "white")
dev.off()
