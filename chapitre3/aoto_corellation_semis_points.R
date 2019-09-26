#Import tableaux Prix
data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)

library (sf)
library(tidyr)

library (usdm)
library(SpatialPosition)
library(cartography)
library(fasterize)
library(spdep)
library (dplyr)
Transac_acquereur<- data_redresse1570533transacs%>% 
  filter(`X.x`!= 0 & `Y.x`!=0, annee.x==2012,  REQTYPBIEN=="AP", REQ_PM2>0)%>%
  select(REQ_PM2,annee.x,X.x, Y.x,ID)
test<- Transac_acquereur



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


spVente<-spVente%>% 
  as( "Spatial")
proj4string(spVente) <-CRS("+init=epsg:2154")
spVente_2<- spTransform(spVente, "+init=epsg:2154")


setwd("~/Shapes/shpIDF_dep_lamb93")
list.files()

lim_IDF.st<- st_read("ile-de-france.shp",
                     stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
lim_IDF.st  <-  st_transform(lim_IDF.st , crs = 2154)


setwd("~/Shapes/datidf")
list.files()
carreauInsee200 <- st_read("car200m_idf_reparGeom.shp",
                           stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee200 <-  st_transform(carreauInsee200, crs = 2154)


carreauInsee200$Carreau_ID<-1:length(carreauInsee200$TARGET_FID)

Cadrillage_200<- carreauInsee200%>%
  st_sf(geometry = .)
Cadrillage_200<-Cadrillage_200%>% select(Carreau_ID,geometry)
cadrillage_200_spdf<-as(Cadrillage_200, "Spatial")
proj4string(cadrillage_200_spdf) <-CRS("+init=epsg:2154")
cadrillage_200_spdf<- spTransform(cadrillage_200_spdf, "+init=epsg:2154")
  
cadrillage_200_raster<-raster(cadrillage_200_spdf, res = 1)
spVente_raster<-raster(spVente_2, res = 1)
##########Lisa fonction

lisa(cadrillage_200_raster, y = spVente_2,cell = spVente_2@data$REQ_PM2,  d2 = 3000,statistic = "I")
?lisa
voisns_poly<- dnearneigh(spVente_2, k=1, longlat = FALSE )
plot(voisns_poly)
?dnearneigh() 


####

Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)


carreaux_resumes <- Cadrillage_200_jointure %>%
  group_by(Carreau_ID) %>%
  summarise(Nombre_transacs=length(which(REQ_PM2!="NA")),
            prix_moyen= mean(REQ_PM2, na.rm=T))%>%
  filter(Nombre_transacs>0)

# carreaux_resumes<-left_join(carreaux_resumes, tabres[,c("Carreau_ID", "Z.Ii")], by= "Carreau_ID")
# carreaux_resumes$signif_auto<-carreaux_resumes$Z.Ii
# carreaux_resumes<-carreaux_resumes%>% filter(signif_auto<=1.96 & signif_auto>=-1.96)

map<-carreaux_resumes
tab<-as.data.frame(spVente_2@data)

tab$Y_std<-scale(tab$REQ_PM2)

map$Y_std<-scale(map$prix_moyen)


map<-as(map, "Spatial")
proj4string(map) <-CRS("+init=epsg:2154")
map<- spTransform(map, "+init=epsg:2154")
coords <- coordinates(spVente_2)

map_nb<-poly2nb(map, queen=TRUE)
Carreau_ID<-map@data$Carreau_ID
ID_transacs<-row.names(spVente_2@data)

map_nb<-dnearneigh(coords, d1=0, d2=500, row.names=ID_transacs)
map_nb<-knearneigh(coords, k=5, longlat=FALSE)
summary(map_nb)
map_nb<-knn2nb(map_nb)
map_nb_w<-nb2listw(map_nb, zero.policy=TRUE)
plot(map)
plot(map_nb, coords, add=T)
summary(map_nb_w)
tab$prix_moyen<-as.vector(tab$prix_moyen)
tab$Y_lag<-lag.listw(map_nb_w,tab$REQ_PM2)
tab$Y_std_lag<-lag.listw(map_nb_w,tab$Y_std)
head(tab)
cor.test(tab$REQ_PM2,tab$Y_lag, na.rm=TRUE)
model<-lm(tab$Y_lag~tab$REQ_PM2)
summary(model)
moran.test(tab$REQ_PM2,map_nb_w,zero.policy = TRUE)


#diagramme moran
par(mfrow=c(1,1), mar=c(4,4,2,2))
plot(x=tab$Y_std,y=tab$Y_std_lag,main="Diagramme de Moran",xlab = "Observed",ylab="Lagged")
abline(h=0,v=0)
modele_moran<-lm(tab$Y_std_lag~tab$Y_std)
abline(modele_moran, lty=2,lwd=2,col="red")
text(-1,-1,"Low-Low",col="grey30")
text(-1,1,"Low-High",col="grey30")
text(1,-1,"High-Low",col="grey30")
text(1,1,"High-High",col="grey30") 


locm<-localmoran(tab$REQ_PM2,map_nb_w,alternative = "two.sided", zero.policy = TRUE)
tab2<-as.data.frame(locm)
tab<-as.data.frame(tab)
tabres<-cbind(tab,tab2)
head(tabres)

#carto

par(mfrow=c(1,2),mar=c(2,2,2,2))
# Carte d'origine
mycols <- carto.pal(pal1 = "green.pal", n1 = 4, pal2 = "orange.pal", n2 = 4)

breaks_lisa<- c(min(tabres$Z.Ii, na.rm=TRUE),-1.96,0,1.96,max(tabres$Z.Ii, na.rm=TRUE))
colors=c("grey","lightpink","lightpink","grey","grey")


choroLayer(spdf = map,df=tab, var = "prix_moyen",method="equal", nclass=8, legend.values.rnd = 1,
           col=mycols,legend.pos = "topright", border=FALSE)
choroLayer(spdf = map,df=tabres, var = "Z.Ii" , breaks = breaks_lisa,
           legend.values.rnd = 1,
           col=colors,legend.pos = "topright", border=FALSE)
title(labelY)
# Carte de Moran
q1<-as.factor(tabres$Y_std>0)
levels(q1)<-c("Low","High")
q2<-as.factor(tabres$Y_std_lag>0)
levels(q2)<-c("Low","High")
MapMoran<-paste(as.character(q1),as.character(q2),sep="-")
MapMoran[abs(tabres$Z.Ii)<1.9]<-"Non Sign."

labels=c("High-High","Low-Low","High-Low","Low-High","Non Sign.")
tab$Moran_type<-factor(MapMoran,levels=labels)
tab$Moran_color<-tab$Moran_type
colors=c("red","blue","lightpink","skyblue2","grey")
levels(tab$Moran_color)<-colors
tab$Moran_color<-as.character(tab$Moran_color)

colors<-c("red","blue","lightpink","skyblue2","grey")
plot(map,col=tab$Moran_color, border=FALSE)
legend("topright",legend=labels, fill=colors,bty="n")
table(tab$Moran_type)

