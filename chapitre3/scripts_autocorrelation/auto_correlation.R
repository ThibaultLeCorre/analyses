#Import tableaux Prix
library (dplyr)
library (sf)
library(tidyr)
Montableau_Appartement_1000_all_5091_2<-Montableau_Appartement_1000_all_5091tiles%>%
  select(Carreau_ID,prixPotentiel,Annee)%>%
  group_by(Carreau_ID)%>%
  filter(!is.na(prixPotentiel))%>%
  spread(Annee,prixPotentiel)%>%
  filter_all(all_vars(!is.na(.)))


setwd("~/Shapes/shpIDF_dep_lamb93")
list.files()

lim_IDF.st<- st_read("ile-de-france.shp",
                     stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
lim_IDF.st  <-  st_transform(lim_IDF.st , crs = 2154)

lim_IDF_spdf<-as(lim_IDF.st, "Spatial")
proj4string(lim_IDF_spdf) <-CRS("+init=epsg:2154")
lim_IDF_spdf<- spTransform(lim_IDF_spdf, "+init=epsg:2154")

setwd("~/Shapes/datidf")
list.files()
carreauInsee1000 <- st_read("car1000m_idf_reparGeom.shp",
                           stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee1000 <-  st_transform(carreauInsee1000, crs = 2154)
carreauInsee1000$Carreau_ID<-1:length(carreauInsee1000$id_c1000)
Cadrillage_1000<-left_join(carreauInsee1000, Montableau_Appartement_1000_all_5091_2, by="Carreau_ID")
Cadrillage_1000<-Cadrillage_1000%>% 
  group_by(Carreau_ID)%>%
  filter(!is.na(`2012`))%>%
  select(Carreau_ID, geometry, `2012`)
Cadrillage_1000$`2012`<-as.numeric(Cadrillage_1000$`2012`)
library(cartography)
choroLayer(Cadrillage_1000,
           var = "2012", method="quantile")


Cadrillage_1000_spdf<-as(Cadrillage_1000, "Spatial")
proj4string(Cadrillage_1000_spdf) <-CRS("+init=epsg:2154")
Cadrillage_1000_spdf<- spTransform(Cadrillage_1000_spdf, "+init=epsg:2154")
coords <- coordinates(Cadrillage_1000_spdf)

library (spdep)


Cadrillage_1000_spdf_nb<- poly2nb(Cadrillage_1000_spdf, row.names = Cadrillage_1000_spdf$Carreau_ID )
# plot(Cadrillage_1000_spdf_nb,coords)
Cadrillage_1000_spdf_lw<-nb2listw(Cadrillage_1000_spdf_nb,zero.policy = TRUE )
?nb2listw
Cadrillage_1000_spdf@data$sd_2012<- as.vector(scale(Cadrillage_1000_spdf@data$X2012))
# Montableau_Appartement_1000_all_5091_2<-as.data.frame(Montableau_Appartement_1000_all_5091_2)
moran.plot(Montableau_Appartement_1000_all_5091_2$sd_2012,Cadrillage_1000_spdf_lw, style = "W")
total_moran<-moran.test(Cadrillage_1000_spdf@data$sd_2012,Cadrillage_1000_spdf_lw,randomisation = FALSE, zero.policy = TRUE)

total_moran$statistic
?moran.test
prix_moranlocaux<-rownames(Cadrillage_1000_spdf@data)
prix_moranlocaux<-localmoran(Cadrillage_1000_spdf@data$sd_2012,Cadrillage_1000_spdf_lw, zero.policy = TRUE)


Cadrillage_1000_spdf@data$pods<-lag.listw(Cadrillage_1000_spdf_lw, Cadrillage_1000_spdf@data$sd_2012)
# Cadrillage_1000_spdf$Density <- Montableau_Appartement_1000_all_5091_2$`2012`
# Cadrillage_1000_spdf$z.li <- prix_moranlocaux[,4]
# Cadrillage_1000_spdf$pvalue <- prix_moranlocaux[,5]


chk<-Cadrillage_1000_spdf@data$sd_2012-mean(Cadrillage_1000_spdf@data$sd_2012)
zi<- prix_moranlocaux[,1]
quadrant <- vector(mode="numeric",length=nrow(prix_moranlocaux))
quadrant[chk>0 & zi>0] <- 1 # H-H
quadrant[chk<0 & zi>0] <- 2 # L-L
quadrant[chk>0 & zi<0] <- 3 # H-L
quadrant[chk<0 & zi<0] <- 4 # L-H

signif <- 0.05
quadrant[prix_moranlocaux[, 5]> signif] <- 5
colors <- c("red", "blue", "lightpink", "skyblue2", rgb(.95, .95, .95))
par(mfrow=c(1,1)) # sets margin parameters for plot space; 
# A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
plot(Cadrillage_1000_spdf, border=FALSE, col=colors[quadrant], main = "LISA Cluster Map, Real estate prices")
plot(lim_IDF_spdf, add=T)
legend("bottomright",legend=c("High-High","Low-Low","High-Low","Low-High"), fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)


####
# Communes

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

#pour carreau Insee
map<- CommunesInsee%>%
  st_sf(geometry = .)

map<-Cadrillage_1000
map<-Cadrillage_200
# tab<- Montableau_Appartement_communes[,c(3:7)]
# tab<- tab%>%
#   select(DepCom,Prix_mcarre_moyen,Annee)%>%
#   group_by(DepCom)%>%
#   filter(!is.na(Prix_mcarre_moyen))%>%
#   spread(Annee,Prix_mcarre_moyen)%>%
#   filter_all(all_vars(!is.na(.)))

tab<-Montableau_Appartement_200_all_45138%>%
  select(Carreau_ID,prixPotentiel,Annee)%>%
  group_by(Carreau_ID)%>%
  filter(!is.na(prixPotentiel))%>%
  spread(Annee,prixPotentiel)%>%
  filter_all(all_vars(!is.na(.)))

tab$Y_std<-scale(tab$`2012`)
map$Carreau_ID<-as.integer(map$Carreau_ID)
map<-left_join(map, tab, by="Carreau_ID")
map<-map%>% 
  group_by(Carreau_ID)%>%
  filter(!is.na(`2012`))

map<-as(map, "Spatial")
proj4string(map) <-CRS("+init=epsg:2154")
map<- spTransform(map, "+init=epsg:2154")
coords <- coordinates(map)

map_nb<-poly2nb(map, queen=TRUE)

map_nb_w<-nb2listw(map_nb, zero.policy = TRUE)
summary(map_nb_w)
tab<-as.data.frame(tab)
tab$X2012<-as.vector(tab$`2012`)
tab$Y_lag<-lag.listw(map_nb_w,tab$X2012)
tab$Y_std_lag<-lag.listw(map_nb_w,tab$Y_std)
head(tab)
cor.test(tab$X2012,tab$Y_lag)
model<-lm(tab$Y_lag~tab$X2012)
summary(model)
moran.test(tab$X2012,map_nb_w,zero.policy = TRUE)


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


locm<-localmoran(tab$X2012,map_nb_w,alternative = "two.sided", zero.policy = TRUE)
tab2<-as.data.frame(locm)
tab<-as.data.frame(tab)
tabres<-cbind(tab,tab2)
head(tabres)

#carto

par(mfrow=c(1,2),mar=c(2,2,2,2))
# Carte d'origine
mycols <- carto.pal(pal1 = "green.pal", n1 = 4, pal2 = "orange.pal", n2 = 4)

breaks_lisa<- c(min(tabres$Z.Ii, na.rm=TRUE),0,1.96,max(tabres$Z.Ii, na.rm=TRUE))

choroLayer(spdf = map,df=tab, var = "X2012",method="equal", nclass=8, legend.values.rnd = 1,
           col=mycols,legend.pos = "topright", border=FALSE)
choroLayer(spdf = map,df=tabres, var = "Z.Ii" , breaks = breaks_lisa,
           legend.values.rnd = 1,
           col=mycols,legend.pos = "topright", border=FALSE)
title(labelY)
# Carte de Moran
q1<-as.factor(tabres$Y_std>0)
levels(q1)<-c("Low","High")
q2<-as.factor(tabres$Y_std_lag>0)
levels(q2)<-c("Low","High")
MapMoran<-paste(as.character(q1),as.character(q2),sep="-")
MapMoran[abs(tabres$Z.Ii)<2]<-"Non Sign."

labels=c("High-High","Low-Low","High-Low","Low-High","Non Sign.")
tab$Moran_type<-factor(MapMoran,levels=labels)
tab$Moran_color<-tab$Moran_type
colors=c("red","blue","lightpink","skyblue2","grey")
levels(tab$Moran_color)<-colors
tab$Moran_color<-as.character(tab$Moran_color)

colors<-c("red","blue","lightpink","skyblue2","grey")
plot(map,col=tab$Moran_color, border=FALSE)
legend("topright",legend=labels, fill=colors,bty="n")
