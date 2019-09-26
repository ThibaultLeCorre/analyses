# Prix_annee_potentiel_200_appartement<- Montableau_Appartement_200_all_45138
# Prix_annee_potentiel_200_appartement<- Prix_annee_potentiel_200_appartement%>%
#   select(Carreau_ID,prixPotentiel,Annee)%>%
#   group_by(Carreau_ID)%>%
#   filter(!is.na(prixPotentiel))%>%
#   spread(Annee,prixPotentiel)%>%
#   filter_all(all_vars(!is.na(.)))

library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)



plot(Cadrillage_200_spdf)

choroLayer(x=Cadrillage_1000,
           var = "2012",
           colNA = "grey",
           method = "quantile",
           border = NA,
           legend.pos = "topright",
           legend.values.rnd = -2)

# choroLayer(spdf = Cadrillage_200_spdf, df = Cadrillage_200_spdf@data, var = "X2012", 
#            border = "grey80", col = "red",method = "quantile", nclass = 6,add=T)
Cadrillage_1000<-Cadrillage_1000%>% filter(!is.na(`2012`))
Cadrillage_1000_contig <- getBorders(Cadrillage_1000, id = "Carreau_ID")
Cadrillage_1000_df<-as.data.frame(Cadrillage_1000)
Cadrillage_1000_df$prix_2012<-Cadrillage_1000_df$`2012`
Cadrillage_1000_df$prix_99<-Cadrillage_1000_df$`1999`
Cadrillage_1000_df$prix_2008<-Cadrillage_1000_df$`2008`
Cadrillage_1000_df$prix_2003<-Cadrillage_1000_df$`2003`
# Cadrillage_1000_df[is.na(Cadrillage_1000_df$prix_2012)] <- 0

discLayer(x = Cadrillage_1000_contig, df=Cadrillage_1000_df, dfid="Carreau_ID",  var = "prix_2012", type = "rel", method = "quantile", 
          nclass = 5, threshold = 0.1, sizemin = 0.1, sizemax = 2, 
          col = "red",  legend.title.txt = "Discontinuities des prix", 
          legend.pos = "topright", add=T)
plot(lim_IDF.st$geometry,add=T)
?discLayer
traceback()




Cadrillage_200<-left_join(Cadrillage_200, Typo_1999_2012_carreaux200_appartement_37158tiles, by="Carreau_ID")
Cadrillage_200<-Cadrillage_200%>% select(Carreau_ID,geometry)
Cadrillage_200_spdf<-as(Cadrillage_200, "Spatial")
proj4string(Cadrillage_200_spdf) <-CRS("+init=epsg:2154")
Cadrillage_200_spdf<- spTransform(Cadrillage_200_spdf, "+init=epsg:2154")

Cadrillage_200_contig <- getBorders(Cadrillage_200, id = "Carreau_ID")



Cadrillage_200<-Cadrillage_200%>% filter(!is.na(`2012`))

Cadrillage_200<-left_join(Cadrillage_200, Typo_1999_2012_carreaux200_appartement_37158tiles, by="Carreau_ID")



Cadrillage_200_df<-as.data.frame(Cadrillage_200)
Cadrillage_200_df$prix_2012<-Cadrillage_200_df$`X2012`
Cadrillage_200_df$prix_99<-Cadrillage_200_df$`1999`
Cadrillage_200_df$prix_2008<-Cadrillage_200_df$`2008`
Cadrillage_200_df$prix_2003<-Cadrillage_200_df$`2003`


choroLayer(x=Cadrillage_200,
           var = "2012",
           colNA = "grey",
           method = "quantile",
           border = NA,
           legend.pos = "topright",
           legend.values.rnd = -2)


typoLayer(x=Cadrillage_200,
          var = "categorie_finale",
          col= cols, ##palettes de couleurs predefinies## 
          border = FALSE,
          legend.pos = "bottomleft",
          legend.title.txt = "Appart_typo_carreaux200",
          legend.values.cex= 0.5,
          colNA="grey")
plot(lim_IDF.st$geometry,add=T)

# Cadrillage_200_contig <- getBorders(Cadrillage_200, id = "Carreau_ID")
discontinutes_2012<-discLayer(x = Cadrillage_200_contig, df=Cadrillage_200_df, dfid="Carreau_ID",  var = "prix_2012", type = "rel", method = "quantile", 
          nclass = 2, threshold = 0.1, sizemin = 0.1, sizemax = 2, 
          col = "red",  legend.title.txt = "Discontinuities in \nGDP per Capita\n(relative)", 
          legend.pos = "topright",add=TRUE)

plot(lim_IDF.st$geometry,add=T)

library(leaflet)
m <- leaflet(padding = 0)

blurp<-discontinutes_2012%>% filter(disc>1.2)

blurp<- as(blurp, "Spatial")
# proj4string(blurp) <-CRS("+init=epsg:4326") 
blurp<- spTransform(blurp, "+init=epsg:4326")
m <- addPolygons(map = m, data = blurp, stroke = TRUE,  popup = blurp$Carreau_ID)
addTiles(map = m, attribution = "OSM et contributeurs")
