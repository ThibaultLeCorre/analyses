# Carto_proj_acquereur_vendeur_1000mcarreau


library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)


# pop_insee_carreau<-BIEN_propre_carro1000 [,c(111:ncol(BIEN_propre_carro1000))]
# 
# pop_insee_carreau<- pop_insee_carreau[!duplicated(pop_insee_carreau$id_1), ]
# pop<-pop_insee_carreau[,c("DEPCOM","comurb2012")]
# pop<-pop[!duplicated(pop$DEPCOM), ]
# orginals_data_BIEN<- left_join(orginals_data_BIEN,pop, by= c("insee"="DEPCOM"))
# # si necessaire virer ou il y a pas de coord, si echantillon lourd garder une suele annee
# data_redresse<- orginals_data_BIEN %>%
#   bind_rows(orginals_data_BIEN %>% filter(comurb2012=="1"))
Transac_acquereur<- data_redresse%>% filter(`X.x`!= 0 & `Y.x`!=0, CSP_AC>=1)
test<- Transac_acquereur



#Cr?ation du fichier SF (lambert II ?tendu, coord dorigine fichier BIEN)

spVente <- st_as_sf(test,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)


#conversion WGS84 et stockage coords

spVente <-  st_transform(spVente, crs = 4326)
coords<- st_coordinates(spVente)
spVente$longWGS84<-coords[,1]   
spVente$latWGS84<-coords[,2]

#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]   
spVente$Ylamb93<-coords[,2]




## essai repr?sentation

osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)




### Importer Elements preparation carto
# setwd( "C:/Users/thibault/Documents/Sync/Work_on_DATA/shapes/limite_mask_idf")
setwd("~/Shapes/limite_mask_idf")
list.files()
Mask.st <- st_read("limite_carroyage_IDF_2.shp",
                   stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
#faire une couche unie avec contours

Mask.st <- st_union(Mask.st)
Mask.st <-st_sf(id = 1, geometry = Mask.st )
par(mfrow = c(1,2))
plot(Mask.st)
dev.off()
# Mask_simplified <-
Mask.st <- Mask.st %>%
  st_buffer(dist = 500) %>%
  st_buffer(dist = -1000) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 500) %>%
  st_buffer(500)
plot(Mask.st$geometry)
# par(mfrow = c(1,1))
#Importation carroyage Insee 1000*1000 et 1*1km
setwd("~/Shapes/datidf")
list.files()
carreauInsee1000 <- st_read("car1000m_idf_reparGeom.shp",
                           stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
carreauInsee1000 <-  st_transform(carreauInsee1000, crs = 2154)

# Passer de sfc a sf, trad : passer d'un objet type multi en uni

#Importer limite_region_departement
# setwd( "C:/Users/thibault/Documents/Sync/Work_on_DATA/shapes/shpIDF_dep_lamb93")
setwd("~/Shapes/shpIDF_dep_lamb93")
lim_IDF.st <- st_read("ile-de-france.shp",
                      stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
# union pour n'avoir que les limites
lim_IDF.st<- st_union(lim_IDF.st)
plot(lim_IDF.st)
# Passer de sfc a sf, trad : passer d'un objet type multi en uni
lim_IDF.st <- st_sf(id = 1, geometry = lim_IDF.st)




#reformatage de list à sf

#pour carreau Insee
carreauInsee1000_2<- carreauInsee1000%>%
  st_sf(geometry = .)

# cadrillage_jointure$carreau<- row.names(cadrillage_jointure)
plot(st_geometry(Mask.st), col = "grey90", lwd = 0.2)
plot(st_geometry(spVente), add=T,col="red",lwd = 3)
plot(carreauInsee1000_2, border = "grey70", add = T)

# Faire l'intersection et les zones urbaines, peut prendre un certain temps
# verif st_crs()
st_crs(cadrillage)
st_crs(Mask.st)
st_crs(spVente)
st_crs(carreauInsee1000_2)


carreauInsee1000_2$Carreau_ID<-1:length(carreauInsee1000_2$id_c1000)


# On affiche la grille
plot(st_geometry(Mask.st))
plot(cadrillage3, col = "grey90", border = "grey70", add = T)
plot(spVente,add=T,col="red", border= "red")
plot(carreauInsee1000_2, col = "grey90", border = "grey70")
# Interpolation des valeurs. 



discret_ouvrier_pourcentage<-quantile(x = carreauInsee1000_jointure_2$Ouvrier[carreauInsee1000_jointure_2$Ouvrier>0]/carreauInsee1000_jointure_2$Nombre_transacs[carreauInsee1000_jointure_2$Ouvrier>0]*100,seq(0, 1, 0.2), na.rm=T)

discret_cadres<-quantile(x = carreauInsee1000_jointure_2$Cadres[carreauInsee1000_jointure_2$Cadres>0]/carreauInsee1000_jointure_2$Nombre_transacs[carreauInsee1000_jointure_2$Cadres>0]*100,seq(0, 1, 0.2), na.rm=T)
discret_ouvrier<-quantile(x = carreauInsee1000_jointure_2$Ouvrier[carreauInsee1000_jointure_2$Ouvrier>0]/carreauInsee1000_jointure_2$Nombre_transacs[carreauInsee1000_jointure_2$Ouvrier>0]*100,seq(0, 1, 0.2), na.rm=T)
discret_ouvrier<-quantile(x = carreauInsee1000_jointure_2$Ouvrier[carreauInsee1000_jointure_2$Ouvrier>0]/carreauInsee1000_jointure_2$Nombre_transacs[carreauInsee1000_jointure_2$Ouvrier>0]*100,seq(0, 1, 0.2), na.rm=T)



cols=carto.pal(pal1 = 'red.pal', n1=6)
# cols=carto.pal(pal1 = 'green.pal', n1 = 5 ,pal2='red.pal', n2=5, middle = med)
opacity <- 80 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)

##bocle a realiser a partir de ce moment

for(Year in unique(test$annee.x)){
  
  spVente <- st_as_sf(test,
                      
                      coords = c("X.x", "Y.x"),
                      
                      agr = "constant",
                      
                      crs = 27572,
                      
                      stringsAsFactors = FALSE)
  
  
  #conversion WGS84 et stockage coords
  
  # spVente <-  st_transform(spVente, crs = 4326)
  # coords<- st_coordinates(spVente)
  # spVente$longWGS84<-coords[,1]   
  # spVente$latWGS84<-coords[,2]
  
  #converison Lamb 93 et stockage coords
  spVente <-  st_transform(spVente, crs = 2154)
  coords<- st_coordinates(spVente)
  spVente$XLamb93<-coords[,1]   
  spVente$Ylamb93<-coords[,2]
  
#carreau Insee
carreauInsee1000_jointure<-st_join(carreauInsee1000_2, spVente, join = st_contains, left=T)



# spVente$carreau<-cadrillage_POP
# cadrillage<-st_sf(cadrillage)

# cadrillage_jointure$carreau<- row.names(cadrillage_jointure)


carreauInsee1000_jointure_2<-carreauInsee1000_jointure%>% group_by(Carreau_ID) %>% summarise(Nombre_transacs=length(which(CSP_AC!="NA")),
                                                                                           Ouvrier=length(which(CSP_AC>=60 & CSP_AC<70)),
                                                                                           Cadres=length(which(CSP_AC>=30 & CSP_AC<40)),
                                                                                           Prof_inter=length(which(CSP_AC>=40 & CSP_AC<50)),
                                                                                           Employes=length(which(CSP_AC>=50 & CSP_AC<60)))



# st_join(cadrillage, spVente["REQ_PRIX","carreau"],by =c("ID"="carreau"))
# st_join(cadrillage, spVente["REQ_PRIX","carreau"]) 




# smoothLayer(x = cadrillage_jointure_2, var ='Prix_moyen',
#             span = 1000, beta = 2, breaks = discret,
#             mask = , border = NA,
#             col = cols,
#             legend.title.txt = "Prix\nPotential",
#             legend.pos = "topright", legend.values.rnd = -2)
# plot(spVente,add=T,col="red", border= "red")


##Pour normaliser par raport a la population changer en sp 



cadrillage_jointure_spdf<-as(carreauInsee1000_jointure_2, "Spatial")
cadrillage_jointure_df<- as.data.frame(cadrillage_jointure_spdf@data)
Mask.spdf<-as(Mask.st, "Spatial")
lim_IDF.spdf<-as(lim_IDF.st, "Spatial")

proj4string(cadrillage_jointure_spdf) <-CRS("+init=epsg:2154")
cadrillage_jointure_spdf<- spTransform(cadrillage_jointure_spdf, "+init=epsg:2154")
proj4string(Mask.spdf) <-CRS("+init=epsg:2154")
Mask.spdf<- spTransform(Mask.spdf, "+init=epsg:2154")
proj4string(lim_IDF.spdf) <-CRS("+init=epsg:2154")
lim_IDF.spdf<- spTransform(lim_IDF.spdf, "+init=epsg:2154")

# 3.Mapping
setwd("~/Sauvegarde_figures/Ouvriers/Pourcentage")
pdf(file=paste0(~"Sauvegarde_figures/Ouvriers/Pourcentage",Year,".pdf"),width = 10, height = 5)
tilesLayer(osmTiles)



smoothLayer(spdf = cadrillage_jointure_spdf, 
            spdfid ='Carreau_ID' ,
            df = cadrillage_jointure_df,
            dfid = 'Carreau_ID' ,
            var ="Ouvrier", var2 ='Nombre_transacs',
            span = 3000, beta = 2,
            # col = cols,
            #  breaks =discret_ouvrier_pourcentage,
            mask = Mask.spdf,
            lwd=0.1,
            legend.title.txt = "Potentiel d'ouvriers",
            legend.pos = "topright", legend.values.rnd = 2,add=T)
layoutLayer(title = paste0("Pourcentage",Year))

dev.off()


#####En nombre absolu
setwd("~/Sauvegarde_figures/Ouvriers/Absolu")
pdf(file=paste0(~"Sauvegarde_figures/Ouvriers/Absolu",Year,".pdf"),width = 10, height = 5)
tilesLayer(osmTiles)
smoothLayer(x = carreauInsee1000_jointure_2, 
            var ="Ouvrier",
            span = 3000, beta = 2,
            # col = cols,
             # breaks =discret_ouvrier_absolu,
            mask = Mask.st,
            lwd=0.1,
            legend.title.txt = "Potentiel d'Ouvriers",
            legend.pos = "topright", legend.values.rnd = 2,add=T)
layoutLayer(title = paste0("Absolu",Year))
dev.off()
}
