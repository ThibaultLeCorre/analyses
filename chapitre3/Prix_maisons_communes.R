library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)

#Boucle_prix_immo_200
# orginals_data_BIEN<-orginals_data_BIEN%>%filter(cluster!=0, ID!=403773 & ID!=253617&ID!=570130&ID!=910347)
# data_redresse<- orginals_data_BIEN %>%
#   bind_rows(orginals_data_BIEN %>% filter(comurb2012=="1"))

Transac_acquereur<- data_redresse%>% 
  filter(`X.x`!= 0 & `Y.x`!=0, !is.na(REQ_PRIX))%>%
  select(ID,REQ_PRIX,annee.x,X.x, Y.x, REQTYPBIEN, NBRPIECE)
test<- Transac_acquereur


#couleurs
# cols=carto.pal(pal1 = 'green.pal', n1 = 5 ,pal2='red.pal', n2=5)
# cols=c("#23b28d","#5ba483","#769678","#89896d","#967b63","#9f6c57","#a75d4c","#ac4d40","#af3a32","#b12322")
cols=c("#006837","#1a9850","#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61","#f46d43","#d73027","#a50026")
opacity <- 80 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)
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


## essai repr?sentation

osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)

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
st_crs(Mask_st_200)
st_crs(spVente)
st_crs(Cadrillage_200)

# pour carreaux Insee noramlement pas necessaire
# carreauInsee200_3<- st_intersection(Mask.st, carreauInsee200_2)
####




Comjointure<-st_join(CommunesInsee, spVente, join = st_contains, left=T)

Comjointure_2<-Comjointure%>% 
  group_by(DepCom, annee.x) %>%
  filter(REQTYPBIEN=="MA"& !is.na(REQ_PRIX))%>%
  summarise(Nombre_transacs=length(which(REQTYPBIEN=="MA")),
            Prix_moyen=mean(REQ_PRIX,na.rm=T)) 





breaks_ratio<-getBreaks(Comjointure_2$Prix_moyen,nclass = 10,method="quantile")

#carto



annees <- unique(spVente$annee.x)
annees <- sort(annees[!is.na(annees)])

Montableau_Maisons<- data.frame(Carreau_ID=NA,NbTransac=NA,Prix_moyen=NA,Annee=NA)
Montableau_Maisons<-Montableau_Maisons%>%filter(!is.na(Annee))


##bocle a realiser a partir de ce moment
#fermer periph plot pour sauvegarde
dev.off()
#etape 2 : boucle
osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)
dev.off()
#etape 2 : boucle
for (cetteAnneeLa in annees){
  spVente_2<-Comjointure_2%>% filter(annee.x==cetteAnneeLa & Nombre_transacs>=5) 
  
  setwd("~/Sauvegarde_figures/prix_commune_maisons")
  pdf(file=paste0(~"Sauvegarde_figures/prix_commune_maisons",cetteAnneeLa ,".pdf"),width = 10, height = 5)
  
  tilesLayer(osmTiles)
  # length(unique(cadrillage_jointure_spdf@data$ID.carr))
  #si raster values do not match break values verifier les valeurs du span par rapport à la discret
  choroLayer(x= spVente_2,
             var="Prix_moyen",
             col = cols,
             breaks =breaks_ratio,
             border = "grey40",
             lwd= 0.1,
             legend.title.txt = "Prix moyen des maisons",
             legend.pos = "topright", legend.values.rnd = -2,add=T)
  # Layout plot
  layoutLayer(title = cetteAnneeLa,
              sources = "RStudio Package Sf et Cartography - Données : BIEN", 
              author = "UMR Geographie-cites ®T.LE CORRE, 2017",
              north=T,
              col = "black",
              coltitle = "white")
  
  
  stewartPrix_df<-as.data.frame(spVente_2)
  
  stewartPrix_df$Annee <- cetteAnneeLa
  
  Montableau_Maisons<-bind_rows(Montableau_Maisons, stewartPrix_df)
  
  
  dev.off()
  rm(spVente_2 )
  
}

Montableau_Maisons_communes<- Montableau_Maisons
# rm(carreaux_resumes, breaks_ratio)
Montableau_Maisons_communes<-Montableau_Maisons_communes%>% select(-geometry)

setwd("~/Projets/Prix_evol_2/table_results_prix_evol_2")
write.csv2(x=Montableau_Maisons_communes,file = "Montableau_Maisons_communes.csv", row.names=FALSE, fileEncoding = "UTF-8")



