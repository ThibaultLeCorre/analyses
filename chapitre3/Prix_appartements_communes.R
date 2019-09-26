#Prix commune
library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)

Transac_acquereur<- data_redresse%>% 
  filter(`X.x`!= 0 & `Y.x`!=0)%>%
  select(ID,REQ_PM2,annee.x,X.x, Y.x, REQTYPBIEN, NBRPIECE)
Prix_commune<-Transac_acquereur%>%filter(REQTYPBIEN=="AP")
Prix_commune<- Prix_commune%>% filter(`X.x`!= 0 & `Y.x`!= 0,REQ_PM2>0)

#regler les modalites cartographqiues

# med<-median(App_BIEN$REQ_PM2, na.rm=T)
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



Comjointure<-st_join(CommunesInsee, spVente, join = st_contains, left=T)

Comjointure_2<-Comjointure%>% 
  group_by(DepCom, annee.x) %>%
  summarise(Nombre_transacs=length(which(REQ_PM2!="NA")),
  Prix_mcarre_moyen=mean(REQ_PM2,na.rm=T)) 



breaks_ratio<-getBreaks(Comjointure_2$Prix_mcarre_moyen,nclass = 10,method="quantile")

#carto



annees <- unique(spVente$annee.x)
annees <- sort(annees[!is.na(annees)])

Montableau_Appartement<- data.frame(Carreau_ID=NA,NbTransac=NA,Prix_mcarre_moyen=NA,Annee=NA)
Montableau_Appartement<-Montableau_Appartement%>%filter(!is.na(Annee))

##bocle a realiser a partir de ce moment
#fermer periph plot pour sauvegarde
osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)
dev.off()
#etape 2 : boucle
for (cetteAnneeLa in annees){
  spVente_2<-Comjointure_2%>% filter(annee.x==cetteAnneeLa, Nombre_transacs>=5) 

setwd("~/Sauvegarde_figures/prix_commune")
pdf(file=paste0(~"Sauvegarde_figures/prix_commune",cetteAnneeLa ,".pdf"),width = 10, height = 5)

tilesLayer(osmTiles)
# length(unique(cadrillage_jointure_spdf@data$ID.carr))
#si raster values do not match break values verifier les valeurs du span par rapport à la discret
choroLayer(x= spVente_2,
           var="Prix_mcarre_moyen",
           col = cols,
           breaks =breaks_ratio,
           border = "grey40",
           lwd= 0.1,
           legend.title.txt = "Prix moyen des appartements au m²",
           legend.pos = "topright", legend.values.rnd = -2,add=T)
# Layout plot
layoutLayer(title = "2012",
            sources = "RStudio Package Sf et Cartography - Données : BIEN", 
            author = "UMR Geographie-cites ®T.LE CORRE, 2017",
            north=T,
            col = "black",
            coltitle = "white")


stewartPrix_df<-as.data.frame(spVente_2)

stewartPrix_df$Annee <- cetteAnneeLa

Montableau_Appartement<-bind_rows(Montableau_Appartement, stewartPrix_df)


dev.off()
rm(spVente_2 )

}

Montableau_Appartement_communes<- Montableau_Appartement
# rm(carreaux_resumes, breaks_ratio)
Montableau_Appartement_communes<-Montableau_Appartement_communes%>% select(-geometry)

setwd("~/Projets/Prix_evol_2/table_results_prix_evol_2")
write.csv2(x=Montableau_Appartement_communes,file = "Montableau_Appartement_communes.csv", row.names=FALSE, fileEncoding = "UTF-8")


