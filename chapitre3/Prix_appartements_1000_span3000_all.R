library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)

#Boucle_prix_immo_1000
# orginals_data_BIEN<-orginals_data_BIEN%>%filter(cluster!=0, ID!=403773 & ID!=253617&ID!=570130&ID!=910347)
# data_redresse<- orginals_data_BIEN %>%
#   bind_rows(orginals_data_BIEN %>% filter(comurb2012=="1"))

Transac_acquereur<- data_redresse%>% 
  filter(`X.x`!= 0 & `Y.x`!=0)%>%
  select(ID,REQ_PM2,annee.x,X.x, Y.x, REQTYPBIEN, NBRPIECE)
test<- Transac_acquereur

#couleurs
cols=carto.pal(pal1 = 'green.pal', n1 = 5 ,pal2='red.pal', n2=5)
 # couleur degueud","#5ba483","#769678","#89896d","#967b63","#9f6c57","#a75d4c","#ac4d40","#af3a32","#b12322")
#cols=c("#006837","#1a9850","#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61","#f46d43","#d73027","#a300026")
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

#Importation carroyage Insee 1000*1000 et 1*1km
setwd("~/Shapes/datidf")
list.files()
carreauInsee1000 <- st_read("car1000m_idf_reparGeom.shp",
                           stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee1000 <-  st_transform(carreauInsee1000, crs = 2154)
#Mask
Mask_st_1000 <- st_union(carreauInsee1000)
Mask_st_1000<-st_sf(id = 1, geometry =Mask_st_1000 )

# Mask_simplified <-
Mask_st_1000 <- Mask_st_1000 %>%
  st_buffer(dist = 500) %>%
  st_buffer(dist = -1000) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 500) %>%
  st_buffer(500)
#pour carreau Insee
Cadrillage_1000<- carreauInsee1000%>%
  st_sf(geometry = .)

st_crs(Mask_st_1000)
st_crs(spVente)
st_crs(Cadrillage_1000)

# pour carreaux Insee noramlement pas necessaire
# carreauInsee1000_3<- st_intersection(Mask.st, carreauInsee1000_2)
####

Cadrillage_1000$Carreau_ID<-1:length(Cadrillage_1000$id_c1000)
Cadrillage_1000_jointure<-st_join(Cadrillage_1000, spVente, join = st_contains, left=T)


Mask_spdf_1000<-as(Mask_st_1000, "Spatial")
proj4string(Mask_spdf_1000) <-CRS("+init=epsg:2154")
Mask_spdf_1000<- spTransform(Mask_spdf_1000, "+init=epsg:2154")


#etape 1
#definir discretization



carreaux_resumes <- Cadrillage_1000_jointure %>%
  group_by(Carreau_ID) %>%
  summarise(Nombre_transacs=length(which(REQTYPBIEN!="NA")))%>%
  filter(Nombre_transacs>0)


carreaux_discret <- Cadrillage_1000_jointure %>%
  group_by(Carreau_ID) %>%
  filter(REQTYPBIEN=="AP" & !is.na(REQ_PM2))%>%
  summarise(Prix=mean(REQ_PM2))


# filter(REQTYPBIEN=="AP")%>%
# summarise(Prix=mean(REQ_PM2))

Cadrillage_1000<-carreaux_resumes%>% select(Carreau_ID,geometry)
cadrillage_1000_spdf<-as(Cadrillage_1000, "Spatial")
proj4string(cadrillage_1000_spdf) <-CRS("+init=epsg:2154")
cadrillage_1000_spdf<- spTransform(cadrillage_1000_spdf, "+init=epsg:2154")


library(ggplot2)
ggplot(carreaux_resumes, aes(x=Prix)) + geom_histogram(binwidth = 1000)

breaks_ratio<-getBreaks(carreaux_discret$Prix,nclass = 10,method="quantile")
rm(carreaux_discret)
########

##Preparation tableaux pour étape 2#
annees <- unique(Cadrillage_1000_jointure$annee.x)
annees <- sort(annees[!is.na(annees)])

Montableau_Appartement<- data.frame(Carreau_ID=NA,NbTransac=NA,Prix=NA,prixPotentiel=NA,Annee=NA)
Montableau_Appartement<-Montableau_Appartement%>%filter(!is.na(Annee))

##bocle a realiser a partir de ce moment
#fermer periph plot pour sauvegarde
dev.off()
#etape 2 : boucle
for (cetteAnneeLa in annees){
  
  spVente_2<-spVente%>% filter(annee.x==cetteAnneeLa, REQTYPBIEN=="AP", !is.na(REQ_PM2))%>%
    as( "Spatial")
  proj4string(spVente_2) <-CRS("+init=epsg:2154")
  spVente_2<- spTransform(spVente_2, "+init=epsg:2154")
  
  
  spVente_2$dummy <- 1
  stewartPrix <- mcStewart(knownpts = spVente_2, 
                           unknownpts = cadrillage_1000_spdf,
                           varname = "REQ_PM2", 
                           typefct = "exponential", span = 3000, beta = 2,
                           mask = Mask_spdf_1000, longlat = FALSE)
  
  stewartTransac <- mcStewart(knownpts = spVente_2, 
                              unknownpts = cadrillage_1000_spdf,
                              varname = "dummy", 
                              typefct = "exponential", span = 3000, beta = 2,
                              mask = Mask_spdf_1000, longlat = FALSE)
  
  
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)
  stewartPrix_SF <- st_as_sf(stewartPrix) %>% 
    rename(Prix = OUTPUT)
  
  
  stewartPrix_SF <- left_join(stewartPrix_SF %>%
                                select(Prix,Carreau_ID),
                              stewartTransac_df%>%
                                select(NbTransac,Carreau_ID),  
                              by="Carreau_ID") %>%
    mutate(prixPotentiel = Prix/ NbTransac)
  
  #attention focntionne pas si environnement de travail pas precise
  setwd("~/Sauvegarde_figures/Appartement_carreau1000_span3000_5091tiles")
  pdf(file=paste0(~"Sauvegarde_figures/Appartement_carreau_1000_span3000_5091tiles",cetteAnneeLa,".pdf"),width = 10, height = 5)
  tilesLayer(osmTiles)
  plot(Mask_st_1000, col= col_mask, border=NA,add=T, main=NULL)
  
  choroLayer(x=stewartPrix_SF,
             var = "prixPotentiel",
             col = cols,
             colNA = col_mask,
             breaks = breaks_ratio,
             border = NA,
             legend.title.txt = cetteAnneeLa,
             legend.pos = "topright", 
             legend.values.rnd = -2,add=T)
  
  
  stewartPrix_df<-as.data.frame(stewartPrix_SF)
  
  stewartPrix_df$Annee <- cetteAnneeLa
  
  Montableau_Appartement<-bind_rows(Montableau_Appartement, stewartPrix_df)
  
  
  dev.off()
  # rm(spVente_2,stewartPrix, stewartTransac, stewartTransac_SF, stewartPrix_SF )
  
}

Montableau_Appartement_1000_all<- Montableau_Appartement

Montableau_Appartement_1000_all<-Montableau_Appartement_1000_all%>% select(-geometry)

setwd("~/Projets/Prix_evol_2/table_results_prix_evol_2")
write.csv2(x=Montableau_Appartement_1000_all,file = "Montableau_Appartement_1000_all_5091tiles.csv", row.names=FALSE, fileEncoding = "UTF-8")

