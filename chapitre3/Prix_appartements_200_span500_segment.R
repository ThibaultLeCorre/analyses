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
  filter(`X.x`!= 0 & `Y.x`!=0, !is.na(REQ_PM2))%>%
  select(ID,REQ_PM2,annee.x,X.x, Y.x, REQTYPBIEN, NBRPIECE)
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

#Importation carroyage Insee 200*200 et 1*1km
setwd("~/Shapes/datidf")
list.files()
carreauInsee200 <- st_read("car200m_idf_reparGeom.shp",
                           stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee200 <-  st_transform(carreauInsee200, crs = 2154)
#Mask
Mask_st_200 <- st_union(carreauInsee200)
Mask_st_200<-st_sf(id = 1, geometry =Mask_st_200 )

# Mask_simplified <-
Mask_st_200 <- Mask_st_200 %>%
  st_buffer(dist = 200) %>%
  st_buffer(dist = -400) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 200) %>%
  st_buffer(200)

#pour carreau Insee
Cadrillage_200<- carreauInsee200%>%
  st_sf(geometry = .)

st_crs(Mask_st_200)
st_crs(spVente)
st_crs(Cadrillage_200)

# pour carreaux Insee noramlement pas necessaire
# carreauInsee200_3<- st_intersection(Mask.st, carreauInsee200_2)
####

Cadrillage_200$Carreau_ID<-1:length(Cadrillage_200$TARGET_FID)
Cadrillage_200_jointure<-st_join(Cadrillage_200, spVente, join = st_contains, left=T)


Mask_spdf_200<-as(Mask_st_200, "Spatial")
proj4string(Mask_spdf_200) <-CRS("+init=epsg:2154")
Mask_spdf_200<- spTransform(Mask_spdf_200, "+init=epsg:2154")


#etape 1
#definir discretization

carreaux_resumes <- Cadrillage_200_jointure %>%
  group_by(Carreau_ID) %>%
  filter(REQTYPBIEN=="AP" & NBRPIECE==2| NBRPIECE==3)%>%
  summarise(Prix=mean(REQ_PM2))
Cadrillage_200<-carreaux_resumes%>% select(Carreau_ID,geometry)
cadrillage_200_spdf<-as(Cadrillage_200, "Spatial")
proj4string(cadrillage_200_spdf) <-CRS("+init=epsg:2154")
cadrillage_200_spdf<- spTransform(cadrillage_200_spdf, "+init=epsg:2154")

library(ggplot2)
ggplot(carreaux_resumes, aes(x=Prix)) + geom_histogram(binwidth = 200)

breaks_ratio<-getBreaks(carreaux_resumes$Prix,nclass = 10,method="quantile")
########

##Preparation tableaux pour étape 2#
annees <- unique(Cadrillage_200_jointure$annee.x)
annees <- sort(annees[!is.na(annees)])

Montableau_Appartement<- data.frame(Carreau_ID=NA,NbTransac=NA,Prix=NA,prixPotentiel=NA,Annee=NA)
Montableau_Appartement<-Montableau_Appartement%>%filter(!is.na(Annee))

##bocle a realiser a partir de ce moment
#fermer periph plot pour sauvegarde
dev.off()
#etape 2 : boucle
for (cetteAnneeLa in annees){
  
  spVente_2<-spVente%>% filter(annee.x==cetteAnneeLa, 
                               REQTYPBIEN=="AP"& NBRPIECE==2| NBRPIECE==3 )%>%
    as( "Spatial")
  proj4string(spVente_2) <-CRS("+init=epsg:2154")
  spVente_2<- spTransform(spVente_2, "+init=epsg:2154")
  
  
  spVente_2$dummy <- 1
  stewartPrix <- mcStewart(knownpts = spVente_2, 
                           unknownpts = cadrillage_200_spdf,
                           varname = "REQ_PM2", 
                           typefct = "exponential", span = 500, beta = 2,
                           mask = Mask_spdf_200, longlat = FALSE)
  
  stewartTransac <- mcStewart(knownpts = spVente_2, 
                              unknownpts = cadrillage_200_spdf,
                              varname = "dummy", 
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_200, longlat = FALSE)
  
  
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
  setwd("~/Sauvegarde_figures/appartement_2_3_pieces_carreau_200_span500")
  pdf(file=paste0(~"Sauvegarde_figures/appartement_2_3_pieces_carreau_200_span500",cetteAnneeLa,".pdf"),width = 10, height = 5)
  tilesLayer(osmTiles)
  plot(Mask_st_200, col= col_mask, border=NA,add=T, main=NULL)
  
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
  rm(spVente_2,stewartPrix, stewartTransac, stewartTransac_SF, stewartPrix_SF )
  
}

Montableau_Appartement_200_segment<- Montableau_Appartement
rm(carreaux_resumes, breaks_ratio)


Montableau_Appartement_200_segment<-Montableau_Appartement_200_segment%>% select(-geometry)

setwd("~/Projets/Prix_evol_2/table_results_prix_evol_2")
write.csv2(x=Montableau_Appartement_200_segment,file = "Montableau_Appartement_200_segment.csv", row.names=FALSE, fileEncoding = "UTF-8")
