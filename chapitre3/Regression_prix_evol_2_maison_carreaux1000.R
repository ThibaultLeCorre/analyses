library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)

Prix_annee_potentiel_1000_Maison<- Montableau_maisons_1000_all_5036tiles
Prix_annee_potentiel_1000_Maison<- Prix_annee_potentiel_1000_Maison%>%
  select(Carreau_ID,prixPotentiel,Annee)%>%
  group_by(Carreau_ID)%>%
  filter(!is.na(prixPotentiel))%>%
  spread(Annee,prixPotentiel)%>%
  filter_all(all_vars(!is.na(.)))


Prix_annee_potentiel_1000_Maison<-as.data.frame(Prix_annee_potentiel_1000_Maison)


test.cor<-cor (Prix_annee_potentiel_1000_Maison$`2012`,Prix_annee_potentiel_1000_Maison$`1999`, use="complete.obs")
result_cor_maison1000<-test.cor^2

Y <- Prix_annee_potentiel_1000_Maison$`2012` # Y variables à expliquer   
X <- Prix_annee_potentiel_1000_Maison$`1999` #variable explicative


reg.lin<-lm(Y~X)
Droite_regression_maison_1000<-reg.lin[[1]]

Prix_annee_potentiel_1000_Maison$test.lm.fit<-reg.lin$fitted.values
resid<-residuals.lm(reg.lin)
Prix_annee_potentiel_1000_Maison$resid<-resid

ecartmodel_maisons1000<-sd(Prix_annee_potentiel_1000_Maison$test.lm.fit)

Prix_annee_potentiel_1000_Maison$categorie_variation<- ifelse(Prix_annee_potentiel_1000_Maison$resid> - (ecartmodel_maisons1000/2) & Prix_annee_potentiel_1000_Maison$resid<= (ecartmodel_maisons1000/2), "evolution_conforme_au_modele",
                                                             ifelse(Prix_annee_potentiel_1000_Maison$resid <= - (ecartmodel_maisons1000/2), "evolution_plus_faible_que_la_prevision",
                                                                    ifelse(Prix_annee_potentiel_1000_Maison$resid >=  (ecartmodel_maisons1000/2), "evolution_plus_fort_que_la _prevision", "non_rs")))


prix_bas99<- mean (Prix_annee_potentiel_1000_Maison$`1999`) - sd(x = Prix_annee_potentiel_1000_Maison$`1999`) 
prix_haut99<- mean (Prix_annee_potentiel_1000_Maison$`1999`) + sd(x = Prix_annee_potentiel_1000_Maison$`1999`) 

##1270.4
Prix_annee_potentiel_1000_Maison$pos_median <-ifelse (Prix_annee_potentiel_1000_Maison$`1999`<prix_haut99 &  Prix_annee_potentiel_1000_Maison$`1999`>prix_bas99, "Prix initiaux dans la moyenne (mean +/- sd)",
                                                     ifelse (Prix_annee_potentiel_1000_Maison$`1999`>=prix_haut99,  "Prix initiaux élevés (mean + sd)",
                                                             ifelse(Prix_annee_potentiel_1000_Maison$`1999`<=prix_bas99, "Prix initiaux faibles (mean - sd)", "non.rs")))


###prix finauxmedian##

Prix_annee_potentiel_1000_Maison$categorie_finale<- paste0(Prix_annee_potentiel_1000_Maison$pos_median, Prix_annee_potentiel_1000_Maison$categorie_variation, sep="_")


##

setwd("~/Shapes/shpIDF_dep_lamb93")
list.files()

lim_IDF.st<- st_read("ile-de-france.shp",
                     stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
lim_IDF.st  <-  st_transform(lim_IDF.st , crs = 2154)

setwd("~/Shapes/datidf")
list.files()
carreauInsee1000 <- st_read("car1000m_idf_reparGeom.shp",
                           stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee1000 <-  st_transform(carreauInsee1000, crs = 2154)

carreauInsee1000$Carreau_ID<-1:length(carreauInsee1000$id_c1000)


#Mask
#Mask
Mask_st_1000 <- st_union(carreauInsee1000)
Mask_st_1000<-st_sf(id = 1, geometry =Mask_st_1000 )

# Mask_simplified <-
Mask_st_1000 <- Mask_st_1000 %>%
  st_buffer(dist = 500) %>%
  st_buffer(dist = -1000) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 500) %>%
  st_buffer(500)

Cadrillage_1000<- carreauInsee1000%>%
  st_sf(geometry = .)

st_crs(Cadrillage_1000)

Cadrillage_1000<-left_join(Cadrillage_1000, Prix_annee_potentiel_1000_Maison, by="Carreau_ID")

tableau_effectif_maisons1000<-table(Prix_annee_potentiel_1000_Maison$categorie_finale)

# Plot the tiles
library(RColorBrewer)
cols <- brewer.pal(9,"Set3")
# cols <-  c("#8FA3AD","#FF73BF","#69003F","#CDDE47", "#417324" )
opacity <- 95# de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)
dev.off()
setwd("~/Projets/modele_trajectoires_structures_prix/figures_typo")
pdf(file="Typo_1999_2012_carreaux1000_Maison_2.pdf",width = 10, height = 5)

layoutLayer(title = "Typologie de marchÃ©s",
            sources = "BIEN",
            author= "Â®T.Le Corre, GÃ©ographie-CitÃ©s 2016",
            scale = 0,
            frame = FALSE,
            col = "black",
            north = TRUE,
            coltitle = "white",
            extent = carreauInsee1000)

typoLayer(x=Cadrillage_1000,
          var = "categorie_finale",
          col= cols, ##palettes de couleurs predefinies## 
          border = FALSE,
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          colNA="grey",add=T)
plot(lim_IDF.st$geometry,add=T)
dev.off()
# Plot 

Typo_1999_2012_carreaux1000_Maison<- Prix_annee_potentiel_1000_Maison

setwd("~/Projets/modele_trajectoires_structures_prix/tables_typo")
write.csv2(x=Typo_1999_2012_carreaux1000_Maison,file = "Typo_1999_2012_carreaux1000_Maison_5030tiles.csv", row.names=FALSE, fileEncoding = "UTF-8")


#######################carto residu######################

breaks_ratio<-  c(min(Prix_annee_potentiel_1000_Maison$resid), -(ecartmodel_maisons1000), - (ecartmodel_maisons1000/2), 0, +(ecartmodel_maisons1000/2), +(ecartmodel_maisons1000), max(Prix_annee_potentiel_1000_Maison$resid))
cols=carto.pal(pal1 = 'blue.pal', n1 = 3 ,pal2='red.pal', n2=3)
Cadrillage_1000<-Cadrillage_1000%>%filter(!is.na(resid))
spVente_2<-Cadrillage_1000%>%
  as( "Spatial")
proj4string(spVente_2) <-CRS("+init=epsg:2154")
spVente_2<- spTransform(spVente_2, "+init=epsg:2154")
Mask_spdf_1000<-as(Mask_st_1000, "Spatial")
proj4string(Mask_spdf_1000) <-CRS("+init=epsg:2154")
Mask_spdf_1000<- spTransform(Mask_spdf_1000, "+init=epsg:2154")

Cadrillage_1000_spdf<-as(Cadrillage_1000, "Spatial")
proj4string(Cadrillage_1000_spdf) <-CRS("+init=epsg:2154")
Cadrillage_1000_spdf<- spTransform(Cadrillage_1000_spdf, "+init=epsg:2154")

spVente_2$dummy <- 1
stewartResidus <- mcStewart(knownpts = spVente_2, 
                            unknownpts = Cadrillage_1000_spdf,
                            varname = "resid", 
                            typefct = "exponential", span = 3000, beta = 2,
                            mask = Mask_st_1000, longlat = FALSE)

stewartTransac <- mcStewart(knownpts = spVente_2, 
                            unknownpts = Cadrillage_1000_spdf,
                            varname = "dummy", 
                            typefct = "exponential", span = 3000, beta = 2,
                            mask = Mask_st_1000, longlat = FALSE)


stewartTransac_SF <- st_as_sf(stewartTransac) %>%
  rename(NbTransac = OUTPUT)
stewartTransac_df<-as.data.frame(stewartTransac_SF)
stewartResidus_SF <- st_as_sf(stewartResidus) %>% 
  rename(Residus = OUTPUT)


stewartResidus_SF <- left_join(stewartResidus_SF %>%
                                 select(Residus,Carreau_ID),
                               stewartTransac_df%>%
                                 select(NbTransac,Carreau_ID),  
                               by="Carreau_ID") %>%
  mutate(ResidusPotentiel = Residus/ NbTransac)

#attention focntionne pas si environnement de travail pas precise
setwd("~/Sauvegarde_figures/residus_Plot")
pdf(file="maisons_PlotResidus_carreau_1000.pdf",width = 10, height = 5)



col_mask<-"#878787"
opacity_mask <-60
col_mask <- paste0(col_mask, opacity_mask)
plot(Mask_st_1000, col= col_mask, border=NA, main=NULL)

choroLayer(x=stewartResidus_SF,
           var = "ResidusPotentiel",
           col = cols,
           colNA = col_mask,
           breaks = breaks_ratio,
           border = NA,
           legend.title.txt = "resid_maison_carreau_1000",
           legend.pos = "topright", 
           legend.values.rnd = -2,add=T)
plot(lim_IDF.st$geometry,add=T)


dev.off()

