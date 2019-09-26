library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)

Prix_annee_communes_maisons<- Montableau_Maisons_communes[,c(3:7)]
Prix_annee_communes_maisons<- Prix_annee_communes_maisons%>%
  select(DepCom,Prix_moyen,Annee)%>%
  group_by(DepCom)%>%
  filter(!is.na(Prix_moyen))%>%
  spread(Annee,Prix_moyen)%>%
  filter_all(all_vars(!is.na(.)))
# Prix_annee_communes_maisons[,c(2:13)]<-as.numeric(Prix_annee_communes_maisons[,c(2:13)])
Prix_annee_communes_maisons<-as.data.frame(Prix_annee_communes_maisons)

# ?arrange

test.cor<-cor (Prix_annee_communes_maisons$`2012`,Prix_annee_communes_maisons$`1999`, use="complete.obs")
result_cor_maison_communes<-test.cor^2

Y <- Prix_annee_communes_maisons$`2012` # Y variables à expliquer   
X <- Prix_annee_communes_maisons$`1999` #variable explicative

reg.lin<-lm(Y~X)
Droite_regression_communes_maisons<-reg.lin[[1]]

Prix_annee_communes_maisons$test.lm.fit<-reg.lin$fitted.values
resid<-residuals.lm(reg.lin)
Prix_annee_communes_maisons$resid<-resid

ecartmodel_maisonscommunes<-sd(Prix_annee_communes_maisons$test.lm.fit)

Prix_annee_communes_maisons$categorie_variation<- ifelse(Prix_annee_communes_maisons$resid> - (ecartmodel_maisonscommunes/2) & Prix_annee_communes_maisons$resid<= (ecartmodel_maisonscommunes/2), "evolution_conforme_au_modele",
                                                              ifelse(Prix_annee_communes_maisons$resid <= - (ecartmodel_maisonscommunes/2), "evolution_plus_faible_que_la_prevision",
                                                                     ifelse(Prix_annee_communes_maisons$resid >=  (ecartmodel_maisonscommunes/2), "evolution_plus_fort_que_la _prevision", "non_rs")))


prix_bas99<- mean (Prix_annee_communes_maisons$`1999`) - sd(x = Prix_annee_communes_maisons$`1999`) 
prix_haut99<- mean (Prix_annee_communes_maisons$`1999`) + sd(x = Prix_annee_communes_maisons$`1999`) 

##1270.4
Prix_annee_communes_maisons$pos_median <-ifelse (Prix_annee_communes_maisons$`1999`<prix_haut99 &  Prix_annee_communes_maisons$`1999`>prix_bas99, "Prix initiaux dans la moyenne (mean +/- sd)",
                                                      ifelse (Prix_annee_communes_maisons$`1999`>=prix_haut99,  "Prix initiaux élevés (mean + sd)",
                                                              ifelse(Prix_annee_communes_maisons$`1999`<=prix_bas99, "Prix initiaux faibles (mean - sd)", "non.rs")))


###prix finauxmedian##

Prix_annee_communes_maisons$categorie_finale<- paste0(Prix_annee_communes_maisons$pos_median, Prix_annee_communes_maisons$categorie_variation, sep="_")



##

setwd("~/Shapes/shpIDF_dep_lamb93")
list.files()

lim_IDF.st<- st_read("ile-de-france.shp",
                     stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
lim_IDF.st  <-  st_transform(lim_IDF.st , crs = 2154)


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

#obtenir shape commune par aggregation 
CommunesInsee<- IrisInsee%>% 
  group_by(DepCom)%>%
  summarize()


#pour carreau Insee
Communes_result_maisons<- CommunesInsee%>%
  st_sf(geometry = .)



####
Communes_result_maisons$DepCom<-as.integer(Communes_result_maisons$DepCom)
Communes_result_maisons<-left_join(Communes_result_maisons, Prix_annee_communes_maisons, by="DepCom")
Communes_result_maisons<- Communes_result_maisons%>%
  st_sf(geometry = .)


tableau_effectif_maisons_communes<-table(Prix_annee_communes_maisons$categorie_finale)

dev.off()
library(RColorBrewer)
cols <- brewer.pal(9,"Set3")
# cols <-  c("#8FA3AD","#FF73BF","#69003F","#CDDE47", "#417324" )
opacity <- 95# de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)
setwd("~/Projets/modele_trajectoires_structures_prix/figures_typo")
pdf(file="Typo_1999_2012_communes_Maisons.pdf",width = 10, height = 5)

layoutLayer(title = "Typologie de marchÃ©s",
            sources = "BIEN",
            author= "Â®T.Le Corre, GÃ©ographie-CitÃ©s 2016",
            scale = 0,
            frame = FALSE,
            col = "black",
            north = TRUE,
            coltitle = "white",
            extent = Communes_result_maisons)

typoLayer(x=Communes_result_maisons,
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



# library(RColorBrewer)
# cols <- brewer.pal(9,"Set3")
# # cols <-  c("#8FA3AD","#FF73BF","#69003F","#CDDE47", "#417324" )
# opacity <- 95# de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
# cols <- paste0(cols, opacity)



Typo_1999_2012_communes_Maisons<- Prix_annee_communes_maisons

setwd("~/Projets/modele_trajectoires_structures_prix/tables_typo")
write.csv2(x=Typo_1999_2012_communes_Maisons,file = "Typo_1999_2012_533communes_Maisons", row.names=FALSE, fileEncoding = "UTF-8")



#######################carto residu######################

breaks_ratio<-  c(min(Prix_annee_communes_maisons$resid), -(ecartmodel_maisonscommunes), - (ecartmodel_maisonscommunes/2), 0, +(ecartmodel_maisonscommunes/2), +(ecartmodel_maisonscommunes), max(Prix_annee_communes_maisons$resid))
cols=carto.pal(pal1 = 'blue.pal', n1 = 3 ,pal2='red.pal', n2=3)
Communes_result_maisons<-Communes_result_maisons%>%filter(!is.na(resid))
setwd("~/Sauvegarde_figures/residus_Plot")
pdf(file="Maisons_PlotResidus_communes.pdf",width = 10, height = 5)

plot(lim_IDF.st$geometry)


choroLayer(x=Communes_result_maisons,
           var = "resid",
           col = cols,
           colNA = col_mask,
           breaks = breaks_ratio,
           border = NA,
           legend.title.txt = "resid_maison_communes",
           legend.pos = "topright", 
           legend.values.rnd = -2,add=T)
plot(lim_IDF.st$geometry,add=T)


dev.off()


