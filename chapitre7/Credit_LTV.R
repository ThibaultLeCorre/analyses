library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)
library(RColorBrewer)

#Boucle_prix_immo_200
# orginals_data_BIEN<-orginals_data_BIEN%>%filter(cluster!=0, ID!=403773 & ID!=253617&ID!=570130&ID!=910347)
# data_redresse<- orginals_data_BIEN %>%
#   bind_rows(orginals_data_BIEN %>% filter(comurb2012=="1"))

Transac_acquereur<- data_redresse%>% 
  filter(`X.x`!= 0 & `Y.x`!=0, !is.na(REQ_PRIX), MTCRED>0)%>%
  select(ID,REQ_PRIX,MTCRED,annee.x,X.x, Y.x)
#construire variable quantitative (notée en booléen) à partir de presence crdit si oui = 1 , si non = 0
# Transac_acquereur$Credit <- ifelse(Transac_acquereur$PRESCREDIT=="O" | Transac_acquereur$PRESCREDIT=="0", 1, 0)
test<- Transac_acquereur


#couleurs
# cols=carto.pal(pal1 = 'blue.pal',n1 = 4,pal2='red.pal', n2=6,  transparency = TRUE)
cols=carto.pal(pal1 = 'red.pal', n1=10, transparency = TRUE)

# cols=c("#23b28d","#5ba483","#769678","#89896d","#967b63","#9f6c57","#a75d4c","#ac4d40","#af3a32","#b12322")
# cols=c("#006837","#1a9850","#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61","#f46d43","#d73027","#a50026")
# opacity <- 80 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
# cols <- paste0(cols, opacity)
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
  summarise(LTV = sum(MTCRED)/sum(REQ_PRIX) *100)
carreaux_resumes <-carreaux_resumes %>% filter(!is.na(LTV) & LTV <150 & LTV>0)
Cadrillage_200<-carreaux_resumes%>% select(Carreau_ID,geometry)
cadrillage_200_spdf<-as(Cadrillage_200, "Spatial")
proj4string(cadrillage_200_spdf) <-CRS("+init=epsg:2154")
cadrillage_200_spdf<- spTransform(cadrillage_200_spdf, "+init=epsg:2154")


library(ggplot2)
ggplot(carreaux_resumes, aes(x=LTV)) + geom_histogram(binwidth = 1)

breaks_ratio<-getBreaks(carreaux_resumes$LTV,nclass = 10,method="quantile")

########

##Preparation tableaux pour étape 2#
annees <- unique(Cadrillage_200_jointure$annee.x)
annees <- sort(annees[!is.na(annees)])

Montableau_LTV_credit<- data.frame(Carreau_ID=NA,Prix=NA,credit=NA,LTV_credit=NA,Annee=NA)
Montableau_LTV_credit<-Montableau_LTV_credit%>%filter(!is.na(Annee))

##bocle a realiser a partir de ce moment

#fermer periph plot pour sauvegarde
dev.off()

#etape 2 : boucle
for (cetteAnneeLa in annees){
  
  spVente_2<-spVente%>% filter(annee.x==cetteAnneeLa)%>%
    as( "Spatial")
  proj4string(spVente_2) <-CRS("+init=epsg:2154")
  spVente_2<- spTransform(spVente_2, "+init=epsg:2154")
  
  
 
  stewartCredit <- mcStewart(knownpts = spVente_2, 
                             unknownpts = cadrillage_200_spdf,
                             varname = "MTCRED", 
                             typefct = "exponential", span = 500, beta = 2,
                             mask = Mask_spdf_200, longlat = FALSE)
  
  stewartPrix <- mcStewart(knownpts = spVente_2, 
                              unknownpts = cadrillage_200_spdf,
                              varname = "REQ_PRIX", 
                              typefct = "exponential", span = 500, beta = 2,
                              mask = Mask_spdf_200, longlat = FALSE)
  
  
  stewartPrix_SF <- st_as_sf(stewartPrix) %>%
    rename(Prix = OUTPUT)
  stewartPrix_df<-as.data.frame(stewartPrix_SF)
  stewartCredit_SF <- st_as_sf(stewartCredit) %>% 
    rename(credit = OUTPUT)
  #si notation en caractere scicnetifique
  options(scipen = 999)
  
  stewartCredit_SF <- left_join(stewartCredit_SF %>%
                                  select(credit,Carreau_ID),
                                stewartPrix_df%>%
                                  select(Prix,Carreau_ID),  
                                by="Carreau_ID") %>%
    mutate(LTV_credit = (credit/ Prix)*100)
  
  #attention focntionne pas si environnement de travail pas precise
  setwd("~/Sauvegarde_figures/LTV")
  pdf(file=paste0(~"Sauvegarde_figures/LTV",cetteAnneeLa,".pdf"),width = 10, height = 5)
  tilesLayer(osmTiles)
  plot(Mask_st_200, col= col_mask, border=NA,add=T, main=NULL)
  
  choroLayer(x=stewartCredit_SF,
             var = "LTV_credit",
             col = cols,
             colNA = col_mask,
             breaks = breaks_ratio,
             border = NA, 
             legend.title.txt = cetteAnneeLa,
             legend.pos = "topright", add=T)
  
  
  stewartCredit_df<-as.data.frame(stewartCredit_SF)
  
  stewartCredit_df$Annee <- cetteAnneeLa
  
  Montableau_LTV_credit<-bind_rows(Montableau_LTV_credit, stewartCredit_df)
  
  
  dev.off()
  # rm(spVente_2,stewartPrix, stewartTransac, stewartTransac_SF, stewartPrix_SF )
  
}

Montableau_LTV_credit_FINAL<- Montableau_LTV_credit
rm(carreaux_resumes, breaks_ratio)

