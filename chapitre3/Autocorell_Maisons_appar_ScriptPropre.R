#Packages
library (sf)
library(tidyr)
library (dplyr)
library (usdm)
library(SpatialPosition)
library(cartography)
library(fasterize)
library(spdep)
library(ggplot2)
library(sparcl)


#Import tableaux Prix
data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
######
Montableau_Moran_Appartement<- data.frame(CeNombredeVoisinsLa=NA,resultat_moran=NA,cetteAnneeLa=NA)
Montableau_Moran_Appartement<-Montableau_Moran_Appartement%>%filter(!is.na(cetteAnneeLa))


Montableau_LISA_Appartement<- data.frame(Nombre_voisins=NA,resultat_moran=NA,Annee=NA, Moran_type=NA, ID=NA, ID_data_redresses=NA, resid=NA)
Montableau_LISA_Appartement<-Montableau_LISA_Appartement%>%filter(!is.na(Annee))


Nombre_voisins<- c(5,10,20,50,100,200)
Nombre_voisins<-sort(Nombre_voisins)
#Donnees
data_redresse1570533transacs$ID_data_redresses<-row.names(data_redresse1570533transacs)
Transac_acquereur<- data_redresse1570533transacs%>% 
  filter(`X.x`!= 0 & `Y.x`!=0,REQTYPBIEN=="AP", REQ_PM2>0)%>%
  select(REQ_PM2,annee.x,X.x, Y.x,ID,ID_data_redresses)


annees <- unique(Transac_acquereur$annee.x)
annees <- sort(annees[!is.na(annees)])

dev.off()
for (cetteAnneeLa in annees){
  
  test<- Transac_acquereur%>% filter(annee.x==cetteAnneeLa)
  
  
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
  
  # "Changement SP"
  spVente_2<-spVente%>% 
    as( "Spatial")
  proj4string(spVente_2) <-CRS("+init=epsg:2154")
  spVente_2<- spTransform(spVente_2, "+init=epsg:2154")
  
  #
  for (CeNombredeVoisinsLa in Nombre_voisins){
    map<-spVente_2
    tab<-spVente_2@data
    #Standradisé prix, sera variable Y dans le modèle de Moran
    tab$Y_std<-scale(tab$REQ_PM2)
    
    #Coords + id
    coords <- coordinates(map)
    ID_transacs<-map@data$ID_data_redresses
    
    
    #Auto crorelation sur plus proches voisins en raison de l'irregularité annuelle du semsis de points
    map_nb<-knearneigh(coords, k=CeNombredeVoisinsLa, longlat=FALSE)
    
    map_nb<-knn2nb(map_nb)
    
    map_nb_w<-nb2listw(map_nb, zero.policy=TRUE)
    
    
    tab$Y_lag<-lag.listw(map_nb_w,tab$REQ_PM2)
    tab$Y_std_lag<-lag.listw(map_nb_w,tab$Y_std)
    head(tab)
    
    cor.test(tab$REQ_PM2,tab$Y_lag, na.rm=TRUE)
    
    model<-lm(tab$Y_lag~tab$REQ_PM2)
    
    summary(model)
    
    # Resulat r² meme que resulatat test de moran
    # rsquare<-model$coefficients[[2]]
    
    #garder résulatat test d emoran dans nouvel objet
    test_de_moran<-moran.test(tab$REQ_PM2,map_nb_w,zero.policy = TRUE)
    
    resultat_moran<- test_de_moran$estimate[[1]]
    resultat_moran<-round(resultat_moran, digits=2)
    resultat_moran_texte<- paste0("Indice de Moran sur valeurs standardisées",sep="=", resultat_moran )
    test_de_moran_std<-lm(tab$Y_std_lag~tab$Y_std)
    

    Results_df_moran<- data.frame(cetteAnneeLa,CeNombredeVoisinsLa,resultat_moran)
    Montableau_Moran_Appartement<-bind_rows(Montableau_Moran_Appartement, Results_df_moran)
    
    
    
    ##LISA
    
    
    locm<-localmoran(tab$REQ_PM2,map_nb_w,alternative = "two.sided", zero.policy = TRUE)
    tab2<-as.data.frame(locm)
    tab<-as.data.frame(tab)
    tabres<-cbind(tab,tab2)
    head(tabres)
    
    #carto
    
    # Carte de Moran
    q1<-as.factor(tabres$Y_std>0)
    levels(q1)<-c("Low","High")
    q2<-as.factor(tabres$Y_std_lag>0)
    levels(q2)<-c("Low","High")
    MapMoran<-paste(as.character(q1),as.character(q2),sep="-")
    MapMoran[abs(tabres$Z.Ii)<1.9]<-"Non Sign."
    
    labels=c("High-High","Low-Low","High-Low","Low-High","Non Sign.")
    tab$Moran_type<-factor(MapMoran,levels=labels)
    tab$Moran_color<-tab$Moran_type
    colors=c("red","blue","lightpink","skyblue2","grey")
    levels(tab$Moran_color)<-colors
    tab$Moran_color<-as.character(tab$Moran_color)
    
 
    
    
    resid<-test_de_moran_std$residuals
    
    tab<- cbind(tab, resid)
    tab$Annee <- cetteAnneeLa
    tab$Nombre_voisins<-CeNombredeVoisinsLa
    
    Results_df_LISA<- data.frame(tab[,c("Annee","Nombre_voisins","Moran_type", "ID", "ID_data_redresses", "resid")])
    
    
    Montableau_LISA_Appartement<-bind_rows(Montableau_LISA_Appartement,Results_df_LISA)
    
    
  }
  
}



spVente <- st_as_sf(test,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)

Montableau_Moran_maison<- data.frame(CeNombredeVoisinsLa=NA,resultat_moran=NA,cetteAnneeLa=NA)
Montableau_Moran_maison<-Montableau_Moran_maison%>%filter(!is.na(cetteAnneeLa))


Montableau_LISA_maison<- data.frame(Nombre_voisins=NA,resultat_moran=NA,Annee=NA, Moran_type=NA, ID=NA, ID_data_redresses=NA, resid=NA)
Montableau_LISA_maison<-Montableau_LISA_maison%>%filter(!is.na(Annee))


Nombre_voisins<- c(5,10,20,50,100,200)
Nombre_voisins<-sort(Nombre_voisins)
#Donnees
data_redresse1570533transacs$ID_data_redresses<-row.names(data_redresse1570533transacs)
Transac_acquereur<- data_redresse1570533transacs%>% 
  filter(`X.x`!= 0 & `Y.x`!=0,REQTYPBIEN=="MA", REQ_PRIX>0)%>%
  select(REQ_PRIX,annee.x,X.x, Y.x,ID,ID_data_redresses)


annees <- unique(Transac_acquereur$annee.x)
annees <- sort(annees[!is.na(annees)])

dev.off()
for (cetteAnneeLa in annees){
  
  test<- Transac_acquereur%>% filter(annee.x==cetteAnneeLa)
  
  
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
  
  # "Changement SP"
  spVente_2<-spVente%>% 
    as( "Spatial")
  proj4string(spVente_2) <-CRS("+init=epsg:2154")
  spVente_2<- spTransform(spVente_2, "+init=epsg:2154")
  
  #
  for (CeNombredeVoisinsLa in Nombre_voisins){
    map<-spVente_2
    tab<-spVente_2@data
    #Standradisé prix, sera variable Y dans le modèle de Moran
    tab$Y_std<-scale(tab$REQ_PRIX)
    
    #Coords + id
    coords <- coordinates(map)
    ID_transacs<-map@data$ID_data_redresses
    
    
    #Auto crorelation sur plus proches voisins en raison de l'irregularité annuelle du semsis de points
    map_nb<-knearneigh(coords, k=CeNombredeVoisinsLa, longlat=FALSE)
    
    map_nb<-knn2nb(map_nb)
    
    map_nb_w<-nb2listw(map_nb, zero.policy=TRUE)
    
    
    tab$Y_lag<-lag.listw(map_nb_w,tab$REQ_PRIX)
    tab$Y_std_lag<-lag.listw(map_nb_w,tab$Y_std)
    head(tab)
    
    cor.test(tab$REQ_PRIX,tab$Y_lag, na.rm=TRUE)
    
    model<-lm(tab$Y_lag~tab$REQ_PRIX)
    
    summary(model)
    
    # Resulat r² meme que resulatat test de moran
    # rsquare<-model$coefficients[[2]]
    
    #garder résulatat test d emoran dans nouvel objet
    test_de_moran<-moran.test(tab$REQ_PRIX,map_nb_w,zero.policy = TRUE)
    
    resultat_moran<- test_de_moran$estimate[[1]]
    resultat_moran<-round(resultat_moran, digits=2)
    resultat_moran_texte<- paste0("Indice de Moran sur valeurs standardisées",sep="=", resultat_moran )
    test_de_moran_std<-lm(tab$Y_std_lag~tab$Y_std)
    
    
    Results_df_moran<- data.frame(cetteAnneeLa,CeNombredeVoisinsLa,resultat_moran)
    Montableau_Moran_maison<-bind_rows(Montableau_Moran_maison, Results_df_moran)
    
  
    ##LISA
    
    
    locm<-localmoran(tab$REQ_PRIX,map_nb_w,alternative = "two.sided", zero.policy = TRUE)
    tab2<-as.data.frame(locm)
    tab<-as.data.frame(tab)
    tabres<-cbind(tab,tab2)
    head(tabres)
    
    #carto
    
    # Carte de Moran
    q1<-as.factor(tabres$Y_std>0)
    levels(q1)<-c("Low","High")
    q2<-as.factor(tabres$Y_std_lag>0)
    levels(q2)<-c("Low","High")
    MapMoran<-paste(as.character(q1),as.character(q2),sep="-")
    MapMoran[abs(tabres$Z.Ii)<1.9]<-"Non Sign."
    
    labels=c("High-High","Low-Low","High-Low","Low-High","Non Sign.")
    tab$Moran_type<-factor(MapMoran,levels=labels)
    tab$Moran_color<-tab$Moran_type
    colors=c("red","blue","lightpink","skyblue2","grey")
    levels(tab$Moran_color)<-colors
    tab$Moran_color<-as.character(tab$Moran_color)
    
    
    
    
    resid<-test_de_moran_std$residuals
    tab<- cbind(tab, resid)
    tab$Annee <- cetteAnneeLa
    tab$Nombre_voisins<-CeNombredeVoisinsLa
    
    Results_df_LISA<- data.frame(tab[,c("Annee","Nombre_voisins","Moran_type", "ID", "ID_data_redresses", "resid")])
    
    
    Montableau_LISA_maison<-bind_rows(Montableau_LISA_maison,Results_df_LISA)
    
    
  
  }
  
}



########
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

setwd("~/Shapes/datidf")
list.files()
carreauInsee1000 <- st_read("car1000m_idf_reparGeom.shp",
                            stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee1000 <-  st_transform(carreauInsee1000, crs = 2154)

Cadrillage_1000<- carreauInsee1000%>%
  st_sf(geometry = .)

Cadrillage_1000$Carreau_ID<-1:length(Cadrillage_1000$id_c1000)

#######
##########



Transac_acquereur<- data_redresse1570533transacs%>% 
  filter(`X.x`!= 0 & `Y.x`!=0)%>%
  select(ID_data_redresses,X.x, Y.x)

spVente <- st_as_sf(Transac_acquereur,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)



#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]   
spVente$Ylamb93<-coords[,2]


Montableau_LISA_Appartement$Type_BIEN<-"Appartements"
Montableau_LISA_maison$Type_BIEN<-"Maisons"

Montableau_LISA<- bind_rows(Montableau_LISA_Appartement,Montableau_LISA_maison)
test<-left_join(spVente, Montableau_LISA, by = "ID_data_redresses")

str(test)
test_2<- test[,c("ID_data_redresses","Annee","Nombre_voisins","Moran_type","Type_BIEN","geometry")]


Cadrillage_1000_jointure<-st_join(Cadrillage_1000, test_2, join = st_contains, left=T)


Cadrillage_1000_jointure$Periode<-ifelse(Cadrillage_1000_jointure$Annee==1996|Cadrillage_1000_jointure$Annee==1999|Cadrillage_1000_jointure$Annee==2003, "Periode_96_2003",
                                         ifelse(Cadrillage_1000_jointure$Annee>=2004&Cadrillage_1000_jointure$Annee<=2007, "Periode_04_2007", 
                                                ifelse(Cadrillage_1000_jointure$Annee>=2008&Cadrillage_1000_jointure$Annee<=2012, "Periode_08_2012", NA)))


str(Cadrillage_1000)
test_CAH_Moran<-Cadrillage_1000_jointure%>%
  select(Carreau_ID,Periode,ID_data_redresses,Nombre_voisins,Moran_type,geometry)%>%
  filter(Nombre_voisins==10 | Nombre_voisins==100, !is.na(Moran_type))%>%
  group_by(Carreau_ID,Periode,Nombre_voisins,Moran_type)%>%
  summarise(Nombre_transacs= length(ID_data_redresses))


test_CAH_Moran2<- as.data.frame(test_CAH_Moran)%>%
  spread(Moran_type,Nombre_transacs, fill = 0)%>%
  mutate(Total_transacs = rowSums(.[5:9]))%>%
  gather("Type", "Nombre_transacs", c(5:9))%>%
  group_by(Carreau_ID, Periode, Nombre_voisins,Type) 

test_CAH_Moran2$freq<- (test_CAH_Moran2$Nombre_transacs/test_CAH_Moran2$Total_transacs)*100

test_CAH_Moran2<-test_CAH_Moran2%>%
  select(-Nombre_transacs, -Total_transacs)%>%
  # group_by(Carreau_ID,Periode,Nombre_voisins)%>%
  spread (Type,freq, fill=0)



test_CAH_Moran2<-as.data.frame(test_CAH_Moran2)

####CAH sur les odds ratio#

myDF <- subset(test_CAH_Moran2[,c(5:9)])
#Sans CSP2


str(myDF)

library(ade4)
library(FactoClass)
AFC <- dudi.coa(df=myDF, scannf=FALSE, nf=ncol(myDF))
plot.dudi(AFC)



distMat <- dist.dudi(AFC, amongrow=TRUE)


CAH <- ward.cluster(distMat, peso = apply(X=myDF, MARGIN=1, FUN=sum) , plots = TRUE, h.clust = 1)



par(mfrow=c(1,2))
barplot(sort(CAH$height / sum(CAH$height), decreasing = TRUE)[1:15] * 100,
        xlab = "Noeuds", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie selon le partitionnement")

barplot(cumsum(sort(CAH$height / sum(CAH$height), decreasing = TRUE))[1:15] * 100,
        xlab = "Nombre de classes", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie expliquée")

dev.off()

myDF$clusters <- cutree(tree = CAH, k = 7)


par(mfrow=c(1,1))
ordreClasses <- unique(myDF$cluster[CAH$order])

plot(as.dendrogram(CAH), leaflab = "none")
dev.off()

# # colors the leaves of a dendrogram
# 
# ggdendrogram(CAH, rotate = FALSE, size = 2)
# y = cutree(CAH, 7)
# ColorDendrogram(CAH, y = y, labels = names(y), main = "My Simulated Data")




############################################

mytest_LISA<-cbind(test_CAH_Moran2,myDF[,c(6)] )
names(mytest_LISA)[10]<-"clusters"



# pdf(file=paste0("~/Projets/Chapitre7/CAH_acquereur_residents/Odd_ratio/CAHOddratio_ValeurAxes.pdf"),width = 10, height = 5)

mytest_LISA$clusters <-  recode(mytest_LISA$clusters, "1"= "4.Voisinages hétérogènes",
                                "2"= "7.Valeurs faibles dans un voisinage de valeurs faibles",
                                "3"= "6.Voisinages mixtes de valeurs faibles et hétérogènes",
                                "4"= "3.Valeurs élevées dans un voisinage de valeurs faibles",
                                "5"= "2.Voisinages mixtes de valeurs élevées et hétérogènes",
                                "6"= "5.Valeurs faibles dans un voisinage de valeurs élevées",
                                "7"= "1.Valeurs élevées dans un voisinage de valeurs élevées")

specificCol <- c("1.Valeurs élevées dans un voisinage de valeurs élevées"="#FF0000",
  "2.Voisinages mixtes de valeurs élevées et hétérogènes"= "#984856",
  "3.Valeurs élevées dans un voisinage de valeurs faibles"= "#ffb6c1" ,
  "4.Voisinages hétérogènes"="#BBBCBC",
  "5.Valeurs faibles dans un voisinage de valeurs élevées"="#7EC0EE", 
   "6.Voisinages mixtes de valeurs faibles et hétérogènes"="#8B84D7",
  "7.Valeurs faibles dans un voisinage de valeurs faibles"= "#00008B" )
# "#00617F"
mytest_Lisa_plot<- mytest_LISA %>%
  group_by(clusters) %>%
  # summarise_all(funs(mean),na.rm=T) %>%
  summarise_at(.vars = vars(`High-High`:`Non Sign.`), .funs = funs(mean(.)))%>%
  gather(key = "Dimension", value = "Data", 2:6) %>%
  ggplot() +
  geom_bar(aes(Dimension, Data, fill=factor(clusters)), stat = "identity") +
   scale_fill_manual(values= specificCol)+
  ggtitle("") +
  labs(subtitle = "") +
  xlab("Pourcentage de transactions correspondantes à la classe du LISA") +
  ylab("Classe du LISA") +
  theme(legend.position='none') +
  labs(caption = "Sources : Echantillon BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~clusters) +
  coord_flip()


setwd("~/Projets/Auto_correlation/Autocorellation_spatiale/cartes_CAH_LISA")
ggsave("mytest_Lisa_plot.pdf",plot= mytest_Lisa_plot,  device = "pdf", width = 310, height = 200, units = "mm", dpi = 330)

# colors <- c("grey","blue","lightpink","red","skyblue2")


col_mask<-"#878787"
opacity_mask <-20
col_mask <- paste0(col_mask, opacity_mask)
opacity <- 98 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
specificCol <- paste0(specificCol, opacity)

#c4a4a7",
# lightpink

Periodes <- unique(mytest_LISA$Periode)
Periodes <- sort(Periodes[!is.na(Periodes)])

Voisinage <- unique(mytest_LISA$Nombre_voisins)

for (cettePeriodeLa in Periodes){
  
  carreaux_filtred <- mytest_LISA %>%
    filter(Periode == cettePeriodeLa)
  
  # carreaux_filtred <-  carreaux_filtred[sample(1: nrow(carreaux_filtred), 15000,replace=T), ]
  
  for (ceVoisinage_La in Voisinage){  
    
    carreaux_filtred_2 <- carreaux_filtred %>%
      filter(Nombre_voisins == ceVoisinage_La)

Test_map<- left_join(Cadrillage_1000, carreaux_filtred_2, by = "Carreau_ID")

Test_map<- Test_map%>%
  st_sf(geometry = .)

pdf(file=paste0("~/Projets/Auto_correlation/Autocorellation_spatiale/cartes_CAH_LISA/",cettePeriodeLa, "_", ceVoisinage_La,".pdf"),width = 10, height = 5)

  tilesLayer(osmTiles)
# plot(Mask_st_1000, col= col_mask, border=NA,main=NULL)

typoLayer(x=Test_map,
          var = "clusters",
          legend.pos = "bottomleft",
          legend.values.cex= 0.5,
          border = NA,
          legend.values.order =c("1.Valeurs élevées dans un voisinage de valeurs élevées",
            "2.Voisinages mixtes de valeurs élevées et hétérogènes",
            "3.Valeurs élevées dans un voisinage de valeurs faibles",
            "4.Voisinages hétérogènes",
            "5.Valeurs faibles dans un voisinage de valeurs élevées",
            "6.Voisinages mixtes de valeurs faibles et hétérogènes",
            "7.Valeurs faibles dans un voisinage de valeurs faibles"),
          legend.title.txt = sprintf("LISA sur %s", ceVoisinage_La),
          col = specificCol,
          colNA=NA,add=T)
plot(lim_IDF.st$geometry,add=T)
layoutLayer()

dev.off()
  }
}
