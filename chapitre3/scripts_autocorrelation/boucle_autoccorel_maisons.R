#Import tableaux Prix
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

###Import shp
# 
# setwd("~/Shapes/shpIDF_dep_lamb93")
# list.files()
# 
# lim_IDF.st<- st_read("ile-de-france.shp",
#                      stringsAsFactors = FALSE) %>%
#   st_cast("POLYGON") %>%
#   st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
# lim_IDF.st  <-  st_transform(lim_IDF.st , crs = 2154)
# 
# 
# setwd("~/Shapes/datidf")
# list.files()
# carreauInsee200 <- st_read("car200m_idf_reparGeom.shp",
#                            stringsAsFactors = FALSE) %>%
#   st_cast("POLYGON") %>%
#   st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
# 
# carreauInsee200 <-  st_transform(carreauInsee200, crs = 2154)
# 
# 
# carreauInsee200$Carreau_ID<-1:length(carreauInsee200$TARGET_FID)
# 
# Cadrillage_200<- carreauInsee200%>%
#   st_sf(geometry = .)
# Cadrillage_200<-Cadrillage_200%>% select(Carreau_ID,geometry)
# cadrillage_200_spdf<-as(Cadrillage_200, "Spatial")
# proj4string(cadrillage_200_spdf) <-CRS("+init=epsg:2154")
# cadrillage_200_spdf<- spTransform(cadrillage_200_spdf, "+init=epsg:2154")
# 
# 
# Mask_st_200 <- st_union(carreauInsee200)
# Mask_st_200<-st_sf(id = 1, geometry =Mask_st_200 )
# 
# par(mfrow = c(1,2))
# # plot(Mask_st_200)
# # Mask_simplified <-
# Mask_st_200 <- Mask_st_200 %>%
#   st_buffer(dist = 200) %>%
#   st_buffer(dist = -400) %>%
#   st_simplify(preserveTopology = FALSE, dTolerance = 200) %>%
#   st_buffer(200)
# Mask_spdf_200<-as(Mask_st_200, "Spatial")
# proj4string(Mask_spdf_200) <-CRS("+init=epsg:2154")
# Mask_spdf_200<- spTransform(Mask_spdf_200, "+init=epsg:2154")
#################


Montableau_Moran_maison<- data.frame(CeNombredeVoisinsLa=NA,resultat_moran=NA,cetteAnneeLa=NA)
Montableau_Moran_maison<-Montableau_Moran_maison%>%filter(!is.na(cetteAnneeLa))


Montableau_LISA_maison<- data.frame(Nombre_voisins=NA,resultat_moran=NA,Annee=NA, Moran_type=NA, ID=NA, ID_data_redresses=NA, resid=NA)
Montableau_LISA_maison<-Montableau_LISA_maison%>%filter(!is.na(Annee))


Nombre_voisins<- c(5,10,20,50,100,200)
Nombre_voisins<-sort(Nombre_voisins)
#Donnees
data_redresse$ID_data_redresses<-row.names(data_redresse)
Transac_acquereur<- data_redresse%>% 
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
    
    droite_reg_coeff<-test_de_moran_std$coefficients
    
    droite_reg <- paste0("y = ",
                         round(droite_reg_coeff[2],3), "x",
                         ifelse(droite_reg_coeff[1] > 0, " + ", " - "),
                         abs(round(droite_reg_coeff[1],3)))
    
    titre_diagrammeMoran<- paste0("Diagramme de Moran pour",sep=" ", CeNombredeVoisinsLa, sep=" ","voisins en",sep=" ",cetteAnneeLa) 
    
    
    setwd("~/Projets/Auto_correlation/Autocorellation_spatiale/Diagrammes_Moran/maisons")
    # pdf(file=paste0(~"/Projets/Auto_correlation/Autocorellation_spatiale/Diagrammes_Moran/maisons",titre_diagrammeMoran,".pdf"),width = 10, height = 5)
    #diagramme moran global
    # par(mfrow=c(1,1))
    
    diagramme_moran_plot<-ggplot(tab, aes(x=Y_std, y=Y_std_lag)) +
      geom_point( size=0.1)+
      geom_abline(intercept = 0, slope = 0, color="black")+
      geom_vline( xintercept = 0, color= "black")+
      stat_smooth(method="lm", se=FALSE)+
      theme_tmd()+
      annotate("text", x = 7, y =8, label = droite_reg, fontface="italic")+
      annotate("text", x = -1, y =-1, label = "Low-Low", color="grey30")+
      annotate("text", x = -1, y =1, label = "Low-High", color="grey30")+
      annotate("text", x = 1, y =-1, label = "High-Low", color="grey30")+
      annotate("text", x = 1, y =1, label = "High-High", color="grey30")+
      labs(title = titre_diagrammeMoran , 
           subtitle = resultat_moran_texte, x= "Observed", y= "Lagged")+ 
      labs(caption = "Sources : BIEN\nRéalisation sous R avec le package stats, spdep et ggplot2\nThibault Le Corre, Géographie-Cités, 2017")
    
    ggsave(filename = paste0(titre_diagrammeMoran,".pdf"),plot=diagramme_moran_plot, device = "pdf", scale=1, width = 10, height = 5, dpi=300)
    # dev.off()
    # Annee <- as.data.frame(cetteAnneeLa)
    # Nombre_voisins<-as.data.frame(CeNombredeVoisinsLa)
    # resultat_moran<-as.data.frame(resultat_moran)
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
    
    
    
    
    
    # colors<-c("grey","blue","lightpink","red","skyblue2")
    # plot(map,col=tab$Moran_color, border=FALSE)
    # typoLayer(spdf =map,df = tab,var = "Moran_type", col= colors, lwd=.1 )
    # ?typoLayer
    # legend("topright",legend=labels, fill=colors,bty="n")
    # table(tab$Moran_type)
    
    
    resid<-test_de_moran_std$residuals
    tab<- cbind(tab, resid)
    tab$Annee <- cetteAnneeLa
    tab$Nombre_voisins<-CeNombredeVoisinsLa
    
    Results_df_LISA<- data.frame(tab[,c("Annee","Nombre_voisins","Moran_type", "ID", "ID_data_redresses", "resid")])
    
    
    Montableau_LISA_maison<-bind_rows(Montableau_LISA_maison,Results_df_LISA)
    
    
    #########################################
    
    
    # getBreaks(resid, method = "sd", nclass=10)
    
    breaks_ratio_resid_moran<-c(min(resid), -sd(resid)*2, -sd(resid), 0, sd(resid), sd(resid)*2, max(resid))
    
    
    spMoran <- st_as_sf(tab,
                        
                        coords = c("XLamb93", "Ylamb93"),
                        
                        agr = "constant",
                        
                        crs = 2154,
                        
                        stringsAsFactors = FALSE)
    spMoran<-spMoran%>%
      as( "Spatial")
    proj4string(spMoran) <-CRS("+init=epsg:2154")
    spMoran<- spTransform(spMoran, "+init=epsg:2154")
    
    
    cols=carto.pal(pal1 = 'blue.pal', n1 = 3 ,pal2='red.pal', n2=3)
    titre_ResidusMoran<- paste0("Cartes des résidus de Moran pour",sep=" ", CeNombredeVoisinsLa, sep=" ","voisins en",sep=" ",cetteAnneeLa) 
    
    
    
    spMoran@data$dummy <- 1
    
    col_mask<-"#878787"
    opacity_mask <-60
    col_mask <- paste0(col_mask, opacity_mask)
    
    setwd("~/Projets/Auto_correlation/Autocorellation_spatiale/Cartes_residusecart_moran/maisons")
    pdf(file=paste0(~"/Projets/Auto_correlation/Autocorellation_spatiale/Cartes_residusecart_moran/maisons",titre_ResidusMoran,".pdf"),width = 10, height = 5)
    
    plot(Mask_spdf_200, col= col_mask, border=NA, main=NULL)
    
    smoothLayer(spdf = spMoran, df = spMoran@data,
                var = 'resid', var2 = 'dummy',
                span = 500, beta = 2,
                breaks= breaks_ratio_resid_moran,
                col=cols,
                mask = Mask_spdf_200,
                legend.title.txt = titre_ResidusMoran,
                legend.values.rnd = 2,
                legend.pos = "topright", add=T)
    
    
    # choroLayer(x=Communes_result,
    #            var = "resid",
    #            col = cols,
    #            colNA = col_mask,
    #            breaks = breaks_ratio,
    #            border = NA,
    #            legend.title.txt = "resid_maison_communes",
    #            legend.pos = "topright", 
    #            legend.values.rnd = -2,add=T)
    plot(lim_IDF.st$geometry,add=T)
    # 
    # plot(lim_IDF.st$geometry)
    
    dev.off()
    
  }
  
}
