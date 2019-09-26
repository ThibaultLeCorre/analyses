library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)

 Montableau_Maisons_communes <- read.csv2("~/Projets/Prix_evol_2/table_results_prix_evol_2/Montableau_Maisons_communes.csv", stringsAsFactors=FALSE)

 Montableau_Appartement_communes <- read.csv2("~/Projets/Prix_evol_2/table_results_prix_evol_2/Montableau_Appartement_communes.csv", stringsAsFactors=FALSE)
 # cols=c("#23b28d","#5ba483","#769678","#89896d","#967b63","#9f6c57","#a75d4c","#ac4d40","#af3a32","#b12322")
 cols=c("#006837","#1a9850","#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61","#f46d43","#d73027","#a50026")

 # opacity <- 80 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
 # cols <- paste0(cols, opacity)
 col_mask<-"#878787"
 opacity_mask <-10
 col_mask <- paste0(col_mask, opacity_mask)
 
 
 
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

 
 Montableau_Prix$DepCom<- ifelse(Montableau_Prix$DepCom == "75101" , "75001",
                                         ifelse(Montableau_Prix$DepCom == "75102" , "75002",
                                                ifelse(Montableau_Prix$DepCom == "75103" , "75003",
                                                       ifelse(Montableau_Prix$DepCom == "75104" , "75004",
                                                              ifelse(Montableau_Prix$DepCom == "75105" , "75005",
                                                                     ifelse(Montableau_Prix$DepCom == "75106" , "75006",
                                                                            ifelse(Montableau_Prix$DepCom == "75107" , "75007",
                                                                                   ifelse(Montableau_Prix$DepCom == "75108" , "75008",
                                                                                          ifelse(Montableau_Prix$DepCom == "75109" , "75009",
                                                                                                 ifelse(Montableau_Prix$DepCom == "75110" , "75010",
                                                                                                        ifelse(Montableau_Prix$DepCom == "75111" , "75011",
                                                                                                               ifelse(Montableau_Prix$DepCom == "75112" , "75012",
                                                                                                                      ifelse(Montableau_Prix$DepCom == "75113", "75013",
                                                                                                                             ifelse(Montableau_Prix$DepCom == "75114" , "75014",
                                                                                                                                    ifelse(Montableau_Prix$DepCom == "75115" , "75015",
                                                                                                                                           ifelse(Montableau_Prix$DepCom == "75116" , "75016",
                                                                                                                                                  ifelse(Montableau_Prix$DepCom == "75117" , "75017",
                                                                                                                                                         ifelse(Montableau_Prix$DepCom == "75118" , "75018",
                                                                                                                                                                ifelse(Montableau_Prix$DepCom == "75119" , "75019",
                                                                                                                                                                       ifelse(Montableau_Prix$DepCom == "75120" , "75020",Montableau_Prix$DepCom))))))))))))))))))))

 
 #############
 
 names(Montableau_Maisons_communes)[3]<-"Prix"
 Montableau_Maisons_communes$Type<-"Maisons"
 names(Montableau_Appartement_communes)[3]<-"Prix"
 Montableau_Appartement_communes$Type<-"Appartements"
 

 Montableau_Prix<-bind_rows(Montableau_Appartement_communes, Montableau_Maisons_communes)
 
 Montableau_Prix<-Montableau_Prix[,c(3:5,8)]
 Montableau_Prix$DepCom<-as.integer(Montableau_Prix$DepCom)
 
 CommunesInsee$DepCom<-as.integer(CommunesInsee$DepCom)
 CommunesInsee_jointure<- left_join(CommunesInsee, Montableau_Prix)
 
 
 annees <- unique(CommunesInsee_jointure$Annee)
 annees <- sort(annees[!is.na(annees)])
 
 Type_BIEN<- unique(CommunesInsee_jointure$Type)
 Type_BIEN <- sort(Type_BIEN[!is.na(Type_BIEN)])
 
 dev.off()
 
 for (cetteAnneeLa in annees){
   
   test<- Montableau_Prix%>% filter(Annee==cetteAnneeLa)
 
 
   for (CeType_BIENLa in Type_BIEN){
 
     
     test2<- test%>% filter(Type==CeType_BIENLa)
     
     valBreaks <- getBreaks(test2$Prix,nclass = 10, method =
                              "quantile", k = NULL, middle = FALSE)
     
     Test_map<- left_join(CommunesInsee, test2, by = "DepCom")
     
     Test_map<- Test_map%>%
       st_sf(geometry = .)
     
     borders <- getBorders(x = Test_map)
     
     Test_Df<- as.data.frame(Test_map)
     
     pdf(file=paste0("~/Projets/Prix_com_LTI/Cates_Prix_Disc/",cetteAnneeLa, "_", CeType_BIENLa,".pdf"),width = 10, height = 5)

     tilesLayer(osmTiles)
      # plot(CommunesInsee, col= col_mask, border=NA,main=NULL)
     
   choroLayer(x=Test_map,
               var = "Prix",
                breaks = valBreaks,
               legend.pos = "bottomleft",
               legend.values.cex= 0.5,
               border = NA,
               legend.title.txt = sprintf("Prix des %s", CeType_BIENLa),
               col = cols,
               colNA=NA,add=T)
     plot(DepInsee$geometry,lwd = 0.5, add=T)
     

     # Discontinuities
     discLayer(x = borders, df = Test_Df,
               var = "Prix", col="#005EB8", nclass=3,
               method="equal", threshold = 0.4, sizemin = 0.5,
               sizemax = 3, type = "rel",legend.values.rnd = 2,
               legend.title.txt = "Discontinuitiés de prix",
               legend.pos = "topright", add=TRUE)
     layoutLayer()
     
     dev.off()
   }
 }

 
 
 