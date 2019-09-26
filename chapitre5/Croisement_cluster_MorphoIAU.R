#
setwd("~/Shapes/IAU_morhpo")
list.files()

IAU_Morpho<- st_read("Decoupage_Morphologique_IDF.shp",
                    stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
plot(st_geometry(IAU_Morpho), col = "grey90", lwd = 0.2)

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



Cadrillage_200_point <- st_centroid(Cadrillage_200, "POINT")
plot(st_geometry(Morpho_Communes), col = "grey90", lwd = 0.2)

Morpho_Carreaux <-st_join(IAU_Morpho, Cadrillage_200_point,join = st_intersects, left=T)
Morpho_Carreaux <- Morpho_Carreaux %>%
  group_by(Carreau_ID)
# Morpho_Communes <- st_intersects(Cadrillage_200,IAU_Morpho )
# Morpho_Communes <-st_join(IAU_Morpho, Cadrillage_200, join = st_contains, left=T)


Morpho_Carreaux$objectid<- ifelse(Morpho_Carreaux$decoup2008==1 ,"1.ville_centre",
                             ifelse(Morpho_Carreaux$decoup2008==2 ,"2.Communes_denses_agglomération",
                                    ifelse(Morpho_Carreaux$decoup2008==3 ,"3.Communes_niveau_moyen_agglomération",
                                           ifelse(Morpho_Carreaux$decoup2008==4 ,"4.Franges_agglomération",
                                                  ifelse(Morpho_Carreaux$decoup2008==5 ,"5.Poles_urbains_Hors_agglo",
                                                         ifelse(Morpho_Carreaux$decoup2008==6,"6.Communes_polarisées_Hors_agglo",
                                                                ifelse(Morpho_Carreaux$decoup2008==7,"7.Communes_rurales",Morpho_Carreaux$decoup2008 )))))))
Morpho_Carreaux<-as.data.frame(Morpho_Carreaux)
Morpho_Carreaux<-left_join(Cadrillage_200,Morpho_Carreaux[,c("Carreau_ID","objectid")])
Morpho_Carreaux<-as.data.frame(Morpho_Carreaux)
Morpho_Carreaux<-left_join(SpUnitt_with_cluster[,c("Carreau_ID","cettePeriodeLa","cah$data.clust$clust")],Morpho_Carreaux[,c("Carreau_ID","objectid")])


Morpho_Carreaux_1<-Morpho_Carreaux%>%
  filter(cettePeriodeLa=="Periode_08_2012")
options(digits=2)
table_IAU_Cluster_1<-as.data.frame(prop.table(table(Morpho_Carreaux_1$objectid,Morpho_Carreaux_1$`cah$data.clust$clust`,Morpho_Carreaux_1$cettePeriodeLa,deparse.level = 1),margin=2)*100)
table_IAU_Cluster_2<-as.data.frame(prop.table(table(Morpho_Carreaux_1$objectid,Morpho_Carreaux_1$`cah$data.clust$clust`,Morpho_Carreaux_1$cettePeriodeLa,deparse.level = 1),margin=2)*100)
table_IAU_Cluster_3<-as.data.frame(prop.table(table(Morpho_Carreaux_1$objectid,Morpho_Carreaux_1$`cah$data.clust$clust`,Morpho_Carreaux_1$cettePeriodeLa,deparse.level = 1),margin=2)*100)
table_IAU_Cluster<-rbind(table_IAU_Cluster_1,table_IAU_Cluster_2,table_IAU_Cluster_3)

table_IAU_Cluster<-
table_IAU_Cluster%>%
  spread(Var2, Freq)
rm(Morpho_Carreaux_1)
grid.table(table_IAU_Cluster, rows=NULL)
?grid.table


Communes_Carreaux <-st_join(CommunesInsee, Cadrillage_200_point,join = st_intersects, left=T)
