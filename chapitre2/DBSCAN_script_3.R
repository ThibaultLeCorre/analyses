
 orginals_data_BIEN<-BIEN_903894rows_128cols%>%filter(!is.na(X) &!is.na(Y))
 library(sf)
spVente <- st_as_sf(orginals_data_BIEN,
                    
                    coords = c("X","Y"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)




#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]   
spVente$Ylamb93<-coords[,2]
spVente_spdf<-as(spVente , "Spatial")
spVente_df<- as.data.frame(spVente_spdf@data)

proj4string(spVente_spdf) <-CRS("+init=epsg:2154")
library(rgdal)
spVente_spdf<- spTransform(spVente_spdf, "+init=epsg:2154")

blob.df<-spVente_df
##
blob.df$Y<- as.integer(blob.df$Ylamb93)
blob.df$X<- as.integer(blob.df$XLamb93)
blob.df<- blob.df[,c(2,6,130,131)]
row.names(blob.df)<-blob.df$ID

# annees<- unique(blob.df$annee)
tableau_RECUP_CLUSTER<- blob.df
row.names(tableau_RECUP_CLUSTER)<-blob.df$ID
tableau_RECUP_CLUSTER$cluster = rep(-1,nrow(tableau_RECUP_CLUSTER))
tableau.db=list()
tableau.fpc=list()

library(dbscan)
length(kNNdist(blob.df[blob.df$annee==1999,c(3,4)], k =  1))
kNNdistplot(blob.df[blob.df$annee==1999,c(3,4)], k =  1000)
abline(h = 10000, lty = 2, col="red")


??kNNdisplot
for (year in unique(blob.df$annee)) {
  show(year)
  # blob.df<-blob.df
  ##
  #trouver le rayon idÃ©al des cercles#
  kNNdistplot(blob.df[blob.df$annee==year,c(3,4)], k =  5)
  abline(h = 1000, lty = 2)
  
  ggsave("Partition_2008.png",plot= partition_2008, "~/Projets/DBSCAN/DBSAN",device = "png", width = 220, height = 150, units = "mm", dpi = 200)
  
  # dbscan package
  res.db <- dbscan::dbscan(blob.df[blob.df$annee==year,c(3,4)], 1000,5,borderPoints = T)
  res.db$ID<-row.names(blob.df[blob.df$annee==year,])
  # Compute DBSCAN using fpc package
  # set.seed(50000000000)
  #res.fpc <- fpc::dbscan(blob.df[blob.df$annee==year,c(3,4)], eps =1000, MinPts =5)
  #res.fpc$ID<-row.names(blob.df[blob.df$annee==year,])
  
  show("dbscan ok")
  
  tableau.db[[as.character(year)]]<- res.db
  show("tab ok")
  # Make sure that both version produce the same results:
  #all(res.fpc$cluster == res.db$cluster)
  #dataframe
  
  tableau_RECUP_CLUSTER$cluster[blob.df$annee==year]<-res.db$cluster
}


plot(tableau.db[["1996"]], blob.df, main = "DBSCAN", frame = FALSE)
library(extrafont)
loadfonts(device="win")
?fviz_cluster
library(factoextra)
fviz_cluster(as(tableau.db[["1996"]],'dbscan_fast'), points.df[points.df$annee==1996,c(3,4)], 
             outlier.shape = ,show.clust.cent = TRUE, 
             ellipse = TRUE,labelsize = 0,  geom = "point",repel = TRUE,
             ellipse.type = "convexe", outlier.color = "black",main = "1996",
             ellipse.alpha = 0, shape = "points" ) + theme_tmd()

partition_2008<-fviz_cluster(as(tableau.db[["2008"]],'dbscan_fast'), blob.df[blob.df$annee==2008,c(4,3)],
             stand = TRUE,
             show.clust.cent = F, ellipse = TRUE, ellipse.type = "convex",
             labelsize = 0,  geom = "point",repel = F, 
             pointsize =0.1 , outlier.color = "red",main = 2012,
             ellipse.alpha = 0.2, shape = "blob" )+ 
    # coord_quickmap(xlim=range(blob.df$X), ylim=range(blob.df$Y))+
  theme_tmd()+
   theme(legend.position="none", axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())+
  geom_point(size=0.1) +
  labs(title = NULL, x= NULL, y= NULL)
  # labs(caption = "Sources : échantillon BD BIEN (non redressé) ; Réalisation : Thibault Le Corre, Géographie-Cités 2017\ncalcul réalisé sour R Studio avec le package dbscan\nFigure réalisée avec le package ggplot2")

ggsave("Partition_2012.pdf",plot= partition_2012, "~/Projets/DBSCAN/DBSAN",device = "pdf", width = 220, height = 200, units = "mm", dpi = 200)
ggsave("Partition_2008.png",plot= partition_2008, "~/Projets/DBSCAN/DBSAN",device = "png", width = 220, height = 150, units = "mm", dpi = 200)

#sortie_plot#

DBSCAN_results_table$typeVar<- ifelse (DBSCAN_results_table$cluster==0,  "Transactions écartées : en dehors d'un cluster",
                                       ifelse (DBSCAN_results_table$cluster>=1 , "Transactions retenues : à l'intérieur d'un cluster",NA ))

table(DBSCAN_results_table$typeVar)


library(ggplot2)
dev.off()
Partionnement<-ggplot(DBSCAN_results_table, aes(X, Y, group=typeVar),xlab=F, ylab=F) +
  geom_point(aes(color =typeVar ),size=0.1, show.legend =T) + labs(x=NULL, y=NULL)+
  coord_quickmap(xlim=range(DBSCAN_results_table$X), ylim=range(DBSCAN_results_table$Y))+
  facet_wrap(~annee,ncol = 3, nrow = 4)+ theme_tmd()+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())+
  labs(title = "Partionnement du semis de points en vue de l'analyse", x= NULL, y= NULL)+
  labs(caption = "Sources : échantillon BD BIEN (non redressé, n = 901843) ; Réalisation : Thibault Le Corre, Géographie-Cités 2017\ncalcul réalisé sour R Studio avec le package dbscan\nFigure réalisée avec le package ggplot2")


ggsave("Partionnement.png",plot= Partionnement, "Figures_DBSCAN", device = "png", width = 150, height = 150, units = "mm", dpi = 330)
ggsave("Partionnement.png",plot= Partionnement, "C:/Users/Doctorant3/Documents/Thibault/Sync/Work_on_DATA/images et cartes/Figures_DBSCAN",device = "png", width = 220, height = 200, units = "mm", dpi = 200)


theme_tmd <- function(){
  tmd <-  theme(
    legend.title = element_text(size = 0, colour = NA,family = "Century Gothic"),
    legend.position = 'bottom',
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA),
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = NA),
    # panel.grid.major = element_line(colour = "gray92", size = 1),
    legend.text = element_text(size = 10,family = "Century Gothic"),
    plot.margin = unit(x = c(0.25, 0.25, 0.25, 0.5), units = "cm"),
    plot.caption= element_text(size = 8,family = "Century Gothic"),
    legend.box.spacing = unit(x = 0.25, units = "cm"),
    legend.margin=margin(t = 0, r = 0, b = -0.2, l = 0,unit = "cm"),
    plot.title=element_text(size =12,family = "Century Gothic",face="bold",hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size =10,family = "Century Gothic"),
    plot.subtitle = element_text(size =10,family = "Century Gothic"))
}


write.table(tableau_RECUP_CLUSTER,file = "DBSCAN_results_table", append=T)
write.csv()
table(DBSCAN_results_table$cluster)
