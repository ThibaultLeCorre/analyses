mytest<-orginals_data_BIEN
# tableau_RECUP_CLUSTER_initial<-tableau_RECUP_CLUSTER
library(dplyr)
mytest<-left_join(mytest, tableau_RECUP_CLUSTER_initial[,c("ID","cluster")], by="ID")%>% filter(cluster!=0)
# [,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE")]

table(mytest$Profil_vendeur, useNA = "ifany")
# mytest$personne_morale_ve_rg<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"morale",
#                                       ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"morale",
#                                              ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"morale",
#                                                     ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"morale",
#                                                            ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"morale",
#                                                                   ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"morale", "particulier_ou_na"))))))
# 


mytest$personne_morale_ve_rg<- ifelse(mytest$QUALITE_VE == "AD"  &!is.na(mytest$QUALITE_VE),"ADM",
                                      ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"Promo",
                                             ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"Marchand",
                                                    ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"SAFER",
                                                           ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"Promo",
                                                                  ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"HLM", "particulier_ou_na"))))))


mytest<-mytest%>% filter(ID,REQ_ANC==2,personne_morale_ve_rg=="Promo")%>%
  select(ID,annee,X,Y)
table(mytest$personne_morale_ve_rg, useNA = "ifany")

library(sf)
spVente <- st_as_sf(mytest,
                    
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
table(duplicated(blob.df$ID))
table(duplicated(data_redresse$ID))
row.names(blob.df)<-unique(blob.df$ID)

# annees<- unique(blob.df$annee)
tableau_RECUP_CLUSTER<- blob.df
row.names(tableau_RECUP_CLUSTER)<-blob.df$ID
tableau_RECUP_CLUSTER$cluster = rep(0,nrow(tableau_RECUP_CLUSTER))

tableau.db=list()
# tableau.fpc=list()


# library(dbscan)
# length(kNNdist(blob.df[blob.df$annee==1999,c(3,4)], k =  1))
# kNNdistplot(blob.df[blob.df$annee==1999,c(3,4)], k =  300)
# abline(h = 10000, lty = 2, col="red")



for (year in unique(blob.df$annee)) {
  show(year)

  # dbscan package
  res.db <- dbscan::dbscan(blob.df[blob.df$annee==year,c(3,4)],eps=300,minPts=3,borderPoints = T)
  res.db$ID<-row.names(blob.df[blob.df$annee==year,])
  # Compute DBSCAN using fpc package
  # set.seed(50000000000)
  # res.fpc <- fpc::dbscan(blob.df[blob.df$annee==1999,c(3,4)], eps =1000, MinPts =5)
  #res.fpc$ID<-row.names(blob.df[blob.df$annee==year,])
  
  show("dbscan ok")
  
  tableau.db[[as.character(year)]]<- res.db
  show("tab ok")
  
  
  # Make sure that both version produce the same results:
  #all(res.fpc$cluster == res.db$cluster)
  #dataframe
  
  tableau_RECUP_CLUSTER$cluster[blob.df$annee==year]<-res.db$cluster
}
table(tableau_RECUP_CLUSTER$cluster)
tableau_RECUP_CLUSTER_Promoteurs<-tableau_RECUP_CLUSTER

#verification resultat
tableau_RECUP_CLUSTER_Promoteurs$typeVar<- ifelse (tableau_RECUP_CLUSTER_Promoteurs$cluster==0,  "Transactions écartées : en dehors d'un cluster",
                                                   ifelse (tableau_RECUP_CLUSTER_Promoteurs$cluster>=1 , "Transactions retenues : à l'intérieur d'un cluster",NA))
table(tableau_RECUP_CLUSTER_Promoteurs$typeVar)
spVente <- st_as_sf(tableau_RECUP_CLUSTER_Promoteurs,
                    
                    coords = c("XLamb93","Ylamb93"),
                    
                    agr = "constant",
                    
                    crs = 2154,
                    
                    stringsAsFactors = FALSE)

library(cartography)
library(dplyr)
spVente<-spVente%>% filter(annee==1999)
typoLayer(spVente,var = "typeVar",border = NULL, lwd=0.1)

setwd("~/Projets/DBSCAN/DBSAN")
write.table(tableau_RECUP_CLUSTER_Promoteurs,file = "DBSCAN_results_table_Promoteurs.csv", append=T, row.names=FALSE, fileEncoding = "UTF-8")

##############################
data_redresse_code_promo<-left_join(data_redresse,tableau_RECUP_CLUSTER_Promoteurs, by = "ID")

data_redresse_code_promo$personne_morale_ve_rg<- ifelse(data_redresse_code_promo$QUALITE_VE == "AD"  &!is.na(data_redresse_code_promo$QUALITE_VE),"ADM",
                                      ifelse(data_redresse_code_promo$QUALITE_VE== "EN"& !is.na(data_redresse_code_promo$QUALITE_VE),"Entreprise",
                                             ifelse(data_redresse_code_promo$QUALITE_VE== "PR"&!is.na(data_redresse_code_promo$QUALITE_VE),"Marchand",
                                                    ifelse(data_redresse_code_promo$QUALITE_VE== "SA"&!is.na(data_redresse_code_promo$QUALITE_VE),"SAFER",
                                                           ifelse(data_redresse_code_promo$QUALITE_VE== "SC"&!is.na(data_redresse_code_promo$QUALITE_VE),"SCI",
                                                                  ifelse(data_redresse_code_promo$QUALITE_VE== "SO"&!is.na(data_redresse_code_promo$QUALITE_VE),"HLM", "particulier_ou_na"))))))



table(data_redresse_code_promo$personne_morale_ve_rg, useNA = "ifany")
table(data_redresse_code_promo$typeVar)

data_redresse_code_promo$personne_morale_ve_rg<- ifelse(data_redresse_code_promo$cluster.y>=1 & !is.na(data_redresse_code_promo$cluster.y), "promo", data_redresse_code_promo$personne_morale_ve_rg)
table(data_redresse_code_promo$REQ_ANC,data_redresse_code_promo$personne_morale_ve_rg)

data_redresse_code_promo$personne_morale_actifs<- ifelse(data_redresse_code_promo$CSP_VE>=1 & data_redresse_code_promo$CSP_VE<=69  &data_redresse_code_promo$personne_morale_ve_rg == "particulier_ou_na","Actifs",
                               ifelse(data_redresse_code_promo$CSP_VE>=70 & data_redresse_code_promo$CSP_VE<=90  & data_redresse_code_promo$personne_morale_ve_rg =="particulier_ou_na", "retraites_inactifs", 
                                      ifelse(is.na(data_redresse_code_promo$CSP_VE) & data_redresse_code_promo$personne_morale_ve_rg=="particulier_ou_na",NA,data_redresse_code_promo$personne_morale_ve_rg)))


table(data_redresse_code_promo$personne_morale_actifs,data_redresse_code_promo$REQ_ANC, useNA = "ifany")


# 
# 
# 
# 
# 
# plot(tableau.db[["1996"]], blob.df, main = "DBSCAN", frame = FALSE)
# library(extrafont)
# loadfonts(device="win")
# ?fviz_cluster
library(factoextra)
# fviz_cluster(as(tableau.db[["1996"]],'dbscan_fast'), points.df[points.df$annee==1996,c(3,4)],
#              outlier.shape = ,show.clust.cent = TRUE,
#              ellipse = TRUE,labelsize = 0,  geom = "point",repel = TRUE,
#              ellipse.type = "convexe", outlier.color = "black",main = "1996",
#              ellipse.alpha = 0, shape = "points" ) + theme_tmd()
# 
# partition_2008<-
par(mfrow=c(1,1))
fviz_cluster(as(tableau.db[["1999"]],'dbscan_fast'), blob.df[blob.df$annee==1999,c(3,4)],
                             stand = TRUE,
                             show.clust.cent = T, ellipse = TRUE, ellipse.type = "convex",
                             labelsize = 1,  geom = "point",repel =F,
                             pointsize =0.1 , outlier.color = "red",main = 2012,
                             ellipse.alpha = 0.2, shape = "blob" )+
  # coord_quickmap(xlim=range(blob.df$X), ylim=range(blob.df$Y))+
  theme_tmd()+
  theme(legend.position="none", axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())+
  geom_point(size=0.1) +
  labs(title = NULL, x= NULL, y= NULL)

library(leaflet)
m <- leaflet()
m <- addTiles(m)
spVente_2<-spVente%>% 
  as( "Spatial")
# proj4string(spVente_2) <-CRS("+init=epsg:4326")
spVente_2<- spTransform(spVente_2, "+init=epsg:4326")
addPopups(map = m, lng = spVente_2@data$X, lat = spVente_2@data$Y,popup = spVente_2@data$cluster )
         

 opacity = 3, 
            color = "black", stroke = TRUE, weight = 1, popup = spVente_2@data$cluser, 
            fill = T, fillColor = "red", fillOpacity = 0.2)
?addPopups
# labs(caption = "Sources : échantillon BD BIEN (non redressé) ; Réalisation : Thibault Le Corre, Géographie-Cités 2017\ncalcul réalisé sour R Studio avec le package dbscan\nFigure réalisée avec le package ggplot2")

# ggsave("Partition_2012.pdf",plot= partition_2012, "~/Projets/DBSCAN/DBSAN",device = "pdf", width = 220, height = 200, units = "mm", dpi = 200)
# ggsave("Partition_2008.png",plot= partition_2008, "~/Projets/DBSCAN/DBSAN",device = "png", width = 220, height = 150, units = "mm", dpi = 200)
# 
# #sortie_plot#
# 
# DBSCAN_results_table$typeVar<- ifelse (DBSCAN_results_table$cluster==0,  "Transactions écartées : en dehors d'un cluster",
#                                        ifelse (DBSCAN_results_table$cluster>=1 , "Transactions retenues : à l'intérieur d'un cluster",NA ))
# 
# table(DBSCAN_results_table$typeVar)
# 
# 
# library(ggplot2)
# dev.off()
# Partionnement<-ggplot(DBSCAN_results_table, aes(X, Y, group=typeVar),xlab=F, ylab=F) +
#   geom_point(aes(color =typeVar ),size=0.1, show.legend =T) + labs(x=NULL, y=NULL)+
#   coord_quickmap(xlim=range(DBSCAN_results_table$X), ylim=range(DBSCAN_results_table$Y))+
#   facet_wrap(~annee,ncol = 3, nrow = 4)+ theme_tmd()+
#   theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())+
#   labs(title = "Partionnement du semis de points en vue de l'analyse", x= NULL, y= NULL)+
#   labs(caption = "Sources : échantillon BD BIEN (non redressé, n = 901843) ; Réalisation : Thibault Le Corre, Géographie-Cités 2017\ncalcul réalisé sour R Studio avec le package dbscan\nFigure réalisée avec le package ggplot2")
# 
# 
# ggsave("Partionnement.png",plot= Partionnement, "Figures_DBSCAN", device = "png", width = 150, height = 150, units = "mm", dpi = 330)
# ggsave("Partionnement.png",plot= Partionnement, "C:/Users/Doctorant3/Documents/Thibault/Sync/Work_on_DATA/images et cartes/Figures_DBSCAN",device = "png", width = 220, height = 200, units = "mm", dpi = 200)
# 
# 
# theme_tmd <- function(){
#   tmd <-  theme(
#     legend.title = element_text(size = 0, colour = NA,family = "Century Gothic"),
#     legend.position = 'bottom',
#     legend.direction = "horizontal",
#     legend.background = element_rect(fill = NA),
#     panel.background = element_rect(fill = "white"), 
#     plot.background = element_rect(fill = NA),
#     # panel.grid.major = element_line(colour = "gray92", size = 1),
#     legend.text = element_text(size = 10,family = "Century Gothic"),
#     plot.margin = unit(x = c(0.25, 0.25, 0.25, 0.5), units = "cm"),
#     plot.caption= element_text(size = 8,family = "Century Gothic"),
#     legend.box.spacing = unit(x = 0.25, units = "cm"),
#     legend.margin=margin(t = 0, r = 0, b = -0.2, l = 0,unit = "cm"),
#     plot.title=element_text(size =12,family = "Century Gothic",face="bold",hjust = 0.5, vjust = 0.5),
#     axis.title = element_text(size =10,family = "Century Gothic"),
#     plot.subtitle = element_text(size =10,family = "Century Gothic"))
# }
# 
# 
# write.table(tableau_RECUP_CLUSTER,file = "DBSCAN_results_table", append=T)
# write.csv()
# table(DBSCAN_results_table$cluster)
