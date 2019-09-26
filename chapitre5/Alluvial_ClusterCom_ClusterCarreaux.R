library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(stringr)   
library(tidyverse)
library(sf)
library()


setwd("~/Shapes/datidf")
list.files()
carreauInsee200 <- st_read("car200m_idf_reparGeom.shp",
                           stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee200 <-  st_transform(carreauInsee200, crs = 2154)
#pour carreau Insee
Cadrillage_200<- carreauInsee200%>%
  st_sf(geometry = .)
Cadrillage_200$Carreau_ID<-1:length(Cadrillage_200$TARGET_FID)

Cadrillage_200_point <- st_centroid(Cadrillage_200, "POINT")
# SpUnitt_with_cluster <- read.csv2("~/Projets/Realisation_ACP/SpUnitt_with_cluster.csv", stringsAsFactors=FALSE)
Cadrillage_200_Cluster<- left_join(SpUnitt_with_cluster[,c(1,2,32)],Cadrillage_200_point, by="Carreau_ID")
colnames(Cadrillage_200_Cluster)[colnames(Cadrillage_200_Cluster) == 'cah.data.clust.clust'] <- 'Clusters_Carreaux'
colnames(Cadrillage_200_Cluster)[colnames(Cadrillage_200_Cluster) == "cettePeriodeLa"] <- 'Periode'
# SpUnitt_Communes_with_cluster <- read.csv2("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes/SpUnitt_Communes_with_cluster.csv", stringsAsFactors=FALSE)
Communes_Carreaux <-st_join(CommunesInsee, Cadrillage_200_point,join = st_intersects, left=T)
Communes_Carreaux<- left_join(Communes_Carreaux[,c(1,14)],Cadrillage_200_Cluster[,c(1,2,3)], by="Carreau_ID")
colnames(SpUnitt_Communes_with_cluster)[colnames(SpUnitt_Communes_with_cluster) == 'cah.data.clust.clust'] <- 'Clusters_Communes'
Communes_Carreaux<- left_join(Communes_Carreaux,SpUnitt_Communes_with_cluster[,c(1,2,48)], by=c("DepCom","Periode"))


Com_Car_alluvial <- as.data.frame(Communes_Carreaux) %>%
  filter(!is.na(Clusters_Carreaux))%>%
  select(Periode,Clusters_Communes,Clusters_Carreaux) %>%
  group_by(Periode,Clusters_Communes,Clusters_Carreaux) %>%
  dplyr::summarise(count=n())%>%
  spread(Clusters_Communes,count, fill=0 ) %>%
  ungroup()  %>%
  select(-`<NA>`)%>%
  gather("Clusters_Communes","count",c(3:10))

library(alluvial)

# alluvial_plot_cah_communes<-
Com_Car_alluvial_96<-Com_Car_alluvial%>%
  filter(Periode=="Periode_96_2003", !is.na(Clusters_Carreaux), !is.na(Clusters_Communes))%>%
  select(-Periode)

setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf("Croisement_Aluvial_Car_Com_P1.pdf",width = 10, height = 5)
 Croisement_Aluvial_Car_Com_P1<- alluvial(select(Com_Car_alluvial_96, -count),freq=Com_Car_alluvial_96$count, hide=Com_Car_alluvial_96$count <300, col = 
             ifelse(Com_Car_alluvial_96$Clusters_Carreaux==1,"orange",
           ifelse(Com_Car_alluvial_96$Clusters_Carreaux==2,"yellow", 
          ifelse(Com_Car_alluvial_96$Clusters_Carreaux==3,"red",
           ifelse(Com_Car_alluvial_96$Clusters_Carreaux==4,"brown",
           ifelse(Com_Car_alluvial_96$Clusters_Carreaux==5,"green",
              ifelse(Com_Car_alluvial_96$Clusters_Carreaux==6,"purple",
             ifelse(Com_Car_alluvial_96$Clusters_Carreaux==7,"pink",
            ifelse(Com_Car_alluvial_96$Clusters_Carreaux==8,"blue",
          ifelse(Com_Car_alluvial_96$Clusters_Carreaux==9,"bisque3",  
      ifelse(Com_Car_alluvial_96$Clusters_Carreaux==10,"brown1",
   ifelse(Com_Car_alluvial_96$Clusters_Carreaux==11,"darkgoldenrod3",NA))))))))))))
 dev.off()
  
Com_Car_alluvial_07<-Com_Car_alluvial%>%
    filter(Periode=="Periode_04_2007", !is.na(Clusters_Carreaux), !is.na(Clusters_Communes))%>%
    select(-Periode)
pdf("Croisement_Aluvial_Car_Com_P2.pdf",width = 10, height = 5)
Croisement_Aluvial_Car_Com_P2<- alluvial(select(Com_Car_alluvial_07, -count),freq=Com_Car_alluvial_07$count, hide=Com_Car_alluvial_07$count <300, col = 
             ifelse(Com_Car_alluvial_07$Clusters_Carreaux==1,"orange",
                    ifelse(Com_Car_alluvial_07$Clusters_Carreaux==2,"yellow", 
                           ifelse(Com_Car_alluvial_07$Clusters_Carreaux==3,"red",
                                  ifelse(Com_Car_alluvial_07$Clusters_Carreaux==4,"brown",
                                         ifelse(Com_Car_alluvial_07$Clusters_Carreaux==5,"green",
                                                ifelse(Com_Car_alluvial_07$Clusters_Carreaux==6,"purple",
                                                       ifelse(Com_Car_alluvial_07$Clusters_Carreaux==7,"pink",
                                                              ifelse(Com_Car_alluvial_07$Clusters_Carreaux==8,"blue",
                                                                     ifelse(Com_Car_alluvial_07$Clusters_Carreaux==9,"bisque3",  
                                                                            ifelse(Com_Car_alluvial_07$Clusters_Carreaux==10,"brown1",
                                                                                   ifelse(Com_Car_alluvial_07$Clusters_Carreaux==11,"darkgoldenrod3",NA))))))))))))

dev.off()
  Com_Car_alluvial_12<-Com_Car_alluvial%>%
    filter(Periode=="Periode_08_2012", !is.na(Clusters_Carreaux), !is.na(Clusters_Communes))%>%
    select(-Periode)
  pdf("Croisement_Aluvial_Car_Com_P3.pdf",width = 10, height = 5)
  Croisement_Aluvial_Car_Com_P3<- alluvial(select(Com_Car_alluvial_12, -count),freq=Com_Car_alluvial_12$count, hide=Com_Car_alluvial_12$count <300, col = 
             ifelse(Com_Car_alluvial_12$Clusters_Carreaux==1,"orange",
                    ifelse(Com_Car_alluvial_12$Clusters_Carreaux==2,"yellow", 
                           ifelse(Com_Car_alluvial_12$Clusters_Carreaux==3,"red",
                                  ifelse(Com_Car_alluvial_12$Clusters_Carreaux==4,"brown",
                                         ifelse(Com_Car_alluvial_12$Clusters_Carreaux==5,"green",
                                                ifelse(Com_Car_alluvial_12$Clusters_Carreaux==6,"purple",
                                                       ifelse(Com_Car_alluvial_12$Clusters_Carreaux==7,"pink",
                                                              ifelse(Com_Car_alluvial_12$Clusters_Carreaux==8,"blue",
                                                                     ifelse(Com_Car_alluvial_12$Clusters_Carreaux==9,"bisque3",  
                                                                            ifelse(Com_Car_alluvial_12$Clusters_Carreaux==10,"brown1",
                                                                                   ifelse(Com_Car_alluvial_12$Clusters_Carreaux==11,"darkgoldenrod3",NA))))))))))))

  dev.off()
  