library(cartography)
library(SpatialPosition)
library(sf)
library(dplyr)
library(tidyr)
library(vegan)
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

setwd("~/Shapes/shpIDF_dep_lamb93")
list.files()

lim_IDF.st<- st_read("ile-de-france.shp",
                     stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
lim_IDF.st  <-  st_transform(lim_IDF.st , crs = 2154)


Cadrillage_200_point <- st_centroid(Cadrillage_200, "POINT")

Communes_Carreaux <-st_join(CommunesInsee, Cadrillage_200_point,join = st_intersects, left=T)


Communes_Carreaux<- left_join(SpUnitt_with_cluster[,c(1,2,32)],Communes_Carreaux, by="Carreau_ID")

Communes_Carreaux<- Communes_Carreaux%>%
  group_by(DepCom, cettePeriodeLa,`cah$data.clust$clust` )%>%
  summarise(Nombrclust= length(`cah$data.clust$clust`))%>%
  spread(key =`cah$data.clust$clust`,value = Nombrclust, fill= 0 )
  Communes_Carreaux$Total_carreaux<- rowSums(Communes_Carreaux[,c(3:13)] )
   
#   Communes_Carreaux_2<-mutate_all(Communes_Carreaux[,-c(1,2)], funs("percent"= ./ (Communes_Carreaux$Total_carreaux)*100))
#   Communes_Carreaux_2<-as.data.frame(Communes_Carreaux_2)
#   Communes_Carreaux<-as.data.frame(Communes_Carreaux)
# Communes_Carreaux<- cbind(Communes_Carreaux[,c("DepCom","cettePeriodeLa")], Communes_Carreaux_2[,c(13:23)] )
#   


Communes_CarreauxPeriode1<-Communes_Carreaux%>%
  filter(cettePeriodeLa=="Periode_96_2003")
Communes_CarreauxPeriode1test<-Communes_CarreauxPeriode1[,c(3:13)]
# HLoc<-HLocfunc(Communes_CarreauxPeriode1test)
# HLoc<-as.data.frame(HLoc)
# Communes_CarreauxPeriode1<-cbind(Communes_CarreauxPeriode1test,HLoc)

Simpson_com1<-diversity(Communes_CarreauxPeriode1test, index = "simpson", MARGIN = 1, base = exp(1))
Simpson_com1<-as.data.frame(Simpson_com1)
Communes_CarreauxPeriode1<-cbind(Communes_CarreauxPeriode1test,Simpson_com1)
specnumber(Communes_CarreauxPeriode1test,  MARGIN = 1)
S1 <- specnumber(Communes_CarreauxPeriode1test) ## rowSums(BCI > 0) does the same...
S1 <-as.data.frame(S1)
Communes_CarreauxPeriode1<-cbind(Communes_CarreauxPeriode1,S1)
# J <- Shannon_com/log(S)
# Shannon_com<-as.data.frame(J)
# Communes_CarreauxPeriode1<-cbind(Communes_CarreauxPeriode1test,Shannon_com)


Communes_CarreauxPeriode2<-Communes_Carreaux%>%
  filter(cettePeriodeLa=="Periode_04_2007")
Communes_CarreauxPeriode2test<-Communes_CarreauxPeriode2[,c(3:13)]

Simpson_com2<-diversity(Communes_CarreauxPeriode2test, index = "simpson", MARGIN = 1, base = exp(1))
Simpson_com2<-as.data.frame(Simpson_com2)
Communes_CarreauxPeriode2<-cbind(Communes_CarreauxPeriode2test,Simpson_com2)
specnumber(Communes_CarreauxPeriode2test,  MARGIN = 1)
S2 <- specnumber(Communes_CarreauxPeriode2test) 
S2 <-as.data.frame(S2)
Communes_CarreauxPeriode2<-cbind(Communes_CarreauxPeriode2,S2)


Communes_CarreauxPeriode3<-Communes_Carreaux%>%
  filter(cettePeriodeLa=="Periode_08_2012")
Communes_CarreauxPeriode3test<-Communes_CarreauxPeriode3[,c(3:13)]

Simpson_com3<-diversity(Communes_CarreauxPeriode3test, index = "simpson", MARGIN = 1, base = exp(1))
Simpson_com3<-as.data.frame(Simpson_com3)
Communes_CarreauxPeriode3<-cbind(Communes_CarreauxPeriode3test,Simpson_com3)
specnumber(Communes_CarreauxPeriode3test,  MARGIN = 1)
S3 <- specnumber(Communes_CarreauxPeriode3test) 
S3 <-as.data.frame(S3)
Communes_CarreauxPeriode3<-cbind(Communes_CarreauxPeriode3,S3)

rm(Simpson_com1,Simpson_com2,Simpson_com3,Simpson_com)
rm(Communes_CarreauxPeriode1test, Communes_CarreauxPeriode2test,Communes_CarreauxPeriode3test,Communes_Carreaux_2)




# Communes_Simpson_pourcentage<- cbind(Communes_Carreaux%>%
#                            distinct(DepCom)%>%
#                            select(DepCom), Communes_CarreauxPeriode1[,c(12:13)], Communes_CarreauxPeriode2[,c(12:13)], Communes_CarreauxPeriode3[,c(12:13)] )

Communes_Carreaux<-as.data.frame(Communes_Carreaux)
Communes_Simpson_effectif<- cbind(Communes_Carreaux%>%
                                       distinct(DepCom)%>%
                                       select(DepCom), Communes_CarreauxPeriode1[,c(12:13)], Communes_CarreauxPeriode2[,c(12:13)], Communes_CarreauxPeriode3[,c(12:13)] )




# Communes_Simpson<-left_join(Communes_Simpson_pourcentage,Communes_Simpson_effectif, by = "DepCom")
Communes_Simpson<-Communes_Simpson_effectif
Communes_Simpson<-Communes_Simpson%>%
  select(DepCom,Simpson_com1,Simpson_com2,Simpson_com3)
mean(Communes_Simpson$Simpson_com1)
sd(Communes_Simpson$Simpson_com1)
median (Communes_Simpson$Simpson_com1)
max(Communes_Simpson$Simpson_com1)
mean(Communes_Simpson$Simpson_com2)
sd(Communes_Simpson$Simpson_com2)
median (Communes_Simpson$Simpson_com2)
max(Communes_Simpson$Simpson_com2)

mean(Communes_Simpson$Simpson_com3)
sd(Communes_Simpson$Simpson_com3)
median (Communes_Simpson$Simpson_com3)
max(Communes_Simpson$Simpson_com3)


Test_map<- left_join(CommunesInsee, Communes_Simpson, by = "DepCom")
Test_map<-Test_map%>%
  group_by(DepCom)%>%
  gather(key = "Test_simpson", "Value", c(2:4))%>%
  filter(!is.na(Value))
Test_breaks<-Test_map%>%
  filter(Value>0)
Breaks_simpson<-getBreaks(Test_breaks$Value,method = "quantile",nclass=5)
Test_map$Value<-ifelse(Test_map$Value==0, NA, Test_map$Value)

Test_map<-Test_map%>%
  group_by(DepCom)%>%
  spread(Test_simpson, Value)

str(Test_map)
hist(Test_map$Simpson_com1, breaks=100)
# Periode_96_2003# Periode_04_2007
# osmTiles <- getTiles(x = Cadrillage_200, type = "stamenbw",  crop = FALSE)
par(mfrow=c(1,1))
cols=carto.pal(pal1 = 'grey.pal', n1 = 6)
# opacity <- 95# de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
# cols <- paste0(cols, opacity)
# tilesLayer(osmTiles)

setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf(file="Indice_entropy_periode1.pdf",width = 10, height = 5)

choroLayer(x=Test_map,
          var = "Simpson_com1",
          col= cols, ##palettes de couleurs predefinies## 
          border = "black",
          lwd = 0.2,
          breaks=Breaks_simpson ,
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          legend.values.rnd = 2,
          colNA="red")
plot(lim_IDF.st$geometry,add=T)

dev.off()

setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf(file="Indice_entropy_periode2.pdf",width = 10, height = 5)

choroLayer(x=Test_map,
           var = "Simpson_com2",
           col= cols, ##palettes de couleurs predefinies## 
           border = "black",
           lwd = 0.2,
           breaks=Breaks_simpson ,
           legend.pos = "bottomleft",
           legend.title.txt = "",
           legend.values.cex= 0.5,
           legend.values.rnd = 2,
           colNA="red")
plot(lim_IDF.st$geometry,add=T)

dev.off()


setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf(file="Indice_entropy_periode3.pdf",width = 10, height = 5)

choroLayer(x=Test_map,
           var = "Simpson_com3",
           col= cols, ##palettes de couleurs predefinies## 
           border = "black",
           lwd = 0.2,
           breaks=Breaks_simpson ,
           legend.pos = "bottomleft",
           legend.title.txt = "",
           legend.values.cex= 0.5,
           legend.values.rnd = 2,
           colNA="red")
plot(lim_IDF.st$geometry,add=T)

dev.off()




###########################################################

Communes_Simpson<-Communes_Simpson_effectif
Communes_Simpson<-Communes_Simpson%>%
  select(DepCom,S1,S2,S3)


Test_map<- left_join(CommunesInsee, Communes_Simpson, by = "DepCom")
Test_map<-Test_map%>%
  group_by(DepCom)%>%
  gather(key = "Test_simpson", "Value", c(2:4))%>%
  filter(!is.na(Value))
sd(Test_map$Value)
# Test_breaks<-Test_map%>%
#   filter(Value>0)
# Breaks_simpson<-getBreaks(Test_breaks$Value,method = "quantile",nclass=5)
# Test_map$Value<-ifelse(Test_map$Value==0, NA, Test_map$Value)
Breaks_simpson<-c(1,2,3,4,5,6,7,8,9)
Test_map<-Test_map%>%
  group_by(DepCom)%>%
  spread(Test_simpson, Value)

str(Test_map)
hist(Test_map$S1, breaks=100)
# Periode_96_2003# Periode_04_2007
# osmTiles <- getTiles(x = Cadrillage_200, type = "stamenbw",  crop = FALSE)
par(mfrow=c(1,1))
cols=carto.pal(pal1 = 'red.pal', n1 =9)
# opacity <- 95# de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
# cols <- paste0(cols, opacity)
# tilesLayer(osmTiles)




setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf(file="Nombre_cluster_periode1.pdf",width = 10, height = 5)

choroLayer(x=Test_map,
           var = "S1",
           col= cols, ##palettes de couleurs predefinies## 
           border = "black",
           lwd = 0.2,
           breaks=Breaks_simpson ,
           legend.pos = "bottomleft",
           legend.title.txt = "",
           legend.values.cex= 0.5,
           legend.values.rnd = 2,
           colNA="red")
plot(lim_IDF.st$geometry,add=T)

dev.off()

setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf(file="Nombre_cluster_periode2.pdf",width = 10, height = 5)

choroLayer(x=Test_map,
           var = "S2",
           col= cols, ##palettes de couleurs predefinies## 
           border = "black",
           lwd = 0.2,
           breaks=Breaks_simpson ,
           legend.pos = "bottomleft",
           legend.title.txt = "",
           legend.values.cex= 0.5,
           legend.values.rnd = 2,
           colNA="red")
plot(lim_IDF.st$geometry,add=T)

dev.off()


setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf(file="Nombre_cluster_periode3.pdf",width = 10, height = 5)

choroLayer(x=Test_map,
           var = "S3",
           col= cols, ##palettes de couleurs predefinies## 
           border = "black",
           lwd = 0.2,
           breaks=Breaks_simpson ,
           legend.pos = "bottomleft",
           legend.title.txt = "",
           legend.values.cex= 0.5,
           legend.values.rnd = 2,
           colNA="red")
plot(lim_IDF.st$geometry,add=T)

dev.off()


###################################################

Test_map<- left_join(Cadrillage_200, SpUnitt_with_cluster%>%
                       filter(cettePeriodeLa == "Periode_04_2007"), by = "Carreau_ID")

Localisation_locale<-
  st_join(CommunesInsee%>% 
            filter(DepCom==91201|DepCom==91326|DepCom==91687), Cadrillage_200_point,join = st_intersects, left=T)
Localisation_locale<-as.data.frame(Localisation_locale)%>%
  select(-geometry)
Localisation_locale<-right_join(Test_map, Localisation_locale,by="Carreau_ID")



osmTiles <- getTiles(x = Localisation_locale, type = "stamenbw",  crop = FALSE, zoom=15)
select_comm<-CommunesInsee%>% 
  filter(DepCom==91201|DepCom==91326|DepCom==91687)




library(RColorBrewer)
cols <- brewer.pal(12,"Set3")

opacity <- 95 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)

setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf(file="Cluster_carreaux_in_communes_Draveil.pdf",width = 10, height = 5)
tilesLayer(osmTiles)
typoLayer(x=Localisation_locale,
          var = "cah$data.clust$clust",
          col= cols, ##palettes de couleurs predefinies## 
          border = FALSE,
          legend.values.order = c("1", 
                                  "3",
                                  "5",
                                  "6",
                                  "7",
                                  "8",
                                  "9"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          colNA="grey",add=TRUE)
plot(select_comm$geometry,add=T,border="red")
layoutLayer(north=T)
dev.off()


Localisation_locale<-
  st_join(CommunesInsee%>% 
            filter(DepCom==92063|DepCom==92050|DepCom==78190 |DepCom== 78146), Cadrillage_200_point,join = st_intersects, left=T)
Localisation_locale<-as.data.frame(Localisation_locale)%>%
  select(-geometry)
Localisation_locale<-right_join(Test_map, Localisation_locale,by="Carreau_ID")



osmTiles <- getTiles(x = Localisation_locale, type = "stamenbw",  crop = FALSE, zoom=15)
select_comm<-CommunesInsee%>% 
  filter(DepCom==92063|DepCom==92050|DepCom==78190 |DepCom== 78146)




library(RColorBrewer)
cols <- brewer.pal(12,"Set3")

opacity <- 95 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)
setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf(file="Cluster_carreaux_in_communes_Rueil_Malmaison.pdf",width = 10, height = 5)
tilesLayer(osmTiles)
typoLayer(x=Localisation_locale,
          var = "cah$data.clust$clust",
          col= cols, ##palettes de couleurs predefinies## 
          border = FALSE,
          legend.values.order = c( 
                                  "3",
                                  "4",
                                  "5",
                                  "6",
                                  "7",
                                  "9",
                                  "10"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          colNA="grey",add=TRUE)
plot(select_comm$geometry,add=T,border="red")
layoutLayer(north=T)
dev.off()


Localisation_locale<-
  st_join(CommunesInsee%>% 
            filter(DepCom==75101|DepCom==75102|DepCom==75103 |DepCom== 75104 |DepCom== 75105|DepCom== 75106|DepCom== 75107), Cadrillage_200_point,join = st_intersects, left=T)
Localisation_locale<-as.data.frame(Localisation_locale)%>%
  select(-geometry)
Localisation_locale<-right_join(Test_map, Localisation_locale,by="Carreau_ID")



osmTiles <- getTiles(x = Localisation_locale, type = "stamenbw",  crop = FALSE, zoom=15)
select_comm<-CommunesInsee%>% 
  filter(DepCom==75101|DepCom==75102|DepCom==75103 |DepCom== 75104 |DepCom== 75105| DepCom== 75106|DepCom== 75107)




library(RColorBrewer)
cols <- brewer.pal(12,"Set3")

opacity <- 95 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)
setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf(file="Cluster_carreaux_in_communes_centre_Paris.pdf",width = 10, height = 5)
tilesLayer(osmTiles)
typoLayer(x=Localisation_locale,
          var = "cah$data.clust$clust",
          col= cols, ##palettes de couleurs predefinies## 
          border = FALSE,
          legend.values.order = c( 
            "11","10"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          colNA="grey",add=TRUE)
plot(select_comm$geometry,add=T,border="red")
layoutLayer(north=T)
dev.off()

#####################################


library(ade4)
library(FactoClass)
Communes_CarreauxPeriode1<-Communes_Carreaux%>%
  filter(cettePeriodeLa=="Periode_04_2007")
Communes_CarreauxPeriode1test<-Communes_CarreauxPeriode1[,c(3:13)]
AFC <- dudi.coa(df=Communes_CarreauxPeriode1test, scannf=FALSE, nf=ncol(Communes_CarreauxPeriode1test))
plot.dudi(AFC)

distMat <- dist.dudi(AFC, amongrow=TRUE)
# Communes_CarreauxPeriode1test<-scale(Communes_CarreauxPeriode1test)
CAH <- ward.cluster(distMat, peso = apply(X=Communes_CarreauxPeriode1test, MARGIN=1, FUN=sum) , plots = TRUE, h.clust = 1)
par(mfrow=c(1,2))
barplot(sort(CAH$height / sum(CAH$height), decreasing = TRUE)[1:15] * 100,
        xlab = "Noeuds", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie selon le partitionnement")

barplot(cumsum(sort(CAH$height / sum(CAH$height), decreasing = TRUE))[1:15] * 100,
        xlab = "Nombre de classes", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie expliquée")
Communes_CarreauxPeriode1test$clusters <- cutree(tree = CAH, k = 8)




Communes_CarreauxPeriode1test %>%
  group_by(clusters) %>%
  summarise_all(funs(mean),na.rm=T) %>%
  gather(key = "Dimension", value = "Data", `1_percent`:`11_percent`) %>%
  ggplot() +
  geom_bar(aes(Dimension, Data, fill = clusters), stat = "identity") +
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~clusters) +
  coord_flip()


Communes_CarreauxPeriode1_with_clust<-cbind(Communes_CarreauxPeriode1[,c("DepCom","cettePeriodeLa")], Communes_CarreauxPeriode1test )

library(RColorBrewer)
cols <- brewer.pal(12,"Set3")


opacity <- 95 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)
Test_map<- left_join(CommunesInsee, Communes_CarreauxPeriode1_with_clust, by = "DepCom")


# Periode_96_2003# Periode_04_2007
# osmTiles <- getTiles(x = Cadrillage_200, type = "stamenbw",  crop = FALSE)
par(mfrow=c(1,1))
tilesLayer(osmTiles)
typoLayer(x=Test_map,
          var = "clusters",
          col= cols, ##palettes de couleurs predefinies## 
          border = FALSE,
          legend.values.order = c("1",
                                  "2", 
                                  "3",
                                  "4",
                                  "5",
                                  "6",
                                  "7",
                                  "8",
                                  "9",
                                  "10",
                                  "11"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          colNA="grey",add=TRUE)

# Autre solution#