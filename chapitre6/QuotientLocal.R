Communes_maisons<- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_533communes_Maisons", stringsAsFactors=FALSE)
Communes_appartements<-read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_communes_appartements", stringsAsFactors=FALSE)

tableauID<-full_join(Communes_appartements,Communes_maisons, by="DepCom")
tableauID<-tableauID[,c(1,2)]
# 3changement en objet spatial

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
# plot(st_geometry(IrisInsee), col = "grey90", lwd = 0.2)

#obtenir shape commune par aggregation 
CommunesInsee<- IrisInsee%>% 
  group_by(DepCom)%>%
  summarize()

tableauID$DepCom<-as.character(tableauID$DepCom)
CommunesInsee<-right_join(CommunesInsee,tableauID)
CommunesInsee<-CommunesInsee[,-c(2)]
#departement
setwd("~/Shapes/shpIDF_dep_lamb93")
list.files()
DepInsee<- st_read("ile-de-france.shp",
                   stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
DepInsee <-  st_transform(DepInsee, crs = 2154)


###

Test_communes<-Tableau_Acquereur_Communes

Test_communes<-Test_communes%>%
  select(DepCom,annee.x,acquereurs,rapport_Acquereurs)%>%
  spread(acquereurs, rapport_Acquereurs)
library(OasisR)

QuotientLocal<-LQ(Test_communes[,c(3:12)])
QuotientLocal<-cbind(Test_communes[,c(1,2)],QuotientLocal)
