library(fasterize)
library(spdep)
library(ggplot2)
library(sf)
library(sp)
library(SpatialPosition)
library(dplyr)
library(tidyr)
library(parallel)
library(doSNOW)
data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x","NUMCOM_AC","PADEPT_AC","BIDEPT", "insee", "CODNAT_AC")]
str(mytest)
mytest<-mytest%>%filter(X.x>100 &Y.x>100)
unique(mytest$QUALITE_AC)
# table(data_redresse_code_promo_2$typeVar, data_redresse_code_promo_2$CSP_VE)
# mytest$X.x<-as.numeric(mytest$X.x)
# mytest$Y.x<-as.numeric(mytest$Y.x)
# mytest$CSP_AC<-as.numeric(mytest$CSP_AC)
# mytest$CSP_VE<-as.numeric(mytest$CSP_VE)
# mytest$annee.x<-as.numeric(mytest$annee.x)
# mytest$NUMCOM_AC<-as.numeric(mytest$NUMCOM_AC)
# mytest$PADEPT_AC<-as.numeric(mytest$PADEPT_AC)
# mytest$BIDEPT<-as.numeric(mytest$BIDEPT)
# mytest$insee<-as.numeric(mytest$insee)
# 1 import table

mytest$Periode<-ifelse(mytest$annee.x==1996|mytest$annee.x==1999|mytest$annee.x==2003, "Periode_96_2003",
                       ifelse(mytest$annee.x>=2004&mytest$annee.x<=2007, "Periode_04_2007", 
                              ifelse(mytest$annee.x>=2008&mytest$annee.x<=2012, "Periode_08_2012", NA)))



############
# Import shap#####

# 
# load("~/Projets/ACP_evolution_profil_marche_backup/ACP_projets_table_final/Environnement_ACP_table_final.RData")

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
CommunesInsee$DepCom <- ifelse(CommunesInsee$DepCom>=75000 & CommunesInsee$DepCom<=76000, 75056, CommunesInsee$DepCom)
CommunesInsee<- CommunesInsee%>% 
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



#######Recodage

mytest<-mytest %>% filter (!is.na(CSP_AC))
# mytest$insee<- ifelse(mytest$BIDEPT==75,mytest$insee==75056, mytest$insee )

spVente <- st_as_sf(mytest,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)



#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]
spVente$Ylamb93<-coords[,2]

CommunesInsee$DepCom<-as.numeric(CommunesInsee$DepCom)
Communes_transcas_join<-st_join(spVente, CommunesInsee, join = st_within, left=T)
Communes_transcas_join<-as.data.frame(Communes_transcas_join)
Communes_transcas_join<-Communes_transcas_join%>%filter(!is.na(ID)& DepCom>0 &!is.na(DepCom))
mytest<-Communes_transcas_join


################# Rccodage commune origine
mytest$Numero_comm_acq<-ifelse(mytest$NUMCOM_AC>=10 & mytest$NUMCOM_AC <100, paste0("0",mytest$NUMCOM_AC),mytest$NUMCOM_AC)
mytest$Numero_comm_acq_2<- ifelse(mytest$NUMCOM_AC<10, paste0("00",mytest$NUMCOM_AC),mytest$NUMCOM_AC)
mytest$Commune_origine_acq<- ifelse(mytest$NUMCOM_AC>=10 & mytest$NUMCOM_AC <100, mytest$Numero_comm_acq, 
                              ifelse(mytest$NUMCOM_AC<10, mytest$Numero_comm_acq_2, mytest$NUMCOM_AC))

mytest$Commune_origine_Final<- paste0(mytest$PADEPT_AC,mytest$Commune_origine_acq)
mytest$Origine_regionale<-ifelse(mytest$PADEPT_AC==75|mytest$PADEPT_AC==91|mytest$PADEPT_AC==92|mytest$PADEPT_AC==93|mytest$PADEPT_AC==94|mytest$PADEPT_AC==95|mytest$PADEPT_AC==77|mytest$PADEPT_AC==78,
                                  "Region_IDF","Autres_region")

#####################

CommunesVoisines <- st_intersects(CommunesInsee,CommunesInsee)

############################ Sur les communes voisines
mytest_2<-mytest %>% filter (Origine_regionale=="Region_IDF" & Commune_origine_Final!=78689 & !is.na(NUMCOM_AC))
setdiff(mytest_2$Commune_origine_Final,CommunesInsee$DepCom)

table(mytest_2$Origine_regionale,useNA = "ifany")
###################################################################################

getNeig <- function(x, y, tabref, nei){ 
  idTrans <- which(x == tabref$DepCom)
  idResid <- which(y == tabref$DepCom)
  vecNei <- nei[[idResid]]
  test<- idTrans %in% vecNei
 return(test)
  }

Acq_portee_proche <- base::mapply(getNeig, x = mytest_2$DepCom, y=mytest_2$Commune_origine_Final, 
                     MoreArgs = list(tabref = CommunesInsee, nei = CommunesVoisines))
#######
Acq_portee_proche_df<-data.frame(matrix(unlist(Acq_portee_proche), byrow=T),stringsAsFactors=FALSE)
table(Acq_portee_proche_df$matrix.unlist.Acq_portee_proche...byrow...T.)
Acq_portee_proche_df<-rename.variable(Acq_portee_proche_df,"matrix.unlist.Acq_portee_proche...byrow...T.", "Com_Same_voisine_acaht")

mytest_2<-cbind(mytest_2,Acq_portee_proche_df)

####### Sur Paris Intra
mytest_2$Paris<- ifelse(mytest_2$Commune_origine_Final==75056, "Paris", "Pas_Paris")

##Sur un autre Departement#
mytest_2$Departement_origine<- ifelse(mytest_2$PADEPT_AC==mytest_2$BIDEPT, "Meme_departement", "Autre_departement")
###Autre region#
str(mytest_2)

##########
###############################################################################
Paris<-CommunesInsee%>% filter(DepCom==75056)
CommunesParis_Limit <- st_intersects(Paris,CommunesInsee)
CommunesInsee_2<-CommunesInsee
CommunesParis_Limit<-CommunesParis_Limit[[1]]
CommunesInsee_2$numero<-row.names(CommunesInsee_2)
CommunesParis_Limit<-as.data.frame(CommunesParis_Limit)

CommunesInsee_2$numero<-as.integer(CommunesInsee_2$numero)

CommunesParis_Limit<-left_join(CommunesInsee_2,CommunesParis_Limit, by= c("numero"="col.id") )
CommunesParis_Limit$Paris_and_limit<-ifelse(CommunesParis_Limit$row.id==1,"Communes_Paris_Limit","Autres_communes")
CommunesParis_Limit<-as.data.frame(CommunesParis_Limit)
CommunesParis_Limit<-CommunesParis_Limit%>%filter(!is.na(Paris_and_limit)& DepCom!=75056 )%>%select(-geometry)

CommunesParis_Limit$DepCom<-as.numeric(CommunesParis_Limit$DepCom)
mytest_2<-left_join(mytest_2,CommunesParis_Limit[,c("DepCom","Paris_and_limit")], by = "DepCom" )
#####################################################################




table(mytest_2$Commune_origine_Final)
Communes_origine_df<-mytest_2%>%
  select(ID,Com_Same_voisine_acaht,Paris,Departement_origine,Paris_and_limit)


Communes_origine_df<-join(mytest, Communes_origine_df, by="ID", type="left", match="first")


# Conditions
Communes_origine_df$Comm_resid_Paris_achat_autre<-ifelse(Communes_origine_df$Origine_regionale=="Region_IDF"&Communes_origine_df$Paris=="Paris" & Communes_origine_df$DepCom!=75056, "Provenance_Paris_Achat_HorsParis", "Autre")
Communes_origine_df$Communes_limitrophe_Paris<-ifelse(Communes_origine_df$Origine_regionale=="Region_IDF"&Communes_origine_df$Paris=="Paris" & Communes_origine_df$DepCom!=75056 & !is.na(Communes_origine_df$Paris_and_limit), "Prov_Paris_Achat_communes_limit", "Autre")



Communes_origine_df$origine<-ifelse(Communes_origine_df$Origine_regionale=="Region_IDF"&Communes_origine_df$Com_Same_voisine_acaht=="TRUE" & Communes_origine_df$Communes_limitrophe_Paris!="Prov_Paris_Achat_communes_limit","meme_commune_et_voisins","Autre")
Communes_origine_df$origine<-ifelse(Communes_origine_df$origine=="Autre"& Communes_origine_df$Communes_limitrophe_Paris=="Prov_Paris_Achat_communes_limit", "Origine_Paris",Communes_origine_df$origine)
Communes_origine_df$origine<-ifelse(Communes_origine_df$origine=="Autre" & Communes_origine_df$Paris=="Paris", "Origine_Paris", Communes_origine_df$origine)
Communes_origine_df$origine<-ifelse(Communes_origine_df$origine=="Autre" & Communes_origine_df$PADEPT_AC==Communes_origine_df$BIDEPT & Communes_origine_df$BIDEPT!=75, "meme_departement_autre_commune", Communes_origine_df$origine)
Communes_origine_df$origine<-ifelse(Communes_origine_df$origine=="Autre"& Communes_origine_df$Origine_regionale=="Region_IDF"  ,"Autres_departements_de_la_region",Communes_origine_df$origine)

Communes_origine_df$origine<-ifelse(is.na(Communes_origine_df$origine) & Communes_origine_df$PADEPT_AC==99, "Provenance_etrangere",Communes_origine_df$origine  )

Communes_origine_df$origine<-ifelse(is.na(Communes_origine_df$origine) & !is.na(Communes_origine_df$PADEPT_AC) & Communes_origine_df$Origine_regionale=="Autres_region", "Autre_region_francaise", Communes_origine_df$origine)

table(Communes_origine_df$origine,Communes_origine_df$BIDEPT)
Communes_origine_df$origine<-ifelse(is.na(Communes_origine_df$origine) & !is.na(Communes_origine_df$PADEPT_AC) & Communes_origine_df$Origine_regionale=="Autres_region", "Autre_region_francaise", Communes_origine_df$origine)

setwd("~/Projets/ACP_evolution_profil_marche_backup/ACP_projets_table_final")
write.csv2(x=Communes_origine_df,file = "Communes_origine_df.csv", row.names=FALSE, fileEncoding = "UTF-8")
