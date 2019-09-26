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
# plot(st_geometry(DepInsee), col = , lwd = 0.2)



data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x")]
DBSCAN_results_table_Promoteurs <- read.csv("~/Projets/DBSCAN/DBSAN/DBSCAN_results_table_Promoteurs.csv", sep="", stringsAsFactors=FALSE)
mytest$ID<-as.numeric(mytest$ID)
mytest<-left_join(mytest,DBSCAN_results_table_Promoteurs, by="ID")
# table(data_redresse_code_promo_2$typeVar, data_redresse_code_promo_2$CSP_VE)
mytest$X.x<-as.numeric(mytest$X.x)
mytest$Y.x<-as.numeric(mytest$Y.x)
mytest$CSP_AC<-as.numeric(mytest$CSP_AC)
mytest$CSP_VE<-as.numeric(mytest$CSP_VE)
mytest$annee.x<-as.numeric(mytest$annee.x)

str(mytest)
unique(mytest$QUALITE_AC)
# 1 import table

annee <- unique(mytest$annee.x)
annee <- sort(annee[!is.na(annee)])
# 2. Boucle d'alimentation

#Types acquéreurs et vendeurs: toutes transactions

mytest$acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM",
                           ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                  ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                         ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                                       ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM", "particulier_ou_na"))))))



mytest$acquereurs<- ifelse(mytest$CSP_AC == 10 & mytest$acquereurs == "particulier_ou_na", "Agriculteurs",
                           ifelse(mytest$CSP_AC >= 20 & mytest$CSP_AC < 30 & mytest$acquereurs == "particulier_ou_na", "Liberales",
                                  ifelse(mytest$CSP_AC >= 30 & mytest$CSP_AC < 40 & mytest$acquereurs == "particulier_ou_na", "CPIS",
                                         ifelse(mytest$CSP_AC >= 40 & mytest$CSP_AC < 50 & mytest$acquereurs == "particulier_ou_na", "Prof_intermediaires",
                                                ifelse(mytest$CSP_AC >= 50 & mytest$CSP_AC < 60 & mytest$acquereurs == "particulier_ou_na", "Employes",
                                                       ifelse(mytest$CSP_AC >= 60 & mytest$CSP_AC < 70 & mytest$acquereurs == "particulier_ou_na", "Ouvriers",
                                                              ifelse(mytest$CSP_AC >= 70 & mytest$CSP_AC < 80 & mytest$acquereurs == "particulier_ou_na", "retraites",
                                                                     ifelse(mytest$CSP_AC == 80 & mytest$acquereurs == "particulier_ou_na", "autres_inactifs", mytest$acquereurs))))))))

table (mytest$acquereurs, useNA = "always")

mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM",
                         ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                       ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"SAFER",
                                              ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                                     ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM", "particulier_ou_na"))))))


mytest$Vendeurs<- ifelse(mytest$CSP_VE == 10 & mytest$Vendeurs == "particulier_ou_na", "Agriculteurs",
                         ifelse(mytest$CSP_VE >= 20 & mytest$CSP_VE < 30 & mytest$Vendeurs == "particulier_ou_na", "Liberales",
                                ifelse(mytest$CSP_VE >= 30 & mytest$CSP_VE < 40 & mytest$Vendeurs == "particulier_ou_na", "CPIS",
                                       ifelse(mytest$CSP_VE >= 40 & mytest$CSP_VE < 50 & mytest$Vendeurs == "particulier_ou_na", "Prof_intermediaires",
                                              ifelse(mytest$CSP_VE >= 50 & mytest$CSP_VE < 60 & mytest$Vendeurs == "particulier_ou_na", "Employes",
                                                     ifelse(mytest$CSP_VE >= 60 & mytest$CSP_VE < 70 & mytest$Vendeurs == "particulier_ou_na", "Ouvriers",
                                                            ifelse(mytest$CSP_VE >= 70 & mytest$CSP_VE < 80 & mytest$Vendeurs == "particulier_ou_na", "retraites",
                                                                   ifelse(mytest$CSP_VE == 80 & mytest$Vendeurs == "particulier_ou_na", "autres_inactifs", mytest$Vendeurs))))))))

mytest$Vendeurs<- ifelse(mytest$cluster>=1 & !is.na(mytest$cluster),"Promoteurs", mytest$Vendeurs)
table(mytest$Vendeurs, useNA = "ifany")


table_acquereur_vendeur<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Vendeurs),!is.na(acquereurs),acquereurs!="SAFER",acquereurs!="particulier_ou_na", Vendeurs!="SAFER", Vendeurs!="particulier_ou_na")

Transac<-table_acquereur_vendeur%>% select(ID,X.x,Y.x,annee.x,acquereurs)

#############Acquereurs#######
test<- Transac

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


Communes_jointure <- st_join(CommunesInsee, spVente, join = st_contains, left=T)
Communes_jointure <- as.data.frame(Communes_jointure)

Communes_categories<-Communes_jointure%>%
  select(annee.x,DepCom,acquereurs)%>%
  group_by(annee.x,DepCom,acquereurs) %>%
  summarise(Nombre_transacs_Acquereurs= length(which(!is.na(acquereurs))))%>%
  spread(acquereurs,Nombre_transacs_Acquereurs,fill = 0)%>%
  gather(key = "acquereurs", value="Nombre_transacs_Acquereurs", c(3:12))

Communes_total <- Communes_jointure%>%
  group_by(annee.x,DepCom) %>%
  summarise(Nombre_transacs_total_Acquereurs=length(which(!is.na(acquereurs))))

Montableau_Profil_acquereur <- left_join(Communes_categories,Communes_total, by= c("DepCom", "annee.x"))
Montableau_Profil_acquereur$rapport_Acquereurs<-(Montableau_Profil_acquereur$Nombre_transacs_Acquereurs/Montableau_Profil_acquereur$Nombre_transacs_total_Acquereurs)*100



#########Vendeurs
Transac<-table_acquereur_vendeur%>% select(ID,X.x,Y.x,annee.x,Vendeurs)
test<- Transac

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


Communes_jointure<-st_join(CommunesInsee, spVente, join = st_contains, left=T)
Communes_jointure<- as.data.frame(Communes_jointure)

Communes_categories<-Communes_jointure%>%
  select(annee.x,DepCom,Vendeurs)%>%
  group_by(annee.x,DepCom,Vendeurs) %>%
  summarise(Nombre_transacs_Vendeurs= length(which(!is.na(Vendeurs))))%>%
  spread(Vendeurs,Nombre_transacs_Vendeurs,fill = 0)%>%
  gather(key = "Vendeurs", value="Nombre_transacs_Vendeurs", c(3:13))

Communes_total <- Communes_jointure%>%
  group_by(annee.x,DepCom) %>%
  summarise(Nombre_transacs_total_Vendeurs=length(which(!is.na(Vendeurs))))

Montableau_Profil_Vendeurs <- left_join(Communes_categories,Communes_total, by= c("DepCom", "annee.x"))
Montableau_Profil_Vendeurs$rapport_Vendeurs<-(Montableau_Profil_Vendeurs$Nombre_transacs_Vendeurs/Montableau_Profil_Vendeurs$Nombre_transacs_total_Vendeurs)*100


setwd("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux")
write.csv2(x=Montableau_Profil_acquereur,file = "Tableau_Acquereur_Communes.csv", row.names=FALSE, fileEncoding = "UTF-8")
setwd("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux")
write.csv2(x=Montableau_Profil_Vendeurs,file = "Tableau_Vendeur_Communes.csv", row.names=FALSE, fileEncoding = "UTF-8")

