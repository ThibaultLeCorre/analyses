library(fasterize)
library(spdep)
library(ggplot2)
library(sf)
library(sp)
library(SpatialPosition)
library(dplyr)
library(tidyr)

# cl<-makeCluster(detectCores(),type="SOCK")
# registerDoSNOW(cl)
# id = mcparallel({ Sys.sleep(5); TRUE })
# mccollect(id)

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



#######Table principal#####
data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x",
                                        "ANNAIS_AC","PRESCREDIT","MTCRED","SITMAT_AC", "REQTYPBIEN", "NBRPIECE", "REQ_ANC", "REQ_PRIX")]
DBSCAN_results_table_Promoteurs <- read.csv("~/Projets/DBSCAN/DBSAN/DBSCAN_results_table_Promoteurs.csv", sep="", stringsAsFactors=FALSE)
mytest$ID<-as.numeric(mytest$ID)
mytest<-left_join(mytest,DBSCAN_results_table_Promoteurs, by="ID")
# table(data_redresse_code_promo_2$typeVar, data_redresse_code_promo_2$CSP_VE)
mytest$X.x<-as.numeric(mytest$X.x)
mytest$Y.x<-as.numeric(mytest$Y.x)
mytest$CSP_AC<-as.numeric(mytest$CSP_AC)
mytest$CSP_VE<-as.numeric(mytest$CSP_VE)
mytest$annee.x<-as.numeric(mytest$annee.x)
mytest$ANNAIS_AC<-as.numeric(mytest$ANNAIS_AC)
mytest$MTCRED<-as.numeric(mytest$MTCRED)
mytest$NBRPIECE<-as.numeric(mytest$NBRPIECE)
str(mytest)
unique(mytest$QUALITE_AC)
# 1 import table

mytest$Periode<-ifelse(mytest$annee.x==1996|mytest$annee.x==1999|mytest$annee.x==2003, "Periode_96_2003",
                       ifelse(mytest$annee.x>=2004&mytest$annee.x<=2007, "Periode_04_2007", 
                              ifelse(mytest$annee.x>=2008&mytest$annee.x<=2012, "Periode_08_2012", NA)))

Periode <- unique(mytest$Periode)
Periode <- sort(Periode[!is.na(Periode)])
# 2. Boucle d'alimentation

#Types acquéreurs et vendeurs: toutes transactions

mytest$acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM",
                           ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                  ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                         ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                                       ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM", "particulier_ou_na"))))))

mytest$acquereurs<- ifelse(mytest$CSP_AC>=1 & mytest$CSP_AC<=69&mytest$acquereurs == "particulier_ou_na","Actifs",
                           ifelse(mytest$CSP_AC>=70 & mytest$CSP_AC<=90 &  mytest$acquereurs =="particulier_ou_na", "retraites_inactifs",
                                  ifelse(is.na(mytest$CSP_AC) & mytest$acquereurs=="particulier_ou_na",NA,mytest$acquereurs)))

table(mytest$QUALITE_VE, useNA = "ifany")

mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM",
                         ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                       ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"SAFER",
                                              ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                                     ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM", "particulier_ou_na"))))))


mytest$Vendeurs<- ifelse(mytest$CSP_VE>=1 & mytest$CSP_VE<=69& mytest$Vendeurs == "particulier_ou_na","Actifs",
                         ifelse(mytest$CSP_VE>=70 & mytest$CSP_VE<=90 & mytest$Vendeurs =="particulier_ou_na", "retraites_inactifs", 
                                ifelse(is.na(mytest$CSP_VE) & mytest$Vendeurs=="particulier_ou_na",NA,mytest$Vendeurs)))

mytest$Vendeurs<- ifelse(mytest$cluster>=1 & !is.na(mytest$cluster),"Promoteurs", mytest$Vendeurs)
table(mytest$Vendeurs, useNA = "ifany")


table_acquereur_vendeur<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Vendeurs),!is.na(acquereurs),acquereurs!="SAFER",acquereurs!="particulier_ou_na", Vendeurs!="SAFER", Vendeurs!="particulier_ou_na")

Transac<-table_acquereur_vendeur%>% select(ID,X.x,Y.x,Periode,acquereurs)
#

##########
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
   select(Periode,DepCom,acquereurs)%>%
   group_by(Periode,DepCom,acquereurs) %>%
   summarise(Nombre_transacs_Acquereurs= length(which(!is.na(acquereurs))))%>%
   spread(acquereurs,Nombre_transacs_Acquereurs,fill = 0)%>%
   gather(key = "acquereurs", value="Nombre_transacs_Acquereurs", c(3:6))
 
 Communes_total <- Communes_jointure%>%
    group_by(Periode,DepCom) %>%
   summarise(Nombre_transacs_total_Acquereurs=length(which(!is.na(acquereurs))))
  
 Montableau_Profil_acquereur <- left_join(Communes_categories,Communes_total, by= c("DepCom", "Periode"))
 Montableau_Profil_acquereur$rapport_Acquereurs<-(Montableau_Profil_acquereur$Nombre_transacs_Acquereurs/Montableau_Profil_acquereur$Nombre_transacs_total_Acquereurs)*100
  


#########Vendeurs
Transac<-table_acquereur_vendeur%>% select(ID,X.x,Y.x,Periode,Vendeurs)
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
  select(Periode,DepCom,Vendeurs)%>%
  group_by(Periode,DepCom,Vendeurs) %>%
  summarise(Nombre_transacs_Vendeurs= length(which(!is.na(Vendeurs))))%>%
  spread(Vendeurs,Nombre_transacs_Vendeurs,fill = 0)%>%
  gather(key = "Vendeurs", value="Nombre_transacs_Vendeurs", c(3:7))

Communes_total <- Communes_jointure%>%
  group_by(Periode,DepCom) %>%
  summarise(Nombre_transacs_total_Vendeurs=length(which(!is.na(Vendeurs))))

Montableau_Profil_Vendeurs <- left_join(Communes_categories,Communes_total, by= c("DepCom", "Periode"))
Montableau_Profil_Vendeurs$rapport_Vendeurs<-(Montableau_Profil_Vendeurs$Nombre_transacs_Vendeurs/Montableau_Profil_Vendeurs$Nombre_transacs_total_Vendeurs)*100



# 
# ####Profil age acquereur#
# 

# ####Fourchette age
mytest$Age_acq = mytest$annee.x-mytest$ANNAIS_AC

mytest$fourchette_age_acq<-mytest$Age_acq
mytest$fourchette_age_acq<- ifelse(mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<30, "[18,30[",
                                   ifelse(mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<50, "[30,50[",
                                          ifelse(mytest$fourchette_age_acq>=50, "[50+",mytest$Age_acq)))

table_age_acquereur<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(fourchette_age_acq), acquereurs=="Actifs" )
table_age_acquereur<-table_age_acquereur%>% filter(Age_acq>17)
Transac<-table_age_acquereur%>% select(ID,X.x,Y.x,Periode,fourchette_age_acq)

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
  select(Periode,DepCom,fourchette_age_acq)%>%
  group_by(Periode,DepCom,fourchette_age_acq) %>%
  summarise(Nombre_transacs_fourchette_age_acq= length(which(!is.na(fourchette_age_acq))))%>%
  spread(fourchette_age_acq,Nombre_transacs_fourchette_age_acq,fill = 0)%>%
  gather(key = "fourchette_age_acq", value="Nombre_transacs_fourchette_age_acq", c(3:5))

Communes_total <- Communes_jointure%>%
  group_by(Periode,DepCom) %>%
  summarise(Nombre_transacs_total_fourchette_age_acq=length(which(!is.na(fourchette_age_acq))))

Montableau_Profil_fourchette_age_acq <- left_join(Communes_categories,Communes_total, by= c("DepCom", "Periode"))
Montableau_Profil_fourchette_age_acq$rapport_fourchette_age_acq<-(Montableau_Profil_fourchette_age_acq$Nombre_transacs_fourchette_age_acq/Montableau_Profil_fourchette_age_acq$Nombre_transacs_total_fourchette_age_acq)*100




# ####Profil type logement_maisons_appartements#
# 
# 
mytest$Type_log<- ifelse( mytest$REQTYPBIEN=="MA" ,"Maisons",
                          ifelse(mytest$REQTYPBIEN=="AP", "Appartements",mytest$REQTYPBIEN))


table_Type_log<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Type_log))

Transac<-table_Type_log%>% select(ID,X.x,Y.x,Periode,Type_log)
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
  select(Periode,DepCom,Type_log)%>%
  group_by(Periode,DepCom,Type_log) %>%
  summarise(Nombre_transacs_Type_log= length(which(!is.na(Type_log))))%>%
  spread(Type_log,Nombre_transacs_Type_log,fill = 0)%>%
  gather(key = "Type_log", value="Nombre_transacs_Type_log", c(3:4))

Communes_total <- Communes_jointure%>%
  group_by(Periode,DepCom) %>%
  summarise(Nombre_transacs_total_Type_log=length(which(!is.na(Type_log))))

Montableau_Profil_Type_log <- left_join(Communes_categories,Communes_total, by= c("DepCom", "Periode"))
Montableau_Profil_Type_log$rapport_Type_log<-(Montableau_Profil_Type_log$Nombre_transacs_Type_log/Montableau_Profil_Type_log$Nombre_transacs_total_Type_log)*100


# 

mytest$Profil_log<- ifelse( mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>7 | mytest$REQTYPBIEN=="AP" & mytest$NBRPIECE>6  ,"Bien_exceptionnel",
                            ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=0 & mytest$NBRPIECE<=1, "studio_et_1_pièce","Logement banal"))


table_Profil_log<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Profil_log))

Transac<-table_Profil_log%>% select(ID,X.x,Y.x,Periode,Profil_log)

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
  select(Periode,DepCom,Profil_log)%>%
  group_by(Periode,DepCom,Profil_log) %>%
  summarise(Nombre_transacs_Profil_log= length(which(!is.na(Profil_log))))%>%
  spread(Profil_log,Nombre_transacs_Profil_log,fill = 0)%>%
  gather(key = "Profil_log", value="Nombre_transacs_Profil_log", c(3:5))

Communes_total <- Communes_jointure%>%
  group_by(Periode,DepCom) %>%
  summarise(Nombre_transacs_total_Profil_log=length(which(!is.na(Profil_log))))

Montableau_Profil_Profil_log <- left_join(Communes_categories,Communes_total, by= c("DepCom", "Periode"))
Montableau_Profil_Profil_log$rapport_Profil_log<-(Montableau_Profil_Profil_log$Nombre_transacs_Profil_log/Montableau_Profil_Profil_log$Nombre_transacs_total_Profil_log)*100

# 

# ####################################
#
# ###ituation matri

mytest$Couple<- ifelse(mytest$SITMAT_AC == "M" | mytest$SITMAT_AC == "P" | mytest$SITMAT_AC == "R", "En_couple",
                       ifelse(mytest$SITMAT_AC == "D"| mytest$SITMAT_AC == "V", "Divorce_ou_veuvage",
                              ifelse(mytest$SITMAT_AC == "C", "Celibataire", NA )))

table_situation_matri<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Couple))
Transac<-table_situation_matri%>% select(ID,X.x,Y.x,Periode,Couple)

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
  select(Periode,DepCom,Couple)%>%
  group_by(Periode,DepCom,Couple) %>%
  summarise(Nombre_transacs_Couple= length(which(!is.na(Couple))))%>%
  spread(Couple,Nombre_transacs_Couple,fill = 0)%>%
  gather(key = "Couple", value="Nombre_transacs_Couple", c(3:5))

Communes_total <- Communes_jointure%>%
  group_by(Periode,DepCom) %>%
  summarise(Nombre_transacs_total_Couple=length(which(!is.na(Couple))))

Montableau_Profil_Couple <- left_join(Communes_categories,Communes_total, by= c("DepCom", "Periode"))
Montableau_Profil_Couple$rapport_Couple<-(Montableau_Profil_Couple$Nombre_transacs_Couple/Montableau_Profil_Couple$Nombre_transacs_total_Couple)*100



# 


# 
# ####Profil Regime marche#

mytest$Credit <- ifelse(mytest$PRESCREDIT!="N" & mytest$PRESCREDIT!="O" , "Credit_cautionne_suppose",
                        ifelse(mytest$PRESCREDIT=="O",  "Avec_credit_hypothécaire",
                               ifelse(mytest$PRESCREDIT=="N",  "Sans_credit", NA)))

table(mytest$Credit)
table_Credit<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Credit),acquereurs=="Actifs" | acquereurs=="retraites_inactifs")
table(table_Credit$Credit)
Transac<-table_Credit%>% select(ID,X.x,Y.x,Periode,Credit)

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
  select(Periode,DepCom,Credit)%>%
  group_by(Periode,DepCom,Credit) %>%
  summarise(Nombre_transacs_Credit= length(which(!is.na(Credit))))%>%
  spread(Credit,Nombre_transacs_Credit,fill = 0)%>%
  gather(key = "Credit", value="Nombre_transacs_Credit", c(3:5))

Communes_total <- Communes_jointure%>%
  group_by(Periode,DepCom) %>%
  summarise(Nombre_transacs_total_Credit=length(which(!is.na(Credit))))

Montableau_Profil_Credit <- left_join(Communes_categories,Communes_total, by= c("DepCom", "Periode"))
Montableau_Profil_Credit$rapport_Credit<-(Montableau_Profil_Credit$Nombre_transacs_Credit/Montableau_Profil_Credit$Nombre_transacs_total_Credit)*100




# 
# ###LTV


table_LTV<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, Credit=="Avec_credit_hypothécaire",acquereurs=="Actifs" | acquereurs=="retraites_inactifs", MTCRED>100,!is.na(REQ_PRIX))

Transac<-table_LTV%>% select(ID,X.x,Y.x,Periode,MTCRED,REQ_PRIX)


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
  
  
 Montableau_LTV<-Communes_jointure %>%
    group_by(Periode,DepCom) %>%
    summarise(Nombre_transacs=length(which(!is.na(ID))),
              MOntCRED= sum(MTCRED,na.rm = TRUE),
              Sum_PRIX= sum(REQ_PRIX,na.rm = TRUE),
              Montant_moyen_credit = (MOntCRED/Nombre_transacs),
              LTV=(MOntCRED/Sum_PRIX)*100)
  

########Communes d'originie, porte du marche#
Communes_origine_df <- read.csv("~/Projets/ACP_evolution_profil_marche_backup/ACP_projets_table_final/Communes_origine_df.csv", sep=";", stringsAsFactors=FALSE)
# 
mytest<-join(mytest, Communes_origine_df[,c("ID","origine")], by="ID", type="left", match="first")

table_origine_acquereur<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(origine))
Transac<-table_origine_acquereur%>% select(ID,X.x,Y.x,Periode,origine)


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

unique(Communes_jointure$origine)
Communes_categories<-Communes_jointure%>%
  select(Periode,DepCom,origine)%>%
  group_by(Periode,DepCom,origine) %>%
  summarise(Nombre_transacs_origine_acquereur= length(which(!is.na(origine))))%>%
  spread(origine,Nombre_transacs_origine_acquereur,fill = 0)%>%
  gather(key = "origine_acquereur", value="Nombre_transacs_origine_acquereur", c(3:6))

Communes_total <- Communes_jointure%>%
  group_by(Periode,DepCom) %>%
  summarise(Nombre_transacs_total_origine_acquereur=length(which(!is.na(origine))))

Montableau_Profil_origine_acquereur <- left_join(Communes_categories,Communes_total, by= c("DepCom", "Periode"))
Montableau_Profil_origine_acquereur$rapport_origine_acquereur<-(Montableau_Profil_origine_acquereur$Nombre_transacs_origine_acquereur/Montableau_Profil_origine_acquereur$Nombre_transacs_total_origine_acquereur)*100

# 






# ####################################

# Jointure Grand tableau de donnees




# select(-Somme_Profil_vendeur,Nombre_totalPotentiel_transac)%>% 
check<-Montableau_Profil_acquereur%>%
  gather(variable, value, -(Periode :acquereurs)) %>%
  unite(acquereurs, acquereurs, variable) %>%
  spread(acquereurs, value)
summary(check)

Tableau_ACP_Final_Communes<- left_join(Montableau_Profil_acquereur%>%
                                gather(variable, value, -(Periode :acquereurs)) %>%
                                unite(acquereurs, acquereurs, variable) %>%
                                spread(acquereurs, value),
                              Montableau_Profil_Vendeurs %>% 
                                gather(variable, value, -(Periode :Vendeurs)) %>%
                                unite(Vendeurs, Vendeurs, variable) %>%
                                spread(Vendeurs, value),
                              by= c("DepCom", "Periode"))

Tableau_ACP_Final_Communes<- left_join(Tableau_ACP_Final_Communes, 
                              Montableau_Profil_fourchette_age_acq%>% 
                                gather(variable, value, -(Periode : fourchette_age_acq  )) %>%
                                unite(fourchette_age_acq  , fourchette_age_acq   , variable) %>%
                                spread(fourchette_age_acq  , value),
                              by= c("DepCom", "Periode"))


Tableau_ACP_Final_Communes<- left_join(Tableau_ACP_Final_Communes,
                              Montableau_Profil_Type_log %>% 
                                gather(variable, value, -(Periode :Type_log )) %>%
                                unite(Type_log, Type_log , variable) %>%
                                spread(Type_log, value),
                              by= c("DepCom", "Periode"))


Tableau_ACP_Final_Communes<- left_join(Tableau_ACP_Final_Communes, 
                              Montableau_Profil_Couple%>% 
                                gather(variable, value, -(Periode : Couple  )) %>%
                                unite(Couple  , Couple   , variable) %>%
                                spread(Couple  , value),
                              by= c("DepCom", "Periode"))


Tableau_ACP_Final_Communes<- left_join(Tableau_ACP_Final_Communes, 
                              Montableau_Profil_Profil_log%>% 
                                gather(variable, value, -(Periode : Profil_log )) %>%
                                unite(Profil_log , Profil_log  , variable) %>%
                                spread(Profil_log , value),
                              by= c("DepCom", "Periode"))



Tableau_ACP_Final_Communes<- left_join(Tableau_ACP_Final_Communes, 
                              Montableau_Profil_Credit%>% 
                                gather(variable, value, -(Periode : Credit   )) %>%
                                unite(Credit ,Credit ,variable) %>%
                                spread(Credit,value),
                              by= c("DepCom", "Periode"))

Tableau_ACP_Final_Communes<- left_join(Tableau_ACP_Final_Communes, 
                              Montableau_LTV[,c("Periode","DepCom","Montant_moyen_credit","LTV")],by= c("DepCom", "Periode"))


Tableau_ACP_Final_Communes<- left_join(Tableau_ACP_Final_Communes, 
                              Montableau_Profil_origine_acquereur%>% 
                                gather(variable, value, -(Periode : origine_acquereur   )) %>%
                                unite(origine_acquereur ,origine_acquereur ,variable) %>%
                                spread(origine_acquereur,value),
                              by= c("DepCom", "Periode"))

summary(Tableau_ACP_Final_Communes)
colnames(Tableau_ACP_Final_Communes)
#Save
setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
write.csv2(x=Tableau_ACP_Final_Communes,file = "Tableau_ACP_Final_Communes.csv", row.names=FALSE, fileEncoding = "UTF-8")
