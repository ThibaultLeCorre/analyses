
library(spdep)
library(ggplot2)
library(sf)
library(sp)
library(SpatialPosition)
library(dplyr)
library(tidyr)


extrapolate_allyears_communes_CONSOLIDATED <- read.csv("~/Projets/Modele_access/extrapolate_allyears_communes_CONSOLIDATED.csv", sep=";", stringsAsFactors=FALSE)
#####
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
#################
BASE_PTZ_2016 <- read.delim("~/Projets/PTZ/PTZ/BASE_PTZ_2016.txt", stringsAsFactors=FALSE)
PTZ<- BASE_PTZ_2016%>% 
  select (cins,iden,ccsp,tegp, an, dept, dtpp, age,vtto,vtpp,cpfl)
PTZ<-PTZ %>% filter (tegp<16, 
                     ccsp!=10 & ccsp!=11 & ccsp!=12 & ccsp!=13 ,
                     dept==75 |dept==91 | dept==92| dept==93 | dept==95 | dept==95 | dept==77 | dept==78   )%>%
  filter( dept==75 |dept==91 | dept==92| dept==93 | dept==94 | dept==95 | dept==77 | dept==78 , vtto<450000, vtpp>762 & vtpp<350000 )%>%
  filter(an>=2001 & an<=2012)

PTZ$ccsp<-ifelse(PTZ$ccsp >= 30 & PTZ$ccsp < 40, "CPIS",
                 ifelse(PTZ$ccsp >= 40 & PTZ$ccsp < 50, "Prof_intermediaires",
                        ifelse(PTZ$ccsp >= 50 & PTZ$ccsp < 60, "Employes",
                               ifelse(PTZ$ccsp >= 60 & PTZ$ccsp < 70, "Ouvriers",NA))))
# 
# PTZ$age<-
#   ifelse(PTZ$ccsp== "retraites", 60, PTZ$age )
# Box plot TEG

PTZ$cins<-ifelse(PTZ$cins >=75000 & PTZ$cins <=76000, 75056, PTZ$cins)

PTZ$age<- ifelse(PTZ$age>=18 & PTZ$age<30, "[18,30[",
                 ifelse(PTZ$age>=30 & PTZ$age<50, "[30,50[",
                        ifelse( PTZ$age>=50, "[50+", NA)))

PTZ<- PTZ%>%
  group_by(ccsp,an)%>%
  filter(!is.na(age), !is.na(ccsp), cins>=75000)%>%
  summarise(n_operation_PTZ=n(),
    TEG_median=median(tegp),
            Duree_median=median(dtpp))

#################################################################

CommunesInsee$DepCom<-as.integer(CommunesInsee$DepCom)
Test_extrapo<-extrapolate_allyears_communes_CONSOLIDATED%>%
  select(idcom,year,medIncome_CAD,medIncome_EMP,medIncome_OUV,medIncome_INT)%>%
  group_by(idcom)%>%
  gather("CSP_resid", "Median_income", c(3:6))

Test_modele<- left_join(CommunesInsee,Test_extrapo, by =c("DepCom"= "idcom") )

Test_modele$CSP_resid<-ifelse(Test_modele$CSP_resid=="medIncome_CAD" , "CPIS",
                 ifelse(Test_modele$CSP_resid=="medIncome_INT", "Prof_intermediaires",
                        ifelse(Test_modele$CSP_resid=="medIncome_EMP", "Employes",
                               ifelse(Test_modele$CSP_resid=="medIncome_OUV", "Ouvriers",NA))))


Test_modele<- left_join(Test_modele,PTZ, by=c("year"="an", "CSP_resid"= "ccsp"))

capacite_emprunts<- function (m,t,n){
  out1<-(1+t)^n - 1
  out2<-t*((1+t)^n)
  out<- out1/out2
  c <- m*out
  return(c)
}
Test_modele$tx_int_mensuels <- ((1+Test_modele$TEG_median/100)^(1/12)-1)


Test_modele$Capacite_total<-capacite_emprunts ((Test_modele$Median_income*0.33)/12,Test_modele$tx_int_mensuels ,Test_modele$Duree_median)

Test_modele<- left_join(Test_modele, Acquereurs_LTV%>%
                          filter(fourchette_age_acq == "[18,30["), by = c("CSP_resid"="acquereurs", "year"="annee.x"))

Test_modele$Capacite_total_With_Apport<- Test_modele$Capacite_total* ((1-(Test_modele$LTV/100))+1)


Test_modele$Capacite_total_2Adultes<- capacite_emprunts ((((Test_modele$Median_income*1.5)*0.33)/12), Test_modele$tx_int_mensuels ,Test_modele$Duree_median)

Test_modele<- left_join(Test_modele, Acquereurs_LTV%>%
                          filter(fourchette_age_acq == "[30,50["), by = c("CSP_resid"="acquereurs", "year"="annee.x"))


Test_modele$Capacite_total_With_Apport_2Adultes<- Test_modele$Capacite_total_2Adultes* ((1-(Test_modele$LTV.y/100))+1)



# Test_modele$Capacite_total<- Test_modele$Capacite_mensuel*Test_modele$Duree_median
# Prepa_PTZ<- left_join(CommunesInsee,PTZ, by =c("DepCom"= "cins") )

specificCol <- c("Com_art_Chef_entreprises" = "#861388",
                 "CPIS"= "#067BC2" ,
                 "Prof_intermediaires"="#5FAD41",
                 "Employes"= "#FF8811",
                 "Ouvriers"="#FF1D15",
                 "retraites"= "#77878B",
                 "autres_inactifs"="#8D775F",
                 "Entreprise_Marchands_SCI"= "#E9DF00",
                 "Biens_publics_et_HLM"= "black",
                 "Promoteurs"="#503D3F")

# options(scipen = 999)
# as.data.frame(Test_modele)%>%
#   group_by(CSP_resid,year)%>%
#   filter(!is.na(CSP_resid))%>%
#   summarise(Capacite = mean(Capacite_total))%>%
#   ggplot(., aes(year, Capacite, fill=CSP_resid)) +
#   geom_line()+
#   geom_path(aes(color = CSP_resid))+
#   scale_color_manual(values = specificCol )+
#   # scale_y_continuous(expand = c(0.01, 0),breaks = c(-10000,0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000,140000,150000,160000,170000))+
#   scale_x_continuous(breaks = c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
#   theme_tmd() +
#   labs(title = "Montant moyens théoriques des capacités d'emprunts", x= "Année" , y= "Montant empruntable théorique moyen")+ 
#   labs(subtitle = "")+
#   labs(caption = "Sources : Echantillon BD BIEN, SGFGAS ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")


mytest<-data_redresse1570533transacs[,c("ID","annee.x","X.x","Y.x","REQTYPBIEN", "NBRPIECE","REQ_PRIX")]



test<- mytest

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

Communes_jointure$Type_Bien<-paste0(Communes_jointure$REQTYPBIEN, Communes_jointure$NBRPIECE)

Communes_jointure$Type_logement<- ifelse (Communes_jointure$Type_Bien=="AP1" | Communes_jointure$Type_Bien=="AP2", "Petit_appartement",
                                          ifelse (Communes_jointure$Type_Bien=="AP3" | Communes_jointure$Type_Bien=="AP4" | Communes_jointure$Type_Bien=="AP5", "Appartement_familial",   
                                                  ifelse (Communes_jointure$Type_Bien=="MA1" | Communes_jointure$Type_Bien=="MA2" | Communes_jointure$Type_Bien=="MA3", "Petite_maison",   
                                                          ifelse ( Communes_jointure$Type_Bien=="MA4" | Communes_jointure$Type_Bien=="MA5" | Communes_jointure$Type_Bien=="MA6", "Maison_familiale",NA))))


table(Communes_jointure$Type_logement)

Communes_jointure$Type<- ifelse(Communes_jointure$Type_logement == "Petit_appartement" | Communes_jointure$Type_logement == "Petite_maison", "Capacite_total_With_Apport",
                            ifelse(Communes_jointure$Type_logement == "Appartement_familial" | Communes_jointure$Type_logement == "Maison_familiale", "Capacite_total_With_Apport_2Adultes",NA))  

table(Communes_jointure$Type)

Communes_prix<-Communes_jointure%>%
  filter(annee.x>2001 )%>%
  group_by(DepCom,annee.x, Type) %>%
  summarise(Nombre_transacs = n(),
            Prix_median = median(REQ_PRIX)) %>%
  filter(Nombre_transacs>5) 

Test_model_2<-Test_modele%>%
  select(DepCom,year,CSP_resid,Capacite_total_With_Apport,Capacite_total_With_Apport_2Adultes)%>%
  group_by(DepCom,year,CSP_resid)%>%
  gather("Type", "Value", c(4,5))%>%
  drop_na()
table(Test_model_2$Type)

Test_model_2<-left_join(Test_model_2, Communes_prix, by = c("DepCom", "year"="annee.x","Type"))


Test_model_2$Ratio_diff_Capacite_Achat<- Test_model_2$Value / Test_model_2$Prix_median


Test_model_2$Type_Menage<- paste0(Test_model_2$CSP_resid,Test_model_2$Type)

Test_model_2$CSP_resid<-Test_model_2$Type_Menage
table(Test_model_2$CSP_resid)


Test_ACP<-Test_model_2%>%
  select(DepCom,year,CSP_resid,Ratio_diff_Capacite_Achat)%>%
  drop_na()%>% 
  # select(-geometry)%>%
  group_by(DepCom,year)%>%
  spread( key = CSP_resid, value =  Ratio_diff_Capacite_Achat)

# Test_model_2 %>% 
#   select(DepCom,year,CSP_resid,Ratio_diff_Capacite_Achat)%>%
#   drop_na()%>% 
#   select(-geometry)%>%
#   group_by(DepCom)%>%
# mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
#   spread(key=CSP_resid, value=Ratio_diff_Capacite_Achat,fill=0) %>%    # spread
#   select(-row_id)
########################ACP

library(FactoMineR)
library(factoextra)
Test_ACP<-left_join(Test_ACP,extrapolate_allyears_communes_CONSOLIDATED[,c("idcom" , "share_EMP","share_OUV","share_INT","share_CAD","year")], by= c("DepCom"= "idcom" , "year") )
Test_ACP$Employ_Pond1<- Test_ACP$EmployesCapacite_total_With_Apport * Test_ACP$share_EMP
Test_ACP$Employ_Pond2<- Test_ACP$EmployesCapacite_total_With_Apport_2Adultes * Test_ACP$share_EMP
Test_ACP$Ouv_Pond1<- Test_ACP$OuvriersCapacite_total_With_Apport * Test_ACP$share_OUV
Test_ACP$Ouv_Pond2<- Test_ACP$OuvriersCapacite_total_With_Apport_2Adultes * Test_ACP$share_OUV
Test_ACP$CPIS_Pond1<- Test_ACP$CPISCapacite_total_With_Apport * Test_ACP$share_CAD
Test_ACP$CPIS_Pond2<- Test_ACP$CPISCapacite_total_With_Apport_2Adultes * Test_ACP$share_CAD
Test_ACP$Inter_Pond1<- Test_ACP$Prof_intermediairesCapacite_total_With_Apport * Test_ACP$share_INT
Test_ACP$Inter_Pond2<- Test_ACP$Prof_intermediairesCapacite_total_With_Apport_2Adultes * Test_ACP$share_INT



row.names(Test_ACP)<-paste(Test_ACP$DepCom, Test_ACP$year, sep="_")

Var_active<-Test_ACP[,c(15:ncol(Test_ACP))]      



res.pca<-PCA(Var_active,graph = TRUE,ncp = 5,scale.unit = TRUE)
summary(res.pca)
dimdesc(res.pca, axes=c(1:5))

Inertie_Axes_ACP_communes<-fviz_eig(res.pca, addlabels = TRUE, ylim = c(0,70))
ggsave("Inertie_Axes_ACP_communes.pdf",plot= Inertie_Axes_ACP_communes, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)


# Contributions des variables à PC5
Contrib_Axe1_bar<-fviz_contrib(res.pca, choice = "var", axes = 1, top = 10,xtickslab.rt = 90)
ggsave("Contrib_Axe1_bar.pdf",plot= Contrib_Axe1_bar, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)

# Contributions des variables à PC2
Contrib_Axe2_bar<-fviz_contrib(res.pca, choice = "var", axes = 2, top = 10 )
ggsave("Contrib_Axe2_bar.pdf",plot= Contrib_Axe2_bar, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)

# Contributions des variables à PC3
Contrib_Axe3_bar<-fviz_contrib(res.pca, choice = "var", axes = 3, top = 10,xtickslab.rt = 90)
ggsave("Contrib_Axe3_bar.pdf",plot= Contrib_Axe3_bar, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)

# Contributions des variables à PC4
Contrib_Axe4_bar<-fviz_contrib(res.pca, choice = "var", axes = 4, top = 10,xtickslab.rt = 90)
ggsave("Contrib_Axe4_bar.pdf",plot= Contrib_Axe4_bar, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)