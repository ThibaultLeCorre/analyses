library(dplyr)
library(SpatialPosition)
library(cartography)
library(tidyr)
mytest<-data_redresse[,c("ID","annee.x","PRESCREDIT","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x","ANNAIS_AC","ANNAIS_VE", "NBRPIECE","REQTYPBIEN","BIDEPT", "REQ_ANC")]

 mytest<-mytest%>% filter(BIDEPT==75)

mytest$personne_morale_AC_rg<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"ADM",
                                                          ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise",
                                                                 ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Marchand",
                                                                        ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                                               ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"SCI",
                                                                                      ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"HLM", "particulier_ou_na"))))))



table(mytest$personne_morale_AC_rg, useNA = "ifany")


mytest$personne_morale_actifs_ac<- ifelse(mytest$CSP_AC>=1 & mytest$CSP_AC<=69&mytest$personne_morale_AC_rg == "particulier_ou_na","Actifs",
                                                              ifelse(mytest$CSP_AC==80  & mytest$personne_morale_AC_rg =="particulier_ou_na", "inactifs",
                                                                     ifelse(mytest$CSP_AC==70  & mytest$personne_morale_AC_rg =="particulier_ou_na" ,"retraites", 
                                                                     ifelse(is.na(mytest$CSP_AC) & mytest$personne_morale_AC_rg=="particulier_ou_na",NA,mytest$personne_morale_AC_rg))))


unique(mytest$personne_morale_actifs_ac)
####Fourchette age
mytest$Age_acq = mytest$annee.x-mytest$ANNAIS_AC
mytest$fourchette_age_acq<-mytest$Age_acq 
# mytest<-mytest%>%filter(fourchette_age_acq>=18)

mytest$fourchette_age_acq<- ifelse(mytest$fourchette_age_acq>=18 & mytest$fourchette_age_acq<30, "Jeunes",
                                          ifelse(mytest$fourchette_age_acq>=30 & mytest$fourchette_age_acq<50, "Age moyen",
                                                                      ifelse(mytest$fourchette_age_acq>=50 , "Vieux",NA)))


mytest$Type_acheteurs<-ifelse(mytest$personne_morale_actifs_ac=="Actifs"|mytest$personne_morale_actifs_ac=="inactifs",paste0(mytest$personne_morale_actifs_ac,"_",mytest$fourchette_age_acq),mytest$personne_morale_actifs_ac)

unique(mytest$Type_acheteurs)


mytest<-mytest%>% filter(REQTYPBIEN!="MA")
mytest$Type_log<- ifelse(mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>=0 & mytest$NBRPIECE<=3,"Petite maison",
                         ifelse(mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>3 & mytest$NBRPIECE<=5,"Maison taille moyenne",
                                ifelse( mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>7,"Maison exceptionnelle",
                                        ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=0 & mytest$NBRPIECE<=1, "studio et 1 pièce",
                                               ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE==2, "2 pièces",
                                                      ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=3 & mytest$NBRPIECE<=5, "Appartement taille moyenne",
                                                             ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=6, "Appartement exceptionnel", NA)))))))

# mytest$Type_log<- ifelse( mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>=0 & mytest$NBRPIECE<=3,"Petite maison (surtout ancienne)",
#                          ifelse(mytest$REQ_ANC==1 &mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>3 & mytest$NBRPIECE<=5,"Maison taille moyenne ancienne",
#                                 ifelse( mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>7,"Maison exceptionnelle (surtout ancienne)",
#                                         ifelse(mytest$REQ_ANC==1 &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=0 & mytest$NBRPIECE<=1, "studio et 1 pièce ancien",
#                                                ifelse(mytest$REQ_ANC==1 &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE==2, "Appartement 2 pièces ancien",
#                                                       ifelse(mytest$REQ_ANC==1 &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=3 & mytest$NBRPIECE<=5, "Appartement taille moyenne ancien",
#                                                              ifelse(mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=6, "Appartement exceptionnel (surtout ancien)", 
#                                                                            ifelse(mytest$REQ_ANC==2 &mytest$REQTYPBIEN=="MA" & mytest$NBRPIECE>3 & mytest$NBRPIECE<=5,"Maison taille moyenne neuve",
#                                                                                           ifelse(mytest$REQ_ANC==2 &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=0 & mytest$NBRPIECE<=1, "studio et 1 pièce neuf",
#                                                                                                  ifelse(mytest$REQ_ANC==2 &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE==2, "Appartement 2 pièces neuf",
#                                                                                                         ifelse(mytest$REQ_ANC==2 &mytest$REQTYPBIEN=="AP"& mytest$NBRPIECE>=3 & mytest$NBRPIECE<=5, "Appartement taille moyenne neuf",NA)))))))))))              
#                                                                    

mytest<-mytest%>% filter(Type_acheteurs!="SAFER"&Type_acheteurs!= "Actifs_NA"&Type_acheteurs!="inactifs_NA" & Type_acheteurs!="HLM" & Type_acheteurs!="ADM")  


Contigence_acquereru_logement<- (table (mytest$Type_acheteurs,mytest$Type_log, deparse.level = 1))/2

options(digits=5)
Khi2test<-chisq.test(Contigence_acquereru_logement)
residus_test_Khi2<-Khi2test$residuals
Khi2test


#Heatmap sur le test des résidu du KHi 2
library(ggplot2)
library(reshape2)
residus_test_Khi2<-melt(residus_test_Khi2, varnames = c("Acquereurs", "Logements"),value.name = "residus")
Contigence_acquereru_logement<-melt(Contigence_acquereru_logement, varnames = c("Acquereurs", "Logements"), value.name = "Effectifs")
tableau_resid_effectifs<-left_join(residus_test_Khi2, Contigence_acquereru_logement, by = c("Logements", "Acquereurs"))
#plot
tableau_resid_effectifs_plot<-ggplot(tableau_resid_effectifs, aes(Acquereurs,Logements, fill= residus)) +
  geom_tile()+
  geom_text(aes(label=round(Effectifs, digits = 0))) +
  theme_tmd() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-100,100), space = "Lab",
                       name= "Variation des résidus" )+
  labs(title = "Répartition des acquisitions en fonction du type de bien et du profil d'acquéreur", x= NULL, y= NULL)+ 
  labs(subtitle = "Heat map sur les résidus du test du Khi2 de Pearson.\nLes valeurs répertorient le nombre d'acquisition pour les principales catégoires d'acquéreurs. Les couleurs représentent les résidus du Khi² (différences entre valeurs observées et valeurs théoriques).\nlecture: Dans la période étudiée, la population active jeune, entre 18 et 30 ans, a acheté 9700 appartements 2 pièce à Paris\nCette valeur est au-dessus de la valeur théorique en cas d'indépendance statistique des relations.",
       x= "Acquéreurs", y= "Types de biens")+
  labs(caption = "Sources : échantillon BD BIEN sur effectifs redressés ; Réalisation : Thibault Le Corre, Géographie-Cités, 2018")

 library(extrafont)
 loadfonts()
 fonts() 
 ggsave("Khi2_type_biens_acquereurs_Paris.pdf",plot= tableau_resid_effectifs_plot,setwd("~/Sauvegarde_figures"), device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
 





###Periode
Transac$Periode<-ifelse(Transac$annee.x==1996|Transac$annee.x==1999|Transac$annee.x==2003, "Periode_96_2003",
                        ifelse(Transac$annee.x>=2004&Transac$annee.x<=2007, "Periode_04_2007", 
                               ifelse(Transac$annee.x>=2008&Transac$annee.x<=2012, "Periode_08_2012", NA)))




Periode <- unique(Transac$Periode)
Periode <- sort(Periode[!is.na(Periode)])


Transac_sf<-st_as_sf(Transac,
                     
                     coords = c("X.x", "Y.x"),
                     
                     agr = "constant",
                     
                     crs = 27572,
                     
                     stringsAsFactors = FALSE)


Transac_sf <-  st_transform(Transac_sf, crs = 2154)
coords<- st_coordinates(Transac_sf)
Transac_sf$XLamb93<-coords[,1]   
Transac_sf$Ylamb93<-coords[,2]
#######################################################

setwd("~/Shapes/datidf")
list.files()
carreauInsee1000 <- st_read("car1000m_idf_reparGeom.shp",
                            stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")

carreauInsee1000 <-  st_transform(carreauInsee1000, crs = 2154)
#Mask
Mask_st_1000 <- st_union(carreauInsee1000)
Mask_st_1000<-st_sf(id = 1, geometry =Mask_st_1000 )
par(mfrow = c(1,2))

Mask_st_1000 <- Mask_st_1000 %>%
  st_buffer(dist = 500) %>%
  st_buffer(dist = -1000) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 500) %>%
  st_buffer(500)

Cadrillage_1000<- carreauInsee1000%>%
  st_sf(geometry = .)

st_crs(Mask_st_1000)

st_crs(Cadrillage_1000)

Cadrillage_1000$Carreau_ID<-1:length(Cadrillage_1000$id_c1000)
#########################################################
Jointure_sf<-st_join(Cadrillage_1000, Transac_sf, join = st_contains, left=T)
allBreaks_ratio <- list()
table(Jointure_sf$fourchette_age_acq)
for (cettePeriodeLa in Periode){
  
  carreaux_filtred <- Jointure_sf %>%
    filter(Periode == cettePeriodeLa)
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs= n(),
              Les_18_25_ans=length(which(fourchette_age_acq =="[18,25["))/n(),
              Les_25_30_ans=length(which(fourchette_age_acq =="[25,30[")),
              Les_30_35_ans=length(which(fourchette_age_acq =="[30,35[")),
              Les_35_40_ans=length(which(fourchette_age_acq =="[35,40[")),
              Les_40_45_ans=length(which(fourchette_age_acq =="[40,45[")),
              Les_45_50_ans=length(which(fourchette_age_acq =="[45,50[")),
              Les_50_55_ans=length(which(fourchette_age_acq =="[50,55[")),
              Les_55_60_ans=length(which(fourchette_age_acq =="[55,60[")),
              Les_plus_60_ans=length(which(fourchette_age_acq =="[60+")))
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  Mask_spdf_1000<-as(Mask_st_1000, "Spatial")
  proj4string(Mask_spdf_1000) <-CRS("+init=epsg:2154")
  Mask_spdf_1000<- spTransform(Mask_spdf_1000, "+init=epsg:2154")
  
  Mescategories <- colnames(carreaux_resumes_spdf@data)
  Mescategories <- Mescategories[-c(1,2)]
  
  for (cetFourchette_ageLa in Mescategories){
    
    stewartBreaks_ratio <- quickStewart(spdf = carreaux_resumes_spdf, 
                                        df = carreaux_resumes_spdf@data, 
                                        var = cetFourchette_ageLa,var2 = "Nombre_transacs",
                                        span = 3000, 
                                        beta = 2, mask = Mask_spdf_1000)
    results_ratio <- stewartBreaks_ratio@data
    results_ratio$Periode <- cettePeriodeLa
    results_ratio$categories <- cetFourchette_ageLa
    allBreaks_ratio[[as.character(cettePeriodeLa)]][[cetFourchette_ageLa]] <- results_ratio
  }
  
}

allBreaksDF_ratio <- purrr::map(allBreaks_ratio, ~bind_rows(.x)) %>% bind_rows()

breaksParCategorie <- allBreaksDF_ratio %>%
  group_by(categories) %>%
  gather(key = id,value=bornes, c(2,3,4))%>%
  summarise(breaksMax = list(getBreaks(bornes, nclass = 10, method = "quantile")))
breaks_ratio<- unname(unlist(c(breaksParCategorie)))

cols=carto.pal(pal1 = 'grey.pal', n1 = 10)  


#etape 2
for (cettePeriodeLa in Periode){
  
  carreaux_filtred <- Jointure_sf %>%
    filter(Periode == cettePeriodeLa)
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs= n(),
              Les_18_25_ans=length(which(fourchette_age_acq =="[18,25["))/n(),
              Les_25_30_ans=length(which(fourchette_age_acq =="[25,30[")),
              Les_30_35_ans=length(which(fourchette_age_acq =="[30,35[")),
              Les_35_40_ans=length(which(fourchette_age_acq =="[35,40[")),
              Les_40_45_ans=length(which(fourchette_age_acq =="[40,45[")),
              Les_45_50_ans=length(which(fourchette_age_acq =="[45,50[")),
              Les_50_55_ans=length(which(fourchette_age_acq =="[50,55[")),
              Les_55_60_ans=length(which(fourchette_age_acq =="[55,60[")),
              Les_plus_60_ans=length(which(fourchette_age_acq =="[60+")))
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  
  Mescategories <- colnames(carreaux_resumes_spdf@data)
  Mescategories <- Mescategories[-c(1,2)]
  
  for (cetFourchette_ageLa in Mescategories){
    
    mesBreaks_ratio <- breaksParCategorie %>% filter(categories == cetFourchette_ageLa) %>% pull() 
    mesBreaks_ratio <- unname(unlist(c(mesBreaks_ratio)))
    
    setwd("~/Sauvegarde_figures/Difference_type_foruchette_age")
    pdf(file=paste0("~/Sauvegarde_figures/Difference_type_foruchette_age/",cetFourchette_ageLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
    
    plot(Mask_spdf_1000)
    smoothLayer(spdf= carreaux_resumes_spdf, df= carreaux_resumes_spdf@data,
                var =cetFourchette_ageLa, var2 = "Nombre_transacs",
                span = 3000, beta = 2,
                col = cols,
                breaks =mesBreaks_ratio,
                mask = Mask_spdf_1000,
                lwd=0.1,
                legend.title.txt = sprintf("Potentiel de %s", cetFourchette_ageLa),
                legend.pos = "topright", legend.values.rnd=2,add=T)
    
    plot(DepInsee_spdf,add=T)
    layoutLayer(title = paste(cetFourchette_ageLa, cettePeriodeLa, sep = " - "))
    
    dev.off()
  }
  
}

DepInsee

DepInsee_spdf <- as(DepInsee, "Spatial")
proj4string(DepInsee_spdf) <-CRS("+init=epsg:2154")
DepInsee_spdf<- spTransform(DepInsee_spdf, "+init=epsg:2154")
