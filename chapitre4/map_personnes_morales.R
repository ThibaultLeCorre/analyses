#Map personnes morales
library(dplyr)
library(SpatialPosition)
library(cartography)
library(tidyr)


mytest<-data_redresse_code_promo_2[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","cluster.y")]
# table(data_redresse_code_promo_2$typeVar, data_redresse_code_promo_2$CSP_VE)

mytest$Acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Administration",
                                                          ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise",
                                                                 ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Marchands de biens",
                                                                        ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                                               ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"SCI",
                                                                                      ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"HLM", "particulier_ou_na"))))))






####### pour le renseignement sur les vendeurs#

mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"Administration",
                                      ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"Entreprise",
                                             ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"Marchands de biens",
                                                    ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"SAFER",
                                                           ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"SCI",
                                                                  ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"HLM", "particulier_ou_na"))))))
mytest$Vendeurs<- ifelse(mytest$cluster.y>=1 & !is.na(mytest$cluster.y),"Promoteurs", mytest$Vendeurs)
table(mytest$Vendeurs)


mytest_acq<-as.data.frame(table(mytest$annee.x,mytest$Acquereurs,useNA = "ifany"))
mytest_acq$Partie<-"Acquereurs"
mytest_ve<-as.data.frame(table(mytest$annee.x,mytest$Vendeurs,useNA = "ifany"))
mytest_ve$Partie<-"Vendeurs"
mytest<-rbind(mytest_acq,mytest_ve)

mytest<-mytest%>% filter(Var2!="particulier_ou_na" & Var2!="SAFER" )


mytest$Année<-factor(mytest$Var1)
mytest$Année<-as.character(mytest$Année)
mytest$Année<-as.numeric(mytest$Année)
mytest$Freq<-as.numeric(mytest$Freq)
mytest$Type<-mytest$Var2

library(reshape2)
library(ggplot2)
Personnes_morales_volumes_plot<-ggplot(mytest, aes(Année, Freq, group=Var2)) +
  geom_line(aes(color = Var2),size=1)+
  scale_x_continuous(breaks = c(1996,1999,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  facet_wrap(~Partie, scales="free")+ 
  labs(title = "Volume des transactions selon le type des personnes morales", 
       subtitle = "Calcul réalisé par année sur les effectifs redressés des transactions", x= NULL, y= "Effectifs")+ 
  labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

###



























##################################################


mytest<-data_redresse_code_promo_2[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","cluster.y","X.x","Y.x")]
# table(data_redresse_code_promo_2$typeVar, data_redresse_code_promo_2$CSP_VE)

mytest$Acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Administration",
                           ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise",
                                  ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Marchands de biens",
                                         ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"SCI",
                                                       ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"HLM", "particulier_ou_na"))))))


mytest$Acquereurs<- ifelse(mytest$CSP_AC>=1 & mytest$CSP_AC<=69 &mytest$Acquereurs == "particulier_ou_na","Actifs",
                           ifelse(mytest$CSP_AC>=70 & mytest$CSP_AC<=90 & mytest$Acquereurs =="particulier_ou_na", "retraites_inactifs", 
                                  ifelse(is.na(mytest$CSP_AC) & mytest$Acquereurs=="particulier_ou_na",NA,mytest$Acquereurs)))

table(mytest$Acquereurs, useNA = "ifany")

mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" &!is.na(mytest$QUALITE_VE),"Administration",
                         ifelse(mytest$QUALITE_VE== "EN"&!is.na(mytest$QUALITE_VE),"Entreprise",
                                ifelse(mytest$QUALITE_VE== "PR"&!is.na(mytest$QUALITE_VE),"Marchands de biens",
                                       ifelse(mytest$QUALITE_VE== "SA"&!is.na(mytest$QUALITE_VE),"SAFER",
                                              ifelse(mytest$QUALITE_VE== "SC"&!is.na(mytest$QUALITE_VE),"SCI",
                                                     ifelse(mytest$QUALITE_VE== "SO"&!is.na(mytest$QUALITE_VE),"HLM", "particulier_ou_na"))))))


mytest$Vendeurs<- ifelse(mytest$CSP_VE>=1 & mytest$CSP_VE<=69& mytest$Vendeurs == "particulier_ou_na","Actifs",
                           ifelse(mytest$CSP_VE>=70 & mytest$CSP_VE<=90 & mytest$Vendeurs =="particulier_ou_na", "retraites_inactifs", 
                                  ifelse(is.na(mytest$CSP_VE) & mytest$Vendeurs=="particulier_ou_na",NA,mytest$Vendeurs)))

mytest$Vendeurs<- ifelse(mytest$cluster.y>=1 & !is.na(mytest$cluster.y),"Promoteurs", mytest$Vendeurs)
table(mytest$Vendeurs, useNA = "ifany")

mytest<- mytest%>% filter(`X.x`!= 0 & `Y.x`!=0, !is.na(Vendeurs),!is.na(Acquereurs),Acquereurs!="SAFER", Vendeurs!="SAFER")


Transac<-mytest
###Periode
Transac$Periode<-ifelse(Transac$annee.x==1996|Transac$annee.x==1999|Transac$annee.x==2003, "Periode_96_2003",
                        ifelse(Transac$annee.x>=2004&Transac$annee.x<=2007, "Periode_04_2007", 
                               ifelse(Transac$annee.x>=2008&Transac$annee.x<=2012, "Periode_08_2012", NA)))
####



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
#########################################################
Jointure_sf<-st_join(Cadrillage_1000, Transac_sf, join = st_contains, left=T)
allBreaks_ratio <- list()
  table(Jointure_sf$Vendeurs)
  for (cettePeriodeLa in Periode){
    
    carreaux_filtred <- Jointure_sf %>%
      filter(Periode == cettePeriodeLa)
    # 
    # carreaux_resumes <- carreaux_filtred %>%
    #   group_by(Carreau_ID) %>%
    #   summarise(Actifs_Vendeurs=length(which(Vendeurs =="Actifs")),
    #             Actifs_Acquereurs=length(which(Acquereurs =="Actifs")),
    #             retraites_Vendeurs=length(which(Vendeurs =="retraites_inactifs")),
    #             retraites_Acquereurs=length(which(Acquereurs =="retraites_inactifs")),
    #             Administration_Vendeurs=length(which(Vendeurs =="Administration")),
    #             Administration_Acquereurs=length(which(Acquereurs =="Administration")),
    #             Entreprise_Vendeurs=length(which(Vendeurs =="Entreprise")),
    #             Entreprise_Acquereurs=length(which(Acquereurs =="Entreprise")),
    #             HLM_Vendeurs=length(which(Vendeurs =="HLM")),
    #             HLM_Acquereurs=length(which(Acquereurs =="HLM")),
    #             Marchands_Vendeurs=length(which(Vendeurs =="Marchands de biens")),
    #             Marchands_Acquereurs=length(which(Acquereurs =="Marchands de biens")),
    #             SCI_Vendeurs=length(which(Vendeurs =="SCI")),
    #             SCI_Acquereurs=length(which(Acquereurs =="SCI")),
    #             Promoteurs_Vendeurs=length(which(Vendeurs =="Promoteurs")))
    #   carreaux_resumes<-as.data.frame(carreaux_resumes)
    #   carreaux_resumes_test<- gather(carreaux_resumes,key = "Type", value= "Value", c(2:16) )
    #             ?gather
    carreaux_filtred <- Jointure_sf %>%
      filter(Periode == cettePeriodeLa)
    
    carreaux_resumes_1<- carreaux_filtred %>%
     group_by(Carreau_ID,Acquereurs)  %>%
      summarise(n_acq=n()) %>%
      spread(Acquereurs,n_acq,fill = 0)%>%
      gather(key = "Type_acquereur", value= "Value", c(2:8))
   
    carreaux_resumes_1<- carreaux_filtred %>%
      group_by(Carreau_ID,Acquereurs)  %>%
      summarise(n_acq=n()) %>%
      spread(Acquereurs,n_acq,fill = 0)%>%
      gather(key = "Type_acquereur", value= "Value_acq", c(2:8))
    
    carreaux_resumes_2<- carreaux_filtred %>%
      group_by(Carreau_ID,Vendeurs)  %>%
      summarise(n_ve=n()) %>%
      spread(Vendeurs,n_ve,fill = 0)%>%
      gather(key = "Type_vendeur", value= "Value_ve", c(2:9))
 
   carreaux_resumes_1<-as.data.frame(carreaux_resumes_1)
   carreaux_resumes_2<-as.data.frame(carreaux_resumes_2)
   carreaux_resumes<-full_join(carreaux_resumes_1,carreaux_resumes_2,by=c("Carreau_ID", "Type_acquereur"="Type_vendeur"))
    carreaux_resumes<-st_join(carreaux_resumes_1,carreaux_resumes_2 )
    carreaux_resumes$rapport<-carreaux_resumes$Value_acq/carreaux_resumes$Value_ve
    ?full_join
    rm(carreaux_resumes_1,carreaux_resumes_2)
     
   n_groups(carreaux_resumes)
    
      summarise(Rapport_acq_ve= n_distinct(Acquereurs)/n_distinct(Vendeurs))
    
    carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
    proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
    carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
    
    Mescategories <- colnames(carreaux_resumes_spdf@data)
    Mescategories <- Mescategories[-c(1)]
    
    
    for (cetAcq_vendeurLa in Mescategories){
   
      stewartBreaks_ratio <- quickStewart(spdf = carreaux_resumes_spdf, 
                                          df = carreaux_resumes_spdf@data, 
                                          var = cetAcq_vendeurLa,
                                          span = 3000, 
                                          beta = 2, mask = Mask_spdf_1000)
      results_ratio <- stewartBreaks_ratio@data
      results_ratio$Periode <- cettePeriodeLa
      results_ratio$categories <- cetAcq_vendeurLa
      allBreaks_ratio[[as.character(cettePeriodeLa)]][[cetAcq_vendeurLa]] <- results_ratio
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
  
  for (cettePeriodeLa in Periode) {
    
    carreaux_filtred <- Jointure_sf %>%
      filter(Periode == cettePeriodeLa)
    
    carreaux_resumes <- carreaux_filtred %>%
      group_by(Carreau_ID) %>%
      summarise(Actifs_Vendeurs=length(which(Vendeurs =="Actifs")),
                Actifs_Acquereurs=length(which(Acquereurs =="Actifs")),
                retraites_Vendeurs=length(which(Vendeurs =="retraites_inactifs")),
                retraites_Acquereurs=length(which(Acquereurs =="retraites_inactifs")),
                Administration_Vendeurs=length(which(Vendeurs =="Administration")),
                Administration_Acquereurs=length(which(Acquereurs =="Administration")),
                Entreprise_Vendeurs=length(which(Vendeurs =="Entreprise")),
                Entreprise_Acquereurs=length(which(Acquereurs =="Entreprise")),
                HLM_Vendeurs=length(which(Vendeurs =="HLM")),
                HLM_Acquereurs=length(which(Acquereurs =="HLM")),
                Marchands_Vendeurs=length(which(Vendeurs =="Marchands de biens")),
                Marchands_Acquereurs=length(which(Acquereurs =="Marchands de biens")),
                SCI_Vendeurs=length(which(Vendeurs =="SCI")),
                SCI_Acquereurs=length(which(Acquereurs =="SCI")),
                Promoteurs_Vendeurs=length(which(Vendeurs =="Promoteurs")))
    
    
    Mescategories <- colnames(carreaux_resumes_spdf@data)
    Mescategories <- Mescategories[-c(1)]
    
   
    for (cetAcq_vendeurLa in Mescategories){
      
      mesBreaks_ratio <- breaksParCategorie %>% filter(categories == cetAcq_vendeurLa) %>% pull() 
      mesBreaks_ratio <- unname(unlist(c(mesBreaks_ratio)))
      
      setwd("~/Sauvegarde_figures/Categoires_acquereurs_vendeurs")
      pdf(file=paste0("~/Sauvegarde_figures/Categoires_acquereurs_vendeurs/",cetAcq_vendeurLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
     
    
      smoothLayer(x = carreaux_resumes,
                  var =cetAcq_vendeurLa, 
                  span = 3000, beta = 2,
                  col = cols,
                  breaks =mesBreaks_ratio,
                  mask = Mask_st_1000,
                  lwd=0.1,
                  legend.title.txt = sprintf("Potentiel de %s", cetAcq_vendeurLa),
                  legend.pos = "topright", legend.values.rnd = 0)
      
      plot(st_geometry(lim_IDF.st),add=T)
      layoutLayer(title = paste(cetAcq_vendeurLa, cettePeriodeLa, sep = " - "))
      
      dev.off()
  }
  
}

  
  ######################################
  
  
  
  
  
  
  
  
  
  
  Jointure_sf<-st_join(Cadrillage_1000, Transac_sf, join = st_contains, left=T)
  allBreaks_ratio <- list()
  table(Jointure_sf$Vendeurs)
  for (cettePeriodeLa in Periode){
    
    carreaux_filtred <- Jointure_sf %>%
      filter(Periode == cettePeriodeLa)
    
    carreaux_resumes <- carreaux_filtred %>%
      group_by(Carreau_ID) %>%
      summarise(Actifs_DiffAcquereur_Vendeurs=length(which(Acquereurs =="Actifs"))-length(which(Vendeurs =="Actifs")),
                retraites_DiffAcquereur_Vendeurs=length(which(Vendeurs =="retraites_inactifs"))-length(which(Acquereurs =="retraites_inactifs")),
                Administration_DiffAcquereur_Vendeurs=length(which(Acquereurs =="Administration"))-length(which(Vendeurs =="Administration")),
                Entreprise_DiffAcquereur_Vendeurs=length(which(Acquereurs =="Entreprise"))-length(which(Vendeurs =="Entreprise")),
                HLM_DiffAcquereur_Vendeurs=length(which(Acquereurs =="HLM"))-length(which(Vendeurs =="HLM")),
                Marchands_DiffAcquereur_Vendeurs=length(which(Acquereurs =="Marchands de biens"))-length(which(Vendeurs =="Marchands de biens")),
                SCI_DiffAcquereur_Vendeurs=length(which(Acquereurs =="SCI"))-length(which(Vendeurs =="SCI")))
    
    
    carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
    proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
    carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
    
    Mescategories <- colnames(carreaux_resumes_spdf@data)
    Mescategories <- Mescategories[-c(1)]
    
    
    for (cetAcq_vendeurLa in Mescategories){
      
      stewartBreaks_ratio <- quickStewart(spdf = carreaux_resumes_spdf, 
                                          df = carreaux_resumes_spdf@data, 
                                          var = cetAcq_vendeurLa,
                                          span = 3000, 
                                          beta = 2, mask = Mask_spdf_1000)
      results_ratio <- stewartBreaks_ratio@data
      results_ratio$Periode <- cettePeriodeLa
      results_ratio$categories <- cetAcq_vendeurLa
      allBreaks_ratio[[as.character(cettePeriodeLa)]][[cetAcq_vendeurLa]] <- results_ratio
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
  
  for (cettePeriodeLa in Periode) {
    
    carreaux_filtred <- Jointure_sf %>%
      filter(Periode == cettePeriodeLa)
    
    carreaux_resumes <- carreaux_filtred %>%
      group_by(Carreau_ID) %>%
      summarise(Actifs_DiffAcquereur_Vendeurs=length(which(Acquereurs =="Actifs"))-length(which(Vendeurs =="Actifs")),
                retraites_DiffAcquereur_Vendeurs=length(which(Vendeurs =="retraites_inactifs"))-length(which(Acquereurs =="retraites_inactifs")),
                Administration_DiffAcquereur_Vendeurs=length(which(Acquereurs =="Administration"))-length(which(Vendeurs =="Administration")),
                Entreprise_DiffAcquereur_Vendeurs=length(which(Acquereurs =="Entreprise"))-length(which(Vendeurs =="Entreprise")),
                HLM_DiffAcquereur_Vendeurs=length(which(Acquereurs =="HLM"))-length(which(Vendeurs =="HLM")),
                Marchands_DiffAcquereur_Vendeurs=length(which(Acquereurs =="Marchands de biens"))-length(which(Vendeurs =="Marchands de biens")),
                SCI_DiffAcquereur_Vendeurs=length(which(Acquereurs =="SCI"))-length(which(Vendeurs =="SCI")))
    
    
    
    Mescategories <- colnames(carreaux_resumes_spdf@data)
    Mescategories <- Mescategories[-c(1)]
    
    
    for (cetAcq_vendeurLa in Mescategories){
      
      mesBreaks_ratio <- breaksParCategorie %>% filter(categories == cetAcq_vendeurLa) %>% pull() 
      mesBreaks_ratio <- unname(unlist(c(mesBreaks_ratio)))
      
      setwd("~/Sauvegarde_figures/Difference_acquereurs_vendeurs")
      pdf(file=paste0("~/Sauvegarde_figures/Difference_acquereurs_vendeurs/",cetAcq_vendeurLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
      
      plot(st_geometry(Mask_st_1000))
      smoothLayer(x = carreaux_resumes,
                  var =cetAcq_vendeurLa, 
                  span = 3000, beta = 2,
                  col = cols,
                  breaks =mesBreaks_ratio,
                  mask = Mask_st_1000,
                  lwd=0.1,
                  legend.title.txt = sprintf("Potentiel de %s", cetAcq_vendeurLa),
                  legend.pos = "topright", legend.values.rnd = 0,add=T)
      
      plot(st_geometry(lim_IDF.st),add=T)
      layoutLayer(title = paste(cetAcq_vendeurLa, cettePeriodeLa, sep = " - "))
      
      dev.off()
    }
    
  }
  
  

