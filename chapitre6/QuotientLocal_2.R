mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","CSP_AC","X.x","Y.x")]

mytest$acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM",
                           ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                  ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                         ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                                       ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM", "particulier_ou_na"))))))



mytest$acquereurs<- ifelse(mytest$CSP_AC == 10 & mytest$acquereurs == "particulier_ou_na", "Agriculteurs",
                           ifelse(mytest$CSP_AC >= 20 & mytest$CSP_AC < 30 & mytest$acquereurs == "particulier_ou_na", "Com_art_Chef_entreprises",
                                  ifelse(mytest$CSP_AC >= 30 & mytest$CSP_AC < 40 & mytest$acquereurs == "particulier_ou_na", "CPIS",
                                         ifelse(mytest$CSP_AC >= 40 & mytest$CSP_AC < 50 & mytest$acquereurs == "particulier_ou_na", "Prof_intermediaires",
                                                ifelse(mytest$CSP_AC >= 50 & mytest$CSP_AC < 60 & mytest$acquereurs == "particulier_ou_na", "Employes",
                                                       ifelse(mytest$CSP_AC >= 60 & mytest$CSP_AC < 70 & mytest$acquereurs == "particulier_ou_na", "Ouvriers",
                                                              ifelse(mytest$CSP_AC >= 70 & mytest$CSP_AC < 80 & mytest$acquereurs == "particulier_ou_na", "retraites",
                                                                     ifelse(mytest$CSP_AC == 80 & mytest$acquereurs == "particulier_ou_na", "autres_inactifs", mytest$acquereurs))))))))




###Periode
mytest$Periode<-ifelse(mytest$annee.x==1996|mytest$annee.x==1999|mytest$annee.x==2003, "Periode_96_2003",
                       ifelse(mytest$annee.x>=2004&mytest$annee.x<=2007, "Periode_04_2007", 
                              ifelse(mytest$annee.x>=2008&mytest$annee.x<=2012, "Periode_08_2012", NA)))

unique(mytest$acquereurs)
mytest <- mytest %>%
  filter(!is.na(acquereurs), acquereurs!="SAFER")

test<- mytest
#Cr?ation du fichier SF (lambert II ?tendu, coord dorigine fichier BIEN)

spVente <- st_as_sf(test,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)


#conversion WGS84 et stockage coords

spVente <-  st_transform(spVente, crs = 4326)
coords<- st_coordinates(spVente)
spVente$longWGS84<-coords[,1]   
spVente$latWGS84<-coords[,2]

#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]   
spVente$Ylamb93<-coords[,2]

## essai repr?sentation
osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)


##Jointure

carreauInsee1000_jointure<-st_join(carreauInsee1000_2, spVente, join = st_contains, left=T)



Periodes <- unique(carreauInsee1000_jointure$Periode)
Periodes <- sort(Periodes[!is.na(Periodes)])

# Results_QuotientLocal<-list()
Results_QuotientLocal<- data.frame(Carreau_ID=NA,CSP=NA, QuotientLocal=NA,Periode=NA)
Results_QuotientLocal<-Results_QuotientLocal%>%
  filter(!is.na(cettePeriodeLa))

for (cettePeriodeLa in Periodes){
  
  carreaux_filtred <- carreauInsee1000_jointure %>%
    filter(Periode == cettePeriodeLa)
  
  # carreaux_filtred <-  carreaux_filtred[sample(1: nrow(carreaux_filtred), 500,replace=T), ]

  # Total <- unique(carreaux_filtred$Total)
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID, acquereurs) %>%
    summarise(Nombre=length(acquereurs))
  
  Total_Transac<-sum(carreaux_resumes$Nombre, na.rm = TRUE)
  
  carreaux_resumes_2 <- as.data.frame(carreaux_filtred) %>%
    group_by(Carreau_ID) %>%
    summarise(Total_Unit=length(acquereurs))

  carreaux_resumes <- left_join(carreaux_resumes, carreaux_resumes_2[,c("Carreau_ID","Total_Unit")])
  
  
  carreaux_resumes_3 <- as.data.frame(carreaux_resumes)%>%
  spread(acquereurs, Nombre, fill=0)

# carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
# proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
# carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")

MesCSP <- colnames(carreaux_resumes_3)
MesCSP <- MesCSP[-c(1,2,3,4,5,6)]

for (cetteCSPLa in MesCSP){
  
  
  carreaux_resumes_4 <- carreaux_resumes_3%>%
    gather("CSP", "Value", 4:13)%>%
    filter(CSP == cetteCSPLa)
  
  TotCSP <- sum(carreaux_resumes_4$Value, na.rm = TRUE)
  

  # carreaux_resumes_3<-semi_join(carreaux_resumes_3, carreaux_resumes[,c("Carreau_ID","Total_Unit")], by="Carreau_ID")
  carreaux_resumes_4$TotCSP<-TotCSP
  carreaux_resumes_4$Total_Transac<-Total_Transac
  
 QuotientLocal<-(carreaux_resumes_4$Value / carreaux_resumes_4$Total_Unit) / ( carreaux_resumes_4$TotCSP / carreaux_resumes_4$Total_Transac)
 
 # QuotientLocal<-as.vector(QuotientLocal)
 testQuotientLocal<-data.frame(carreaux_resumes_4[,c("Carreau_ID","CSP")])
 # testQuotientLocal$Carreau_ID<-unique(carreaux_resumes_4$Carreau_ID)
 testQuotientLocal<- cbind(testQuotientLocal,QuotientLocal)
 testQuotientLocal$Periode <- cettePeriodeLa
 # testQuotientLocal$CSP <- cetteCSPLa
 # Results_QuotientLocal[[cettePeriodeLa]][[cetteCSPLa]] <- testQuotientLocal

 Results_QuotientLocal<- rbind(Results_QuotientLocal, testQuotientLocal)
 
 QuotientLocal_sf <- inner_join (carreauInsee1000_2, testQuotientLocal, by = "Carreau_ID")
 

 Mybreaks<- c(0,0.5,1,1.5,3,max(QuotientLocal_sf$QuotientLocal))
 
 QuotientLocal_sf$Dummies<- 1
 
 Mescouleurs<- carto.pal(pal1 = "blue.pal",n1 = 2, pal2 = "wine.pal", n2 = 3 )
 
 carreaux_resumes_spdf <- as(QuotientLocal_sf, "Spatial")
 proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
 carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
 
 # QuotientLocal_sf <-QuotientLocal_sf%>%
 #   filter(QuotientLocal>=0.001)
 
 pdf(file=paste0("~/Projets/Chapitre7/Cartes_acquereurs/Cartes_acq_QuotientLocal/",cetteCSPLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
 tilesLayer(osmTiles)
 
 
 # display.carto.all(n = 10)
# choroLayer(QuotientLocal_sf, 
#            var ="QuotientLocal", breaks =Mybreaks,
#            col = Mescouleurs,
#            lwd=0.001,
#            legend.title.txt = sprintf("QuotientLocal", cetteCSPLa),
#            legend.pos = "topright", legend.values.rnd = 6,add=TRUE)
# layoutLayer(title = paste("Nombre par annee", cetteCSPLa, cettePeriodeLa, sep = " - "))

 
 
 smoothLayer(spdf = carreaux_resumes_spdf, 
             spdfid ='Carreau_ID' ,
             df =  carreaux_resumes_spdf@data,
             dfid = 'Carreau_ID' ,
             var ="QuotientLocal", var2 = "Dummies",
             span = 3000, beta = 2,
             col = Mescouleurs,
             breaks =Mybreaks,
             mask = Mask.spdf,
             lwd=0.1,
             legend.title.txt = sprintf("QuotientLocal", cetteCSPLa),
             legend.pos = "topright", legend.values.rnd = 6,add=TRUE)
 layoutLayer(title = paste("Nombre par annee", cetteCSPLa, cettePeriodeLa, sep = " - "))
 
 dev.off()
 
}

}

 

QuotientLocal <- purrr::map(testQuotientLocal, ~bind_rows(.x)) %>% bind_rows()

QuotientLocal <- QuotientLocal %>%
  group_by(cetteCSPLa)

for (cettePeriodeLa in Periodes){
 
   carreaux_filtred <- carreauInsee1000_jointure %>%
    filter(Periode == cettePeriodeLa)
  

  MesCSP <- colnames(carreaux_resumes_spdf@data)
  MesCSP <- MesCSP[-c(1,12)]
  
  for (cetteCSPLa in MesCSP){
    

smoothLayer(x = carreaux_resumes_2, 
            var ="Value", var2 = "TotCSP",
            span = 3000, beta = 2,
            col = maPalette,
            breaks =mesBreaks_ratio,
            mask = Mask.st,
            lwd=0.1,
            legend.title.txt = sprintf("Probabilite by year ", cetteCSPLa),
            legend.pos = "topright", legend.values.rnd = 6,add=TRUE)
layoutLayer(title = paste("Nombre par annee", cetteCSPLa, cettePeriodeLa, sep = " - "))
