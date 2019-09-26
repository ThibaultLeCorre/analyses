Tableau_Acquereur_Carreaux1000 <- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Carreaux1000.csv", stringsAsFactors=FALSE)
mytest<-Tableau_Acquereur_Carreaux1000
mytest$Periode<-ifelse(mytest$cetteanneeLa==1996|mytest$cetteanneeLa==1999|mytest$cetteanneeLa==2003, "Periode_96_2003",
                       ifelse(mytest$cetteanneeLa>=2004&mytest$cetteanneeLa<=2007, "Periode_04_2007", 
                              ifelse(mytest$cetteanneeLa>=2008&mytest$cetteanneeLa<=2012, "Periode_08_2012", NA)))

mytest$cetacquereurLa<- ifelse(mytest$cetacquereurLa == "Liberales", "Com_art_Chef_entreprises",mytest$cetacquereurLa)
mytest_2 <- mytest %>%
  group_by(Periode,Carreau_ID,cetacquereurLa) %>%
  summarise(Total_type= sum(Somme_Profil_acquereur))
mytest_3 <- mytest %>%
  group_by(Periode,cetacquereurLa) %>%
  summarise(Total_acq = sum(Somme_Profil_acquereur))
mytest<- left_join(mytest_2, mytest_3)
mytest$Proba<- (mytest$Total_type / mytest$Total_acq)*100



mytest<-mytest %>%
  group_by(Periode,cetacquereurLa) %>%
mutate(rank = ntile(Proba,n = 10))


mytest<-mytest %>%
  group_by(Periode,cetacquereurLa, rank) %>%
  mutate(Weight_Rank = sum(Proba))


mytest<-left_join(carreauInsee1000_2, mytest, by="Carreau_ID")

######################
Periodes <- unique(mytest$Periode)
Periodes <- sort(Periodes[!is.na(Periodes)])

MesCSP <- unique(mytest$cetacquereurLa)
MesCSP <- sort(MesCSP[!is.na(MesCSP)])



for (cettePeriodeLa in Periodes){
  
mytest_2<- mytest%>%
  filter(Periode==cettePeriodeLa)


for (cetteCSPLa in MesCSP){
  
  mytest_3<- mytest_2%>%
    filter(cetacquereurLa == cetteCSPLa)
  
  carreaux_resumes_spdf <- as(mytest_3, "Spatial")
  proj4string(carreaux_resumes_spdf) <- CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")

pdf(file=paste0("~/Projets/Chapitre7/Cartes_acquereurs/Cartes_acq_NombreByYears_SumProba/",cetteCSPLa, "_", cettePeriodeLa,".pdf"),width = 10, height = 5)
tilesLayer(osmTiles)

maPalette <- cspColors[[cetteCSPLa]]

smoothLayer(spdf =carreaux_resumes_spdf, 
            spdfid ='Carreau_ID' ,
            df =  carreaux_resumes_spdf@data,
            dfid = 'Carreau_ID' ,
            var ="Proba",
            span = 3000, beta = 2,
            col = maPalette,
            nclass=10,
            mask = Mask.spdf,
            lwd=0.1,
            legend.title.txt = sprintf("Probabilite by year ", cetteCSPLa),
            legend.pos = "topright", legend.values.rnd = 5,add=T)
layoutLayer(title = paste("Nombre par annee", cetteCSPLa, cettePeriodeLa, sep = " - "))

dev.off()

  }
}


