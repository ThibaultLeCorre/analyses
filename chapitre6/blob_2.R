# Breaks en pourcentage
# voir boucle precedente dans blob

annees <- unique(carreauInsee1000_jointure$annee.x)
annees <- sort(annees[!is.na(annees)])


allBreaks_ratio <- list()
#etape 1
for (cetteAnneeLa in annees){
  
  carreaux_filtred <- carreauInsee1000_jointure %>%
    filter(annee.x == cetteAnneeLa)
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs=length(which(CSP_AC!="NA")),
              Ouvrier=length(which(CSP_AC>=60 & CSP_AC<70)),
              Cadres=length(which(CSP_AC>=30 & CSP_AC<40)),
              Prof_inter=length(which(CSP_AC>=40 & CSP_AC<50)),
              Employes=length(which(CSP_AC>=50 & CSP_AC<60)))
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  
  
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  MesCSP <- colnames(carreaux_resumes_spdf@data)
  MesCSP <- MesCSP[-c(1:2)]

  
  for (cetteCSPLa in MesCSP){
  
  stewartBreaks_ratio <- quickStewart(spdf = carreaux_resumes_spdf, 
                                df = carreaux_resumes_spdf@data, 
                                var = cetteCSPLa, var2 = "Nombre_transacs",
                                span = 3000, 
                                beta = 2, mask = Mask.spdf)
  results_ratio <- stewartBreaks_ratio@data
  results_ratio$annee <- cetteAnneeLa
  results_ratio$CSP <- cetteCSPLa
  allBreaks_ratio[[as.character(cetteAnneeLa)]][[cetteCSPLa]] <- results_ratio
}

}

allBreaksDF_ratio <- purrr::map(allBreaks_ratio, ~bind_rows(.x)) %>% bind_rows()
ggplot(allBreaksDF_ratio, aes(center, fill = as.factor(annee))) + geom_density(alpha = 0.1) + facet_wrap(~CSP, scales = "free")

breaksParCSP_ratio <- allBreaksDF_ratio %>%
  group_by(CSP) %>%
  summarise(breaksMax = list(quantile(max, probs = seq(0, 1, 0.2))))

#etape 2

for (cetteAnneeLa in annees){
  
  carreaux_filtred <- carreauInsee1000_jointure %>%
    filter(annee.x == cetteAnneeLa)
  
  carreaux_resumes <- carreaux_filtred %>%
    group_by(Carreau_ID) %>%
    summarise(Nombre_transacs=length(which(CSP_AC!="NA")),
              Ouvrier=length(which(CSP_AC>=60 & CSP_AC<70)),
              Cadres=length(which(CSP_AC>=30 & CSP_AC<40)),
              Prof_inter=length(which(CSP_AC>=40 & CSP_AC<50)),
              Employes=length(which(CSP_AC>=50 & CSP_AC<60)))
  
  
  MesCSP <- colnames(carreaux_resumes_spdf@data)
  MesCSP <- MesCSP[-c(1:2)]
  
  carreaux_resumes_spdf <- as(carreaux_resumes, "Spatial")
  proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
  carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")
  
  for (cetteCSPLa in MesCSP){
    
     mesBreaks_ratio <- breaksParCSP_ratio %>% filter(CSP == cetteCSPLa) %>% pull() 
     mesBreaks_ratio <- unname(unlist(c(0, mesBreaks_ratio)))
    
    maPalette <- cspColors[[cetteCSPLa]]
    
    pdf(file=paste0("~/Sauvegarde_figures/TousPotentielsCSP_Ratio/",cetteCSPLa, "_", cetteAnneeLa,".pdf"),width = 10, height = 5)
    tilesLayer(osmTiles)
  
  
    smoothLayer(spdf =  carreaux_resumes_spdf, 
                spdfid ='Carreau_ID' ,
                df =  carreaux_resumes_spdf@data,
                dfid = 'Carreau_ID' ,
                var =cetteCSPLa, var2 ='Nombre_transacs',
                span = 3000, beta = 2,
                 col = maPalette,
                breaks =mesBreaks_ratio,
                mask = Mask.spdf,
                lwd=0.1,
                legend.title.txt = sprintf("Potentiel de %s", cetteCSPLa),
                legend.pos = "topright", legend.values.rnd = 2,add=TRUE)
    layoutLayer(title = paste("Pourcentage", cetteCSPLa, cetteAnneeLa, sep = " - "))
    
    dev.off()
  }
  
}
