


dev.off()
#etape 2 : boucle
for (cetteAnneeLa in annees){
  
  spVente_2<-spVente%>% filter(annee.x==cetteAnneeLa & REQTYPBIEN=="MA")%>%
    as( "Spatial")
  proj4string(spVente_2) <-CRS("+init=epsg:2154")
  spVente_2<- spTransform(spVente_2, "+init=epsg:2154")
  
  
  spVente_2$dummy <- 1
  stewartPrix <- mcStewart(knownpts = spVente_2, 
                           unknownpts = cadrillage_1000_spdf,
                           varname = "REQ_PRIX", 
                           typefct = "exponential", span = 3000, beta = 2,
                           mask = Mask_spdf_1000, longlat = FALSE)
  
  stewartTransac <- mcStewart(knownpts = spVente_2, 
                              unknownpts = cadrillage_1000_spdf,
                              varname = "dummy", 
                              typefct = "exponential", span = 3000, beta = 2,
                              mask = Mask_spdf_1000, longlat = FALSE)
  
  
  stewartTransac_SF <- st_as_sf(stewartTransac) %>%
    rename(NbTransac = OUTPUT)
  stewartTransac_df<-as.data.frame(stewartTransac_SF)
  stewartPrix_SF <- st_as_sf(stewartPrix) %>% 
    rename(Prix = OUTPUT)
  
  
  stewartPrix_SF <- left_join(stewartPrix_SF %>%
                                select(Prix,Carreau_ID),
                              stewartTransac_df%>%
                                select(NbTransac,Carreau_ID),  
                              by="Carreau_ID") %>%
    mutate(prixPotentiel = Prix/ NbTransac)
  
  #attention focntionne pas si environnement de travail pas precise
  setwd("~/Sauvegarde_figures/maisons_carreau1000_span3000_5036tiles")
  pdf(file=paste0(~"Sauvegarde_figures/maisons_carreau_1000_span3000_5036tiles",cetteAnneeLa,".pdf"),width = 10, height = 5)
  tilesLayer(osmTiles)
  plot(Mask_st_1000, col= col_mask, border=NA,add=T, main=NULL)
  
  choroLayer(x=stewartPrix_SF,
             var = "prixPotentiel",
             col = cols,
             colNA = col_mask,
             breaks = breaks_ratio,
             border = NA,
             legend.title.txt = cetteAnneeLa,
             legend.pos = "topright", 
             legend.values.rnd = -2,add=T)
  
  
  stewartPrix_df<-as.data.frame(stewartPrix_SF)
  
  stewartPrix_df$Annee <- cetteAnneeLa
  
  Montableau_maisons<-bind_rows(Montableau_maisons, stewartPrix_df)
  
  
  dev.off()
  # rm(spVente_2,stewartPrix, stewartTransac, stewartTransac_SF, stewartPrix_SF )
  
}
