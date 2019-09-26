 data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","X.x","Y.x")]


cols=c("#006837","#1a9850","#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61","#f46d43","#d73027","#a50026")
opacity <- 80 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)
col_mask<-"#878787"
opacity_mask <-60
col_mask <- paste0(col_mask, opacity_mask)

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

## essai repr?sentation

osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)



# carreauInsee1000_jointure<-st_join(carreauInsee1000, spVente, join = st_contains, left=F)
# 
# carreauInsee1000_jointure<-carreauInsee1000_jointure%>%
#   group_by(id_c1000)%>%
#   summarise(Nombre_transacs=length(ID))%>%
#   filter(Nombre_transacs>=1)


carreaux_resumes_spdf <- as(carreauInsee1000_jointure, "Spatial")
proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")

Mask.spdf <- as(Mask.st, "Spatial")
proj4string(Mask.spdf) <-CRS("+init=epsg:2154")
Mask.spdf<- spTransform(Mask.spdf, "+init=epsg:2154")

stewartPrix <- mcStewart(knownpts = carreaux_resumes_spdf, 
                         unknownpts = carreaux_resumes_spdf,
                         varname = "Nombre_transacs", 
                         typefct = "exponential", span = 3000, beta = 2,
                         mask = Mask.spdf, longlat = FALSE)

stewartTransac_SF <- st_as_sf(stewartPrix) %>%
  rename(NbTransac = OUTPUT)



valBreaks <- getBreaks(stewartTransac_SF$NbTransac,nclass = 10, method =
                         "quantile", k = NULL, middle = FALSE)

carreauInsee1000_jointure2<-carreauInsee1000_jointure

setwd("~/Sauvegarde_figures/Densite")
pdf(file=paste0("Densite_1000.pdf"),width = 11.19, height = 7.66)
tilesLayer(osmTiles)
plot(Mask.st, col= col_mask, border=NA,add=T, main=NULL)
smoothLayer(x = carreauInsee1000_jointure2,
            var ="Nombre_transacs", 
            span = 3000, beta = 2,
            col = cols,
            mask = Mask.st,
            breaks = valBreaks,
            lwd=0.1,
            legend.title.txt = "Potentiels de vente dans un voisinage de 3km",
            legend.pos = "topright", legend.values.rnd = 0, add=T)

layoutLayer(title = "",
            sources = "Limites Insee, fonds de carte OSM", 
            author = "UMR Geographie-cites ®T.LE CORRE, 2017",
            north=T,
            col = "black",
            coltitle = "white")

dev.off()



carreauInsee200_jointure<-st_join(carreauInsee200, spVente, join = st_contains, left=F)

carreauInsee200_jointure<-carreauInsee200_jointure%>%
  group_by(TARGET_FID)%>%
  summarise(Nombre_transacs=length(ID))%>%
  filter(Nombre_transacs>=1)


carreaux_resumes_spdf <- as(carreauInsee200_jointure, "Spatial")
proj4string(carreaux_resumes_spdf) <-CRS("+init=epsg:2154")
carreaux_resumes_spdf<- spTransform(carreaux_resumes_spdf, "+init=epsg:2154")

Mask.spdf <- as(Mask_st_200, "Spatial")
proj4string(Mask.spdf) <-CRS("+init=epsg:2154")
Mask.spdf<- spTransform(Mask.spdf, "+init=epsg:2154")

stewartPrix <- mcStewart(knownpts = carreaux_resumes_spdf, 
                         unknownpts = carreaux_resumes_spdf,
                         varname = "Nombre_transacs", 
                         typefct = "exponential", span = 500, beta = 2,
                         mask = Mask.spdf, longlat = FALSE)

stewartTransac_SF <- st_as_sf(stewartPrix) %>%
  rename(NbTransac = OUTPUT)



valBreaks <- getBreaks(stewartTransac_SF$NbTransac,nclass = 10, method =
                         "quantile", k = NULL, middle = FALSE)

carreauInsee200_jointure2<-carreauInsee200_jointure

setwd("~/Sauvegarde_figures/Densite")
pdf(file=paste0("Densite_200.pdf"),width = 11.19, height = 7.66)
tilesLayer(osmTiles)
plot(Mask_st_200, col= col_mask, border=NA,add=T, main=NULL)
smoothLayer(x = carreauInsee200_jointure2,
            var ="Nombre_transacs", 
            span = 500, beta = 2,
            col = cols,
            mask = Mask_st_200,
            breaks = valBreaks,
            lwd=0.1,
            legend.title.txt = "Potentiels de vente dans un voisinage de 500m",
            legend.pos = "topright", legend.values.rnd = 0, add=T)

layoutLayer(title = "",
            sources = "Limites Insee, fonds de carte OSM", 
            author = "UMR Geographie-cites ®T.LE CORRE, 2017",
            north=T,
            col = "black",
            coltitle = "white")

dev.off()



# CommunesInsee_jointure<-st_join(CommunesInsee, spVente, join = st_contains, left=F)
# 
# CommunesInsee_jointure<-CommunesInsee_jointure%>%
#   group_by(DepCom)%>%
#   summarise(Nombre_transacs=length(ID))%>%
#   filter(Nombre_transacs>=1)


valBreaks <- getBreaks(CommunesInsee_jointure$Nombre_transacs,nclass = 10, method =
                         "quantile", k = NULL, middle = FALSE)


CommunesInsee_jointure2<-CommunesInsee_jointure

setwd("~/Sauvegarde_figures/Densite")
pdf(file=paste0("Densite_Communes.pdf"),width = 11.19, height = 7.66)
tilesLayer(osmTiles)
plot(CommunesInsee, col= col_mask, border=NA,add=T, main=NULL)
choroLayer(x = CommunesInsee_jointure2,
           var ="Nombre_transacs", 
           col = cols,
           colNA = col_mask,
           lwd = 0.1,
           breaks = valBreaks,add=T)
plot(DepInsee, col=NA, border="black",add=T, main=NULL)
dev.off()


Pop<- recensement99_2012_Pop_total_communes

CommunesInsee_jointure2<- left_join(CommunesInsee_jointure2, Pop[,c("DepCom","RGP2012")])
CommunesInsee_jointure2$Rap_Pop_Transacs<- (CommunesInsee_jointure2$Nombre_transacs / CommunesInsee_jointure2$RGP2012)*100


valBreaks <- getBreaks(CommunesInsee_jointure2$Rap_Pop_Transacs,nclass = 10, method =
                         "quantile", k = NULL, middle = FALSE)

setwd("~/Sauvegarde_figures/Densite")
pdf(file=paste0("Densite_Communes_POP.pdf"),width = 11.19, height = 7.66)
tilesLayer(osmTiles)
plot(CommunesInsee, col= col_mask, border=NA,add=T, main=NULL)
choroLayer(x = CommunesInsee_jointure2,
           var ="Rap_Pop_Transacs", 
           col = cols,
           colNA = col_mask,
           lwd = 0.1,
           breaks = valBreaks,add=T)
plot(DepInsee, col=NA, border="black",add=T, main=NULL)
dev.off()





























CommunesInsee_jointure2$DepCom<- ifelse(CommunesInsee_jointure2$DepCom == "75101" , "75001",
                                   ifelse(CommunesInsee_jointure2$DepCom == "75102" , "75002",
                                          ifelse(CommunesInsee_jointure2$DepCom == "75103" , "75003",
                                                 ifelse(CommunesInsee_jointure2$DepCom == "75104" , "75004",
                                                        ifelse(CommunesInsee_jointure2$DepCom == "75105" , "75005",
                                                               ifelse(CommunesInsee_jointure2$DepCom == "75106" , "75006",
                                                                      ifelse(CommunesInsee_jointure2$DepCom == "75107" , "75007",
                                                                             ifelse(CommunesInsee_jointure2$DepCom == "75108" , "75008",
                                                                                    ifelse(CommunesInsee_jointure2$DepCom == "75109" , "75009",
                                                                                           ifelse(CommunesInsee_jointure2$DepCom == "75110" , "75010",
                                                                                                  ifelse(CommunesInsee_jointure2$DepCom == "75111" , "75011",
                                                                                                         ifelse(CommunesInsee_jointure2$DepCom == "75112" , "75012",
                                                                                                                ifelse(CommunesInsee_jointure2$DepCom == "75113", "75013",
                                                                                                                       ifelse(CommunesInsee_jointure2$DepCom == "75114" , "75014",
                                                                                                                              ifelse(CommunesInsee_jointure2$DepCom == "75115" , "75015",
                                                                                                                                     ifelse(CommunesInsee_jointure2$DepCom == "75116" , "75016",
                                                                                                                                            ifelse(CommunesInsee_jointure2$DepCom == "75117" , "75017",
                                                                                                                                                   ifelse(CommunesInsee_jointure2$DepCom == "75118" , "75018",
                                                                                                                                                          ifelse(CommunesInsee_jointure2$DepCom == "75119" , "75019",
                                                                                                                                                                 ifelse(CommunesInsee_jointure2$DepCom == "75120" , "75020",CommunesInsee_jointure2$DepCom))))))))))))))))))))

