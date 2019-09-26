#Packages
library (sf)
library(tidyr)
library (dplyr)
library (usdm)
library(SpatialPosition)
library(cartography)
library(fasterize)
library(spdep)
library(ggplot2)
library(sparcl)


#Import tableaux Prix
data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
Revenus_Dispo_FullYears_IDF <- read.csv("~/tables/Revenus_Dispo_FullYears_IDF.csv", sep=";", stringsAsFactors=FALSE)
#Ajoute RDP 1996
Revenus_Dispo_FullYears_IDF[nrow(Revenus_Dispo_FullYears_IDF) + 1,] = list(1996,16189)
str(Revenus_Dispo_FullYears_IDF)
# Revenus_Dispo_FullYears_IDF$Annee<-as.integer(Revenus_Dispo_FullYears_IDF$Annee)
# Revenus_Dispo_FullYears_IDF$Annee<-as.integer(Revenus_Dispo_FullYears_IDF$Annee)
##
Deciles_Years_Prix<- data_redresse1570533transacs%>% 
  filter(`X.x`!= 0 & `Y.x`!=0, REQ_PRIX>0)%>%
  select(REQ_PRIX,annee.x)%>%
  group_by(annee.x)%>%
  summarise(`1.Premier_decile` = quantile(REQ_PRIX, probs=0.10),
            `2.Second_decile` = quantile(REQ_PRIX, probs=0.20),
            `3.Troisieme_decile` = quantile(REQ_PRIX, probs=0.30),
            `4.Quatrieme_decile` = quantile(REQ_PRIX, probs=0.40),
            `5.Cinquieme_decile` = quantile(REQ_PRIX, probs=0.50),
            `6.Sixieme_decile` = quantile(REQ_PRIX, probs=0.60),
             `7.Septieme_decile`= quantile(REQ_PRIX, probs=0.70),
            `8.Huitieme_decile` = quantile(REQ_PRIX, probs=0.80),
            `9.Neuvieme_decile` = quantile(REQ_PRIX, probs=0.90))%>%
  gather("Deciles", "Prix", 2:10 )

cols=c("#006837","#1a9850","#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61","#f46d43","#d73027","#a50026")


specificCol <- c(`1.Premier_decile`="#006837",
                 `2.Second_decile`= "#1a9850",
                 `3.Troisieme_decile`= "#66bd63" ,
                 `4.Quatrieme_decile`= "#a6d96a",
                `5.Cinquieme_decile`="#fee08b",
                 `6.Sixieme_decile`= "#fdae61",
                 `7.Septieme_decile`= "#f46d43",
                 `8.Huitieme_decile`= "#d73027",
                 `9.Neuvieme_decile`= "#a50026")


#
Prix_Revenus<-left_join(Deciles_Years_Prix, Revenus_Dispo_FullYears_IDF, by = c("annee.x"="Annee"))
#
Prix_Revenus$Revenu_Annee_decile <- Prix_Revenus$Prix / Prix_Revenus$Revenu_Brut_Dispo



Prix_Revenus_plot<- 
  Prix_Revenus%>%
  ggplot(., aes(annee.x , Revenu_Annee_decile, group= Deciles)) +
  geom_line()+
    geom_path(aes(color = Deciles))+
   scale_fill_manual(values= specificCol)+
  scale_color_manual(values= specificCol)+
  # scale_fill_manual(values= c("#006837","#1a9850","#66bd63","#a6d96a","#d9ef8b","#fee08b","#fdae61","#f46d43","#d73027","#a50026"))+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  scale_y_continuous(breaks = c(2.5,5,7.5,10,12.5,15,17.5,20,22.5,25)) +
  theme_tmd() +
  labs(title = "Nombre d'années de revenus pour l'acquisition d'un bien immobilier en IDF", x= "Année" , y= "Nombre d'années de revenus pour l'acquisition d'un bien")+ 
  labs(subtitle = "Les revenus correspondent aux revenus disponibles annuels moyens des ménages franciliens pour chaque année\nLes déciles correspondent aux prix des maisons et appartements pour l'ensemble de la région")+
  labs(caption = "Sources : Echantillon BD BIEN, Insee - Comptes économiques régionaux des ménages\n Réalisation : Thibault Le Corre, UMR Géographie-cités, 2017")

setwd("~/Projets/Prix_com_LTI")
ggsave("Prix_Revenus_plot.pdf",plot= Prix_Revenus_plot,  device = "pdf", width = 310, height = 200, units = "mm", dpi = 330)
ggsave("Prix_Revenus_plot.png",plot= Prix_Revenus_plot,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)


#####


mytest<- data_redresse1570533transacs%>% 
  filter(`X.x`!= 0 & `Y.x`!=0, REQ_PRIX>0)%>%
  select(ID,`X.x`,`Y.x`,REQ_PRIX,annee.x)

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

osmTiles <- getTiles(x = spVente, type = "stamenbw",  crop = FALSE)


setwd("~/Shapes/shpIDF_dep_lamb93")
list.files()
DepInsee<- st_read("ile-de-france.shp",
                   stringsAsFactors = FALSE) %>%
  st_cast("POLYGON") %>%
  st_sf(stringsAsFactors = FALSE, crs = 2154, sf_column_name = "geometry")
DepInsee <-  st_transform(DepInsee, crs = 2154)

#####


Typo_1999_2012_533communes_Maisons <- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_533communes_Maisons", stringsAsFactors=FALSE)
Typo_1999_2012_533communes_Maisons$blurp<-"coincoin"
Typo_1999_2012_communes_appartements <- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_communes_appartements", stringsAsFactors=FALSE)
Typo_1999_2012_communes_appartements$blurp<-"coincoin"
Typo1<-Typo_1999_2012_533communes_Maisons[,c("DepCom", "blurp")]
Typo2<-Typo_1999_2012_communes_appartements[,c("DepCom", "blurp")]
Type<-bind_rows(Typo1,Typo2)
Type[!duplicated(Type$DepCom), ]
# blurp <- Type$DepCom[!duplicated(Type$DepCom)]
# blurp <-as.data.frame(blurp)
# Type<-left_join(blurp,Type,by = c("blurp"="DepCom"))


Type$DepCom<-as.numeric(Type$DepCom)


Comjointure<-st_join(CommunesInsee, spVente, join = st_contains, left=T)

Comjointure_PRIX<-left_join(Type,Comjointure, by = "DepCom")

Comjointure_PRIX<- Comjointure_PRIX%>%
  group_by(DepCom, annee.x)%>%
  summarise(Prix_median = median(REQ_PRIX, na.rm = T))



Comjointure_PRIX<-left_join(Comjointure_PRIX, Revenus_Dispo_FullYears_IDF, by = c("annee.x"="Annee"))
Comjointure_PRIX$Revenu_Annee_decile <- Comjointure_PRIX$Prix_median / Comjointure_PRIX$Revenu_Brut_Dispo


valBreaks <- getBreaks(Comjointure_PRIX$Revenu_Annee_decile,nclass = 10, method =
                         "quantile", k = NULL, middle = FALSE)

annees <- unique(mytest$annee.x)
annees <- sort(annees[!is.na(annees)])

dev.off()

for (cetteAnneeLa in annees){
  
  test<- Comjointure_PRIX%>% filter(annee.x==cetteAnneeLa)

  
  
  Test_map<- left_join(CommunesInsee, test, by = "DepCom")
  
    Test_map<- Test_map%>%
      st_sf(geometry = .)

    
    pdf(file=paste0("~/Projets/Prix_com_LTI/Cartes_Prix_Incomeregion/",cetteAnneeLa,".pdf"),width = 10, height = 5)
    
    tilesLayer(osmTiles)
    # plot(CommunesInsee, col= col_mask, border=NA,main=NULL)
    
    choroLayer(x=Test_map,
               var = "Revenu_Annee_decile",
               breaks = valBreaks,
               legend.pos = "bottomleft",
               legend.values.cex= 0.5,
               border = NA,
               legend.title.txt = sprintf("Nombre d'années de revenus"),
               col = cols,
               colNA=NA,add=T, legend.values.rnd = 2)
    plot(DepInsee$geometry,lwd = 0.5, add=T)
  
 
    layoutLayer()
    
    dev.off()
  }










Type$DepCom<- ifelse(Type$DepCom == "75101" , "75001",
                                        ifelse(Type$DepCom == "75102" , "75002",
                                               ifelse(Type$DepCom == "75103" , "75003",
                                                      ifelse(Type$DepCom == "75104" , "75004",
                                                             ifelse(Type$DepCom == "75105" , "75005",
                                                                    ifelse(Type$DepCom == "75106" , "75006",
                                                                           ifelse(Type$DepCom == "75107" , "75007",
                                                                                  ifelse(Type$DepCom == "75108" , "75008",
                                                                                         ifelse(Type$DepCom == "75109" , "75009",
                                                                                                ifelse(Type$DepCom == "75110" , "75010",
                                                                                                       ifelse(Type$DepCom == "75111" , "75011",
                                                                                                              ifelse(Type$DepCom == "75112" , "75012",
                                                                                                                     ifelse(Type$DepCom == "75113", "75013",
                                                                                                                            ifelse(Type$DepCom == "75114" , "75014",
                                                                                                                                   ifelse(Type$DepCom == "75115" , "75015",
                                                                                                                                          ifelse(Type$DepCom == "75116" , "75016",
                                                                                                                                                 ifelse(Type$DepCom == "75117" , "75017",
                                                                                                                                                        ifelse(Type$DepCom == "75118" , "75018",
                                                                                                                                                               ifelse(Type$DepCom == "75119" , "75019",
                                                                                                                                                                      ifelse(Type$DepCom == "75120" , "75020",Type$DepCom))))))))))))))))))))


