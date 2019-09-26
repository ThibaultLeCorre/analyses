# Indivius moyens : les espaces le splus proche sdu centre de gravité des classes
library(cartography)
library(SpatialPosition)
cah$desc.ind
cah$desc.ind$para
SpUnitt_with_cluster%>% filter(`res.kmeans$cluster`==   7518)
#regarder ID_saptial_unit et filtrer

SpUnitt_with_cluster%>%filter(ID_spatial_unit==   32739  )

#regarder Carreau_ID sur Cadrillage 
Unit_moyenne_Class<- Cadrillage_200%>% filter( Carreau_ID==25327      )
osm_class<- getTiles(x = Unit_moyenne_Class, type = "osm",  crop = FALSE)
tilesLayer(osm_class)
?getTiles




#Cluster 1 : 

description_variables_cluster<-SpUnitt_with_cluster %>%
  group_by(`cah$data.clust$clust`)%>%
  summarise_all(funs(mean), na.rm=T)%>%
select(-cettePeriodeLa, -Carreau_ID)
  
description_variables_cluster_plot<-description_variables_cluster%>%
      gather(key = "Variables", value = "Data", Actifs_Pourcentage_Profil_acquereur:Provenance_etrangere_Pourcentage_Origine_acquereur) %>%
  ggplot() +
  geom_bar(aes(`cah$data.clust$clust`, Data), stat = "identity") +
      theme_tmd()+
  ggtitle("Profil des 11 clusters des régimes de marchés sur les moyennes des variables en entrée de l'ACP") +
  labs(subtitle = "") +
  xlab("Clusters") +
  ylab("") +
  theme(legend.position='none') +
      labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
      facet_wrap(~Variables, scales = "free") +
  coord_flip()

library(extrafont)
loadfonts()
fonts() 
ggsave("description_variables_cluster_plot.pdf",plot= description_variables_cluster_plot,setwd("~/Projets/Realisation_ACP/Realisation_ACP"), device = "pdf", width = 520, height = 240, units = "mm", dpi = 330)

test.active_effectifs <- test[,c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,78,79,83,86,89,92,95,98)]
                              

library(alluvial)


d_alluvial <- SpUnitt_with_cluster %>%
  select(Carreau_ID,cettePeriodeLa,`cah$data.clust$clust`) %>%
  spread(cettePeriodeLa,`cah$data.clust$clust`) %>%
  group_by(Periode_96_2003,Periode_04_2007,Periode_08_2012) %>%
  summarize(count=n()) %>%
  ungroup()
alluvial(select(d_alluvial,-count),freq=d_alluvial$count, hide=d_alluvial$count < 300)


############Changement classe 4 vers 56 et 2
SpUnit_periode_unique<-SpUnitt_with_cluster%>% 
  select(Carreau_ID,cettePeriodeLa,`cah$data.clust$clust`) %>%
  spread(cettePeriodeLa,`cah$data.clust$clust`)%>%
  filter(Periode_96_2003=="4",Periode_04_2007=="2" | Periode_04_2007=="6" | Periode_04_2007=="4"  )

SpUnit_periode_unique$Changements<- ifelse (SpUnit_periode_unique$Periode_04_2007=="4","Meme_Cluster4",
                                     ifelse ( SpUnit_periode_unique$Periode_04_2007=="2", "Passage_Cluster2",
                                     ifelse ( SpUnit_periode_unique$Periode_04_2007=="6", "Passage_Cluster6", "Autres")))

table(SpUnit_periode_unique$Changements)
#Clsuter 4 vers 2 = 3501 ; 3691 vers 6, 3631 stay clus4

$PassageClus4_vers5_et_2<- 
  ifelse(SpUnitt_with_cluster$cettePeriodeLa=="Periode_96_2003" & SpUnitt_with_cluster$`cah$data.clust$clust`==4 &  
           SpUnitt_with_cluster$cettePeriodeLa=="Periode_04_2007" & SpUnitt_with_cluster$`cah$data.clust$clust`==2, "Chan"  )
SpUnit_periode_unique<-left_join(SpUnit_periode_unique,SpUnitt_with_cluster )
  

description_Passage_clust2_vers6et2<-SpUnit_periode_unique %>%
  group_by(Changements, cettePeriodeLa)%>%
  summarise_all(funs(mean), na.rm=T)%>%
  select( -Carreau_ID, -Periode_04_2007,-Periode_08_2012,-Periode_96_2003)

description_Passage_clust2_vers6et2_08_2012_plot<-description_Passage_clust2_vers6et2%>%
   filter(cettePeriodeLa=="Periode_08_2012")%>%
  gather(key = "Variables", value = "Data", Actifs_Pourcentage_Profil_acquereur:Provenance_etrangere_Pourcentage_Origine_acquereur) %>%
  ggplot() +
  geom_bar(aes(Changements, Data, fill= Changements), stat = "identity") +
  theme_tmd()+
  ggtitle("") +
  labs(subtitle = "") +
  xlab("Clusters") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(cettePeriodeLa~Variables, scales = "free") +
  coord_flip()
library(extrafont)
loadfonts()
fonts() 
ggsave("description_Passage_clust2_vers6et2_08_2012_plot.pdf",plot= description_Passage_clust2_vers6et2_08_2012_plot,setwd("~/Projets/Realisation_ACP/Realisation_ACP"), device = "pdf", width = 520, height = 240, units = "mm", dpi = 330)




#############Plot carte 

# Cluster 1 : ID 20737, 9981, 12461
# Cluster 3 : ID 29956 ; 27520
# Cluster 8 : ID 25469 ; 34183 ; 32123
# Cluster 7 : ID 48820 ; 5813; 24370

Cadrillage_200_point <- st_centroid(Cadrillage_200, "POINT")

Clust1387_sf<- Cadrillage_200_point %>% filter( Carreau_ID==20737|Carreau_ID== 9981|Carreau_ID== 12461|Carreau_ID==29956|
                                                Carreau_ID==27520|Carreau_ID==25469|Carreau_ID==34183|Carreau_ID==32123| Carreau_ID==48820|Carreau_ID==5813|Carreau_ID==24370)

osm_class<- getTiles(x = Cadrillage_200_point, type = "stamenbw",  crop = FALSE)

tilesLayer(osm_class)

Clust1387_sf$Cluster<- ifelse (Clust1387_sf$Carreau_ID==20737|Clust1387_sf$Carreau_ID== 9981|Clust1387_sf$Carreau_ID== 12461, "Cluster 1",
                               ifelse (Clust1387_sf$Carreau_ID==29956|Clust1387_sf$Carreau_ID==27520, "Cluster 3", 
                                       ifelse (Clust1387_sf$Carreau_ID==25469|Clust1387_sf$Carreau_ID==34183|Clust1387_sf$Carreau_ID==32123, "Cluster 8", 
                                               ifelse ( Clust1387_sf$Carreau_ID==48820|Clust1387_sf$Carreau_ID==5813|Clust1387_sf$Carreau_ID==24370, "Cluster 7" ,NA))))

typoLayer(Clust1387_sf,
          var ="Cluster", add = T, lwd = 3)


# Cluster 1 : ID 45970, 58865,37764
# Cluster 3 : ID 57931 ; 45880 ; 49340
# Cluster 6 : ID 25936 
# cluster7 : ID 35320 ; 22409 ; 32896

Cadrillage_200_point <- st_centroid(Cadrillage_200, "POINT")

Clust1387_sf<- Cadrillage_200_point %>% filter( Carreau_ID==45970|Carreau_ID== 58865|Carreau_ID== 37764|Carreau_ID==57931|Carreau_ID==25327|
                                                  Carreau_ID==45880|Carreau_ID==49340|Carreau_ID==25936|Carreau_ID==35320|Carreau_ID==22409|Carreau_ID==32896)

osm_class<- getTiles(x = Cadrillage_200_point, type = "stamenbw",  crop = FALSE)

tilesLayer(osm_class)

Clust1387_sf$Cluster<- ifelse (Clust1387_sf$Carreau_ID==45970|Clust1387_sf$Carreau_ID== 58865|Clust1387_sf$Carreau_ID== 37764, "Cluster 1",
                               ifelse (Clust1387_sf$Carreau_ID==57931|Clust1387_sf$Carreau_ID==45880 |Clust1387_sf$Carreau_ID==49340, "Cluster 3", 
                                       ifelse (Clust1387_sf$Carreau_ID==25936|Clust1387_sf$Carreau_ID==25327, "Cluster 6", 
                                               ifelse ( Clust1387_sf$Carreau_ID==35320|Clust1387_sf$Carreau_ID==22409|Clust1387_sf$Carreau_ID==32896, "Cluster 7" ,NA))))

typoLayer(Clust1387_sf,
          var ="Cluster", add = T, lwd = 3)


