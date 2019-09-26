library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(stringr)   
library(tidyverse)

########################
test<-Tableau_ACP_Final_Communes
test$DepCom<-as.integer(test$DepCom)
test<-left_join(test, Variables_Supp_ACP_Communes, by = c("DepCom", "Periode"))

test$n_operation_PTZ[is.na(test$n_operation_PTZ)] <- 0
test$Achat_ancien_PTZ[is.na(test$Achat_ancien_PTZ)] <- 0
test$Achat_neuf_PTZ[is.na(test$Achat_neuf_PTZ)] <- 0
test$Prets_Sociaux[is.na(test$Prets_Sociaux)] <- 0
test$Prets_libre[is.na(test$Prets_libre)] <- 0

test$Pourc_Operation_PTZ<- (test$n_operation_PTZ/test$Actifs_Nombre_transacs_Acquereurs)*100

#select variables
#Comme modif structurelle du credit on ne garde que les acahts sans credit
colnames(test)
test.active_pourcentage <- test[, c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,71,72,73,76,79,82,85,
                                    87:91,93:95, 98:101,104:109 )]
#ACP
str(test.active_pourcentage)
colnames(test.active_pourcentage)
#PCA
test.active_pourcentage<-test.active_pourcentage %>% drop_na()
Communes_retenues<-test.active_pourcentage%>%
  select(DepCom, Periode)%>%
  spread(key = Periode, value = Periode) %>% 
  drop_na()

Communes_retenues<-as.data.frame(Communes_retenues[,c(1)])
Communes_retenues$DepCom<-as.numeric(Communes_retenues[,c(1)])

test.active_pourcentage<-left_join(Communes_retenues, test.active_pourcentage, by = c("DepCom"))
row.names(test.active_pourcentage)<-paste0(test.active_pourcentage$DepCom, test.active_pourcentage$Periode, sep="_")

Var_active_pourcentage<-test.active_pourcentage[,c(3:47)]                                

library(missMDA)

res.pca<-PCA(Var_active_pourcentage[,c(1:45)], quanti.sup = c(13,14, 28:ncol(Var_active_pourcentage)),graph = TRUE,ncp = 5,scale.unit = TRUE)

Inertie_Axes_ACP_communes<-fviz_eig(res.pca, addlabels = TRUE, ylim = c(0,30))
ggsave("Inertie_Axes_ACP_communes.pdf",plot= Inertie_Axes_ACP_communes, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)

dimdesc(res.pca, axes=c(1:5))
library(factoextra)
setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
Contrib_AXE12<-fviz_pca_var(res.pca, axes = c(1,2),col.var = "contrib",
             alpha.var = "contrib",col.quanti.sup = "red",
             repel = TRUE, select.var = list(contrib = 12))
ggsave("Contrib_AXE12.pdf",plot= Contrib_AXE12, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)

Cos_AXE12<-fviz_pca_var(res.pca, axes = c(1,2),col.var = "cos2",
             alpha.var = "cos2",col.quanti.sup = "red",
             repel = TRUE, select.var = list(cos2 =0.2))
ggsave("Cos_AXE12.pdf",plot= Cos_AXE12, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)

Contrib_AXE34<-fviz_pca_var(res.pca, axes = c(3,4),col.var = "contrib", alpha.var = "contrib",
             repel = TRUE, select.var = list(contrib = 8))
ggsave("Contrib_AXE34.pdf",plot= Contrib_AXE34, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
Cos_AXE34<-fviz_pca_var(res.pca, axes = c(3,4),col.var = "cos2",
                         alpha.var = "cos2",col.quanti.sup = "red",
                         repel = TRUE, select.var = list(cos2 =0.2))
ggsave("Cos_AXE34.pdf",plot= Cos_AXE34, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)


Contrib_AXE15<-fviz_pca_var(res.pca, axes = c(1,5),col.var = "contrib", 
                            alpha.var = "contrib",col.quanti.sup = "red",
             repel = TRUE, select.var = list(contrib = 8))

ggsave("Contrib_AXE15.pdf",plot= Contrib_AXE15, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)
Cos_AXE15<-fviz_pca_var(res.pca, axes = c(1,5),col.var = "cos2",
                       alpha.var = "cos2",col.quanti.sup = "red",
                       repel = TRUE, select.var = list(cos2 =0.2))
ggsave("Cos_AXE15.pdf",plot= Cos_AXE15, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)


description_variables <- dimdesc(res.pca, axes=c(1:5))
library("tibble", lib.loc="/opt/R/3.5.1/lib64/R/library")
description_variables_formattes <- lapply(description_variables, FUN = function(x){rownames_to_column(as.data.frame(x$quanti))})
Coord_Axes_ACP_Communes <- bind_rows(description_variables_formattes, .id = "Axe")
library("gridExtra", lib.loc="/opt/R/3.5.1/lib64/R/library")
setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
pdf("Coord_Axes_ACP_Communes.pdf",Coord_Axes_ACP_Communes, height=70, width=15)
grid.table(Coord_Axes_ACP_Communes, rows=NULL)
dev.off()

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

# Contributions des variables à PC5
Contrib_Axe5_bar<-fviz_contrib(res.pca, choice = "var", axes = 5, top = 10,xtickslab.rt = 90)
ggsave("Contrib_Axe5_bar.pdf",plot= Contrib_Axe5_bar, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)

#############
res.acp.df<-as.data.frame(res.pca$ind$coord)
cah <- HCPC(res.acp.df, graph=TRUE, nb.clust = 8)
plot(cah, choice="tree", angle=60)


clust.hcpc <- as.numeric(cah$data.clust$clust)
clust.cutree <- dendextend:::cutree(cah$call$t$tree, k=8, order_clusters_as_data = FALSE)
idx <- order(as.numeric(names(clust.cutree)))
clust.cutree <- clust.cutree[idx]
( tbl <- table(clust.hcpc, clust.cutree) )
( lbls <- apply(tbl,2,which.max) )
library(ggplot2)
library("ggdendro")
library(dendextend)
# setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
# pdf("Tree_Cah_ACP_Communes.pdf",Tree_Cah_ACP_Communes, height=180, width=380)
cah$call$t$tree %>% 
  color_branches(k=8, groupLabels =lbls) %>% 
  set("labels_cex", .5) %>% 
  plot(horiz=T) 
# dev.off()

######################

Cluster_CAh_df<-as.data.frame(cah$data.clust$clust)
Cluster_CAh_df$ID_CAH<-as.numeric(row.names(Cluster_CAh_df))
test.active_pourcentage<-as.data.frame(test.active_pourcentage)

SpUnitt_with_cluster<-cbind(test.active_pourcentage,Cluster_CAh_df)

####################################################
# Visualisation classes CAH
CAH_for_visu<-as.data.frame(cah$data.clust)

Cluster_Dimension_ACP_communes<-CAH_for_visu %>%
  group_by(clust) %>%
  summarise_all(funs(mean),na.rm=T) %>%
  gather(key = "Dimension", value = "Data", Dim.1:Dim.5) %>%
  ggplot() +
  geom_bar(aes(Dimension, Data, fill = clust), stat = "identity") +
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~clust) +
  coord_flip()
ggsave("Cluster_Dimension_ACP_communes.pdf",plot= Cluster_Dimension_ACP_communes, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)


# scale_this <- function(x) as.vector(scale(x))
Variables_supp_acp_communes<-as.data.frame(scale(SpUnitt_with_cluster[,c(3:47)]))
Variables_supp_acp_communes<-cbind(Variables_supp_acp_communes,SpUnitt_with_cluster[,c(48,49)])
Variables_supp_acp_communes_plot<-Variables_supp_acp_communes %>%
  group_by(`cah$data.clust$clust`) %>%
  # summarise_all(funs(mean),na.rm=T) %>%
  summarise_at(.vars = vars(Actifs_rapport_Acquereurs:Pourc_Operation_PTZ), .funs = funs(mean(.)))%>%
  gather(key = "Dimension", value = "Data", 29:46) %>%
  ggplot() +
  geom_bar(aes(Dimension, Data, fill = `cah$data.clust$clust`), stat = "identity") +
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~`cah$data.clust$clust`) +
  coord_flip()
ggsave("Variables_supp_acp_communes_plot.pdf",plot= Variables_supp_acp_communes_plot, device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)


library(SpatialPosition)
library(cartography)
library(RColorBrewer)

cols <- brewer.pal(12,"Set3")

opacity <- 95 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- paste0(cols, opacity)
CommunesInsee$DepCom<-as.integer(CommunesInsee$DepCom)
Test_map<- left_join(CommunesInsee, SpUnitt_with_cluster%>%
                       filter(Periode == "Periode_96_2003"), by = "DepCom")

dev.off()
setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
 # Periode_96_2003# Periode_04_2007
# osmTiles <- getTiles(x = CommunesInsee, type = "stamenbw",  crop = FALSE)

pdf("Map_CAH_Communes_Periode1.pdf",width = 10, height = 5)
tilesLayer(osmTiles)
layoutLayer()
typoLayer(x=Test_map,
          var = "cah$data.clust$clust",
          col= cols, ##palettes de couleurs predefinies## 
          border = "black",
          legend.values.order = c("1",
                                  "2", 
                                  "3",
                                  "4",
                                  "5",
                                  "6",
                                  "7",
                                  "8"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          lwd = 0.2,
          colNA="grey",add=TRUE)
plot(lim_IDF.st$geometry,add=T)

dev.off()

Test_map<- left_join(CommunesInsee, SpUnitt_with_cluster%>%
                       filter(Periode == "Periode_04_2007"), by = "DepCom")

dev.off()
setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
# Periode_96_2003# Periode_04_2007
# osmTiles <- getTiles(x = CommunesInsee, type = "stamenbw",  crop = FALSE)

pdf("Map_CAH_Communes_Periode2.pdf",width = 10, height = 5)
tilesLayer(osmTiles)
layoutLayer()
typoLayer(x=Test_map,
          var = "cah$data.clust$clust",
          col= cols, ##palettes de couleurs predefinies## 
          border = "black",
          legend.values.order = c("1",
                                  "2", 
                                  "3",
                                  "4",
                                  "5",
                                  "6",
                                  "7",
                                  "8"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          lwd = 0.2,
          colNA="grey",add=TRUE)
plot(lim_IDF.st$geometry,add=T)

dev.off()

Test_map<- left_join(CommunesInsee, SpUnitt_with_cluster%>%
                       filter(Periode == "Periode_08_2012"), by = "DepCom")

dev.off()
setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
# Periode_96_2003# Periode_04_2007
# osmTiles <- getTiles(x = CommunesInsee, type = "stamenbw",  crop = FALSE)

pdf("Map_CAH_Communes_Periode3.pdf",width = 10, height = 5)
tilesLayer(osmTiles)
layoutLayer()
typoLayer(x=Test_map,
          var = "cah$data.clust$clust",
          col= cols, ##palettes de couleurs predefinies## 
          border = "black",
          legend.values.order = c("1",
                                  "2", 
                                  "3",
                                  "4",
                                  "5",
                                  "6",
                                  "7",
                                  "8"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          lwd = 0.2,
          colNA="grey",add=TRUE)
plot(lim_IDF.st$geometry,add=T)

dev.off()
# detach("package:plyr", unload=TRUE) 
library(alluvial)
d_alluvial <- SpUnitt_with_cluster %>%
  select(DepCom,Periode,`cah$data.clust$clust`) %>%
  spread(Periode,`cah$data.clust$clust`) %>%
  group_by(Periode_96_2003,Periode_04_2007,Periode_08_2012) %>%
  dplyr::summarise(count=n())%>%
  ungroup()

pdf("alluvial_plot_cah_communes.pdf",width = 10, height = 5)
alluvial_plot_cah_communes<-alluvial(select(d_alluvial,-count),freq=d_alluvial$count, hide=d_alluvial$count <5, col = 
          ifelse(d_alluvial$Periode_96_2003==1,"orange",
       ifelse(d_alluvial$Periode_96_2003==2,"yellow", 
       ifelse(d_alluvial$Periode_96_2003==3,"red",
      ifelse(d_alluvial$Periode_96_2003==4,"brown",
        ifelse(d_alluvial$Periode_96_2003==5,"green",
    ifelse(d_alluvial$Periode_96_2003==6,"purple",
    ifelse(d_alluvial$Periode_96_2003==7,"pink",
      ifelse(d_alluvial$Periode_96_2003==8,"blue",NA)))))))))

dev.off()

description_variables_cluster<-SpUnitt_with_cluster %>%
  group_by(`cah$data.clust$clust`)%>%
  summarise_all(funs(mean), na.rm=T)%>%
  select(-Periode, -DepCom)
colnames(description_variables_cluster)
description_variables_cluster_plot<-description_variables_cluster%>%
  gather(key = "Variables", value = "Data", Actifs_rapport_Acquereurs:Provenance_etrangere_rapport_origine_acquereur) %>%
  ggplot() +
  geom_bar(aes(`cah$data.clust$clust`, Data), stat = "identity") +
  ggtitle("Profil des 11 clusters des régimes de marchés sur les moyennes des variables en entrée de l'ACP") +
  labs(subtitle = "") +
  xlab("Clusters") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~Variables, scales = "free") +
  coord_flip()


setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
write.csv2(x=SpUnitt_with_cluster,file = "SpUnitt_Communes_with_cluster.csv", row.names=FALSE, fileEncoding = "UTF-8")
