library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(stringr)   
library(tidyverse)


#TCAM 
TCAM_prix <- Typo_1999_2012_carreaux200_appartement_37158tiles
TCAM_prix<- TCAM_prix[,c(1:13)]

CAGR <- function(df, variables){
  CAGR <- vector()
  cagr <- list()
  for(i in 2:length(variables)){
    
    # vector def
    pop1 <- df[, variables[i-1]]
    pop2 <- df[, variables[i]]
    year1 <- as.integer(str_replace_all(variables[i-1], "[A-Z]", replacement = ""))
    year2 <- as.integer(str_replace_all(variables[i], "[A-Z]", replacement = ""))
    # compute n and CAGR
    n <- year2 - year1
    result <- ((pop2/pop1)^(1/n)-1)*100
    cagr[[length(cagr) + 1]] <- result 
  }
  
  return(cagr)
} 

#discrter TCAM
TCAM_prix<-CAGR(TCAM_prix,variables = c(2,3,4,5,6,7,8,9,10,11,12,13) )
TCAM_prix <- as.data.frame(do.call(cbind, TCAM_prix))

TCAM_prix<-cbind(Typo_1999_2012_carreaux200_appartement_37158tiles[,c(1)], TCAM_prix)
tableauID<-full_join(Typo_1999_2012_carreaux200_appartement_37158tiles,Typo_1999_2012_carreaux200_Maison_36693tiles, by="Carreau_ID")
tableauID<-tableauID[,c(1,2)]


########################
test<-Tableau_ACP_Final
str(Tableau_ACP_Final)


#select variables
#Comme modif structurelle du credit on ne garde que les acahts sans credit
colnames(test)
test.active_pourcentage <- test[, c(1,2,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,70,72,73,76,79,82,85)]

# test.active_effectifs <- test[, c(1,2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,78,79,83,86,89,92,95,98)
# row.names(test.active_pourcentage)<-paste0(test.active_pourcentage$Carreau_ID,test.active_pourcentage$cettePeriodeLa)
# test.active_pourcentage<-test.active_pourcentage[,c(3:34)]
# test.active_pourcentage<-test.active_pourcentage%>%filter(!is.na(Actifs_Pourcentage_Profil_acquereur) & !is.na(Montant_moyen_credit))
summary(test.active_pourcentage)
#ACP
str(test.active_pourcentage)
colnames(test.active_pourcentage)
#PCA
Var_active_pourcentage<-test.active_pourcentage[,c(3:29)]
#Maisons et appartment en variable quanti supplemetaires
library(missMDA)
res.comp = imputePCA(Var_active_pourcentage,ncp=5)
colnames(res.comp$completeObs)
res.pca<-PCA(res.comp$completeObs[,c(1:27)], quanti.sup = c(13,14),graph = TRUE,ncp = 5,scale.unit = TRUE)
res.pca$var
#Graph et visu
summary(res.pca, ncp=7, nbelements= 100)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0,25))

res.pca$quanti.sup
Contrib_var<-res.pca$var$contrib
eig.val <- get_eigenvalue(res.pca) 
var<-get_pca_var(res.pca)
library(corrplot)
corrplot(var$cos2, is.corr=FALSE, tl.cex = 0.5)


fviz_pca_var(res.pca, axes = c(1,2),col.var = "contrib",
              alpha.var = "contrib",
             repel = TRUE, select.var = list(contrib = 8))
fviz_pca_var(res.pca, axes = c(3,4),col.var = "contrib", alpha.var = "contrib",
             repel = TRUE, select.var = list(contrib = 8))
fviz_pca_var(res.pca, axes = c(1,5),col.var = "contrib", alpha.var = "contrib",
             repel = TRUE, select.var = list(contrib = 8))
library(corrplot)
corrplot(res.pca$var$contrib, is.corr=FALSE, tl.cex = 0.5,) 
# Contributions des variables à PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10,xtickslab.rt = 45,tl.cex = 0.1, 
             sort.val ="asc", labelsize=0.1)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10,xtickslab.rt = 45,tl.cex = 0.1, 
             sort.val ="asc", labelsize=0.1)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10,xtickslab.rt = 45,tl.cex = 0.1, 
             sort.val ="asc", labelsize=0.1)
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10,xtickslab.rt = 45,tl.cex = 0.1, 
             sort.val ="asc", labelsize=0.1)
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10,xtickslab.rt = 45,tl.cex = 0.1, 
             sort.val ="asc", labelsize=0.1)
plot.PCA(res.pca, axes=c(1, 2), choix="var", habillage=13)
dimdesc(res.pca, axes=c(1,2,3,4,5))

# Mise en forme du tableau pour export Excel

description_variables <- dimdesc(res.pca, axes=c(1:5))
description_variables_formattes <- lapply(description_variables, FUN = function(x){rownames_to_column(as.data.frame(x$quanti))})
description_variables_export <- bind_rows(description_variables_formattes, .id = "Axe")

res.pca$var$contrib
description_variables_formattes <- lapply(description_variables, FUN = function(x){rownames_to_column(as.data.frame(x$quanti))})
description_variables_export <- bind_rows(description_variables_formattes, .id = "Axe")


plotellipses(res.pca)
# Contributions des variables à PC2
fviz_contrib(res.pca, choice = "var", axes = 1, top = 8)
# Contributions des variables à PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
plot.PCA(res.pca, axes=c(3, 4), choix="var", habillage=13)
dimdesc(res.pca, axes=c(3,4))
# Contributions des variables à PC4
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10,xtickslab.rt = 90)
# Contributions des variables à PC5
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10 )
# Contributions des variables à PC6
fviz_contrib(res.pca, choice = "var", axes = 6, top = 10,xtickslab.rt = 90)
# Contributions des variables à PC7
fviz_contrib(res.pca, choice = "var", axes = 7, top = 10,xtickslab.rt = 90)

fviz_pca_var(res.pca, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))



###Clustering##########################################

#Kmeans
res.acp.df<-as.data.frame(res.pca$ind$coord)
res.kmeans <-kmeans(res.pca$ind$coord[,1:5],centers = 20000, iter.max=40)
res.kmeans_df <-as.data.frame(res.kmeans$centers)
#######CAH sur Kmeans####
cah <- HCPC(res.kmeans_df, graph=TRUE, nb.clust =11)
library(ade4)
dist.dudi(res.kmeans_df)
library(fastcluster)
hclust(res.kmeans_df, method = "ward.D2")
library(ggdendro)
ggdendrogram
ggdendrogram(arbre, segments = TRUE, labels = TRUE)
par(mfrow=c(1,1))
fviz_cluster(cah,show.clust.cent = T)
cah$call$t$inert.gain
cah$call$t$nb.clust

#Plot dendogram

clust.hcpc <- as.numeric(cah$data.clust$clust)
clust.cutree <- dendextend:::cutree(cah$call$t$tree, k=11, order_clusters_as_data = FALSE)
idx <- order(as.numeric(names(clust.cutree)))
clust.cutree <- clust.cutree[idx]
( tbl <- table(clust.hcpc, clust.cutree) )
( lbls <- apply(tbl,2,which.max) )

cah$call$t$tree %>% 
  color_branches(k=11, groupLabels =lbls) %>% 
  set("labels_cex", .5) %>% 
  plot(horiz=T) 
##################################

fviz_eig(cah, addlabels = TRUE, ylim = c(0,25))

plot.HCPC (cah, choice="bar",rect = TRUE,max.plot = 20,tree.barplot = T)
(cah$call$t$intra)
cah$call$t$within
  

library("ggdendro")
library(dendextend)
dend<-cah$data.clust[,c(1:5)] %>% dist %>% 
  hclust %>% as.dendrogram %>%
  set("branches_k_color", k=11)
ggd1 <- as.ggdend(dend)
ggplot(ggd1)


plot(cah, choice = "bar")

fviz_dend(cah, cex = 0, k = 11,
          color_labels_by_k = FALSE, rect = TRUE)
# Dendrogramme
fviz_dend(cah, show_labels = TRUE)
# Individus
fviz_cluster(res.hcpc, geom = "point", main = "Factor map")

#########
Cluster_CAh_df<-as.data.frame(cah$data.clust$clust)
Cluster_CAh_df$ID_CAH<-as.numeric(row.names(Cluster_CAh_df))
df_kmeans<-as.data.frame(res.kmeans$cluster)
df_kmeans$ID_spatial_unit<-as.numeric(row.names(df_kmeans))
Result_final_CAH<- left_join(df_kmeans,Cluster_CAh_df, by= c("res.kmeans$cluster"="ID_CAH"))
# 

SpUnitt_with_cluster<-cbind(test.active_pourcentage,Result_final_CAH)

####################################################
# Visualisation classes CAH
CAH_for_visu<-as.data.frame(cah$data.clust)

CAH_for_visu %>%
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

library(SpatialPosition)
library(cartography)
library(RColorBrewer)

cols <- brewer.pal(12,"Set3")

opacity <- 95 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
cols <- p0aste0(cols, opacity)
Test_map<- left_join(Cadrillage_200, SpUnitt_with_cluster%>%
                       filter(cettePeriodeLa == "Periode_96_2003"), by = "Carreau_ID")


# Periode_96_2003# Periode_04_2007
# osmTiles <- getTiles(x = Cadrillage_200, type = "stamenbw",  crop = FALSE)
tilesLayer(osmTiles)
typoLayer(x=Test_map,
          var = "cah$data.clust$clust",
          col= cols, ##palettes de couleurs predefinies## 
          border = FALSE,
          legend.values.order = c("1",
                                  "2", 
                                  "3",
                                  "4",
                                  "5",
                                  "6",
                                  "7",
                                  "8",
                                  "9",
                                  "10",
                                  "11"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          colNA="grey",add=TRUE)
# Autre solution#
res_hcpc<-HCPC(res.pca, cluster.CA = "columns",min =5, max = 10,consol=FALSE,description = TRUE, kk = 10)
# ou
res.consol <- NULL ## bug work-around
hc2 <- HCPC(test_Clara[,3:8], kk = 100, consol = FALSE)


# ######################

# Methode CLARA
test_Clara<-cbind(test.active_pourcentage[,c(1,2)],res.pca$ind$coord[,1:6] )
row.names(test_Clara)<-paste0(test_Clara$Carreau_ID,test_Clara$cettePeriodeLa)
Res.clara<-clara(x=test_Clara[,3:8], k = 6, metric = "euclidean", stand = FALSE,
                 samples = 1000, pamLike = TRUE)

Pam_result<-pam(test_Clara[1:60000,3:8],k = 6, metric = "euclidean", do.swap = FALSE,stand = FALSE)
fviz_nbclust(test_Clara[1:60000,3:8], Pam_result, method = "silhouette")

ACP_acq<-PCA(test.active, graph = TRUE)


res_hcpc<-HCPC(test.active_pourcentage, min =5, max = 10,consol=FALSE,description = TRUE, kk = 10)

res.kmeans <- kmeans(test.active_pourcentage[,c(3:34)], 1000, iter.max=20 )


res.kmeans$centers





plot.HCPC(cah, choice="tree")

res.kmeans$centers
res.pca$ind
res_pvc <- pvclust(scale(na.omit(res.pca)), method.hclust="ward", method.dist = "euclidean", nboot=10000)

## run MCA as in ?MCA
ACP_acq <- PCA(test.active, graph = FALSE)

hc <- HCPC(ACP_acq, kk = Inf, consol = FALSE,graph.scale="inertia")
hc <- HCPC(ACP_acq, kk = Inf, consol = FALSE)
hc$desc.ind
hc$desc.var

hc$ind$
  hc$data.clust$clust
hc$call$bw.before.consol
Results_acp<-data.frame(hc$call$X)
cbind.data.frame(hc$call,tableauID[,1])



## run HCPC from 30 k means centres
res.consol <- NULL ## bug work-around
hc2 <- HCPC(ACP_acq, kk = 100, consol = FALSE, order= FALSE)


hc2$data.clust$clust
var <- get_pca_var(ACP_acq)
library(corrplot)
corrplot(var$contrib, is.corr=FALSE)  


fviz_cluster(res_hcpc,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)
