library(FactoMineR)
library(factoextra)


mytest_ODDS_ACP<- left_join(mytest_ODDS,Mytest_Poids, by=c("DepCom", "annee.x"))
mytest_ODDS_ACP<-right_join(mytest_ODDS_ACP,tableauID)

Communes_retenues<-mytest_ODDS_ACP%>%
  select(annee.x,Poids_Marche,DepCom)%>%
  group_by(DepCom) %>% 
  spread(annee.x,Poids_Marche) %>%
  drop_na(.)
mytest_ODDS_ACP<-mytest_ODDS_ACP[,-c(31)]
mytest_ODDS_ACP<- right_join(mytest_ODDS_ACP,Communes_retenues[,c(1,2)])

mytest_ODDS_ACP<-mytest_ODDS_ACP[,-c(31)]
test.active_pourcentage <- mytest_ODDS_ACP[, c(1,2,5:8,12:15,17,19:21,25 )]
Var_active<-test.active_pourcentage[,c(3:ncol(test.active_pourcentage))]  
Var.supp<- Var_active[,c(5:8,13)]

res.pca<-PCA(Var_active[,c(1:ncol(Var_active))],quanti.sup = c(5:8,13),graph = TRUE,ncp = 4,scale.unit = TRUE)

Inertie_Axes_ACP_communes<-fviz_eig(res.pca, addlabels = TRUE, ylim = c(0,40))

fviz_pca_var(res.pca, axes = c(1,2),col.var = "contrib",
                            alpha.var = "contrib",col.quanti.sup = "red",
                            repel = TRUE)
fviz_pca_var(res.pca, axes = c(1,2),col.var = "cos2",
             alpha.var = "cos2",col.quanti.sup = "red",
             repel = TRUE, select.var = list(cos2 =0.2))

fviz_pca_var(res.pca, axes = c(3,4),col.var = "contrib", alpha.var = "contrib",col.quanti.sup = "red",
             repel = TRUE, select.var = list(contrib = 8))
fviz_pca_var(res.pca, axes = c(3,4),col.var = "cos2",
             alpha.var = "cos2",col.quanti.sup = "red",
             repel = TRUE, select.var = list(cos2 =0.2))

fviz_pca_var(res.pca, axes = c(4,5),col.var = "contrib", alpha.var = "contrib",col.quanti.sup = "red",
             repel = TRUE, select.var = list(contrib = 8))

res.pca$var$contrib
res.acp.df<-as.data.frame(res.pca$ind$coord)
cah <- HCPC(res.acp.df, graph=FALSE, nb.clust =5)
plot(cah, choice="tree", angle=60)

library(ggplot2)
library("ggdendro")
library(dendextend)
clust.hcpc <- as.numeric(cah$data.clust$clust)
clust.cutree <- dendextend:::cutree(cah$call$t$tree, k=5, order_clusters_as_data = TRUE)

idx <- order(as.numeric(names(clust.cutree)))
clust.cutree <- clust.cutree[idx]
( tbl <- table(clust.hcpc, clust.cutree) )
( lbls <- apply(tbl,2,which.max) )


cah$call$t$tree %>% 
  color_branches(k=5, groupLabels =lbls) %>% 
  set("labels_cex", .5) %>% 
  plot(horiz=T) 



Cluster_CAh_df<-as.data.frame(cah$data.clust$clust)
Cluster_CAh_df$ID_CAH<-as.numeric(row.names(Cluster_CAh_df))
test.active_pourcentage<-as.data.frame(test.active_pourcentage)

SpUnitt_with_cluster<-cbind(test.active_pourcentage,Cluster_CAh_df)


SpUnitt_with_cluster%>%
  group_by(`cah$data.clust$clust`)%>%
  summarise(ood= mean(oddCPIS),
            ood_2= mean(oddOuvriers),
            ood_em= mean(oddEmployes),
            ood_P= mean(oddProf_inter),
            pard_PI= mean(Prof_intermediaires_AC),
            pard_Pop= mean(Employes_AC),
            pard_Pop2= mean(Ouvriers_AC),
            CPIS_acaht= mean(CPIS_AC))

            ####################################################
# Visualisation classes CAH
CAH_for_visu<-as.data.frame(cah$data.clust)

Cluster_Dimension_ACP_communes<-CAH_for_visu %>%
  group_by(clust) %>%
  summarise_all(funs(mean),na.rm=T) %>%
  gather(key = "Dimension", value = "Data", Dim.1:Dim.4) %>%
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

SoldeResid_Var_supp<- test_residents_solde%>%
  group_by(CODGEO, variables)%>%
  summarise(taux_var = ((`2012` - `1999`) / `1999`)*100 )%>%
  spread(variables, taux_var)
  
Variables_supp_acp_communes<-cbind(Var.supp,SpUnitt_with_cluster[,c(1,16)])
Variables_supp_acp_communes[,c(1:5)]<- scale(Variables_supp_acp_communes[,c(1:5)])
Variables_supp_acp_communes_plot<-Variables_supp_acp_communes %>%
  group_by(`cah$data.clust$clust`) %>%
  # summarise_all(funs(mean),na.rm=T) %>%
  summarise_at(.vars = vars(CPIS:Poids_Marche), .funs = funs(mean(.)))%>%
  gather(key = "Dimension", value = "Data",2:6) %>%
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


Test_map<- left_join(CommunesInsee, SpUnitt_with_cluster%>%
                       filter(annee.x == "2010"), by = "DepCom")

# library(mapview)
# mapview(Test_map, zcol = "cah$data.clust$clust")

colNATiles <- '#FFFFFF'
opacity <- 95 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
colNATiles <- paste0(colNATiles, opacity)
# osmTiles <- getTiles(x = CommunesInsee, type = "stamenbw",  crop = FALSE)

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
                                  "5"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          lwd = 0.2,
          colNA= colNATiles,add=TRUE)
plot(lim_IDF.st$geometry,add=T)
