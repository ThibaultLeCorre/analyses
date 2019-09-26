
library(sf)
library(dplyr)
library(tidyr)




 Typo_1999_2012_carreaux200_appartement_37158tiles <- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_carreaux200_appartement_37158tiles.csv", stringsAsFactors=FALSE)
  Typo_1999_2012_carreaux200_Maison_36693tiles <- read.csv2("~/Projets/modele_trajectoires_structures_prix/tables_typo/Typo_1999_2012_carreaux200_Maison_36693tiles.csv", stringsAsFactors=FALSE)


 colnames(SpUnitt_with_cluster)[colnames(SpUnitt_with_cluster) == 'cah$data.clust$clust'] <- 'Clusters_Carreaux'
SpUnitt_with_cluster$Clusters_Carreaux<- ifelse(SpUnitt_with_cluster$Clusters_Carreaux==1,"Marché banal pavillonaire péri-urbain en primo accession",
ifelse(SpUnitt_with_cluster$Clusters_Carreaux==2,"Marché familial pavillonaire banal en périphérie de l'agglo et dans le péri urbain",
 ifelse(SpUnitt_with_cluster$Clusters_Carreaux==4,"Marché familial pavillonaire privilégié",
 ifelse(SpUnitt_with_cluster$Clusters_Carreaux==5,"Marché de la promotion immobilière",
ifelse(SpUnitt_with_cluster$Clusters_Carreaux==6 | SpUnitt_with_cluster$Clusters_Carreaux==3 | SpUnitt_with_cluster$Clusters_Carreaux==7,"Marché banals de l'agglomération avec évolution forte du régime",
ifelse(SpUnitt_with_cluster$Clusters_Carreaux==7,"Marché de la primo accession dans les espaces urbains denses",
ifelse(SpUnitt_with_cluster$Clusters_Carreaux==8,"Marché des retraités",
ifelse(SpUnitt_with_cluster$Clusters_Carreaux==9,"Marché privilégié de l'agglomération",
ifelse(SpUnitt_with_cluster$Clusters_Carreaux==10,"Marché d'investissements et d'occupation résidentielle dans les espaces centraux",
ifelse(SpUnitt_with_cluster$Clusters_Carreaux==11,"Marché du luxe", SpUnitt_with_cluster$Clusters_Carreaux))))))))))                                                           

Prix_cluster_appart<- left_join(SpUnitt_with_cluster[,c(1,2,32)]%>%
                                  spread(key = cettePeriodeLa, value = Clusters_Carreaux),Typo_1999_2012_carreaux200_appartement_37158tiles, by ="Carreau_ID")

Prix_cluster_maison<- left_join(SpUnitt_with_cluster[,c(1,2,32)]%>%
                                  spread(key = cettePeriodeLa, value = Clusters_Carreaux),Typo_1999_2012_carreaux200_Maison_36693tiles, by ="Carreau_ID")

library(ggplot2)

setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="Evol_AppartprixClustersPeriode1.pdf",width = 12, height = 10)
Prix_cluster_appart%>%
  mutate(`1.TCAM9603`=(((`X2003`/`X1996`)^(1/8)-(1))*100))%>%
  mutate(`2.TCAM0307`=(((`X2007`/`X2004`)^(1/4)-(1))*100))%>%
  mutate(`3.TCAM0712`=(((`X2012`/`X2008`)^(1/5)-(1))*100))%>%
ggplot(., aes(x=factor(Periode_96_2003), y=`1.TCAM9603`)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_96_2003)))+
  xlab("Régimes") + ylab("Evolution des prix") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(-10,30)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Evolution des prix au m²",title="",
       subtitle="")
# caption="Source: BIEN\n réalisation : T.Le Corre, UMR Géographie-Cités, 2017")
dev.off()

setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="Evol_AppartprixClustersPeriode2.pdf",width = 12, height = 10)
Prix_cluster_appart%>%
  mutate(`1.TCAM9603`=(((`X2003`/`X1996`)^(1/8)-(1))*100))%>%
  mutate(`2.TCAM0307`=(((`X2007`/`X2004`)^(1/4)-(1))*100))%>%
  mutate(`3.TCAM0712`=(((`X2012`/`X2008`)^(1/5)-(1))*100))%>%
  ggplot(., aes(x=factor(Periode_04_2007), y=`2.TCAM0307`)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_04_2007))) +
   xlab("Régimes") + ylab("Evolution des prix") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(-10,30)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Evolution des prix au m²",title="",
       subtitle="")
# caption="Source: BIEN\n réalisation : T.Le Corre, UMR Géographie-Cités, 2017")
dev.off()


setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="Evol_AppartprixClustersPeriode3.pdf",width = 12, height = 10)
Prix_cluster_appart%>%
  mutate(`1.TCAM9603`=(((`X2003`/`X1996`)^(1/8)-(1))*100))%>%
  mutate(`2.TCAM0307`=(((`X2007`/`X2004`)^(1/4)-(1))*100))%>%
  mutate(`3.TCAM0712`=(((`X2012`/`X2008`)^(1/5)-(1))*100))%>%
  ggplot(., aes(x=factor(Periode_08_2012), y=`3.TCAM0712`)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_08_2012))) +
  xlab("Régimes") + ylab("Evolution des prix") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(-10,30)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Evolution des prix au m²",title="",
       subtitle="")
# caption="Source: BIEN\n réalisation : T.Le Corre, UMR Géographie-Cités, 2017")
dev.off()

####################################################################################

setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="PrixAppartClustersPeriode1.pdf",width = 12, height = 10)
ggplot(Prix_cluster_appart, aes(x=factor(Periode_96_2003), y=X2003)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_96_2003))) +
  # xlab("Année") + ylab("Prix au m²") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(0,20000)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Prix au m²",title="",
       subtitle="")
dev.off()

setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="PrixAppartClustersPeriode2.pdf",width = 12, height = 10)
ggplot(Prix_cluster_appart, aes(x=factor(Periode_04_2007), y=X2007)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9),aes(fill=factor(Periode_04_2007))) +
  # xlab("Année") + ylab("Prix au m²") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(0,20000)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Prix au m²",title="",
       subtitle="")
dev.off()

setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="PrixAppartClustersPeriode3.pdf",width = 12, height = 10)
ggplot(Prix_cluster_appart, aes(x=factor(Periode_08_2012), y=X2012)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_08_2012))) +
  # xlab("Année") + ylab("Prix au m²") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(0,20000)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Prix au m²",title="",
       subtitle="")
dev.off()

##########################################################################

setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="Evol_MaisontprixClustersPeriode1.pdf",width = 12, height = 10)
Prix_cluster_maison%>%
  mutate(`1.TCAM9603`=(((`X2003`/`X1996`)^(1/8)-(1))*100))%>%
  mutate(`2.TCAM0307`=(((`X2007`/`X2004`)^(1/4)-(1))*100))%>%
  mutate(`3.TCAM0712`=(((`X2012`/`X2008`)^(1/5)-(1))*100))%>%
  ggplot(., aes(x=factor(Periode_96_2003), y=`1.TCAM9603`)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_96_2003))) +
  xlab("Régimes") + ylab("Evolution des prix") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(-10,30)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Evolution des prix",title="",
       subtitle="")
# caption="Source: BIEN\n réalisation : T.Le Corre, UMR Géographie-Cités, 2017")
dev.off()

setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="Evol_MaisonprixClustersPeriode2.pdf",width = 12, height =10)
Prix_cluster_maison%>%
  mutate(`1.TCAM9603`=(((`X2003`/`X1996`)^(1/8)-(1))*100))%>%
  mutate(`2.TCAM0307`=(((`X2007`/`X2004`)^(1/4)-(1))*100))%>%
  mutate(`3.TCAM0712`=(((`X2012`/`X2008`)^(1/5)-(1))*100))%>%
  ggplot(., aes(x=factor(Periode_04_2007), y=`2.TCAM0307`)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_04_2007))) +
  xlab("Régimes") + ylab("Evolution des prix") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(-10,30)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Evolution des prix",title="",
       subtitle="")
# caption="Source: BIEN\n réalisation : T.Le Corre, UMR Géographie-Cités, 2017")
dev.off()


setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="Evol_MaisonprixClustersPeriode3.pdf",width = 12, height = 10)
Prix_cluster_maison%>%
  mutate(`1.TCAM9603`=(((`X2003`/`X1996`)^(1/8)-(1))*100))%>%
  mutate(`2.TCAM0307`=(((`X2007`/`X2004`)^(1/4)-(1))*100))%>%
  mutate(`3.TCAM0712`=(((`X2012`/`X2008`)^(1/5)-(1))*100))%>%
  ggplot(., aes(x=factor(Periode_08_2012), y=`3.TCAM0712`)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_08_2012))) +
  xlab("Régimes") + ylab("Evolution des prix") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(-10,30)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Evolution des prix",title="",
       subtitle="")
# caption="Source: BIEN\n réalisation : T.Le Corre, UMR Géographie-Cités, 2017")
dev.off()




###############################################################


setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="PrixMaisonClustersPeriode1.pdf",width = 12, height = 10)
ggplot(Prix_cluster_maison, aes(x=factor(Periode_96_2003), y=X2003)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_96_2003))) +
  # xlab("Année") + ylab("Prix au m²") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(0,3000000)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Prix net de vente",title="",
       subtitle="")
dev.off()

setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="PrixMaisonClustersPeriode2.pdf",width = 12, height = 10)
ggplot(Prix_cluster_maison, aes(x=factor(Periode_04_2007), y=X2007)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_04_2007))) +
  # xlab("Année") + ylab("Prix au m²") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(0,3000000)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Prix net de vente",title="",
       subtitle="")
dev.off()

setwd("~/Projets/Realisation_ACP/Realisation_ACP")
pdf(file="PrixMaisonClustersPeriode3.pdf",width = 12, height = 10)
ggplot(Prix_cluster_maison, aes(x=factor(Periode_08_2012), y=X2012)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), aes(fill=factor(Periode_08_2012))) +
  # xlab("Année") + ylab("Prix au m²") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(0,3000000)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
   theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Prix net de vente",title="",
       subtitle="")
dev.off()


