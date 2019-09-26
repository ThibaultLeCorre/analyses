library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)
library(ggplot2)

Tableau_Acquereur_Carreaux200 <- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Carreaux200.csv", stringsAsFactors=FALSE)
SpUnitt_with_cluster <- read.csv2("~/Projets/Realisation_ACP/SpUnitt_with_cluster.csv", stringsAsFactors=FALSE)


mytest<-Tableau_Acquereur_Carreaux200


mytest$Periode<-ifelse(mytest$cetteanneeLa==1996|mytest$cetteanneeLa==1999|mytest$cetteanneeLa==2003, "Periode_96_2003",
                       ifelse(mytest$cetteanneeLa>=2004&mytest$cetteanneeLa<=2007, "Periode_04_2007", 
                              ifelse(mytest$cetteanneeLa>=2008&mytest$cetteanneeLa<=2012, "Periode_08_2012", NA)))


carreaux_resumes <- mytest %>%
  group_by(Carreau_ID, cetacquereurLa, Periode) %>%
  summarise(Sum_ACQ=sum(Somme_Profil_acquereur ))

carreaux_resumes <- carreaux_resumes %>%
  group_by(Carreau_ID, Periode) %>%
  mutate(Sum_Total_Unit=sum(Sum_ACQ ))

carreaux_resumes$Pourc_Acq_Unit<- (carreaux_resumes$Sum_ACQ / carreaux_resumes$Sum_Total_Unit)*100


carreaux_resumes <- carreaux_resumes %>%
  select(-Sum_ACQ, -Sum_Total_Unit) %>%
  group_by(Carreau_ID, Periode) %>%
  spread(cetacquereurLa, Pourc_Acq_Unit, fill=0)

SpUnitt_with_cluster<-SpUnitt_with_cluster[,c("Carreau_ID","cettePeriodeLa","cah.data.clust.clust")]
SpUnitt_with_cluster$Periode<-SpUnitt_with_cluster$cettePeriodeLa


Divers_Regimes<- left_join(carreaux_resumes,SpUnitt_with_cluster)

Divers_Regimes$cluster<-Divers_Regimes$cah.data.clust.clust


Divers_Regimes %>%
  gather(key = "Acquereurs", "Pourc", 3:12)%>%
  mutate(PourcStd = base::scale(Pourc)) %>%
   filter(Acquereurs=="CPIS" | Acquereurs=="Prof_intermediaires" |  Acquereurs=="Ouvriers" |  Acquereurs=="Employes" )%>%
  ggplot() +
  geom_violin(aes(y = PourcStd, x= Acquereurs)) +
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme_tmd ()+
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  # facet_grid(Acquereurs~cluster )
   facet_wrap(~cluster)



Divers_Regimes_2<- Divers_Regimes%>%
  gather(key = "Acquereurs", "Pourc", 3:12)%>%
  group_by(Carreau_ID, Periode,cluster )%>%
  summarise(NombrCategories= length(which(Pourc>10)))%>%
  group_by(Periode,cluster)%>%
  summarise(Moyenne_categories = mean(NombrCategories))



Divers_RegimesPeriode1<-Divers_Regimes%>%
  filter(cettePeriodeLa=="Periode_96_2003")%>%
  gather(key = "Acquereurs", "Pourc", 3:12)%>%
  group_by(cluster,Acquereurs )%>%
  summarise(PourcCategories= mean(Pourc))%>%
  spread(Acquereurs, PourcCategories,fill=0)

Divers_RegimesPeriode1<-Divers_RegimesPeriode1[,c(2:11)]

library(vegan)
?diversity
diversity(Divers_RegimesPeriode1, index = "shannon", MARGIN = 1, base = exp(1))
Simpson_com1<-diversity(Divers_RegimesPeriode1, index = "simpson", MARGIN = 1, base = exp(1))
Simpson_com1<-as.data.frame(Simpson_com1)
Communes_CarreauxPeriode1<-cbind(Communes_CarreauxPeriode1test,Simpson_com1)
specnumber(Simpson_com1,  MARGIN = 1)
S1 <- specnumber(Communes_CarreauxPeriode1test) ## rowSums(BCI > 0) does the same...
S1 <-as.data.frame(S1)
Communes_CarreauxPeriode1<-cbind(Communes_CarreauxPeriode1,S1)