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
  group_by(Periode) %>%
  mutate(Sum_Total_Region=sum(Sum_ACQ ))


carreaux_resumes <- carreaux_resumes %>%
  group_by(cetacquereurLa,Periode) %>%
  mutate(Sum_Acq_Region=sum(Sum_ACQ ))

carreaux_resumes$Moyenne_Region_Acq<- (carreaux_resumes$Sum_Acq_Region / carreaux_resumes$Sum_Total_Region)*100

carreaux_resumes$QuotientLocal<-carreaux_resumes$Pourc_Acq_Unit / carreaux_resumes$Moyenne_Region_Acq

carreaux_resumes<- carreaux_resumes[,c("Carreau_ID","Periode","cetacquereurLa","QuotientLocal")]


SpUnitt_with_cluster<-SpUnitt_with_cluster[,c("Carreau_ID","cettePeriodeLa","cah.data.clust.clust")]
SpUnitt_with_cluster$Periode<-SpUnitt_with_cluster$cettePeriodeLa

Test_Quotient_Local<- left_join(carreaux_resumes,SpUnitt_with_cluster)


Test_Quotient_Local$cluster<-Test_Quotient_Local$cah.data.clust.clust



Test_Quotient_Local %>%
  group_by(Periode, cetacquereurLa, cluster) %>%
   summarise( Moyenne_Quotient = mean (QuotientLocal, na.rm=T)) %>%
   filter(cetacquereurLa=="CPIS" | cetacquereurLa=="Prof_intermediaires" |  cetacquereurLa=="Ouvriers" |  cetacquereurLa=="Employes" )%>%
  ggplot() +
  geom_bar(aes(cetacquereurLa,Moyenne_Quotient, fill = cetacquereurLa), stat = "identity") +
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme_tmd ()+
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
   facet_grid(Periode~cluster ) +
  # facet_wrap(~cluster)
  coord_flip()
