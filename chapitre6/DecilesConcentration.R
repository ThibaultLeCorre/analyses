
Tableau_Acquereur_Carreaux1000 <- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Carreaux1000.csv", stringsAsFactors=FALSE)
mytest<-Tableau_Acquereur_Carreaux1000
mytest$Periode<-ifelse(mytest$cetteanneeLa==1996|mytest$cetteanneeLa==1999|mytest$cetteanneeLa==2003, "1.Periode_96_2003",
                       ifelse(mytest$cetteanneeLa>=2004&mytest$cetteanneeLa<=2007, "2.Periode_04_2007", 
                              ifelse(mytest$cetteanneeLa>=2008&mytest$cetteanneeLa<=2012, "3.Periode_08_2012", NA)))

mytest$cetacquereurLa<- ifelse(mytest$cetacquereurLa == "Liberales", "Com_art_Chef_entreprises",mytest$cetacquereurLa)
mytest_2 <- mytest %>%
  group_by(Periode,Carreau_ID,cetacquereurLa) %>%
  summarise(Total_type= sum(Somme_Profil_acquereur))
mytest_3 <- mytest %>%
  group_by(Periode,cetacquereurLa) %>%
  summarise(Total_acq = sum(Somme_Profil_acquereur))
mytest<- left_join(mytest_2, mytest_3)
mytest$Proba<- (mytest$Total_type / mytest$Total_acq)*100

mytest_10emeDecile<-mytest %>%
  group_by(Periode,cetacquereurLa) %>%
  mutate(rank = ntile(Proba,n = 10))

options(scipen = 999)
mytest_10emeDecile<-mytest_10emeDecile %>%
  group_by(Periode,cetacquereurLa, rank) %>%
  mutate(Weight_Rank = sum(Proba))%>%
  select(Periode,cetacquereurLa, rank,Weight_Rank ) 

mytest_10emeDecile<- mytest_10emeDecile[!duplicated(mytest_10emeDecile), ]

DecilesConcentration_plot<-mytest_10emeDecile%>%
  filter(cetacquereurLa!= "Agriculteurs" & cetacquereurLa!= "autres_inactifs" & cetacquereurLa!= "Biens_publics_et_HLM" )%>%
  ggplot(., aes(rank, Weight_Rank, group=cetacquereurLa)) +
  geom_line(aes(color = cetacquereurLa))+
  scale_color_manual(values = specificCol ) +
  scale_x_continuous(breaks = c(1,2, 3,4,5,6,7,8,9,10)) +
  theme_tmd() +
  facet_wrap(~Periode,scale="free")+
  labs(title = "Courbes d'évolution de la répartition des acquéreurs en fonction du décile\nd'intensité du marché ", x= "Déciles d'intensité" , y= "Pourcentage des achats dans le décile d'intensité du marché")+
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2019")

setwd("~/Projets/Chapitre7")
ggsave("DecilesConcentration_plot.png",plot= DecilesConcentration_plot,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)