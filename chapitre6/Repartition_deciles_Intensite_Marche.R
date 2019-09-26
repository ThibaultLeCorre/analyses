Tableau_Acquereur_Carreaux1000 <- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Carreaux1000.csv", stringsAsFactors=FALSE)
mytest<-Tableau_Acquereur_Carreaux1000
# mytest$cetteanneeLa<-ifelse(mytest$cetteanneeLa==1996|mytest$cetteanneeLa==1999|mytest$cetteanneeLa==2003, "cetteanneeLa_96_2003",
#                        ifelse(mytest$cetteanneeLa>=2004&mytest$cetteanneeLa<=2007, "cetteanneeLa_04_2007", 
#                               ifelse(mytest$cetteanneeLa>=2008&mytest$cetteanneeLa<=2012, "cetteanneeLa_08_2012", NA)))

mytest$cetacquereurLa<- ifelse(mytest$cetacquereurLa == "Liberales", "Com_art_Chef_entreprises",mytest$cetacquereurLa)
mytest_2 <- mytest %>%
  group_by(cetteanneeLa,Carreau_ID,cetacquereurLa) %>%
  summarise(Total_type= sum(Somme_Profil_acquereur))
mytest_3 <- mytest %>%
  group_by(cetteanneeLa,cetacquereurLa) %>%
  summarise(Total_acq = sum(Somme_Profil_acquereur))
mytest<- left_join(mytest_2, mytest_3)
mytest$Proba<- (mytest$Total_type / mytest$Total_acq)*100

mytest_10emeDecile<-mytest %>%
  group_by(cetteanneeLa,cetacquereurLa) %>%
  mutate(rank = ntile(Proba,n = 10))

options(scipen = 999)
mytest_10emeDecile<-mytest_10emeDecile %>%
  group_by(cetteanneeLa,cetacquereurLa, rank) %>%
  mutate(Weight_Rank = sum(Proba))%>%
  select(cetteanneeLa,cetacquereurLa, rank,Weight_Rank ) %>%
  filter(rank %in% c(10))

mytest_10emeDecile<- mytest_10emeDecile[!duplicated(mytest_10emeDecile), ]

mytest_100emeDecile<-mytest %>%
  group_by(cetteanneeLa,cetacquereurLa) %>%
  mutate(rank = ntile(Proba,n = 100))

options(scipen = 999)
mytest_100emeDecile<-mytest_100emeDecile %>%
  group_by(cetteanneeLa,cetacquereurLa, rank) %>%
  mutate(Weight_Rank = sum(Proba))%>%
  select(cetteanneeLa,cetacquereurLa, rank,Weight_Rank ) %>%
  filter(rank %in% c(100))

mytest_100emeDecile<- mytest_100emeDecile[!duplicated(mytest_100emeDecile), ]

mytest_1000<- bind_rows(mytest_100emeDecile,mytest_10emeDecile)
mytest_1000$Type<-"2.Carreaux1000"
################################################################################

Tableau_Acquereur_Carreaux200 <- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Carreaux200.csv", stringsAsFactors=FALSE)
mytest<-Tableau_Acquereur_Carreaux200
# mytest$cetteanneeLa<-ifelse(mytest$cetteanneeLa==1996|mytest$cetteanneeLa==1999|mytest$cetteanneeLa==2003, "cetteanneeLa_96_2003",
#                        ifelse(mytest$cetteanneeLa>=2004&mytest$cetteanneeLa<=2007, "cetteanneeLa_04_2007", 
#                               ifelse(mytest$cetteanneeLa>=2008&mytest$cetteanneeLa<=2012, "cetteanneeLa_08_2012", NA)))

mytest$cetacquereurLa<- ifelse(mytest$cetacquereurLa == "Liberales", "Com_art_Chef_entreprises",mytest$cetacquereurLa)
mytest_2 <- mytest %>%
  group_by(cetteanneeLa,Carreau_ID,cetacquereurLa) %>%
  summarise(Total_type= sum(Somme_Profil_acquereur))
mytest_3 <- mytest %>%
  group_by(cetteanneeLa,cetacquereurLa) %>%
  summarise(Total_acq = sum(Somme_Profil_acquereur))
mytest<- left_join(mytest_2, mytest_3)
mytest$Proba<- (mytest$Total_type / mytest$Total_acq)*100

mytest_10emeDecile<-mytest %>%
  group_by(cetteanneeLa,cetacquereurLa) %>%
  mutate(rank = ntile(Proba,n = 10))

options(scipen = 999)
mytest_10emeDecile<-mytest_10emeDecile %>%
  group_by(cetteanneeLa,cetacquereurLa, rank) %>%
  mutate(Weight_Rank = sum(Proba))%>%
  select(cetteanneeLa,cetacquereurLa, rank,Weight_Rank ) %>%
  filter(rank %in% c(10))

mytest_10emeDecile<- mytest_10emeDecile[!duplicated(mytest_10emeDecile), ]

mytest_100emeDecile<-mytest %>%
  group_by(cetteanneeLa,cetacquereurLa) %>%
  mutate(rank = ntile(Proba,n = 100))

options(scipen = 999)
mytest_100emeDecile<-mytest_100emeDecile %>%
  group_by(cetteanneeLa,cetacquereurLa, rank) %>%
  mutate(Weight_Rank = sum(Proba))%>%
  select(cetteanneeLa,cetacquereurLa, rank,Weight_Rank ) %>%
  filter(rank %in% c(100))

mytest_100emeDecile<- mytest_100emeDecile[!duplicated(mytest_100emeDecile), ]

mytest_200<- bind_rows(mytest_100emeDecile,mytest_10emeDecile)
mytest_200$Type<-"1.Carreaux200"
#############
Tableau_Acquereur_Communes<- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Communes.csv", stringsAsFactors=FALSE)
mytest<-Tableau_Acquereur_Communes
# mytest$annee.x<-ifelse(mytest$annee.x==1996|mytest$annee.x==1999|mytest$annee.x==2003, "annee.x_96_2003",
#                        ifelse(mytest$annee.x>=2004&mytest$annee.x<=2007, "annee.x_04_2007", 
#                               ifelse(mytest$annee.x>=2008&mytest$annee.x<=2012, "annee.x_08_2012", NA)))

mytest$acquereurs<- ifelse(mytest$acquereurs == "Liberales", "Com_art_Chef_entreprises",mytest$acquereurs)
mytest_2 <- mytest %>%
  group_by(annee.x,DepCom,acquereurs) %>%
  summarise(Total_type= sum(Nombre_transacs_Acquereurs))
mytest_3 <- mytest %>%
  group_by(annee.x,acquereurs) %>%
  summarise(Total_acq = sum(Nombre_transacs_Acquereurs))
mytest<- left_join(mytest_2, mytest_3)
mytest$Proba<- (mytest$Total_type / mytest$Total_acq)*100

mytest_10emeDecile<-mytest %>%
  group_by(annee.x,acquereurs) %>%
  mutate(rank = ntile(Proba,n = 10))

options(scipen = 999)
mytest_10emeDecile<-mytest_10emeDecile %>%
  group_by(annee.x,acquereurs, rank) %>%
  mutate(Weight_Rank = sum(Proba))%>%
  select(annee.x,acquereurs, rank,Weight_Rank ) %>%
  filter(rank %in% c(10))

mytest_10emeDecile<- mytest_10emeDecile[!duplicated(mytest_10emeDecile), ]

mytest_100emeDecile<-mytest %>%
  group_by(annee.x,acquereurs) %>%
  mutate(rank = ntile(Proba,n = 100))

options(scipen = 999)
mytest_100emeDecile<-mytest_100emeDecile %>%
  group_by(annee.x,acquereurs, rank) %>%
  mutate(Weight_Rank = sum(Proba))%>%
  select(annee.x,acquereurs, rank,Weight_Rank ) %>%
  filter(rank %in% c(100))

mytest_100emeDecile<- mytest_100emeDecile[!duplicated(mytest_100emeDecile), ]

mytest_Communes<- bind_rows(mytest_100emeDecile,mytest_10emeDecile)
mytest_Communes$Type<-"3.Communes"

str(mytest_Communes)

names(mytest_Communes)[1]<-"cetteanneeLa"
names(mytest_Communes)[2]<-"cetacquereurLa"
###########

mytest_Final<- bind_rows(mytest_200,mytest_1000, mytest_Communes)

mytest_Final$rank<- ifelse(mytest_Final$rank==10, "Unités qui correspondent au 10ème décile",
                           ifelse(mytest_Final$rank==100, "Unités qui correspondent au 100ème décile", NA ))


Concentration_plot<- mytest_Final%>%
  filter(cetacquereurLa!= "Agriculteurs" & cetacquereurLa!= "autres_inactifs" & cetacquereurLa!= "Biens_publics_et_HLM" )%>%
  ggplot(., aes(cetteanneeLa, Weight_Rank, group=cetacquereurLa)) +
  geom_line(aes(color = cetacquereurLa))+
  scale_color_manual(values = specificCol ) +
  scale_x_continuous(breaks = c(1996,1999, 2003,2006,2008,2010,2012)) +
  theme_tmd() +
  facet_grid(rank~Type,scale="free")+
  labs(title = "Concentration des achats dans les unités spatiales avec le plus grand nombre de transactions en fonction des catégories sociales", x= NULL , y= "Pourcentage représentée dans le décile d'intensité du marché")+ 
  labs(subtitle = "lecture : En 2003, 60 % des acquistions des Ouvriers est réalisé dans 10 % des unités spatiales du carroyage 1000m².\nCes dernières correspondent au dernier décile en fonction du nombre d'achats Ouvriers.")+ 
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2019")

setwd("~/Projets/Chapitre7/Cartes_acquereurs")
ggsave("Concentration_plot.png",plot= Concentration_plot,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)


# mytest_Communes%>%
#   filter(acquereurs!= "Agriculteurs" & acquereurs!= "autres_inactifs" & acquereurs!= "Biens_publics_et_HLM" )%>%
#   ggplot(., aes(annee.x, Weight_Rank, group=acquereurs)) +
#   geom_line(aes(color = acquereurs))+
#   scale_color_manual(values = specificCol ) +
#   scale_x_continuous(breaks = c(1996,1999, 2003,2006,2008,2010,2012)) +
#   theme_tmd() +
#   facet_wrap(~rank,scale="free")+
#   labs(title = "Courbes d'évolution de la répartition des acquéreurs en fonction du décile\nd'intensité du marché ", x= NULL , y= "Pourcentage représentée dans le décile d'intensité du marché")+ 
#   labs(subtitle = "lecture : En 2003, 60 % des acquistions des Ouvriers s'est réalisé dans 10 % des unités spatiales,\nces dernières correspondent au dernier décile en fonction du nombre d'achats Ouvriers.")+ 
#   labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2019")

###########Pourcentage de careaux sans renseigenements

Tableau_Acquereur_Carreaux1000 <- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Carreaux1000.csv", stringsAsFactors=FALSE)
mytest<-Tableau_Acquereur_Carreaux1000
# mytest$cetteanneeLa<-ifelse(mytest$cetteanneeLa==1996|mytest$cetteanneeLa==1999|mytest$cetteanneeLa==2003, "cetteanneeLa_96_2003",
#                        ifelse(mytest$cetteanneeLa>=2004&mytest$cetteanneeLa<=2007, "cetteanneeLa_04_2007", 
#                               ifelse(mytest$cetteanneeLa>=2008&mytest$cetteanneeLa<=2012, "cetteanneeLa_08_2012", NA)))

mytest$cetacquereurLa<- ifelse(mytest$cetacquereurLa == "Liberales", "Com_art_Chef_entreprises",mytest$cetacquereurLa)
mytest_2 <- mytest %>%
  group_by(cetteanneeLa,cetacquereurLa) %>%
  summarise(Pourc_sans_RS= length(which(Somme_Profil_acquereur==0))/length(which(Somme_Profil_acquereur>=0))*100)


mytest_2%>%
  filter(cetacquereurLa!= "Agriculteurs" & cetacquereurLa!= "autres_inactifs" & cetacquereurLa!= "Biens_publics_et_HLM" )%>%
  ggplot(., aes(cetteanneeLa, Pourc_sans_RS, group=cetacquereurLa)) +
  geom_line(aes(color = cetacquereurLa))+
  scale_color_manual(values = specificCol ) +
  scale_x_continuous(breaks = c(1996,1999, 2003,2006,2008,2010,2012)) +
  theme_tmd() +
  labs(title = "Pourcentage d'unités spatiales sans acquisitions selon la CSP d'appartenance ", x= NULL , y= "Pourcentage représentée dans le décile d'intensité du marché")+ 
  labs(subtitle = "lecture : En 2003, 60 % des acquistions des Ouvriers s'est réalisé dans 10 % des unités spatiales,\nces dernières correspondent au dernier décile en fonction du nombre d'achats Ouvriers.")+ 
  labs(caption = "Sources : Echantillon BD BIEN ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2019")



# 
# 
# mytest <-mytest %>%
#   group_by(cetteanneeLa,cetacquereurLa,Proba)%>%
#   mutate(cumsumProba = cumsum(Proba))
# 
# 
specificCol <- c("Agriculteurs" = "#503D3F",
                 "Com_art_Chef_entreprises" = "#861388",
                 "CPIS"= "#067BC2" ,
                 "Prof_intermediaires"="#5FAD41",
                 "Employes"= "#FF8811",
                 "Ouvriers"="#FF1D15",
                 "retraites"= "#77878B",
                 "autres_inactifs"="#8D775F",
                 "Entreprise_Marchands_SCI"= "#E9DF00",
                 "Biens_publics_et_HLM"= "black",
                 "SAFER"="grey")

# mytest %>%
#   ggplot(aes(x = Proba, colour = factor(cetacquereurLa))) +
#   stat_lorenz(desc = TRUE) +
#   coord_fixed() +
#   geom_abline(linetype = "dashed") +
#   theme_minimal() +
#   hrbrthemes::scale_x_percent() +
#   hrbrthemes::scale_y_percent() +
#   hrbrthemes::theme_ipsum()+
#   facet_wrap(~cetteanneeLa)+
#   scale_color_manual(values = specificCol ) 
# 
# 
# ggplot(mytest,aes(x= cumsum, y=Proba, group=cetteanneeLas)) +
#   geom_line(), color="#990000") +
#   scale_x_continuous(name="Cumulative share of X", limits=c(0,100)) + 
#   scale_y_continuous(name="Cumulative share of Y", limits=c(0,100)) +
#   facet_wrap(~cetacquereurLa)
#   
#   
#   
#   options(scipen = 999)
#   Nombre_transactions_CSP<-ggplot(mytest_Acq_BIEN, aes(annee.x, Value, group=Acquereurs)) +
#     geom_line(aes(color = Acquereurs))+
#     geom_line(aes(color =Acquereurs),size=1,show.legend =F)+
#     geom_line(aes(color = Acquereurs,linetype = "dashed"), show.legend =F) + 
#     scale_color_manual(values = specificCol ) +
#     # geom_point(size=0.75)+