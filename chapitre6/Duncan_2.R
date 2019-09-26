carreauInsee1000_jointure<- Tableau_Acquereur_Carreaux1000 <- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Carreaux1000.csv", stringsAsFactors=FALSE)
carreauInsee200_jointure<-Tableau_Acquereur_Carreaux200 <- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Carreaux200.csv", stringsAsFactors=FALSE)
Tableau_Acquereur_Communes<- read.csv2("~/Projets/Chapitre7/Preparation_Tables_Groupes_Sociaux/Tableau_Acquereur_Communes.csv", stringsAsFactors=FALSE)

names(carreauInsee200_jointure)[2]<-"annee.x"
names(carreauInsee200_jointure)[3]<-"acquereurs"
names(carreauInsee1000_jointure)[2]<-"annee.x"
names(carreauInsee1000_jointure)[3]<-"acquereurs"
###
str(carreauInsee200_jointure)
carreauInsee200_jointure <- carreauInsee200_jointure%>%
  filter(annee.x!="NA")
Annee <- unique(carreauInsee200_jointure$annee.x)
MesCSP <- unique(carreauInsee200_jointure$acquereurs)

Results_Duncan_200 <- list()

for (CetteAnnee in Annee){
  
  carreaux_filtred <- carreauInsee200_jointure %>%
    filter(annee.x == CetteAnnee)
  
  Total_Ville_Periode <-  carreaux_filtred%>%
    summarise(Total_Ville_Periode= sum(Somme_Profil_acquereur))
  Total_Ville_Periode <- unique(Total_Ville_Periode$Total_Ville_Periode)
  
  Nombre_total_Unit<-carreaux_filtred %>%
    group_by(Carreau_ID)%>%
    summarise(Nombre_total_Unit= sum(Somme_Profil_acquereur))
  
  for (cetteCSPLa in MesCSP){
    
    
    carreaux_filtred_2 <- as.data.frame(carreaux_filtred) %>%
      filter(acquereurs == cetteCSPLa)%>%
      group_by(Carreau_ID) 
    # %>%summarise(Nombre_acquereurs= sum(Somme_Profil_acquereur))
    
    carreaux_filtred_2 <-left_join(as.data.frame(Nombre_total_Unit), carreaux_filtred_2, by= "Carreau_ID")
    
    carreaux_filtred_2$Somme_Profil_acquereur[is.na(carreaux_filtred_2$Somme_Profil_acquereur)] <- 0
    
     xi<- carreaux_filtred_2$Somme_Profil_acquereur
     X<- sum(carreaux_filtred_2$Somme_Profil_acquereur, na.rm = T)
     
     ti<- carreaux_filtred_2$Nombre_total_Unit
     t<- Total_Ville_Periode
    
    xiX<- xi/X
      tixi<-ti-xi
      TX<-t-X
 tixiTX<- tixi/TX
      bup<- xiX- tixiTX
    
    ##Dissimilarité#
    # tit<-ti/t
    # xiXtit<- xiX-tit
    # bup<- xiXtit
     
     Resduncan <- 0.5*sum(abs(bup))
     
     # Resduncan <- 0.5*sum(abs((carreaux_filtred_2$Somme_Profil_acquereur/sum(carreaux_filtred_2$Somme_Profil_acquereur))-(((carreaux_filtred_2$Nombre_total_Unit - carreaux_filtred_2$Somme_Profil_acquereur)) / (((Total_Ville_Periode)-sum(carreaux_filtred_2$Somme_Profil_acquereur))))))
    # index<- id(carreaux_filtred_2, vars = c("Nombre_acquereurs", "Nombre_total_Unit"))
    
    testduncan <- list()
    # testduncan$Indice<- index
    testduncan$Indice<- Resduncan
    testduncan$annee <- CetteAnnee
    testduncan$CSP <- cetteCSPLa
    Results_Duncan_200[[as.character(CetteAnnee)]][[cetteCSPLa]] <- testduncan
    
  } 
  
}
# library(MLID)



Results_Duncan_200_2 <- purrr::map(Results_Duncan_200, ~bind_rows(.x)) %>% bind_rows()
Results_Duncan_200_2$Scale<-"Carreaux_200"

###

carreauInsee1000_jointure <- carreauInsee1000_jointure%>%
  filter(annee.x!="NA")
Annee <- unique(carreauInsee1000_jointure$annee.x)
MesCSP <- unique(carreauInsee1000_jointure$acquereurs)

Results_Duncan_1000 <- list()

for (CetteAnnee in Annee){
  
  carreaux_filtred <- carreauInsee1000_jointure %>%
    filter(annee.x == CetteAnnee)
  
  Total_Ville_Periode <-  carreaux_filtred%>%
    summarise(Total_Ville_Periode= sum(Somme_Profil_acquereur))
  Total_Ville_Periode <- unique(Total_Ville_Periode$Total_Ville_Periode)
  
  Nombre_total_Unit<-carreaux_filtred %>%
    group_by(Carreau_ID)%>%
    summarise(Nombre_total_Unit= sum(Somme_Profil_acquereur))
  
  for (cetteCSPLa in MesCSP){
    
    
    carreaux_filtred_2 <- as.data.frame(carreaux_filtred) %>%
      filter(acquereurs == cetteCSPLa)%>%
      group_by(Carreau_ID) 
    # %>%summarise(Nombre_acquereurs= sum(Somme_Profil_acquereur))
    
    carreaux_filtred_2 <-left_join(as.data.frame(Nombre_total_Unit), carreaux_filtred_2, by= "Carreau_ID")
    
    carreaux_filtred_2$Somme_Profil_acquereur[is.na(carreaux_filtred_2$Somme_Profil_acquereur)] <- 0
    
    xi<- carreaux_filtred_2$Somme_Profil_acquereur
    X<- sum(carreaux_filtred_2$Somme_Profil_acquereur, na.rm = T)
    
    ti<- carreaux_filtred_2$Nombre_total_Unit
    t<- Total_Ville_Periode
    
    xiX<- xi/X
    tixi<-ti-xi
    TX<-t-X
    tixiTX<- tixi/TX
    bup<- xiX- tixiTX
    
    ##Dissimilarité#
    # tit<-ti/t
    # xiXtit<- xiX-tit
    # bup<- xiXtit
    
    Resduncan <- 0.5*sum(abs(bup))
    
    # Resduncan <- 0.5*sum(abs((carreaux_filtred_2$Somme_Profil_acquereur/sum(carreaux_filtred_2$Somme_Profil_acquereur))-(((carreaux_filtred_2$Nombre_total_Unit - carreaux_filtred_2$Somme_Profil_acquereur)) / (((Total_Ville_Periode)-sum(carreaux_filtred_2$Somme_Profil_acquereur))))))
    # index<- id(carreaux_filtred_2, vars = c("Nombre_acquereurs", "Nombre_total_Unit"))
  
    testduncan <- list()
    # testduncan$Indice<- index
    testduncan$Indice<- Resduncan
    testduncan$annee <- CetteAnnee
    testduncan$CSP <- cetteCSPLa
    Results_Duncan_1000[[as.character(CetteAnnee)]][[cetteCSPLa]] <- testduncan
    
    } 
  
}

Results_Duncan_1000_2 <- purrr::map(Results_Duncan_1000, ~bind_rows(.x)) %>% bind_rows()
Results_Duncan_1000_2$Scale<-"Carreaux_1000"

###########
Results_Duncan_Communes <- list()

for (CetteAnnee in Annee){
  
  carreaux_filtred <- Tableau_Acquereur_Communes%>%
    filter(annee.x == CetteAnnee)
  
  Total_Ville_Periode <-  carreaux_filtred%>%
    summarise(Total_Ville_Periode= sum(Nombre_transacs_Acquereurs))
  Total_Ville_Periode <- unique(Total_Ville_Periode$Total_Ville_Periode)
  
  Nombre_total_Unit<-carreaux_filtred %>%
    group_by(DepCom)%>%
    summarise(Nombre_total_Unit= sum(Nombre_transacs_Acquereurs))
  
  for (cetteCSPLa in MesCSP){
    
    
    
    carreaux_filtred_2 <- as.data.frame(carreaux_filtred) %>%
      filter(acquereurs == cetteCSPLa)%>%
      group_by(DepCom) 
    # %>%summarise(Nombre_acquereurs= sum(Somme_Profil_acquereur))
    
    carreaux_filtred_2 <-left_join(as.data.frame(Nombre_total_Unit), carreaux_filtred_2, by= "DepCom")
    
    carreaux_filtred_2$Nombre_transacs_Acquereurs[is.na(carreaux_filtred_2$Nombre_transacs_Acquereurs)] <- 0
    
    xi<- carreaux_filtred_2$Nombre_transacs_Acquereurs
    X<- sum(carreaux_filtred_2$Nombre_transacs_Acquereurs, na.rm = T)
    
    ti<- carreaux_filtred_2$Nombre_total_Unit
    t<- Total_Ville_Periode
    
    xiX<- xi/X
    tixi<-ti-xi
    TX<-t-X
    tixiTX<- tixi/TX
    bup<- xiX- tixiTX
    
    ##Dissimilarité#
    # tit<-ti/t
    # xiXtit<- xiX-tit
    # bup<- xiXtit
    
    Resduncan <- 0.5*sum(abs(bup))
    
    # Resduncan <- 0.5*sum(abs((carreaux_filtred_2$Somme_Profil_acquereur/sum(carreaux_filtred_2$Somme_Profil_acquereur))-(((carreaux_filtred_2$Nombre_total_Unit - carreaux_filtred_2$Somme_Profil_acquereur)) / (((Total_Ville_Periode)-sum(carreaux_filtred_2$Somme_Profil_acquereur))))))
    # index<- id(carreaux_filtred_2, vars = c("Nombre_acquereurs", "Nombre_total_Unit"))
    testduncan <- list()
    testduncan$Indice<- Resduncan
    testduncan$annee <- CetteAnnee
    testduncan$CSP <- cetteCSPLa
    Results_Duncan_Communes[[as.character(CetteAnnee)]][[cetteCSPLa]] <- testduncan
    
  } 
  
}

Results_Duncan_Communes_2 <- purrr::map(Results_Duncan_Communes, ~bind_rows(.x)) %>% bind_rows()
Results_Duncan_Communes_2$Scale<-"Communes"


Results_Duncan<- bind_rows(Results_Duncan_200_2, Results_Duncan_Communes_2, Results_Duncan_1000_2)

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

Results_Duncan%>%
  filter(CSP!="Agriculteurs" & CSP!="Biens_publics_et_HLM")%>%
  ggplot(., aes(annee, Indice, group=CSP)) +
  geom_line(aes(color = CSP))+
  geom_line(aes(color =CSP),size=1,show.legend =F)+
  geom_line(aes(color = CSP,linetype = "dashed"), show.legend =F) + 
  scale_color_manual(values = specificCol ) +
  # geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  facet_wrap(~Scale,  ncol=3, scale="fix")+
  labs(title = "Indices de ségrégation sur les achats de logements en île-de-France selon les CSP", x= "Année", y= "Valeur d'indice")+ 
  labs(subtitle = "lecture : En 1999 à une échelle de carreaux de 1km², environ 25% des employés aurait dû acheter ailleurs\npour obtenir une répartition spatiale parfaite" )+
  labs(caption = "Sources : Echantillon redressé de la base BIEN. Réalisation : Thibault Le Corre, 2017")







Results_Duncan_200_2%>%
  filter(CSP!="Agriculteurs" & CSP!="Biens_publics_et_HLM")%>%
  ggplot(., aes(annee, Indice, group=CSP)) +
  geom_line(aes(color = CSP))+
  geom_line(aes(color =CSP),size=1,show.legend =F)+
  geom_line(aes(color = CSP,linetype = "dashed"), show.legend =F) + 
  # geom_point(size=0.75)+
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  labs(title = "Indices de ségrégation sur les achats de logements en île-de-France selon les CSP", x= "Année", y= "Valeur d'indice")+ 
  labs(subtitle = "lecture : En 1999 à une échelle de carreaux de 1km², environ 25% des employés aurait dû acheter ailleurs\npour obtenir une répartition spatiale parfaite" )+
  labs(caption = "Sources : Echantillon redressé de la base BIEN. Réalisation : Thibault Le Corre, 2017")


