# Import
library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)
library(broom)
#import des 2 dataframe
Montableau_Appartement_200_all <- read.csv2("~/Projets/Prix_evol_2/table_results_prix_evol_2/Montableau_Appartement_200_all.csv", stringsAsFactors=FALSE)
Montableau_Maison_200_all <- read.csv2("~/Projets/Prix_evol_2/table_results_prix_evol_2/Montableau_Maison_200_all.csv", stringsAsFactors=FALSE)
str(Montableau_Maison_200_all)
#joind eles 2 data frame
Tableau_appart_maison<- full_join (Montableau_Appartement_200_all, Montableau_Maison_200_all, by = c("Carreau_ID","Annee"))

Tableau_appart_maison<- Tableau_appart_maison[,c(1,4,5,8)]
#Annee 1999
Tableau_1999<- Tableau_appart_maison%>% group_by(Carreau_ID)%>% filter(Annee=="1999")
Tableau_1999$prixPotentiel.x<-ifelse(Tableau_1999$prixPotentiel.x<=50, NA, Tableau_1999$prixPotentiel.x)
Tableau_1999<-as.data.frame(Tableau_1999)
Tableau_1999<-Tableau_1999%>% filter( !is.na(prixPotentiel.y) | !is.na(prixPotentiel.x))




Tableau_1999$prixPotentiel_appart<- Tableau_1999$prixPotentiel.x
Tableau_1999$prixPotentiel_maison<- Tableau_1999$prixPotentiel.y

Tableau_1999%>% summarise(nombre_na_appart=length(which(is.na(prixPotentiel_appart))),
                          nombre_na_maison=length(which(is.na(prixPotentiel_maison))))

# Comme il y a plus de carreaux NA pour les appart on fait le modèle sur les prix des maisons
#test de correl et de determination des 2 variables
test.cor<-cor(log(Tableau_1999$prixPotentiel_appart),log(Tableau_1999$prixPotentiel_maison),use="complete.obs")
test.cor<-cor (Tableau_1999$prixPotentiel_maison,Tableau_1999$prixPotentiel_appart, use="complete.obs")
R2_1999<-test.cor^2

Y <- log(Tableau_1999$prixPotentiel_maison)#variable explicative
X <- log(Tableau_1999$prixPotentiel_appart) #variables à expliquer
unique(is.na(Y))

#test de regression
reg.lin<-lm(X~Y)
plot(X,Y)
#recuperation des coeff
coefficients(reg.lin)
coefRegr <- coefficients(reg.lin)

Tableau_appart_maison$FITTED<- augment(reg.lin)
#caclul des nouvelles valeurs
Tableau_appart_maison$log_prix_new<-ifelse(is.na(Tableau_appart_maison$prixPotentiel.x) , coefRegr[1] + coefRegr[2] *log( Tableau_appart_maison$prixPotentiel.y), NA)
Tableau_appart_maison$log_prix_new
exp(Tableau_appart_maison$log_prix_new)
###Année 2012


Tableau_appart_maison<- Tableau_appart_maison%>% filter(Annee=="2012")
str(Tableau_appart_maison)
#test de correl et de determination des 2 variables
test.cor<-cor (log(Tableau_appart_maison$prixPotentiel.x),log(Tableau_appart_maison$prixPotentiel.y))
test.cor^2

Y <- Tableau_appart_maison$prixPotentiel.y#variable explicative
X <- Tableau_appart_maison$prixPotentiel.x #variables à expliquer
plot (log(Y),log(X))

#test de regression
reg.lin<-lm(log(X)~log(Y))
reg.lin<-lm(X~Y)

#recuperation des coeff
coefficients(reg.lin)
coefRegr <- coefficients(reg.lin)
Tableau_appart_maison$FITTED <- coefRegr[1]
Tableau_appart_maison$FITTED<- augment(reg.lin)
#caclul des nouvelles valeurs
Tableau_appart_maison$New<-ifelse(is.na(Tableau_appart_maison$prixPotentiel.x) , coefRegr[1] + coefRegr[2] * Tableau_appart_maison$prixPotentiel.y, Tableau_appart_maison$prixPotentiel.x)
