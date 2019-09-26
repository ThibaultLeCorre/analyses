

library(ggplot2)





library(tidyr)
library(dplyr)


#######Table principal#####
data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","X.x","Y.x",
                                        "ANNAIS_AC","SITMAT_AC", "ANNAIS_VE","SITMAT_VE","REQTYPBIEN")]
DBSCAN_results_table_Promoteurs <- read.csv("~/Projets/DBSCAN/DBSAN/DBSCAN_results_table_Promoteurs.csv", sep="", stringsAsFactors=FALSE)
mytest$ID<-as.numeric(mytest$ID)
mytest<-left_join(mytest,DBSCAN_results_table_Promoteurs, by="ID")


#Types acquéreurs et vendeurs: toutes transactions

mytest$acquereurs<- ifelse(mytest$QUALITE_AC == "AD"  &!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM",
                           ifelse(mytest$QUALITE_AC== "EN"& !is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                  ifelse(mytest$QUALITE_AC== "PR"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                         ifelse(mytest$QUALITE_AC== "SA"&!is.na(mytest$QUALITE_AC),"SAFER",
                                                ifelse(mytest$QUALITE_AC== "SC"&!is.na(mytest$QUALITE_AC),"Entreprise_Marchands_SCI",
                                                       ifelse(mytest$QUALITE_AC== "SO"&!is.na(mytest$QUALITE_AC),"Biens_publics_et_HLM", "particulier_ou_na"))))))



mytest$acquereurs<- ifelse(mytest$CSP_AC == 10 & mytest$acquereurs == "particulier_ou_na", "Agriculteurs",
                           ifelse(mytest$CSP_AC >= 20 & mytest$CSP_AC < 30 & mytest$acquereurs == "particulier_ou_na", "Com_art_Chef_entreprises",
                                  ifelse(mytest$CSP_AC >= 30 & mytest$CSP_AC < 40 & mytest$acquereurs == "particulier_ou_na", "CPIS",
                                         ifelse(mytest$CSP_AC >= 40 & mytest$CSP_AC < 50 & mytest$acquereurs == "particulier_ou_na", "Prof_intermediaires",
                                                ifelse(mytest$CSP_AC >= 50 & mytest$CSP_AC < 60 & mytest$acquereurs == "particulier_ou_na", "Employes",
                                                       ifelse(mytest$CSP_AC >= 60 & mytest$CSP_AC < 70 & mytest$acquereurs == "particulier_ou_na", "Ouvriers",
                                                              ifelse(mytest$CSP_AC >= 70 & mytest$CSP_AC < 80 & mytest$acquereurs == "particulier_ou_na", "retraites",
                                                                     ifelse(mytest$CSP_AC == 80 & mytest$acquereurs == "particulier_ou_na", "autres_inactifs", mytest$acquereurs))))))))




mytest<-mytest%>%filter(!is.na(acquereurs), acquereurs!= "SAFER")
# 

##################################


mytest$Vendeurs<- ifelse(mytest$QUALITE_VE == "AD" & !is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM",
                         ifelse(mytest$QUALITE_VE== "EN"& !is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                ifelse(mytest$QUALITE_VE== "PR"& !is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                       ifelse(mytest$QUALITE_VE== "SA"& !is.na(mytest$QUALITE_VE),"SAFER",
                                              ifelse(mytest$QUALITE_VE== "SC"& !is.na(mytest$QUALITE_VE),"Entreprise_Marchands_SCI",
                                                     ifelse(mytest$QUALITE_VE== "SO"& !is.na(mytest$QUALITE_VE),"Biens_publics_et_HLM", "particulier_ou_na"))))))


mytest$Vendeurs<- ifelse(mytest$CSP_VE == 10 & mytest$Vendeurs == "particulier_ou_na", "Agriculteurs",
                         ifelse(mytest$CSP_VE >= 20 & mytest$CSP_VE < 30 & mytest$Vendeurs == "particulier_ou_na", "Com_art_Chef_entreprises",
                                ifelse(mytest$CSP_VE >= 30 & mytest$CSP_VE < 40 & mytest$Vendeurs == "particulier_ou_na", "CPIS",
                                       ifelse(mytest$CSP_VE >= 40 & mytest$CSP_VE < 50 & mytest$Vendeurs == "particulier_ou_na", "Prof_intermediaires",
                                              ifelse(mytest$CSP_VE >= 50 & mytest$CSP_VE < 60 & mytest$Vendeurs == "particulier_ou_na", "Employes",
                                                     ifelse(mytest$CSP_VE >= 60 & mytest$CSP_VE < 70 & mytest$Vendeurs == "particulier_ou_na", "Ouvriers",
                                                            ifelse(mytest$CSP_VE >= 70 & mytest$CSP_VE < 80 & mytest$Vendeurs == "particulier_ou_na", "retraites",
                                                                   ifelse(mytest$CSP_VE == 80 & mytest$Vendeurs == "particulier_ou_na", "autres_inactifs", mytest$Vendeurs))))))))

mytest$Vendeurs<- ifelse(mytest$cluster>=1 & !is.na(mytest$cluster),"Promoteurs", mytest$Vendeurs)


mytest<-mytest%>%filter(!is.na(Vendeurs), Vendeurs!= "SAFER")


mytest$Couple_Acquereur_Vendeur<- paste(mytest$acquereurs, mytest$Vendeurs, sep ="_" )

# mytest$Periode<-ifelse(mytest$annee.x==1996 | mytest$annee.x==1999 | mytest$annee.x==2003, "1.Periode_96_2003",
#                        ifelse(mytest$annee.x>=2004 & mytest$annee.x<=2007, "2.Periode_04_2007", 
#                               ifelse(mytest$annee.x>=2008 & mytest$annee.x<=2012, "3.Periode_08_2012", NA)))


myCouple<-mytest %>% 
  group_by(annee.x, acquereurs,Vendeurs)%>% 
  add_count(Couple_Acquereur_Vendeur)%>%
  select(annee.x,acquereurs,Vendeurs, n)



myGeneralFlows<- myCouple[!duplicated(myCouple), ]
NVentes<-sum(myGeneralFlows$n)


library(ggplot2)
library(reshape2)


specificCol <- c("Com_art_Chef_entreprises" = "#861388",
                 "CPIS"= "#067BC2" ,
                 "Prof_intermediaires"="#5FAD41",
                 "Employes"= "#FF8811",
                 "Ouvriers"="#FF1D15",
                 "retraites"= "#77878B",
                 "autres_inactifs"="#8D775F",
                 "Entreprise_Marchands_SCI"= "#E9DF00",
                 "Biens_publics_et_HLM"= "black",
                 "Promoteurs"="#503D3F")


myGeneralFlows_Vendeurs<-myGeneralFlows%>%
  group_by(annee.x, Vendeurs)%>%
  transmute(acquereurs, percent = n/sum(n)*100)%>%
  filter (acquereurs!="Agriculteurs", acquereurs!="Biens_publics_et_HLM", acquereurs!="autres_inactifs", acquereurs!="Com_art_Chef_entreprises",  
          Vendeurs!="Agriculteurs", Vendeurs!="Biens_publics_et_HLM", Vendeurs!="autres_inactifs", Vendeurs!="Com_art_Chef_entreprises" )%>%
  ggplot(., aes(annee.x, percent,group= acquereurs)) +
  geom_line(aes(color= acquereurs))+
  scale_color_manual(values = specificCol )+
  # geom_text(aes(label=round(percent, digits = 0))) +
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  # scale_fill_gradient2(low = "blue", high = "red", mid = NULL, 
  #                      midpoint = FALSE, limit = c(0,50), space = "Lab",
  #                      name= "" )+
  facet_wrap(~Vendeurs,scale="free")+
labs(title = "Pourcentage des logements vendus selon selon le profil des acquéreurs", x= NULL, y= NULL)+ 
  labs(subtitle = "Lecture : En 2003, 30 % des logements vendus par des Employés ont été achetés par des Professions Intermédiaires",
       x= NULL, y= NULL)+
  labs(caption = "Sources : échantillon BD BIEN sur effectifs redressés ; Réalisation : Thibault Le Corre, Géographie-Cités, 2018")

setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
ggsave("myGeneralFlows_Vendeurs.png",plot= myGeneralFlows_Vendeurs,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)


myGeneralFlows_Acquereurs<-myGeneralFlows%>%
  group_by(annee.x, acquereurs)%>%
  transmute(Vendeurs, percent = n/sum(n)*100)%>%
  filter (acquereurs!="Agriculteurs", acquereurs!="Biens_publics_et_HLM", acquereurs!="autres_inactifs", acquereurs!="Com_art_Chef_entreprises",  
          Vendeurs!="Agriculteurs", Vendeurs!="Biens_publics_et_HLM", Vendeurs!="autres_inactifs", Vendeurs!="Com_art_Chef_entreprises" )%>%
  ggplot(., aes(annee.x, percent,group= Vendeurs)) +
  geom_line(aes(color= Vendeurs))+
  scale_color_manual(values = specificCol )+
  # geom_text(aes(label=round(percent, digits = 0))) +
  scale_x_continuous(breaks = c(1996,1999, 2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  # theme_tmd() +
  # scale_fill_gradient2(low = "blue", high = "red", mid = NULL, 
  #                      midpoint = FALSE, limit = c(0,50), space = "Lab",
  #                      name= "" )+
  facet_wrap(~acquereurs,scale="free")+
  labs(title = "Pourcentage des logements achetés selon le profil des vendeurs", x= NULL, y= NULL)+ 
  labs(subtitle = "Lecture : En 2003, 15 % des logements achetés par des Employés ont été vendus par des CPIS",
       x= NULL, y= NULL)+
  labs(caption = "Sources : échantillon BD BIEN sur effectifs redressés ; Réalisation : Thibault Le Corre, Géographie-Cités, 2018")

setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
ggsave("myGeneralFlows_Acquereurs.png",plot= myGeneralFlows_Acquereurs,  device = "png", width = 310, height = 200, units = "mm", dpi = 330)
setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
ggsave("myGeneralFlows_Acquereurs.pdf",plot= myGeneralFlows_Acquereurs,  device = "pdf", width = 310, height = 200, units = "mm", dpi = 330)



library(igraph)
# library(tnet)          
# library(networkD3)
library(flows)
library(reshape2)

myCouple<-mytest %>% 
  group_by(acquereurs,Vendeurs)%>% 
  add_count(Couple_Acquereur_Vendeur)%>%
  filter (acquereurs!="Agriculteurs", acquereurs!="Biens_publics_et_HLM", acquereurs!="autres_inactifs",  
          Vendeurs!="Agriculteurs", Vendeurs!="Biens_publics_et_HLM", Vendeurs!="autres_inactifs" )%>%
  select(acquereurs,Vendeurs, n)

myGeneralFlows<- myCouple[!duplicated(myCouple), ]
NVentes<-sum(myGeneralFlows$n)
#################################################################

myFlows_Acq<-prepflows(mat = myGeneralFlows, i = "acquereurs", j = "Vendeurs", fij = "n")

#Sur l'ensemble des achats on retient les groupes qui ont vendu au moins  10 % de ces acahts
flowSel1_Acq <- firstflows(mat = myFlows_Acq/rowSums(myFlows_Acq)*100, method = "xfirst", 
                       k = 10)

flowSel_Acq <- myFlows_Acq * flowSel1_Acq


myGeneralFlows_Acq<-melt(flowSel_Acq,varnames = c("acquereurs","Vendeurs"))

myGeneralFlows_Acq<-myGeneralFlows_Acq%>%filter(value>100)
myGeneralFlows_Acq$Rapport_Vente<-myGeneralFlows_Acq$value/NVentes*100
myGeneralFlows_Acq<-myGeneralFlows_Acq%>%
  group_by(acquereurs)%>%
  mutate(Tot_achats_Cat= sum(value))

Tot_Cat_Acq<- myGeneralFlows_Acq%>%
  select(acquereurs, Tot_achats_Cat)%>%
  distinct()
sum(myGeneralFlows_Acq$Rapport_Vente)

graph_df<- myGeneralFlows_Acq[,c("acquereurs","Vendeurs","value")]
g1 <- graph_from_data_frame(graph_df, directed = TRUE)
E(g1)$weight <- E(g1)$value

# Set node size based on audience size:
V(g1)$Tot_achat <- Tot_Cat_Acq$Tot_achats_Cat

#Le marché de l'achat est représenté à 77.82% en dehors des catégoires soustraites
dev.off()
setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
pdf(file="Network_Qui_achete_a_qui.pdf",width = 30, height = 15)

plot(g1, layout = layout_in_circle, edge.width= (E(g1)$weight)/10000 ,arrow.size = 0.2, arrow.width = 0.2, vertex.size= sqrt(V(g1)$Tot_achat)/20)
dev.off()
# tkplot(g1, canvas.width = 450, canvas.height = 450)


setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
write.csv2(x = myGeneralFlows_Acq, "myGeneralFlows_Acq.csv")
write.csv2(x = Tot_Cat_Acq, "Tot_Cat_Acq.csv")
#########################################################################
myFlows_Vend<-prepflows(mat = myGeneralFlows, i = "Vendeurs", j = "acquereurs", fij = "n")

#Sur l'ensemble des ventes on retient les groupes qui ont acheté au moins  10 % de ces ventes
flowSel1_Vend <- firstflows(mat = myFlows_Vend/rowSums(myFlows_Vend)*100, method = "xfirst", 
                           k = 10)

flowSel_Vend <- myFlows_Vend * flowSel1_Vend


myGeneralFlows_Vend<-melt(flowSel_Vend,varnames = c("Vendeurs","acquereurs"))

myGeneralFlows_Vend<-myGeneralFlows_Vend%>%filter(value>100)
myGeneralFlows_Vend$Rapport_Vente<-myGeneralFlows_Vend$value/NVentes*100

myGeneralFlows_Vend<-myGeneralFlows_Vend%>%
  group_by(Vendeurs)%>%
  mutate(Tot_Vend_Cat= sum(value))
Tot_Cat_Ve<- myGeneralFlows_Vend%>%
  select(Vendeurs, Tot_Vend_Cat)%>%
  distinct()

sum(myGeneralFlows_Vend$Rapport_Vente)
#Le marché de la vente est représenté à 76.7%
graph_df<- myGeneralFlows_Vend[,c("Vendeurs","acquereurs","value")]
g1 <- graph_from_data_frame(graph_df, directed = TRUE)
E(g1)$weight <- E(g1)$value

V(g1)$Tot_Vend <- Tot_Cat_Ve$Tot_Vend_Cat

dev.off()
setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
pdf(file="Network_Qui_Vend_a_qui.pdf",width = 30, height = 15)
plot(g1, layout = layout_in_circle, edge.width= (E(g1)$weight)/10000 ,arrow.size = 0.2, arrow.width = 0.2, vertex.size= sqrt(V(g1)$Tot_Vend)/20)
dev.off()


setwd("~/Projets/Chapitre7/Evolution_generale_marche_acquereurs")
write.csv2(x = myGeneralFlows_Vend, "myGeneralFlows_Vend.csv")
write.csv2(x = Tot_Cat_Ve, "Tot_Cat_Ve.csv")




