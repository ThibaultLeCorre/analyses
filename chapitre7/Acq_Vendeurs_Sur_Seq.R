# data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","QUALITE_AC","QUALITE_VE","CSP_AC","CSP_VE","insee")]
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



mytest<-left_join(mytest,SpUnitt_for_seq[,c("DepCom","CAHOM")], by= c("insee"="DepCom"))

myCouple<-mytest %>% 
  filter(!is.na(CAHOM)) %>% 
  group_by(CAHOM, acquereurs,Vendeurs)%>% 
  add_count(Couple_Acquereur_Vendeur)

library(igraph)
# library(tnet)          
# library(networkD3)
library(flows)
library(reshape2)

myFirstClassTrajec<-myCouple %>%
  filter(CAHOM=="Classe.1", Vendeurs!="retraites") %>%
  filter (acquereurs!="Agriculteurs", acquereurs!="Biens_publics_et_HLM", acquereurs!="autres_inactifs",acquereurs!="Entreprise_Marchands_SCI",  acquereurs!="Com_art_Chef_entreprises", acquereurs!="retraites", 
          Vendeurs!="Agriculteurs", Vendeurs!="Biens_publics_et_HLM", Vendeurs!="autres_inactifs",Vendeurs!="Entreprise_Marchands_SCI", Vendeurs!="Com_art_Chef_entreprises" )%>%
  select(acquereurs,Vendeurs, n)


myFirstClassTrajec<- myFirstClassTrajec[!duplicated(myFirstClassTrajec), ]
NVentes<-sum(myFirstClassTrajec$n)

myFlows_Class1<-prepflows(mat = myFirstClassTrajec, i = "acquereurs", j = "Vendeurs", fij = "n")
myFlows_Class1Bis <- firstflows(mat = myFlows_Class1/rowSums(myFlows_Class1)*100, method = "xfirst", 
                           k = 10)

statmat(mat = myFlows_Class1, output = "all", verbose = FALSE)

flowSel <- myFlows_Class1 * myFlows_Class1Bis


myGeneralFlows_Class1<-melt(flowSel,varnames = c("acquereurs","Vendeurs"))

myGeneralFlows_Class1<-myGeneralFlows_Class1%>%filter(value>100)
myGeneralFlows_Class1$Rapport_Vente<-myGeneralFlows_Class1$value/NVentes*100
myGeneralFlows_Class1<-myGeneralFlows_Class1%>%
  group_by(acquereurs)%>%
  mutate(Tot_achats_Cat= sum(value))

Tot_Class1<- myGeneralFlows_Class1%>%
  select(acquereurs, Tot_achats_Cat)%>%
  distinct()
sum(myGeneralFlows_Class1$Rapport_Vente)

setwd("~/Projets/Chapitre7/CSP_ACQ_Regimes_Marches")
write.csv2(x = myGeneralFlows_Class1, "myGeneralFlows_Class1.csv")
write.csv2(x = Tot_Class1, "Tot_Class1.csv")

graph_df<- myGeneralFlows_Class1[,c("acquereurs","Vendeurs","value")]
g1 <- graph_from_data_frame(graph_df, directed = TRUE)
E(g1)$weight <- E(g1)$value


V(g1)$Tot_achat <- Tot_Class1$Tot_achats_Cat


plot(g1, layout = layout_in_circle, edge.width= (E(g1)$weight)/1000 ,arrow.size = 0.2, arrow.width = 0.2, vertex.size= sqrt(V(g1)$Tot_achat)/20)
dev.off()



###############################################################################################################



mySecondClassTrajec<-myCouple %>%
  filter(CAHOM=="Classe.2", Vendeurs!="retraites") %>%
  filter (acquereurs!="Agriculteurs", acquereurs!="Biens_publics_et_HLM", acquereurs!="autres_inactifs",acquereurs!="Entreprise_Marchands_SCI",  acquereurs!="Com_art_Chef_entreprises", acquereurs!="retraites", 
          Vendeurs!="Agriculteurs", Vendeurs!="Biens_publics_et_HLM", Vendeurs!="autres_inactifs",Vendeurs!="Entreprise_Marchands_SCI", Vendeurs!="Com_art_Chef_entreprises" )%>%
  select(acquereurs,Vendeurs, n)


mySecondClassTrajec<- mySecondClassTrajec[!duplicated(mySecondClassTrajec), ]
NVentes<-sum(mySecondClassTrajec$n)

myFlows_Class2<-prepflows(mat = mySecondClassTrajec, i = "acquereurs", j = "Vendeurs", fij = "n")
myFlows_Class2Bis <- firstflows(mat = myFlows_Class2/rowSums(myFlows_Class2)*100, method = "xfirst", 
                                k = 10)

statmat(mat = myFlows_Class2, output = "all", verbose = FALSE)

flowSel <- myFlows_Class2 * myFlows_Class2Bis


myGeneralFlows_Class2<-melt(flowSel,varnames = c("acquereurs","Vendeurs"))

myGeneralFlows_Class2<-myGeneralFlows_Class2%>%filter(value>100)
myGeneralFlows_Class2$Rapport_Vente<-myGeneralFlows_Class2$value/NVentes*100
myGeneralFlows_Class2<-myGeneralFlows_Class2%>%
  group_by(acquereurs)%>%
  mutate(Tot_achats_Cat= sum(value))

Tot_Class2<- myGeneralFlows_Class2%>%
  select(acquereurs, Tot_achats_Cat)%>%
  distinct()
sum(myGeneralFlows_Class2$Rapport_Vente)


setwd("~/Projets/Chapitre7/CSP_ACQ_Regimes_Marches")
write.csv2(x = myGeneralFlows_Class2, "myGeneralFlows_Class2.csv")
write.csv2(x = Tot_Class2, "Tot_Class2.csv")

graph_df<- myGeneralFlows_Class2[,c("acquereurs","Vendeurs","value")]
g1 <- graph_from_data_frame(graph_df, directed = TRUE)
E(g1)$weight <- E(g1)$value
V(g1)$Tot_achat <- Tot_Class2$Tot_achats_Cat

plot(g1, layout = layout_in_circle, edge.width= (E(g1)$weight)/1000 ,arrow.size = 0.2, arrow.width = 0.2, vertex.size= sqrt(V(g1)$Tot_achat)/20)
dev.off()




###############################################################################################################



myThirdClassTrajec<-myCouple %>%
  filter(CAHOM=="Classe.3", Vendeurs!="retraites") %>%
  filter (acquereurs!="Agriculteurs", acquereurs!="Biens_publics_et_HLM", acquereurs!="autres_inactifs",acquereurs!="Entreprise_Marchands_SCI",  acquereurs!="Com_art_Chef_entreprises", acquereurs!="retraites", 
          Vendeurs!="Agriculteurs", Vendeurs!="Biens_publics_et_HLM", Vendeurs!="autres_inactifs",Vendeurs!="Entreprise_Marchands_SCI", Vendeurs!="Com_art_Chef_entreprises" )%>%
  select(acquereurs,Vendeurs, n)

myThirdClassTrajec<- myThirdClassTrajec[!duplicated(myThirdClassTrajec), ]
NVentes<-sum(myThirdClassTrajec$n)

myFlows_Class3<-prepflows(mat = myThirdClassTrajec, i = "acquereurs", j = "Vendeurs", fij = "n")
myFlows_Class3Bis <- firstflows(mat = myFlows_Class3/rowSums(myFlows_Class3)*100, method = "xfirst", 
                                k = 10)

statmat(mat = myFlows_Class3, output = "all", verbose = FALSE)

flowSel <- myFlows_Class3 * myFlows_Class3Bis


myGeneralFlows_Class3<-melt(flowSel,varnames = c("acquereurs","Vendeurs"))

myGeneralFlows_Class3<-myGeneralFlows_Class3%>%filter(value>100)
myGeneralFlows_Class3$Rapport_Vente<-myGeneralFlows_Class3$value/NVentes*100
myGeneralFlows_Class3<-myGeneralFlows_Class3%>%
  group_by(acquereurs)%>%
  mutate(Tot_achats_Cat= sum(value))

Tot_Class3<- myGeneralFlows_Class3%>%
  select(acquereurs, Tot_achats_Cat)%>%
  distinct()
sum(myGeneralFlows_Class3$Rapport_Vente)

setwd("~/Projets/Chapitre7/CSP_ACQ_Regimes_Marches")
write.csv2(x = myGeneralFlows_Class3, "myGeneralFlows_Class3.csv")
write.csv2(x = Tot_Class3, "Tot_Class3.csv")

graph_df<- myGeneralFlows_Class3[,c("acquereurs","Vendeurs","value")]
g1 <- graph_from_data_frame(graph_df, directed = TRUE)
E(g1)$weight <- E(g1)$value
V(g1)$Tot_achat <- Tot_Class3$Tot_achats_Cat

plot(g1, layout = layout_in_circle, edge.width= (E(g1)$weight)/1000 ,arrow.size = 0.2, arrow.width = 0.2, vertex.size= sqrt(V(g1)$Tot_achat)/20)

##############################################################################################################################################




myFourthClassTrajec<-myCouple %>%
  filter(CAHOM=="Classe.4", Vendeurs!="retraites") %>%
  filter (acquereurs!="Agriculteurs", acquereurs!="Biens_publics_et_HLM", acquereurs!="autres_inactifs",acquereurs!="Entreprise_Marchands_SCI",  acquereurs!="Com_art_Chef_entreprises", acquereurs!="retraites", 
          Vendeurs!="Agriculteurs", Vendeurs!="Biens_publics_et_HLM", Vendeurs!="autres_inactifs",Vendeurs!="Entreprise_Marchands_SCI", Vendeurs!="Com_art_Chef_entreprises" )%>%
  select(acquereurs,Vendeurs, n)
myFourthClassTrajec<- myFourthClassTrajec[!duplicated(myFourthClassTrajec), ]
NVentes<-sum(myFourthClassTrajec$n)

myFlows_Class4<-prepflows(mat = myFourthClassTrajec, i = "acquereurs", j = "Vendeurs", fij = "n")
myFlows_Class4Bis <- firstflows(mat = myFlows_Class4/rowSums(myFlows_Class4)*100, method = "xfirst", 
                                k = 10)

statmat(mat = myFlows_Class4, output = "all", verbose = FALSE)

flowSel <- myFlows_Class4 * myFlows_Class4Bis


myGeneralFlows_Class4<-melt(flowSel,varnames = c("acquereurs","Vendeurs"))

myGeneralFlows_Class4<-myGeneralFlows_Class4%>%filter(value>100)
myGeneralFlows_Class4$Rapport_Vente<-myGeneralFlows_Class4$value/NVentes*100
myGeneralFlows_Class4<-myGeneralFlows_Class4%>%
  group_by(acquereurs)%>%
  mutate(Tot_achats_Cat= sum(value))

Tot_Class4<- myGeneralFlows_Class4%>%
  select(acquereurs, Tot_achats_Cat)%>%
  distinct()
sum(myGeneralFlows_Class4$Rapport_Vente)

setwd("~/Projets/Chapitre7/CSP_ACQ_Regimes_Marches")
write.csv2(x = myGeneralFlows_Class4, "myGeneralFlows_Class4.csv")
write.csv2(x = Tot_Class4, "Tot_Class4.csv")

graph_df<- myGeneralFlows_Class4[,c("acquereurs","Vendeurs","value")]
g1 <- graph_from_data_frame(graph_df, directed = TRUE)
E(g1)$weight <- E(g1)$value
V(g1)$Tot_achat <- Tot_Class4$Tot_achats_Cat

plot(g1, layout = layout_in_circle, edge.width= (E(g1)$weight)/1000 ,arrow.size = 0.2, arrow.width = 0.2, vertex.size= sqrt(V(g1)$Tot_achat)/20)


##############################################################################################################################################




myFifthClassTrajec<-myCouple %>%
  filter(CAHOM=="Classe.5", Vendeurs!="retraites") %>%
  filter (acquereurs!="Agriculteurs", acquereurs!="Biens_publics_et_HLM", acquereurs!="autres_inactifs",acquereurs!="Entreprise_Marchands_SCI",  acquereurs!="Com_art_Chef_entreprises", acquereurs!="retraites", 
          Vendeurs!="Agriculteurs", Vendeurs!="Biens_publics_et_HLM", Vendeurs!="autres_inactifs",Vendeurs!="Entreprise_Marchands_SCI", Vendeurs!="Com_art_Chef_entreprises" )%>%
  select(acquereurs,Vendeurs, n)

myFifthClassTrajec<- myFifthClassTrajec[!duplicated(myFifthClassTrajec), ]
NVentes<-sum(myFifthClassTrajec$n)

myFlows_Class5<-prepflows(mat = myFifthClassTrajec, i = "acquereurs", j = "Vendeurs", fij = "n")
myFlows_Class5Bis <- firstflows(mat = myFlows_Class5/rowSums(myFlows_Class5)*100, method = "xfirst", 
                                k = 10)

statmat(mat = myFlows_Class5, output = "all", verbose = FALSE)

flowSel <- myFlows_Class5 * myFlows_Class5Bis


myGeneralFlows_Class5<-melt(flowSel,varnames = c("acquereurs","Vendeurs"))

myGeneralFlows_Class5<-myGeneralFlows_Class5%>%filter(value>100)
myGeneralFlows_Class5$Rapport_Vente<-myGeneralFlows_Class5$value/NVentes*100
myGeneralFlows_Class5<-myGeneralFlows_Class5%>%
  group_by(acquereurs)%>%
  mutate(Tot_achats_Cat= sum(value))

Tot_Class5<- myGeneralFlows_Class5%>%
  select(acquereurs, Tot_achats_Cat)%>%
  distinct()
sum(myGeneralFlows_Class5$Rapport_Vente)

setwd("~/Projets/Chapitre7/CSP_ACQ_Regimes_Marches")
write.csv2(x = myGeneralFlows_Class5, "myGeneralFlows_Class5.csv")
write.csv2(x = Tot_Class5, "Tot_Class5.csv")


graph_df<- myGeneralFlows_Class5[,c("acquereurs","Vendeurs","value")]
g1 <- graph_from_data_frame(graph_df, directed = TRUE)
E(g1)$weight <- E(g1)$value
V(g1)$Tot_achat <- Tot_Class5$Tot_achats_Cat

plot(g1, layout = layout_in_circle, edge.width= (E(g1)$weight)/1000 ,arrow.size = 0.2, arrow.width = 0.2, vertex.size= sqrt(V(g1)$Tot_achat)/20)
