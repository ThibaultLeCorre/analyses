SpUnitt_for_seq<- SpUnitt_with_cluster[,c(1,2,16)]
SpUnitt_for_seq<-SpUnitt_for_seq%>%
  group_by(DepCom) %>% 
  spread(annee.x,`cah$data.clust$clust`) %>%
  drop_na(.)





#### Encoding Class
# CAGRcol <- grep("[Vv]", names(UMZ), value = TRUE)
# 
# labelClass <- c("Decay", "Growth", "StrongGrowth")
# med<- median(UMZ)
# 
# for (i in CAGRcol) {
#   data <- UMZ[[i]] 
#   min<-min(data)
#   max<- max(data)
#   
#   ### Setting breaks
#   valBreaks <- c(min,0,med,max)
#   
#   UMZ[paste(i, "_Class",sep="")] <- cut(data,
#                                         breaks = valBreaks,
#                                         labels = labelClass,
#                                         include.lowest = TRUE,
#                                         right= FALSE   )
# }
# 
# # str(UMZ)


##### 2.3 / Optimal Matching


#####2.3.1 / sequences caracterisation

##partition du tableau par classe à faire

library(TraMineR)
library(RColorBrewer)
# 2,"#91BFDB" 4, "#FEE090" 6,"#D73027"
# color <- c("#4575B4" ,"#E0F3F8" ,"#FC8D59")
#color <- c("lightblue", "orange", "red")
Com.seq <- seqdef(SpUnitt_for_seq, var = 2:12) 

Com.seq <- seqrecode(Com.seq, recodes = list("Très Aisée renforcées par le marché"=1, "Aisée_MarchéProfilClassMoyInf_SurEmployés" = 2,
                                             "MixAisé_MarchéActif_Stabilité"= 3, "Mix_PeuActifversPopulaireOuvrier"= 4, 
                                             "Mix_forteStabilité"=5, "Populaire_Moyennisation"= 6, 
                                             "TrèsPopulaire_stabilité"= 7, "TrèsPopulaire_Gentrification"=8))

Com.seq <- seqrecode(Com.seq, recodes = list("Très Aisée renforcées par le marché"=1,"Mix" = c(2,3,4,5),
                                             "PopMoyennisationSansCadre_ComPop"= c(6,7),  "Populaire_Gentrification"=8))

Com.seq <- seqrecode(Com.seq, recodes = list("Très Aisée renforcées par le marché"=1,"Mix_MarchéAiséMoyen" = c(4,3),
                                             "Mix"= c(2,5), 
                                             "PopMoyennisation"= c(6,7), "Populaire_Gentrification"=8))

Com.seq <- seqrecode(Com.seq, recodes = list("SRTrèsAisée renforcées par le marché"=1,
                                             "Mix_MarchéMix" = c(4,3),
                                             "SRMixAisé_MarcheInterPopu"= c(5,2), 
                                             "SRPop_MarcheInterPopu"= 6, 
                                             "SRPop_MarcheInter"= 7, 
                                             "SRTrèsPop_Gentrification"=8))


Com.seq <- seqrecode(Com.seq, recodes = list("SRTrèsAisée_Renforcémarché"=1,
                                             "SRAisée_Maintientmarché"=2,
                                             "SRAisée_Mixmarché"=c(3,4),
                                             "SRMix_MarchéAisé" = 5,
                                             "SRMix_MarchéPopu" = 6,
                                             "SRPop_MarchePopu"= 7,
                                             "SRPop_MarcheInter"= 8,  
                                             "SRTrèsPop_MarcheInterPop"= 9, 
                                             "SRTrèsPop_MarcheInterCPIS"=10))


Com.seq <- seqrecode(Com.seq, recodes = list("Gentrification"=1,
                                             "SRAisée_Maintientmarché"=2,
                                             "CommPopuMoyennisation" = 3,
                                             "CommMixMarchéPopu" = 4,
                                             "CommMixMarchéTrèsPopu"= 5))


Com.seq <- seqrecode(Com.seq, recodes = list("SRTrèsAisée_Renforcémarché"=1,
                                             "Aisée_MarchéProfInter_Employés"=c(2,3),
                                             "Mix_MarcheCPIS" = 4,
                                             "Pop_MarchéPopu" = 5,
                                             "Pop_MarchéProfInter_Ouvrier"= 6,
                                             "TrèsPop_MarchéPop"= 7))



Com.seq <- seqrecode(Com.seq, recodes = list("MarchéAisé"=1,
                                             "ExclusionPop"=2,
                                             "Moyennisation" = 3,
                                             "MixPopuEmploye" = 4,
                                             "Popu"= 5))

Com.seq <- seqrecode(Com.seq, recodes = list("Marchés de catégories supérieures dans les espaces privilégiés"=1,
"Processus de changement social par le haut. Marchés de catégories supérieures dans des espaces très populaires"=2,
"Moyennisation par le marché dans des espaces mixtes"= 3,
"Marchés mixtes avec surreprésentation des Employés dans des espaces populaires"= 4,
"Marchés populaires avec surreprésentation des Ouvriers dans des espaces populaires"= 5))



# Com.seq <- seqrecode(Com.seq, recodes = list("SRTrèsAisée_Renforcémarché"=1,
#                                              "Populaire_avec_Gentrification"=2,
#                                              "MixMoyennisation" = c(3, 4,
#                                              "PopuMarchéStablePopuOuvrier"= 5))

seqstatl(Com.seq)
seqdim(Com.seq)
# colnames(UMZ.seq) <- c("1996-1999","1999-2003","2003-2004","2004-2005","2005-2006", "2006-2007","2007-2008","2008-2009","2009-2010","2010-2011","2011-2012")
par(mfrow = c(2, 2))

# seqiplot(UMZ.seq, title = "Index plot (first 10 sequences)",
#          withlegend = F)
seqHtplot(Com.seq,  withlegend=F, title = "Entropy")
# seqmsplot(UMZ.seq,  withlegend=F, title="Modal Profile")
seqdplot(Com.seq, title = "Distribution des états", withlegend = F)
seqfplot(Com.seq, title = "Fréquences des séquences", withlegend = F,
         pbarw = TRUE)
seqlegend(Com.seq,fontsize=0.7)
seqtrate(Com.seq)
seqIplot(Com.seq,  title = "Séquences des états", sortv="from.start", withlegend =F)
dev.off()
?seqIplot

########  CAH


library(cluster)

#Definition des couts

##(cout constant pour LevenshteinII et Trate pour Dynamic Hamming)



 # couts <- seqsubm(Com.seq,method="CONSTANT", cval=100)
couts
couts <- seqsubm(Com.seq,method="TRATE")



#matrice de distances_

##(changer Indel ou sm selon la distance utilisee : LevenshteinII ou Hamming)



seq.om <- seqdist(Com.seq, method="OM", indel=100,sm=couts)



# classification choix du nb de classes



seq.agnes <- agnes(as.dist(seq.om), method="ward", keep.diss=FALSE)

# seq.agnes2 <- agnes(as.dist(seq.om), method="single", keep.diss=FALSE)

# seq.agnes3 <- agnes(as.dist(seq.om), method="complete", keep.diss=FALSE)

#





#

par(mfrow = c(1, 1))

plot(as.dendrogram(seq.agnes), leaflab= "none")

plot(sort(seq.agnes$height, decreasing=TRUE)[1:20],
     
     type="s", xlab="nb de classes", ylab="inertie")







nbcl <- 5
seq.part <- stats::cutree(seq.agnes, nbcl)



seq.part <- factor(seq.part,labels=paste("Classe",1:nbcl,sep='.'))



# library(NbClust)

# NbClust(data = UMZ.seq,diss = distom, distance = NULL, method = "ward.D2",min.nc = 2, max.nc = 15,index = "all")

### State distribution plot





seqplot(Com.seq, group=seq.part, type="d" ,
        
        border=NA, withlegend=T)







### index plot



#ordre <- cmdscale(as.dist(seq.om),k=1)

#ordre <- sort.list

seqiplot(Com.seq, group=seq.part,sortv="from.start", title = "Index plot",
         
         tlim=0, space=0, border=NA, with.legend=T, yaxis=FALSE )



# distance moyenne des sequences au centre de la classe





meanDistClass <- round(aggregate(disscenter(as.dist(seq.om), group=seq.part),
                                 
                                 list(seq.part),mean)[,-1],1)



#sequence frequency plot



#seqfplot(UMZ.seq[seq.part != "classe.2", ], group=seq.part[seq.part != "classe.2"],

#        title = "Sequence frequency plot", withlegend=T)

#

seqfplot(Com.seq, group=seq.part,  with.legend=T, border="darkgrey")



str(seq.part)

levels(seq.part)

# etat modal de chaque classe



seqmsplot(Com.seq, group=seq.part, with.legend=T, main ="Profil Modal")



# entropie par classe



seqHtplot(Com.seq, group=seq.part, with.legend=T, main = "Entropie intraclasse")


SpUnitt_for_seq$CAHOM <- seq.part

setwd("~/Projets/Chapitre7/CAH_acquereur_residents")
write.csv2(x =SpUnitt_for_seq, "SpUnitt_for_seq.csv"  )


colNATiles <- '#FFFFFF'
opacity <- 10 # de 00 (presque transparent) Ã  99 (complÃ¨tement opaque) /!\ Que des nombres Ã  2 chiffres
colNATiles <- paste0(colNATiles, opacity)

Test_map<- left_join(CommunesInsee, SpUnitt_for_seq, by = "DepCom")


# annee.x_96_2003# annee.x_04_2007
# osmTiles <- getTiles(x = Spvente, type = "stamenbw",  crop = FALSE)


tilesLayer(osmTiles)
layoutLayer()
typoLayer(x=Test_map,
          var = "CAHOM",
          col= cols, ##palettes de couleurs predefinies## 
          border = NA,
          legend.values.order = c("Classe.1",
                                  "Classe.2", 
                                  "Classe.3",
                                  "Classe.4",
                                  "Classe.5"),
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          lwd = 0.2,
          colNA=colNATiles,add=TRUE)
plot(lim_IDF.st$geometry,add=T)



SoldeResid_Var_supp<- test_residents_solde%>%
  group_by(CODGEO, variables)%>%
  summarise(taux_var = ((`2012` - `1999`) / `1999`)*100 )%>%
  spread(variables, taux_var)


SoldeResid_Var_supp<-left_join(SpUnitt_for_seq[,c("DepCom", "CAHOM")], SoldeResid_Var_supp, by = c("DepCom"="CODGEO"))



SoldeResid_Var_supp %>%
  group_by(CAHOM) %>%
  # summarise_all(funs(mean),na.rm=T) %>%
  summarise_at(.vars = vars(CPIS:Prof_intermediaires), .funs = funs(mean(.)))%>%
  gather(key = "Dimension", value = "Data",2:5) %>%
  ggplot() +
  geom_bar(aes(Dimension, Data, fill = CAHOM), stat = "identity") +
  ggtitle("") +
  labs(subtitle = "") +
  xlab("") +
  ylab("") +
  theme(legend.position='none') +
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2018")+
  facet_wrap(~CAHOM ) +
  coord_flip()




Marche_Residents2<- left_join(Marche_Residents, SpUnitt_for_seq,by= "DepCom")


library(devtools)
install_github("eringrand/RUncommon")
source_gist("524eade46135f6348140")

Marche_Residents2%>%
  ggplot(.,aes(x = Value_acquereurs, y = Value_Residents, label=Value_Residents, group=CAHOM)) +
  geom_jitter(aes(Value_acquereurs,Value_Residents, colour=CSP_acquereurs),) + 
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(aes(Value_acquereurs,Value_Residents, colour=CSP_acquereurs), method=lm, se=FALSE, formula = y ~ x) +
  facet_grid(CAHOM~CSP_acquereurs, scales="free") +
  # geom_text(aes(x = 25, y = 300, label = lm_eqn(lm(SoldeTotal.y ~ SoldeTotal.x, Solde_Marche_Residents))), parse = TRUE)+
  labs(x = "Solde du marché", y = "Solde Résidentiel")
