library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)
library(stringr)   
library(tidyverse)
library(TraMineR)
library(RColorBrewer)
##### Loading data

setwd("~/")
list.files()

# Prix_annee_potentiel_200_appartement<-as.data.frame(Montableau_Appartement_200_all)
# 
# Prix_annee_potentiel_200_appartement<- Prix_annee_potentiel_200_appartement%>%
#   select(Carreau_ID,prixPotentiel,Annee)%>%
#   group_by(Carreau_ID)%>%
#   filter(!is.na(prixPotentiel))%>%
#   spread(Annee,prixPotentiel)%>%
#   filter_all(all_vars(!is.na(.)))
# Prix_annee_potentiel_200_appartement<-as.data.frame(Prix_annee_potentiel_200_appartement)

UMZ <- Typo_1999_2012_carreaux200_appartement%>% filter(categorie_variation== "evolution_plus_fort_que_la _prevision")
UMZ<- UMZ[,c(1:13)]
unique(Typo_1999_2012_carreaux200_appartement$categorie_finale)


# Typo_1999_2012_carreaux200_appartement$categorie_finale<-ifelse(Typo_1999_2012_carreaux200_appartement$categorie_finale=="Prix initiaux faibles (mean - sd)evolution_conforme_au_modele_","Prix_initiaux_faibles_evolution_conforme_au_modele" ,
#        ifelse(Typo_1999_2012_carreaux200_appartement$categorie_finale=="Prix initiaux dans la moyenne (mean +/- sd)evolution_conforme_au_modele_","Prix_initiaux_dans_la_moyenne_evolution_conforme_au_modele" ,
#               ifelse(Typo_1999_2012_carreaux200_appartement$categorie_finale=="Prix initiaux dans la moyenne (mean +/- sd)evolution_plus_faible_que_la_prevision_","Prix initiaux_dans_la_moyenne_evolution_plus_faible_que_la_prevision" ,
#                      ifelse(Typo_1999_2012_carreaux200_appartement$categorie_finale=="Prix initiaux faibles (mean - sd)evolution_plus_fort_que_la _prevision_", "Prix_initiaux_faibles_evolution_plus_fort_que_la _prevision" ,
#                             ifelse(Typo_1999_2012_carreaux200_appartement$categorie_finale=="Prix initiaux dans la moyenne (mean +/- sd)evolution_plus_fort_que_la _prevision_","Pri_initiaux_dans_la_moyenne_evolution_plus_fort_que_la _prevision" ,
#                                    ifelse(Typo_1999_2012_carreaux200_appartement$categorie_finale=="Prix initiaux faibles (mean - sd)evolution_plus_faible_que_la_prevision_" ,"Prix_initiaux_faibles_evolution_plus_faible_que_la_prevision" ,
#                                           ifelse(Typo_1999_2012_carreaux200_appartement$categorie_finale=="Prix initiaux élevés (mean + sd)evolution_plus_faible_que_la_prevision_","Prix_initiaux_eleves_evolution_plus_faible_que_la_prevision_",
#                                                  ifelse(Typo_1999_2012_carreaux200_appartement$categorie_finale=="Prix initiaux élevés (mean + sd)evolution_conforme_au_modele_","Prix_initiaux_eleves_evolution_conforme_au_modele" ,
#                                                         ifelse(Typo_1999_2012_carreaux200_appartement$categorie_finale=="Prix initiaux élevés (mean + sd)evolution_plus_fort_que_la _prevision_","Prix_initiaux_eleves_evolution_plus_fort_que_la _prevision",NA)))))))))
# ##############################################################
#######  PART ONE : RECODING TRAJECTORIES SIGNATURES  ###############


MesCroissances <- unique(Typo_1999_2012_carreaux200_appartement$categorie_variation)

dev.off()
for (MaCroissanceLa in MesCroissances){
  UMZ<-Typo_1999_2012_carreaux200_appartement%>% 
    filter(categorie_variation==MaCroissanceLa)%>%
    select(c(1:13))


#### 1.1/ Compute Average Annual Growth Rate  (CAGR)



##### CAGR function

CAGR <- function(df, variables){
  CAGR <- vector()
  cagr <- list()
  for(i in 2:length(variables)){
    
    # vector def
    pop1 <- df[, variables[i-1]]
    pop2 <- df[, variables[i]]
    year1 <- as.integer(str_replace_all(variables[i-1], "[A-Z]", replacement = ""))
    year2 <- as.integer(str_replace_all(variables[i], "[A-Z]", replacement = ""))
    # compute n and CAGR
    n <- year2 - year1
    result <- ((pop2/pop1)^(1/n)-1)*100
    cagr[[length(cagr) + 1]] <- result 
  }
  
  return(cagr)
} 


####   Applying Function to df   (à trouver : nommage colonne automatique)
#on vire l'année 1996
dfCAGR <- CAGR(df = UMZ ,variables = colnames(UMZ)[3:13])
dfCAGR <- as.data.frame(do.call(cbind, dfCAGR))

UMZ <- cbind(UMZ,dfCAGR)

########### 1.2/ Defining demographic state 



##### Exploring CAGR distibution


CAGRvalues <- select(UMZ,contains("V"))
CAGRvalues <- UMZ[,14:23]
CAGRall <- stack(CAGRvalues)


CAGRmin <- min(CAGRall[,1])
CAGRmax <- max(CAGRall[,1])

# par(mfrow=c(1,1))
# hist(CAGRall[,1], breaks=400, freq=FALSE, 
#      xlim = c(CAGRmin,CAGRmax),main = "CAGR")

# summary(CAGRall[,1])
# sd(CAGRall[,1])
#estimation des TCAM totaux et categorisation
# S1 <- mean((CAGRall[,1])) - sd(CAGRall[,1])
# S2 <- mean((CAGRall[,1]))
# S3 <- mean((CAGRall[,1])) + sd(CAGRall[,1])
# S4 <- mean((CAGRall[,1])) + 2*sd(CAGRall[,1])



#### Encoding Class
CAGRcol <- grep("[Vv]", names(UMZ), value = TRUE)

labelClass <- c("Decay", "Growth", "StrongGrowth", "ExceptionalGrowth")

for (i in CAGRcol) {
  data <- UMZ[[i]] 
  min<-min(data)
  max<- max(data)
  ### Setting breaks
  valBreaks <- c(min,0,6.608,14.520,max)
  
  UMZ[paste(i, "_Class",sep="")] <- cut(data,
                                        breaks = valBreaks,
                                        labels = labelClass,
                                        include.lowest = TRUE,
                                        right= FALSE   )
}

# str(UMZ)


##### 2.3 / Optimal Matching


#####2.3.1 / sequences caracterisation

##partition du tableau par classe à faire


# 2,"#91BFDB" 4, "#FEE090" 6,"#D73027"
# color <- c("#4575B4" ,"#E0F3F8" ,"#FC8D59")
#color <- c("lightblue", "orange", "red")
UMZ.seq <- seqdef(UMZ, var = 24:33) 


seqstatl(UMZ.seq)
seqdim(UMZ.seq)
colnames(UMZ.seq) <- c("1999-2003","2003-2004","2004-2005","2005-2006", "2006-2007","2007-2008","2008-2009","2009-2010","2010-2011","2011-2012")
par(mfrow = c(2, 2))

# seqiplot(UMZ.seq, title = "Index plot (first 10 sequences)",
#          withlegend = F)
setwd("~/Sauvegarde_figures/sequences_longi_croissances")
pdf(file=paste0(~"Sauvegarde_figures/sequences_longi_croissances",MaCroissanceLa,".pdf"),width = 10, height = 5)

seqHtplot(UMZ.seq,  withlegend=F, title = "Entropy")
# seqmsplot(UMZ.seq,  withlegend=F, title="Modal Profile")
seqdplot(UMZ.seq, title = "State distribution plot", withlegend = FALSE)
seqfplot(UMZ.seq, title = "Sequence frequency plot", withlegend = FALSE,
         pbarw = TRUE)
seqlegend(UMZ.seq,fontsize=1)
dev.off()
} 







# Decription des classes avec la frequence en SPS
#donne le nomre des unités spatiales sur sequence frequency plot
# seqtab(UMZ.seq, tlim = 1:10, format="SPS")

# Transition rate
#sur l'ensemble dew transitions, donne le pourcentage de trnasition  d'un etat à un autre état
# TransitionRate <- seqtrate(UMZ.seq)
# TransitionRate
MesCategoFinales <- unique(Typo_1999_2012_carreaux200_appartement$categorie_finale)
dev.off()
for (MaCategorFinaleLa in MesCategoFinales){
  UMZ<-Typo_1999_2012_carreaux200_appartement%>% 
    filter(categorie_finale==MaCategorFinaleLa)%>%
    select(c(1:13))
  
  
  #### 1.1/ Compute Average Annual Growth Rate  (CAGR)
  
  
  
  ##### CAGR function
  
  CAGR <- function(df, variables){
    CAGR <- vector()
    cagr <- list()
    for(i in 2:length(variables)){
      
      # vector def
      pop1 <- df[, variables[i-1]]
      pop2 <- df[, variables[i]]
      year1 <- as.integer(str_replace_all(variables[i-1], "[A-Z]", replacement = ""))
      year2 <- as.integer(str_replace_all(variables[i], "[A-Z]", replacement = ""))
      # compute n and CAGR
      n <- year2 - year1
      result <- ((pop2/pop1)^(1/n)-1)*100
      cagr[[length(cagr) + 1]] <- result 
    }
    
    return(cagr)
  } 
  
  
  ####   Applying Function to df   (à trouver : nommage colonne automatique)
  #on vire l'année 1996
  dfCAGR <- CAGR(df = UMZ ,variables = colnames(UMZ)[3:13])
  dfCAGR <- as.data.frame(do.call(cbind, dfCAGR))
  
  UMZ <- cbind(UMZ,dfCAGR)
  
  ########### 1.2/ Defining demographic state 
  
  
  
  ##### Exploring CAGR distibution
  
  
  CAGRvalues <- select(UMZ,contains("V"))
  CAGRvalues <- UMZ[,14:23]
  CAGRall <- stack(CAGRvalues)
  
  
  CAGRmin <- min(CAGRall[,1])
  CAGRmax <- max(CAGRall[,1])
  
  # par(mfrow=c(1,1))
  # hist(CAGRall[,1], breaks=400, freq=FALSE, 
  #      xlim = c(CAGRmin,CAGRmax),main = "CAGR")
  
  # summary(CAGRall[,1])
  # sd(CAGRall[,1])
  #estimation des TCAM totaux et categorisation
  # S1 <- mean((CAGRall[,1])) - sd(CAGRall[,1])
  # S2 <- mean((CAGRall[,1]))
  # S3 <- mean((CAGRall[,1])) + sd(CAGRall[,1])
  # S4 <- mean((CAGRall[,1])) + 2*sd(CAGRall[,1])
  
  
  
  #### Encoding Class
  CAGRcol <- grep("[Vv]", names(UMZ), value = TRUE)
  
  labelClass <- c("Decay", "Growth", "ExceptionalGrowth")
  
  for (i in CAGRcol) {
    data <- UMZ[[i]] 
    min<-min(data)
    max<- max(data)
    ### Setting breaks
    valBreaks <- c(min,0,14.520,max)
    
    UMZ[paste(i, "_Class",sep="")] <- cut(data,
                                          breaks = valBreaks,
                                          labels = labelClass,
                                          include.lowest = TRUE,
                                          right= FALSE   )
  }
  
  # str(UMZ)
  
  
  ##### 2.3 / Optimal Matching
  
  
  #####2.3.1 / sequences caracterisation
  
  ##partition du tableau par classe à faire
  
  
  # 2,"#91BFDB" 4, "#FEE090" 6,"#D73027"
  # color <- c("#4575B4" ,"#E0F3F8" ,"#FC8D59")
  #color <- c("lightblue", "orange", "red")
  UMZ.seq <- seqdef(UMZ, var = 24:33) 
  
  
  seqstatl(UMZ.seq)
  seqdim(UMZ.seq)
  colnames(UMZ.seq) <- c("1999-2003","2003-2004","2004-2005","2005-2006", "2006-2007","2007-2008","2008-2009","2009-2010","2010-2011","2011-2012")
  par(mfrow = c(2, 2))
  
  # seqiplot(UMZ.seq, title = "Index plot (first 10 sequences)",
  #          withlegend = F)
  setwd("~/Sauvegarde_figures/sequences_longi_croissances")
  pdf(file=paste0(~"Sauvegarde_figures/sequences_longi_croissances",MaCategorFinaleLa,".pdf"),width = 10, height = 5)
  
  seqHtplot(UMZ.seq,  withlegend=F, title = "Entropy")
  # seqmsplot(UMZ.seq,  withlegend=F, title="Modal Profile")
  seqdplot(UMZ.seq, title = "State distribution plot", withlegend = FALSE)
  seqfplot(UMZ.seq, title = "Sequence frequency plot", withlegend = FALSE,
           pbarw = TRUE)
  seqlegend(UMZ.seq,fontsize=1)
  dev.off()
  dev.off()
  
  getFlows <- GetCrossFlows(df = UMZ[,c(24:33)])
  setwd("~/Sauvegarde_figures/sequences_longi_slideplot")
  pdf(file=paste0(~"Sauvegarde_figures/sequences_longi_slideplot",MaCategorFinaleLa,".pdf"),width = 10, height = 5)
  # source("function_slideplot.R")
  
  # df= colonne temporelles
 
  SlidePlot(listFlows = getFlows, threshold = 10,mask = FALSE,thickmin =0.1,showfreq = FALSE )
  dev.off()
  
} 



