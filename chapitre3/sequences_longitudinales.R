library(sf)
library(SpatialPosition)
library(cartography)
library(dplyr)
library(tidyr)


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
unique(Typo_1999_2012_carreaux200_appartement$categorie_variation)



##############################################################
#######  PART ONE : RECODING TRAJECTORIES SIGNATURES  ###############


#### 1.1/ Compute Average Annual Growth Rate  (CAGR)



##### CAGR function
library(stringr)   
library(tidyverse)


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



dfCAGR <- CAGR(df = UMZ ,variables = colnames(UMZ)[2:13])
dfCAGR <- as.data.frame(do.call(cbind, dfCAGR))

UMZ <- cbind(UMZ,dfCAGR)



########### 1.2/ Defining demographic state 



##### Exploring CAGR distibution


CAGRvalues <- select(UMZ,contains("V"))
CAGRvalues <- UMZ[,14:24]
CAGRall <- stack(CAGRvalues)


CAGRmin <- min(CAGRall[,1])
CAGRmax <- max(CAGRall[,1])

par(mfrow=c(1,1))
hist(CAGRall[,1], breaks=400, freq=FALSE, 
     xlim = c(CAGRmin,CAGRmax),main = "CAGR")

summary(CAGRall[,1])
sd(CAGRall[,1])
#estimation des TCAM totaux et categorisation
S1 <- mean((CAGRall[,1])) - sd(CAGRall[,1])
S2 <- mean((CAGRall[,1]))
S3 <- mean((CAGRall[,1])) + sd(CAGRall[,1])
S4 <- mean((CAGRall[,1])) + 2*sd(CAGRall[,1])



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

str(UMZ)


##### 2.3 / Optimal Matching


#####2.3.1 / sequences caracterisation

##partition du tableau par classe à faire

library(TraMineR)
library(RColorBrewer)
# 2,"#91BFDB" 4, "#FEE090" 6,"#D73027"
color <- c("#4575B4" ,"#E0F3F8" ,"#FC8D59")
#color <- c("lightblue", "orange", "red")
UMZ.seq <- seqdef(UMZ, var = 25:35) 


seqstatl(UMZ.seq)
seqdim(UMZ.seq)
colnames(UMZ.seq) <- c("1996-1999","1999-2003","2003-2004","2004-2005","2005-2006", "2006-2007","2007-2008","2008-2009","2009-2010","2010-2011","2011-2012")
par(mfrow = c(2, 2))

# seqiplot(UMZ.seq, title = "Index plot (first 10 sequences)",
#          withlegend = F)
seqHtplot(UMZ.seq,  withlegend=F, title = "Entropy")
seqmsplot(UMZ.seq,  withlegend=F, title="Modal Profile")
seqdplot(UMZ.seq, title = "State distribution plot", withlegend = FALSE)
seqfplot(UMZ.seq, title = "Sequence frequency plot", withlegend = FALSE,
         pbarw = TRUE)
seqlegend(UMZ.seq,fontsize=1)


# Decription des classes avec la frequence en SPS
#donne le nomre des unités spatiales sur sequence frequency plot
seqtab(UMZ.seq, tlim = 1:10, format="SPS")

# Transition rate
#sur l'ensemble dew transitions, donne le pourcentage de trnasition  d'un etat à un autre état
TransitionRate <- seqtrate(UMZ.seq)
TransitionRate



#####2.3.2 / OM CAH

#ça fait une cah sur les trajectoires

########  CAH

library(TraMineR)
library(cluster)


#Definition des couts 
##(cout constant pour LevenshteinII et Trate pour Dynamic Hamming)

couts <- seqsubm(UMZ.seq,method="CONSTANT", cval=3)
#couts <- seqsubm(UMZ.seq,method="TRATE")

#matrice de distances_
##(changer Indel ou sm selon la distance utilisee : LevenshteinII ou Hamming)
str(UMZ.seq)
seq.om <- seqdist(UMZ.seq, method="OM", indel=1, sm=couts)
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

nbcl <- 3
seq.part <- cutree(seq.agnes, nbcl)

seq.part <- factor(seq.part,labels=paste("Cluster",1:nbcl,sep='.'))

# library(NbClust)
# NbClust(data = UMZ.seq,diss = distom, distance = NULL, method = "ward.D2",min.nc = 2, max.nc = 15,index = "all")
### State distribution plot


seqplot(UMZ.seq, group=seq.part, type="d" ,
        border=NA, withlegend=T)



### index plot

#ordre <- cmdscale(as.dist(seq.om),k=1)
#ordre <- sort.list 
seqiplot(UMZ.seq, group=seq.part,sortv="from.start", title = "Index plot",
         tlim=0, space=0, border=NA, withlegend=T, yaxis=FALSE )

# distance moyenne des sequences au centre de la classe

meanDistClass <- round(aggregate(disscenter(as.dist(seq.om), group=seq.part), 
                                 list(seq.part),mean)[,-1],1)

#sequence frequency plot

#seqfplot(UMZ.seq[seq.part != "classe.2", ], group=seq.part[seq.part != "classe.2"],
#        title = "Sequence frequency plot", withlegend=T)
#
seqfplot(UMZ.seq, group=seq.part,  withlegend=T, border="darkgrey")

str(seq.part)
levels(seq.part)
# etat modal de chaque classe

seqmsplot(UMZ.seq, group=seq.part, withlegend=T, title="Modal Profile")

# entropie par classe

seqHtplot(UMZ.seq, group=seq.part, withlegend=T, title = "Entropy")


### Stockage des classes dans le tableau

UMZ$OMclustersCAH <- seq.part
UMZ$OMclustersCAH <- as.integer(UMZ$OMclustersCAH)





