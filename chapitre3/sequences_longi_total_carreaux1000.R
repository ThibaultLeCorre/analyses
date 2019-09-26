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

Prix_annee_potentiel_1000_appartement<- Montableau_Appartement_1000_all_5091tiles

Prix_annee_potentiel_1000_appartement<- Prix_annee_potentiel_1000_appartement%>%
  select(Carreau_ID,prixPotentiel,Annee)%>%
  group_by(Carreau_ID)%>%
  filter(!is.na(prixPotentiel))%>%
  spread(Annee,prixPotentiel)%>%
  filter_all(all_vars(!is.na(.)))

UMZ<-as.data.frame(Prix_annee_potentiel_1000_appartement)



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
med<-median(CAGRall$values)

# CAGRmin <- min(CAGRall[,1])
# CAGRmax <- max(CAGRall[,1])

# par(mfrow=c(1,1))
# hist(CAGRall[,1], breaks=400, freq=FALSE, 
#      xlim = c(CAGRmin,CAGRmax),main = "CAGR")

# summary(CAGRall[,1])
# sd(CAGRall[,1])
# #estimation des TCAM totaux et categorisation
# S1 <- mean((CAGRall[,1])) - sd(CAGRall[,1])
# S2 <- mean((CAGRall[,1]))
# S3 <- mean((CAGRall[,1])) + sd(CAGRall[,1])
# S4 <- mean((CAGRall[,1])) + 2*sd(CAGRall[,1])



#### Encoding Class
CAGRcol <- grep("[Vv]", names(UMZ), value = TRUE)

labelClass <- c("Decay", "Growth", "StrongGrowth")
med<- median(UMZ)

for (i in CAGRcol) {
  data <- UMZ[[i]] 
  min<-min(data)
  max<- max(data)
  
  ### Setting breaks
  valBreaks <- c(min,0,med,max)
  
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

library(TraMineR)
library(RColorBrewer)
# 2,"#91BFDB" 4, "#FEE090" 6,"#D73027"
# color <- c("#4575B4" ,"#E0F3F8" ,"#FC8D59")
#color <- c("lightblue", "orange", "red")
UMZ.seq <- seqdef(UMZ, var = 25:35) 


seqstatl(UMZ.seq)
seqdim(UMZ.seq)
colnames(UMZ.seq) <- c("1996-1999","1999-2003","2003-2004","2004-2005","2005-2006", "2006-2007","2007-2008","2008-2009","2009-2010","2010-2011","2011-2012")
par(mfrow = c(2, 2))

# seqiplot(UMZ.seq, title = "Index plot (first 10 sequences)",
#          withlegend = F)
seqHtplot(UMZ.seq,  withlegend=F, title = "Entropy")
# seqmsplot(UMZ.seq,  withlegend=F, title="Modal Profile")
seqdplot(UMZ.seq, title = "State distribution plot", withlegend = FALSE)
seqfplot(UMZ.seq, title = "Sequence frequency plot", withlegend = FALSE,
         pbarw = TRUE)
seqlegend(UMZ.seq,fontsize=1)

dev.off()





