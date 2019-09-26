Prix_annee_potentiel_200_appartement<- Montableau_Appartement_200_all_45138
Prix_annee_potentiel_200_appartement<- Prix_annee_potentiel_200_appartement%>%
  select(Carreau_ID,prixPotentiel,Annee)%>%
  group_by(Carreau_ID)%>%
  filter(!is.na(prixPotentiel))%>%
  spread(Annee,prixPotentiel)%>%
  filter_all(all_vars(!is.na(.)))
  
  
  library(dplyr)
library(tidyr)
library(cartography)


Prix_communes_appartement<- Montableau_Appartement_communes%>%
  select(DepCom,Prix_mcarre_moyen,Annee)%>%
  group_by(DepCom)%>%
  filter(!is.na(Prix_mcarre_moyen))%>%
  spread(Annee,Prix_mcarre_moyen)%>%
  filter_all(all_vars(!is.na(.)))

Montableau_Maisons_communes$Prix_moyen<-as.numeric(Montableau_Maisons_communes$Prix_moyen)
Prix_communes_maisons<- Montableau_Maisons_communes%>%
  select(DepCom,Prix_moyen,Annee)%>%
  group_by(DepCom)%>%
  filter(!is.na(Prix_moyen))%>%
  spread(Annee,Prix_moyen)%>%
  filter_all(all_vars(!is.na(.)))

data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest<-data_redresse1570533transacs[,c("ID","annee.x","insee","REQ_PRIX","REQTYPBIEN")]
str(data_redresse1570533transacs)
Prix_communes_maisons<- mytest%>%
  filter(REQTYPBIEN=="MA")%>%
  group_by(insee,annee.x)%>%
  summarise(Prix_moyen= mean(REQ_PRIX))%>%
  filter(!is.na(Prix_moyen))%>%
  spread(annee.x,Prix_moyen)%>%
  filter_all(all_vars(!is.na(.)))

UMZ<-Prix_communes_maisons

UMZ<-Prix_communes_appartement
CAGRcol<- colnames(UMZ)[c(2:13)]
labelClass <- c(0,1, 2, 3, 4,5,6,7,8,9)

for (i in CAGRcol) {
  data <- UMZ[[i]] 

  ### Setting breaks
  valBreaks <- getBreaks(data,nclass = 10,method="quantile")
  
  UMZ[paste(i, "_Class",sep="")] <- cut(data,
                                        breaks = valBreaks,
                                        labels = labelClass,
                                        include.lowest = TRUE,
                                        right= FALSE, na.rm=T  )
}




library(ggplot2)

getFlows <- GetCrossFlows(df = UMZ[,c(14:25)])


# df= colonne temporelles

SlidePlot(listFlows = getFlows, threshold = 1,mask = FALSE,thickmin =0.01,showfreq = FALSE )

dev.off()





table(UMZ$`1999_Class`, UMZ$`2012_Class`)
?table


Prix_annee_potentiel_1000_appartement<- Montableau_Appartement_1000_all_5091tiles
Prix_annee_potentiel_1000_appartement<- Prix_annee_potentiel_1000_appartement%>%
  select(Carreau_ID,prixPotentiel,Annee)%>%
  group_by(Carreau_ID)%>%
  filter(!is.na(prixPotentiel))%>%
  spread(Annee,prixPotentiel)%>%
  filter_all(all_vars(!is.na(.)))


Prix_communes_appartement<- Montableau_Appartement_communes
Prix_communes_appartement<- Prix_communes_appartement%>%
  select(DepCom,Prix_mcarre_moyen,Annee)%>%
  group_by(DepCom)%>%
  filter(!is.na(Prix_mcarre_moyen))%>%
  spread(Annee,Prix_mcarre_moyen)%>%
  filter_all(all_vars(!is.na(.)))
