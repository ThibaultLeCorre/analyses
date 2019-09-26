
library(dplyr)
library(tidyr)
#tables logement
# par communes
logement_1999_2$COM<-ifelse(logement_1999_2$COM==75056,logement_1999_2$COMA, logement_1999_2$COM )
log99<- logement_1999_2%>%
  select(-c(1,4:6))%>%
  group_by(COM)%>%
  summarise_all(funs(sum))%>%
    select(-c(3,9:10))
log99$Annee<-1999
log99$Nobre_RS<- log99$Nobre_RS+log99$Nombre_log_occasionnels
log99<-log99%>%select(-c(5))

log12<- logement_2012%>%
  select(-c(2:5, 7:13))%>%
  group_by(COM)%>%
  summarise_all(funs(sum),na.rm=T)%>%
  select(-c(2, 7:47, 50 : ncol(.) ))
log12$Annee<-2012

log08<- logement_2008%>%
  select(-c(2:4, 9:50, 52:ncol(.)))
log08$Annee<-2008
colnames(log08)<-c("DepCom", "NbrLog","NbrRP","NbrSecOcc","NbrVac","NbrRPProp","NbrRPLoc","Annee")
colnames(log12)<-c("DepCom", "NbrLog","NbrRP","NbrSecOcc","NbrVac","NbrRPProp","NbrRPLoc","Annee")
colnames(log99)<-c("DepCom", "NbrLog","NbrRP","NbrSecOcc","NbrVac","NbrRPProp","NbrRPLoc","Annee")


Logements<- bind_rows(log99,log08,log12)
Logements<- Logements%>%
  group_by(DepCom,Annee)%>%
gather(key = "Catego", value = "Value",c(2:7))%>%
  spread(Catego,Value)

Logements<-Logements%>%
  group_by(DepCom,Annee)%>%
  summarise(Pourc_RP= (NbrRP /NbrLog )*100,
            Pourc_LogRPLoc= (NbrRPLoc /NbrRP )*100,
            Pourc_LogRPProp= (NbrRPProp  /NbrRP )*100,
            Pourc_LogSecOcc= (NbrSecOcc   /NbrLog  )*100,
            Pourc_Vac= (NbrVac   /NbrLog  )*100)
setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
write.csv2(x=Logements,file = "Logements.csv", row.names=FALSE, fileEncoding = "UTF-8")

#################

Origine_revenus<- Structure_revenus%>%
  filter(Annee==2001 | Annee==2006 | Annee ==2010, !is.na(PAUT))%>%
  select(c(1,2,9:ncol(.)))

Origine_revenus$Annee<-ifelse(Origine_revenus$Annee==2001, 1999, 
                     ifelse(Origine_revenus$Annee==2006 , 2008,
                               ifelse(  Origine_revenus$Annee==2010, 2012,Origine_revenus$Annee)))
setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
write.csv2(x=Origine_revenus,file = "Origine_revenus.csv", row.names=FALSE, fileEncoding = "UTF-8")
####

Pop_active_dont_CSP_Communes_IDF_1999 <- read.csv("~/tables/Pop_active_dont_CSP_Communes_IDF_1999.csv", sep=";", comment.char="#", stringsAsFactors=FALSE)
Pop_active_dont_CSP_Communes_IDF_2008 <- read.csv2("~/tables/Pop_active_dont_CSP_Communes_IDF_2008.csv", comment.char="#", stringsAsFactors=FALSE)
# Pop_active_dont_CSP_Iris_IDF_2012 <- read.csv2("~/tables/Pop_active_dont_CSP_Iris_IDF_2012.csv", stringsAsFactors=FALSE)
# str(Pop_active_dont_CSP_Iris_IDF_2012)
# colnames(Pop_active_dont_CSP_Iris_IDF_2012)
# CSP_Age_Pop2012<-
#   Pop_active_dont_CSP_Iris_IDF_2012%>%
#   select(-c(1:5, 7:13))%>%
#   group_by(COM)%>%
#   summarise_all(funs(sum),na.rm=T)
# CSP_Age_Pop2012$P12_POP15Pplus<- rowSums(CSP_Age_Pop2012[,c(14:18)])
# CSP_Age_Pop2012$P12_POPActive<- rowSums(CSP_Age_Pop2012[,c(55:60)])
# #On garde les varaibles qui nous interessent
# colnames(CSP_Age_Pop2012)
# CSP_Age_Pop2012<-
#   CSP_Age_Pop2012%>%
#   select(-c(3:6, 8:44, 46,49:73))
# colnames(CSP_Age_Pop2012)
# CSP_Age_Pop2012<-
#   CSP_Age_Pop2012%>%
#   select(-c(2,3,8,9))
# colnames(CSP_Age_Pop2012)<-c("DepCom", "Pop_cadres","Pop_employes","Pop_ouvriers","Pop_active")
# CSP_Age_Pop2012$Annee<-2012

colnames(Pop_active_dont_CSP_Communes_IDF_2013)
CSP_Age_Pop2012<-
  Pop_active_dont_CSP_Communes_IDF_2013%>%
  select(c(1,6,7,18,58,60,61))
colnames(CSP_Age_Pop2012)
colnames(CSP_Age_Pop2012)<-c("DepCom","Pop_15_64", "Pop_15_24", "Pop_active","Pop_cadres","Pop_employes","Pop_ouvriers")
CSP_Age_Pop2012$Annee<-2012

CSP_Age_Pop2008<-Pop_active_dont_CSP_Communes_IDF_2008
colnames(CSP_Age_Pop2008)
CSP_Age_Pop2008<-
  CSP_Age_Pop2008%>%
  select(c(1,5,6,17,57,59,60 ))
colnames(CSP_Age_Pop2008)
colnames(CSP_Age_Pop2008)<-c("DepCom","Pop_15_64", "Pop_15_24", "Pop_active","Pop_cadres","Pop_employes","Pop_ouvriers")
CSP_Age_Pop2008$Annee<-2008

CSP_Age_Pop1999<-Pop_active_dont_CSP_Communes_IDF_1999
colnames(CSP_Age_Pop1999)
CSP_Age_Pop1999<-
  CSP_Age_Pop1999%>%
  select(c(1,5,6,20,22,23))
colnames(CSP_Age_Pop1999)
colnames(CSP_Age_Pop1999)<-c("DepCom","Pop_15_64", "Pop_active","Pop_cadres","Pop_employes","Pop_ouvriers")
CSP_Age_Pop1999$Annee<-1999



CSP_Age_Pop<- bind_rows(CSP_Age_Pop1999,CSP_Age_Pop2008,CSP_Age_Pop2012)
CSP_Age_Pop<-CSP_Age_Pop%>%
  select(-c("Pop_15_24"))

PopCom<-recensement99_2012_Pop_total_communes
PopCom$DepCom<-PopCom$DEPCOM


colnames(PopCom)
PopCom<-PopCom%>%
  group_by(DepCom)%>%
  gather(key = "Annee", value = "Pop_total",c(7:9)) %>%
  select(c(7,8,9))
PopCom$Annee<-ifelse(PopCom$Annee=="RGP2012", 2012, 
              ifelse(   PopCom$Annee=="RGP2007", 2008,
               ifelse(  PopCom$Annee=="RGP1999", 1999,PopCom$Annee)))
  


PopCom$DepCom<-as.integer(PopCom$DepCom)
PopCom$Annee<-as.numeric(PopCom$Annee)
CSP_Age_Pop<-left_join(CSP_Age_Pop,PopCom, by = c("DepCom", "Annee"))
colnames(CSP_Age_Pop)
CSP_Age_Pop<-CSP_Age_Pop%>%
  group_by(DepCom,Annee)%>%
  summarise(Pourc_Pop_Active= (Pop_active/Pop_15_64)*100,
            Pourc_Pop_cadres= (Pop_cadres/Pop_active)*100,
        Pourc_Pop_ouvriers_employes= ((Pop_ouvriers+Pop_employes)/Pop_active)*100)
CSP_Age_Pop$Periode<-ifelse(CSP_Age_Pop$Annee==1999,"Periode_96_2003",
                            ifelse(CSP_Age_Pop$Annee==2008,"Periode_04_2007",    
                                   ifelse(CSP_Age_Pop$Annee==2012,"Periode_08_2012",  CSP_Age_Pop$Annee)))

setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
write.csv2(x=CSP_Age_Pop,file = "CSP_Age_Pop.csv", row.names=FALSE, fileEncoding = "UTF-8")



unique(CSP_Age_Pop$Periode)
Logements$Periode<-ifelse(Logements$Annee==1999,"Periode_96_2003",
                            ifelse(Logements$Annee==2008,"Periode_04_2007",    
                                   ifelse(Logements$Annee==2012,"Periode_08_2012",  Logements$Annee)))
unique(Logements$Periode)

Origine_revenus$Periode<-ifelse(Origine_revenus$Annee==1999,"Periode_96_2003",
                          ifelse(Origine_revenus$Annee==2008,"Periode_04_2007",    
                                 ifelse(Origine_revenus$Annee==2012,"Periode_08_2012",  Origine_revenus$Annee)))

Origine_revenus$DepCom<-Origine_revenus$COM
Variables_Supp_ACP_Communes<- left_join(Logements,CSP_Age_Pop, by = c("DepCom", "Periode"))

Variables_Supp_ACP_Communes<- left_join(Variables_Supp_ACP_Communes,PTZ_communes, by = c("DepCom", "Periode"))

Variables_Supp_ACP_Communes<- left_join(Variables_Supp_ACP_Communes,Origine_revenus, by = c("DepCom", "Periode"))







# 
# 
# 
# 
# 
