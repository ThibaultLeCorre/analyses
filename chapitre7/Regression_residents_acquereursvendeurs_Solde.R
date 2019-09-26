library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
#regressions_ (solde) acquereurs_solde résidents

#voir script Odds_ratio_acq_residents pour les données

options(scipen=999)

CSP_residentsCommunes_IDF_990812 <- read.csv2("~/Projets/Chapitre7/CAH_acquereur_residents/CSP_residentsCommunes_IDF_990812.csv", stringsAsFactors=FALSE)
CSP_residentsCommunes_IDF_990812$Periode<-ifelse(CSP_residentsCommunes_IDF_990812$annee==1999, "Periode_96_2003",
                                                 ifelse(CSP_residentsCommunes_IDF_990812$annee==2008, "Periode_04_2007", 
                                                        ifelse(CSP_residentsCommunes_IDF_990812$annee==2012, "Periode_08_2012", NA)))
test_residents<-CSP_residentsCommunes_IDF_990812
Residents<- c("Agriculteurs","Liberales","CPIS","Prof_intermediaires","Employes","Ouvriers")

test_residents$Agriculteurs<-as.numeric(test_residents$Agriculteurs)
test_residents$Liberales<-as.numeric(test_residents$Liberales)
test_residents$CPIS<-as.numeric(test_residents$CPIS)
test_residents$Prof_intermediaires<-as.numeric(test_residents$Prof_intermediaires)
test_residents$Employes<-as.numeric(test_residents$Employes)
test_residents$Ouvriers<-as.numeric(test_residents$Ouvriers)


test_residents$total_residents<- rowSums(test_residents[,c(3:8)])

test_residents<-as.data.frame(test_residents) %>% 
 dplyr:: select(-annee,-Actifs, -total_residents)%>%
  group_by(CODGEO,Periode)%>% 
  gather("CSP_residents","Value", 2:7)%>% 
  spread(Periode, Value, fill=0)

test_residents<-as.data.frame(test_residents)
test_residents$Solde1<-test_residents$Periode_04_2007-test_residents$Periode_96_2003
test_residents$Solde2<-test_residents$Periode_08_2012-test_residents$Periode_04_2007
test_residents$SoldeTotal<-test_residents$Periode_08_2012-test_residents$Periode_96_2003

###########################################################

mytest<- mytest_BIEN%>%
  group_by(DepCom,Periode, CSP_AC)%>%
  filter(CSP_AC %in% Acquereurs)%>%
 summarise(n_acq=n())%>%
ungroup() %>%
  spread(CSP_AC,n_acq,fill = 0)


test_acquereurs<-as.data.frame(mytest) %>% 
  group_by(DepCom,Periode)%>% 
  gather("CSP_acquereurs","Value", 3:8)%>% 
  spread(Periode, Value, fill=0)

# test_acquereurs$Solde1<-test_acquereurs$Periode_04_2007-test_acquereurs$Periode_96_2003
# test_acquereurs$Solde2<-test_acquereurs$Periode_08_2012-test_acquereurs$Periode_04_2007
# test_acquereurs$SoldeTotal<-test_acquereurs$Periode_08_2012-test_acquereurs$Periode_96_2003


####################

# data_redresse1570533transacs <- read.csv("~/BIEN/data_redresse1570533transacs.csv", stringsAsFactors=FALSE)
mytest_BIEN<-data_redresse1570533transacs[,c("annee.x","CSP_VE", "X.x", "Y.x")]


mytest_BIEN$CSP_VE<- ifelse(mytest_BIEN$CSP_VE == 10 , "Agriculteurs",
                            ifelse(mytest_BIEN$CSP_VE >= 20 & mytest_BIEN$CSP_VE < 30, "Liberales",
                                   ifelse(mytest_BIEN$CSP_VE >= 30 & mytest_BIEN$CSP_VE < 40, "CPIS",
                                          ifelse(mytest_BIEN$CSP_VE >= 40 & mytest_BIEN$CSP_VE < 50, "Prof_intermediaires",
                                                 ifelse(mytest_BIEN$CSP_VE >= 50 & mytest_BIEN$CSP_VE < 60, "Employes",
                                                        ifelse(mytest_BIEN$CSP_VE >= 60 & mytest_BIEN$CSP_VE < 70, "Ouvriers",
                                                               ifelse(mytest_BIEN$CSP_VE >= 70 & mytest_BIEN$CSP_VE < 80, "retraite",
                                                                      ifelse(mytest_BIEN$CSP_VE == 80, "autres_inVEtifs_VE", "non_rs_VE" ))))))))

mytest_BIEN$Periode<-ifelse(mytest_BIEN$annee.x==1996|mytest_BIEN$annee.x==1999|mytest_BIEN$annee.x==2003, "Periode_96_2003",
                            ifelse(mytest_BIEN$annee.x>=2004&mytest_BIEN$annee.x<=2007, "Periode_04_2007", 
                                   ifelse(mytest_BIEN$annee.x>=2008&mytest_BIEN$annee.x<=2012, "Periode_08_2012", NA)))


#######################


spVente <- st_as_sf(mytest_BIEN,
                    
                    coords = c("X.x", "Y.x"),
                    
                    agr = "constant",
                    
                    crs = 27572,
                    
                    stringsAsFactors = FALSE)



#converison Lamb 93 et stockage coords
spVente <-  st_transform(spVente, crs = 2154)
coords<- st_coordinates(spVente)
spVente$XLamb93<-coords[,1]
spVente$Ylamb93<-coords[,2]


Communes_jointure<-st_join(CommunesInsee, spVente, join = st_contains, left=T)
mytest_BIEN<- as.data.frame(Communes_jointure)

rm(Communes_jointure)
##########

# Years<- c(1999,2008,2012)
Vendeurs<- c("Agriculteurs","Liberales","CPIS", "Prof_intermediaires","Employes", "Ouvriers")
mytest_vendeurs<- mytest_BIEN%>%
  group_by(DepCom,Periode, CSP_VE)%>%
  filter(CSP_VE %in% Vendeurs)%>%
  summarise(n_ve=n())%>%
  ungroup() %>%
  spread(CSP_VE,n_ve,fill = 0)
rm(mytest_BIEN)


test_vendeurs<-as.data.frame(mytest_vendeurs) %>% 
  group_by(DepCom,Periode)%>% 
  gather("CSP_vendeurs","Value", 3:8)%>% 
  spread(Periode, Value, fill=0)

# test_vendeurs$Solde1<-test_vendeurs$Periode_04_2007-test_vendeurs$Periode_96_2003
# test_vendeurs$Solde2<-test_vendeurs$Periode_08_2012-test_vendeurs$Periode_04_2007
# test_vendeurs$SoldeTotal<-test_vendeurs$Periode_08_2012-test_vendeurs$Periode_96_2003


Solde_Acq_Ve<-dplyr::left_join(as.data.frame(test_acquereurs), as.data.frame(test_vendeurs),by= c("DepCom", "CSP_acquereurs"= "CSP_vendeurs"))

Solde_Acq_Ve$Solde1<-Solde_Acq_Ve$Periode_96_2003.x-Solde_Acq_Ve$Periode_96_2003.y
Solde_Acq_Ve$Solde2<-Solde_Acq_Ve$Periode_04_2007.x-Solde_Acq_Ve$Periode_04_2007.y
Solde_Acq_Ve$Solde3<-Solde_Acq_Ve$Periode_08_2012.x-Solde_Acq_Ve$Periode_08_2012.y
Solde_Acq_Ve$Total_acq<-Solde_Acq_Ve$Periode_96_2003.x+Solde_Acq_Ve$Periode_04_2007.x+Solde_Acq_Ve$Periode_08_2012.x
Solde_Acq_Ve$Total_vendeur<-Solde_Acq_Ve$Periode_96_2003.y+Solde_Acq_Ve$Periode_04_2007.y+Solde_Acq_Ve$Periode_08_2012.y
Solde_Acq_Ve$SoldeTotal<-Solde_Acq_Ve$Total_acq-Solde_Acq_Ve$Total_vendeur

############################################

Solde_Marche_Residents<- left_join(Solde_Acq_Ve, test_residents,by= c("DepCom"="CODGEO", "CSP_acquereurs"="CSP_residents"))
str(Solde_Marche_Residents)
lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}


library(devtools)
install_github("eringrand/RUncommon")
library(devtools)
source_gist("524eade46135f6348140")
# library(ggpmisc)
# lm_eqn(Solde_Marche_Residents)
# 
# Solde_Marche_Residents%>%
#   dplyr::select(DepCom, CSP_acquereurs,SoldeTotal.x, SoldeTotal.y)%>%
# ggplot() +
#   geom_jitter(aes(SoldeTotal.x,SoldeTotal.y, colour=CSP_acquereurs),) + geom_smooth(aes(SoldeTotal.x,SoldeTotal.y, colour=CSP_acquereurs), method=lm, se=FALSE, formula = y ~ x) +
#   facet_wrap(~CSP_acquereurs, scales="free") +
#   geom_text(aes(x = 25, y = 300, label = lm_eqn(lm(SoldeTotal.y ~ SoldeTotal.x, Solde_Marche_Residents))), parse = TRUE)+
#   labs(x = "Solde du marché", y = "Solde Résidentiel")

Solde_Marche_Residents2<- left_join(Solde_Marche_Residents, SpUnitt_for_seq[,c("DepCom","CAHOM")],by= "DepCom")

Solde_Marche_Residents2%>%
  dplyr::select(DepCom, CSP_acquereurs,SoldeTotal.x, SoldeTotal.y,CAHOM)%>%
  filter (CSP_acquereurs!= "Agriculteurs" & CSP_acquereurs!= "Liberales", !is.na(CAHOM)  )%>%
  ggplot(.,aes(x = SoldeTotal.x, y = SoldeTotal.y, label=SoldeTotal.y)) +
  geom_jitter(aes(SoldeTotal.x,SoldeTotal.y, colour=CSP_acquereurs),) + 
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(aes(SoldeTotal.x,SoldeTotal.y, colour=CSP_acquereurs), method=lm, se=FALSE, formula = y ~ x) +
  facet_grid(CAHOM~CSP_acquereurs, scales="free",margins = T) +
  # geom_text(aes(x = 25, y = 300, label = lm_eqn(lm(SoldeTotal.y ~ SoldeTotal.x, Solde_Marche_Residents))), parse = TRUE)+
  labs(x = "Solde du marché", y = "Solde Résidentiel")










CPIS_solde<- Solde_Marche_Residents%>%
  select(DepCom, CSP_acquereurs,SoldeTotal.x, SoldeTotal.y)%>%
  filter(CSP_acquereurs=="CPIS", !is.na(SoldeTotal.x))

# CPIS_solde[,c(2:13)]<-as.numeric(CPIS_solde[,c(2:13)])
CPIS_solde<-as.data.frame(CPIS_solde)

# ?arrange


test.cor<-cor (CPIS_solde$SoldeTotal.x,CPIS_solde$SoldeTotal.y, use="complete.obs")
test.cor_2<-test.cor^2

Y <- CPIS_solde$SoldeTotal.y # Y variables à expliquer   
X <- CPIS_solde$SoldeTotal.x #variable explicative


reg.lin<-lm(Y~X)
Droite_regression<-reg.lin[[1]]

CPIS_solde$test.lm.fit<-reg.lin$fitted.values

resid<-residuals.lm(reg.lin)

CPIS_solde$resid<-resid

ecartmodel<-sd(CPIS_solde$test.lm.fit)



CPIS_solde$categorie_variation<- ifelse(CPIS_solde$resid> - (ecartmodel/2) & CPIS_solde$resid<= (ecartmodel/2), "Conforme_au_modele",
                                                 ifelse(CPIS_solde$resid <= - (ecartmodel/2), "Plus_faible_que_la_prevision",
                                                        ifelse(CPIS_solde$resid >=  (ecartmodel/2), "Plus_fort_que_la _prevision", "non_rs")))


Communes_result<-left_join(CommunesInsee, CPIS_solde, by="DepCom")


library(cartography)
layoutLayer(title = "Solde Marchés-Résidents",
            sources = "BIEN",
            author= "Â®T.Le Corre, GÃ©ographie-CitÃ©s 2016",
            scale = 0,
            frame = FALSE,
            col = "black",
            north = TRUE,
            coltitle = "white",
            extent = Communes_result)

typoLayer(x=Communes_result,
          var = "categorie_variation",
          col= cols, ##palettes de couleurs predefinies## 
          border = TRUE,lwd = 0.1,
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          colNA="grey",add=T)







Prof_inter_solde<- Solde_Marche_Residents%>%
  select(DepCom, CSP_acquereurs,SoldeTotal.x, SoldeTotal.y)%>%
  filter(CSP_acquereurs=="Prof_intermediaires", !is.na(SoldeTotal.x))

# Prof_inter_solde[,c(2:13)]<-as.numeric(Prof_inter_solde[,c(2:13)])
Prof_inter_solde<-as.data.frame(Prof_inter_solde)

# ?arrange


test.cor<-cor (Prof_inter_solde$SoldeTotal.x,Prof_inter_solde$SoldeTotal.y, use="complete.obs")
test.cor_2<-test.cor^2

Y <- Prof_inter_solde$SoldeTotal.y # Y variables à expliquer   
X <- Prof_inter_solde$SoldeTotal.x #variable explicative


reg.lin<-lm(Y~X)
Droite_regression<-reg.lin[[1]]

Prof_inter_solde$test.lm.fit<-reg.lin$fitted.values

resid<-residuals.lm(reg.lin)

Prof_inter_solde$resid<-resid

ecartmodel<-sd(Prof_inter_solde$test.lm.fit)



Prof_inter_solde$categorie_variation<- ifelse(Prof_inter_solde$resid> - (ecartmodel/2) & Prof_inter_solde$resid<= (ecartmodel/2), "Conforme_au_modele",
                                        ifelse(Prof_inter_solde$resid <= - (ecartmodel/2), "Plus_faible_que_la_prevision",
                                               ifelse(Prof_inter_solde$resid >=  (ecartmodel/2), "Plus_fort_que_la _prevision", "non_rs")))


Communes_result<-left_join(CommunesInsee, Prof_inter_solde, by="DepCom")


# library(cartography)
layoutLayer(title = "Solde Marchés-Résidents",
            sources = "BIEN",
            author= "Â®T.Le Corre, GÃ©ographie-CitÃ©s 2016",
            scale = 0,
            frame = FALSE,
            col = "black",
            north = TRUE,
            coltitle = "white",
            extent = Communes_result)

typoLayer(x=Communes_result,
          var = "categorie_variation",
          col= cols, ##palettes de couleurs predefinies## 
          border = TRUE,lwd = 0.1,
          legend.pos = "bottomleft",
          legend.title.txt = "",
          legend.values.cex= 0.5,
          colNA="grey",add=T)
