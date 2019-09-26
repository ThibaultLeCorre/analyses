
CSP_residentsCommunes_IDF_990812 <- read.csv2("~/Projets/Chapitre7/CAH_acquereur_residents/CSP_residentsCommunes_IDF_990812.csv", stringsAsFactors=FALSE)
CSP_residentsCommunes_IDF_990812$Periode<-ifelse(CSP_residentsCommunes_IDF_990812$annee==1999, "Periode_96_2003",
                                                 ifelse(CSP_residentsCommunes_IDF_990812$annee==2008, "Periode_04_2007", 
                                                        ifelse(CSP_residentsCommunes_IDF_990812$annee==2012, "Periode_08_2012", NA)))
test_residents<-CSP_residentsCommunes_IDF_990812
colnames(test_residents[4])<-"ComArtCEts"
names(test_residents)[names(test_residents) == 'Liberales'] <- 'ComArtCEts'
Residents<- c("Agriculteurs","ComArtCEts","CPIS","Prof_intermediaires","Employes","Ouvriers")

test_residents$Agriculteurs<-as.numeric(test_residents$Agriculteurs)
test_residents$ComArtCEts<-as.numeric(test_residents$ComArtCEts)
test_residents$CPIS<-as.numeric(test_residents$CPIS)
test_residents$Prof_intermediaires<-as.numeric(test_residents$Prof_intermediaires)
test_residents$Employes<-as.numeric(test_residents$Employes)
test_residents$Ouvriers<-as.numeric(test_residents$Ouvriers)


test_residents$total_residents<- rowSums(test_residents[,c(3:8)])

test_residents<-as.data.frame(test_residents) %>% 
  dplyr:: select(-annee,-Actifs)%>%
  group_by(CODGEO,Periode)%>% 
  mutate_at(Residents, funs(./total_residents*100))%>%
  select(-total_residents)%>%
  gather("CSP_residents","Value_Residents", 2:7)
  


###########################################################


mytest_BIEN<-data_redresse1570533transacs[,c("annee.x","CSP_AC", "X.x", "Y.x")]


mytest_BIEN$CSP_AC<- ifelse(mytest_BIEN$CSP_AC == 10 , "Agriculteurs",
                            ifelse(mytest_BIEN$CSP_AC >= 20 & mytest_BIEN$CSP_AC < 30, "ComArtCEts",
                                   ifelse(mytest_BIEN$CSP_AC >= 30 & mytest_BIEN$CSP_AC < 40, "CPIS",
                                          ifelse(mytest_BIEN$CSP_AC >= 40 & mytest_BIEN$CSP_AC < 50, "Prof_intermediaires",
                                                 ifelse(mytest_BIEN$CSP_AC >= 50 & mytest_BIEN$CSP_AC < 60, "Employes",
                                                        ifelse(mytest_BIEN$CSP_AC >= 60 & mytest_BIEN$CSP_AC < 70, "Ouvriers",
                                                               ifelse(mytest_BIEN$CSP_AC >= 70 & mytest_BIEN$CSP_AC < 80, "retraite",
                                                                      ifelse(mytest_BIEN$CSP_AC == 80, "autres_inactifs_AC", "non_rs_AC" ))))))))

mytest_BIEN$Periode<-ifelse(mytest_BIEN$annee.x==1996|mytest_BIEN$annee.x==1999|mytest_BIEN$annee.x==2003, "Periode_96_2003",
                            ifelse(mytest_BIEN$annee.x>=2004&mytest_BIEN$annee.x<=2007, "Periode_04_2007", 
                                   ifelse(mytest_BIEN$annee.x>=2008&mytest_BIEN$annee.x<=2012, "Periode_08_2012", NA)))


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


Acquereurs<- c("Agriculteurs","ComArtCEts","CPIS", "Prof_intermediaires","Employes", "Ouvriers")

mytest_acquereurs<- as.data.frame(Communes_jointure)%>%
  group_by(DepCom,Periode, CSP_AC)%>%
  filter(CSP_AC %in% Acquereurs)%>%
  summarise(n_ac=n())%>%
  ungroup() %>%
  spread(CSP_AC,n_ac,fill = 0)


mytest_acquereurs$total_acquereurs<- rowSums(mytest_acquereurs[,c(3:8)])

mytest_acquereurs<- mytest_acquereurs%>%
  group_by(DepCom,Periode)%>%
  filter(total_acquereurs>=20)%>%
  mutate_at(Acquereurs, funs(./total_acquereurs*100))%>%
  select(-total_acquereurs)%>%
  gather("CSP_acquereurs","Value_acquereurs", 3:8)


Marche_Residents<- left_join(mytest_acquereurs, test_residents,by= c("DepCom"="CODGEO", "CSP_acquereurs"="CSP_residents", "Periode"))


library(devtools)
install_github("eringrand/RUncommon")
source_gist("524eade46135f6348140")

pdf(file="~/Projets/Chapitre7/CAH_acquereur_residents/Reg_Acq_Resid.pdf",width = 20, height = 15)


Marche_Residents%>%
  ggplot(.,aes(x = Value_acquereurs, y = Value_Residents, label=Value_Residents, group=Periode)) +
  geom_jitter(aes(Value_acquereurs,Value_Residents, colour=CSP_acquereurs),) + 
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  geom_smooth(aes(Value_acquereurs,Value_Residents, colour=CSP_acquereurs), method=lm, se=FALSE, formula = y ~ x) +
  facet_grid(`Periode`~CSP_acquereurs, scales="free") +
  # geom_text(aes(x = 25, y = 300, label = lm_eqn(lm(SoldeTotal.y ~ SoldeTotal.x, Solde_Marche_Residents))), parse = TRUE)+
  labs(x = "Pourcentage d'acquéreurs sur le marché", y = "Pourcentage de la population résidnete communale ")
# library(GGally)

dev.off()


Marche_Residents<-left_join(mytest_acquereurs, test_residents,by= c("DepCom"="CODGEO","Periode"))
 

ggpairs(Marche_Residents, columns = c(3:14), groupColumn=2)
