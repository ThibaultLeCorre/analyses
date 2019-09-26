

library(ggplot2)

breaks_ratio<-  c(min(Typo_1999_2012_carreaux200_appartement$resid),-1300 , -700, 0, 700 , 1300, max(Typo_1999_2012_carreaux200_appartement$resid))

Typo_1999_2012_carreaux200_appartement$Evol_resid<- ifelse(Typo_1999_2012_carreaux200_appartement$resid<=-1300, "Evol_N1EC",
                                                           ifelse( Typo_1999_2012_carreaux200_appartement$resid>-1300  | Typo_1999_2012_carreaux200_appartement$resid<=-700, "Evol_N05_N1EC",
                                                                   ifelse( Typo_1999_2012_carreaux200_appartement$resid>-700 | Typo_1999_2012_carreaux200_appartement$resid<=0, "Evol_N05_0",
                                                                           ifelse( Typo_1999_2012_carreaux200_appartement$resid>0 | Typo_1999_2012_carreaux200_appartement$resid<=700, "Evol_P0_P05EC",
                                                                                   ifelse( Typo_1999_2012_carreaux200_appartement$resid>700 | Typo_1999_2012_carreaux200_appartement$resid<=1300, "Evol_P05EC_1PEC",
                                                                                           ifelse( Typo_1999_2012_carreaux200_appartement$resid>1300, "Evol_P1EC", NA))))))
#                                                            
#                                                            


cols=carto.pal(pal1 = 'blue.pal', n1 = 3 ,pal2='red.pal', n2=3)

ggplot(Typo_1999_2012_carreaux200_appartement, aes(x=X2012, y=X1999)) + 
  geom_point(aes( color= categorie_finale)) + 
  geom_smooth(method=lm, n=10)+theme_tmd()

ggplot(Prix_annee_potentiel_1000_appartement, aes(x=`2012`, y=`1999`)) + 
  geom_point(aes( color=categorie_variation)) + 
  geom_smooth(method=lm, n=10)

ggplot(Prix_annee_potentiel_200_Maison, aes(x=`2012`, y=`1999`)) + 
  geom_point(aes( color=categorie_finale)) + 
  geom_smooth(method=lm, n=10)

ggplot(Prix_annee_potentiel_1000_Maison, aes(x=`2012`, y=`1999`)) + 
  geom_point(aes( color=categorie_finale)) + 
  geom_smooth(method=lm, n=10)

ggplot(Prix_annee_communes, aes(x=`2012`, y=`1999`)) + 
  geom_point(aes( color=categorie_finale)) + 
  geom_smooth(method=lm, n=100)

ggplot(Prix_annee_communes_maisons, aes(x=`2012`, y=`1999`)) + 
  geom_point(aes( color=categorie_finale)) + 
  geom_smooth(method=lm, n=10)
