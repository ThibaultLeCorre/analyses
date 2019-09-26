library(ggplot2)
library(ggmap)
library(maptools)
library(ggthemes)
library(dplyr)
library(patchwork)
##garder type comune urbanisée pour redrssement et info carreaux
pop_insee_carreau<-BIEN_propre_carro200 [,c(111:ncol(BIEN_propre_carro200))]

pop_insee_carreau<- pop_insee_carreau[!duplicated(pop_insee_carreau$id_1), ]
pop<-pop_insee_carreau[,c("DEPCOM","comurb2012")]
pop<-pop[!duplicated(pop$DEPCOM), ]
orginals_data_BIEN<- left_join(orginals_data_BIEN,pop, by= c("insee"="DEPCOM"))
#attention patchwork rentre en conflit avec d'autres package précédents
table (orginals_data_BIEN$comurb2012)


devtools::install_github("thomasp85/patchwork")
View(orginals_data_BIEN[,100:134])
table(orginals_data_BIEN$NBRPIECE)

#histogramme par pièces
distrib_pieces <- orginals_data_BIEN %>%
  bind_rows(orginals_data_BIEN %>% filter(comurb2012=="1"))


distrib_pieces$REQTYPBIEN<- ifelse(distrib_pieces$REQTYPBIEN=="AP", "Appartements", 
                                   ifelse(distrib_pieces$REQTYPBIEN=="MA", "Maisons", NA))

distrib_pieces<-distrib_pieces%>% filter(NBRPIECE<=10)
ggplot(distrib_pieces, aes(x=NBRPIECE, fill=factor(REQTYPBIEN))) + 
  geom_bar()+
  scale_x_continuous(limits =c(0, 10), breaks = seq(1:10))+
  scale_fill_discrete(name = "Types de logements")+
  # scale_fill_manual(values=palette_1_colors) +
  theme_tmd() + 
  labs(x="Nombre de pièces", y="Nombre de transactions", title="Nombre de pièces des logements en transaction",
       subtitle="Effectif total renseigné : 875476 logements. Effectif redressé  : 1546623 logements \n796 logements de l'effectif renseigné >= 10 pièces (max=74)")
       # , 
       # caption="Source: BIEN. Réalisation T.Le Corre. UMR Géographie-Cités, 2017"



 

data_redresse<- orginals_data_BIEN %>%
  bind_rows(orginals_data_BIEN %>% filter(comurb2012=="1"))

App_BIEN<-data_redresse%>% 
  filter(REQTYPBIEN=="AP", REQ_PM2>=1)

Appartement_value_hist <-ggplot(App_BIEN, aes(x=REQ_PM2, fill=factor(REQ_ANC))) + 
  geom_histogram(binwidth=50, bins= 50)+
  xlim(xlim = c(-0, 15000)) +
    ylim(ylim = c(-0, 15000)) +
  # scale_fill_manual(values=palette_1_colors) +
  theme_tmd() + 
  labs(x="Prix au m²", y="Nombre de transactions avec redressement\nsur des intervalles de 50 euros au m²", title="Ensemble des appartements franciliens",
       subtitle="Prix en valeur nominale (1996 - 2012)\n602 transactions avant redressement > 15000 euros au m²"
       # , 
       # caption="Source: BIEN. Réalisation T.Le Corre. UMR Géographie-Cités, 2017"
       ) 


App_BIEN_segments<-data_redresse%>%
  filter(REQTYPBIEN=="AP", REQ_PM2>=1, NBRPIECE==2 | NBRPIECE==3)

Appartement_2_3pieces_value_hist <- 
  ggplot(App_BIEN_segments, aes(x=REQ_PM2, fill=factor(REQ_ANC))) + 
  geom_histogram(binwidth=50, bins= 50,show.legend=F)+
  xlim(xlim = c(-0, 15000)) +
    ylim(ylim=c(0,15000))+
  # scale_fill_manual(values=palette_1_colors) +
  theme_tmd() + 
  labs(x="Prix au m²", title="Appartements de deux et trois pièces",
       subtitle="190 transactions avant redressement > 15000 euros au m²", 
       caption="Source: BIEN. Réalisation T.Le Corre. UMR Géographie-Cités, 2017") 

histogramme_appartements_redresse<-Appartement_value_hist  +  Appartement_2_3pieces_value_hist 
 ggsave("histogramme_appartements_redresse.pdf", histogramme_appartements_redresse, width = 10, height = 5, device = "pdf")




#valeur interquartil#
ecart_interquart<- orginals_data_BIEN%>% 
  group_by(annee.x, REQTYPBIEN)%>%
 summarise(Ecart_PM2=(quantile(x = REQ_PM2,c(0.75), na.rm=T)-quantile(x = REQ_PM2,c(0.25), na.rm=T)) / quantile(x = REQ_PM2,c(0.5), na.rm=T),
           Ecart_Prix=(quantile(x = REQ_PRIX,c(0.75), na.rm=T)-quantile(x = REQ_PRIX,c(0.25), na.rm=T)) / quantile(x = REQ_PRIX,c(0.5), na.rm=T))



ecart_interquart_segment_maison<- MA_BIEN_segments%>% 
  group_by(annee.x)%>%
  summarise(ecart_segment_maison=(quantile(x = REQ_PRIX,c(0.75), na.rm=T)-quantile(x = REQ_PRIX,c(0.25), na.rm=T)) / quantile(x = REQ_PRIX,c(0.5), na.rm=T))


ecart_interquart_segment_appartement<- App_BIEN_segments%>% 
  group_by(annee.x)%>%
  summarise(ecart_segment_appartement=(quantile(x = REQ_PM2,c(0.75), na.rm=T)-quantile(x = REQ_PM2,c(0.25), na.rm=T)) / quantile(x = REQ_PM2,c(0.5), na.rm=T))

ecart_interquart_segment_appartement$REQTYPBIEN_segment_app<- "Segment appartements"
ecart_interquart_segment_maison$REQTYPBIEN_segment<- "Segment maisons"

ecart_interquart_segment<- full_join(ecart_interquart_segment_appartement, ecart_interquart_segment_maison, by= "annee.x")

ecart_interquart$ecart<- ifelse(ecart_interquart$REQTYPBIEN=="AP", ecart_interquart$Ecart_PM2,
                                ifelse(ecart_interquart$REQTYPBIEN=="MA", ecart_interquart$Ecart_Prix, NA ))

ecart_interquart$REQTYPBIEN<- ifelse(ecart_interquart$REQTYPBIEN=="AP", "Appartements",
                                ifelse(ecart_interquart$REQTYPBIEN=="MA", "Maisons", NA ))

ecart_interquart_plot<-ggplot(ecart_interquart, aes(x=annee.x, y=ecart, group=REQTYPBIEN, fill=REQTYPBIEN)) + 
  geom_line(aes(color=REQTYPBIEN,show.legend =F), size=1.5)+
  scale_x_continuous(breaks = c(1996,1999,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd()+
  labs(title = "Evolution des écarts interquartiles relatifs des prix immobiliers", 
       subtitle = "Calculée sur la base des prix au m² pour les appartements, des prix absolus pour les maisons", x= NULL, y= NULL)+ 
  labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2017")


# Violin plotR
?geom_boxplot
violon_plot_appartements <- ggplot(App_BIEN, aes(x=factor(annee.x), y=REQ_PM2)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), fill="#F8766D") +
  # xlab("Année") + ylab("Prix au m²") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(0,15000)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
   stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Prix au m²",title="Prix nominaux au m²\ndes appartements en Île-de-France",
       subtitle="1198 transactions avec redressement écartées de la visualisation (>15000 euros au m²)\nLa moyenne est représentée par les points bleus\nLa médiane par les points rouges\nLes déciles extrêmes sont représentés par les traits en gris")
       # caption="Source: BIEN\n réalisation : T.Le Corre, UMR Géographie-Cités, 2017")

violon_plot_appartements_segment<- ggplot(App_BIEN_segments, aes(x=factor(annee.x), y=REQ_PM2)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9),fill="#F8766D") +
  # xlab("Année") + ylab("Prix au m²") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(0,15000)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y=NULL,
       title="Prix nominaux au m²\ndu segment des appartements\nde 2 et 3 pièces en Île-de-France",
       subtitle="190 transactions écartées de la visualisation (>15000 euros au m²)",
       # \nLa moyenne est représentée par les points bleus, la médiane par les points rouges ",
       caption="Source: BIEN\n réalisation sous R avec le package ggplot2\n T.Le Corre, UMR Géographie-Cités, 2017")


violon_plot_maisons <- ggplot(MA_BIEN, aes(x=factor(annee.x), y=REQ_PRIX)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), fill="#00BFC4") +
  # xlab("Année") + ylab("Prix au m²") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(0,1000000)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y="Prix au m²", title="Prix nominaux absolus\ndes maisons en Île-de-France",
       subtitle="2622 transactions écartées de la visualisation (>1,000,000 euros)\nLa moyenne est représentée par les points bleus\nLa médiane par les points rouges\nLes déciles extrêmes sont représentés par les traits en gris")
       # caption="Source: BIEN\n réalisation sous R avec le package ggplot2\n T.Le Corre, UMR Géographie-Cités, 2017")

violon_plot_maisons_segment<- ggplot(MA_BIEN_segments, aes(x=factor(annee.x), y=REQ_PRIX)) + 
  geom_violin(color = "grey50", draw_quantiles = c(0.1,0.9), fill="#00BFC4") +
  # xlab("Année") + ylab("Prix au m²") + 
  geom_boxplot(width=0.1,outlier.colour=NA)+
  ylim(0,1000000)+
  stat_summary(fun.y=mean, geom="point", size=1, colour="blue") +
  stat_summary(fun.y=median, geom="point", size=1, color="red")+
  theme_tmd()+
  theme(legend.position="none")+
  labs(x=NULL,y=NULL,title="Prix nominaux absolus\ndu segment des maisons de 4 et 5 pièces\nen Île-de-France",
       subtitle="298 transactions écartées de la visualisation (>1,000,000 euros)",
       caption="Source: BIEN\n réalisation sous R avec le package ggplot2\n T.Le Corre, UMR Géographie-Cités, 2017")
  


violon_plot_appartements + violon_plot_appartements_segment 
violon_plot_maisons+violon_plot_maisons_segment 
