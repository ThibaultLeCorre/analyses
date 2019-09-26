#valeur interquartil#
#valeur interdecil#
ecart_interquart<- data_redresse%>% 
  group_by(annee.x, REQTYPBIEN)%>%
  summarise(Ecart_PM2=(quantile(x = REQ_PM2,c(0.75), na.rm=T)-quantile(x = REQ_PM2,c(0.25), na.rm=T)) / quantile(x = REQ_PM2,c(0.5), na.rm=T),
            Ecart_Prix=(quantile(x = REQ_PRIX,c(0.75), na.rm=T)-quantile(x = REQ_PRIX,c(0.25), na.rm=T)) / quantile(x = REQ_PRIX,c(0.5), na.rm=T))



ecart_interquart_segments_MA<- data_redresse%>% 
  filter (REQTYPBIEN=="MA"& REQ_PRIX>=1 & NBRPIECE==4 | NBRPIECE==5)
ecart_interquart_segments_AP<- data_redresse%>%
  filter (REQTYPBIEN=="AP"& REQ_PM2>=1 & NBRPIECE==2 | NBRPIECE==3)
ecart_interquart_segments<- bind_rows (ecart_interquart_segments_MA, ecart_interquart_segments_AP)

ecart_interquart_segments<- ecart_interquart_segments%>% 
  group_by(annee.x, REQTYPBIEN)%>%
  summarise(Ecart_PM2=(quantile(x = REQ_PM2,c(0.75), na.rm=T)-quantile(x = REQ_PM2,c(0.25), na.rm=T)) / quantile(x = REQ_PM2,c(0.5), na.rm=T),
            Ecart_Prix=(quantile(x = REQ_PRIX,c(0.75), na.rm=T)-quantile(x = REQ_PRIX,c(0.25), na.rm=T)) / quantile(x = REQ_PRIX,c(0.5), na.rm=T))

# ecart_interquart_segments<- ecart_interquart_segments%>% 
#   REQTYPBIEN = case_when(REQTYPBIEN == "AP" ~ "Appartements 2 ou 3 pièces",
#             REQTYPBIEN == "MA" ~ "Maisons 4 ou 5 pièces")

# App_BIEN_segments<-data_redresse%>%
#   filter(REQTYPBIEN=="AP", REQ_PM2>=1, NBRPIECE==2 | NBRPIECE==3)
# App_BIEN<-data_redresse%>% 
#   filter(REQTYPBIEN=="AP", REQ_PM2>=1)
# MA_BIEN_segments<-data_redresse%>%
#   filter(REQTYPBIEN=="MA", REQ_PRIX>=1, NBRPIECE==4 | NBRPIECE==5)
# MA_BIEN<-data_redresse%>% 
#   filter(REQTYPBIEN=="MA", REQ_PRIX>=1)



ecart_interquart$ecart<- ifelse(ecart_interquart$REQTYPBIEN=="AP", ecart_interquart$Ecart_PM2,
                              ifelse(ecart_interquart$REQTYPBIEN=="MA", ecart_interquart$Ecart_Prix, NA ))


ecart_interquart_segments$ecart<- ifelse(ecart_interquart_segments$REQTYPBIEN=="AP", ecart_interquart_segments$Ecart_PM2,
                                       ifelse(ecart_interquart_segments$REQTYPBIEN=="MA", ecart_interquart_segments$Ecart_Prix, NA ))

ecart_interquart$REQTYPBIEN<- ifelse(ecart_interquart$REQTYPBIEN=="AP", "Appartements",
                                   ifelse(ecart_interquart$REQTYPBIEN=="MA", "Maisons", NA ))



ecart_interquart_segments$REQTYPBIEN<- ifelse(ecart_interquart_segments$REQTYPBIEN=="AP", "Appartements 2 ou 3 pièces",
                                            ifelse(ecart_interquart_segments$REQTYPBIEN=="MA", "Maisons 4 ou 5 pièces", NA ))


ecart_interquart<-bind_rows(ecart_interquart,ecart_interquart_segments)

ecart_interquart$typeVar <- substr(ecart_interquart$REQTYPBIEN, start = 0, stop = 2)
ecart_interquart$blob <- ifelse(ecart_interquart$REQTYPBIEN=="Appartements 2 ou 3 pièces" | ecart_interquart$REQTYPBIEN=="Maisons 4 ou 5 pièces", "Segments", "Marché total")

# ecart_interquart$blob_2 <- ifelse(is.na (ecart_interquart$REQ_ANC), "transactions complètes", "transactions biens neuf et anciens")


          # ecart_interquart_plot<-
          ggplot(ecart_interquart, aes(x=annee.x, y=ecart, group=REQTYPBIEN, fill=REQTYPBIEN)) +
            geom_line(aes(linetype=blob, color=typeVar,show.legend =F), size=1.5)+
            scale_linetype_manual(values=c("solid","dotted"))+
            scale_x_continuous(breaks = c(1996,1999,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
            theme_tmd()+
            
            labs(title = "Evolution des écarts interquartiles relatifs des prix immobiliers", 
                 subtitle = "Calculée sur la base des prix au m² pour les appartements, des prix absolus pour les maisons\nEffectifs redressés", x= NULL, y= NULL)+ 
            labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2017")
          
     #######Neuf et ancien       
            ecart_interquart_neuf_ancien<- data_redresse%>% 
              group_by(annee.x, REQTYPBIEN, REQ_ANC)%>%
              summarise(Ecart_PM2=(quantile(x = REQ_PM2,c(0.75), na.rm=T)-quantile(x = REQ_PM2,c(0.25), na.rm=T)) / quantile(x = REQ_PM2,c(0.5), na.rm=T),
                        Ecart_Prix=(quantile(x = REQ_PRIX,c(0.75), na.rm=T)-quantile(x = REQ_PRIX,c(0.25), na.rm=T)) / quantile(x = REQ_PRIX,c(0.5), na.rm=T))
          
            ecart_interquart_neuf_ancien$ecart<- ifelse(ecart_interquart_neuf_ancien$REQTYPBIEN=="AP", ecart_interquart_neuf_ancien$Ecart_PM2,
                                                        ifelse(ecart_interquart_neuf_ancien$REQTYPBIEN=="MA", ecart_interquart_neuf_ancien$Ecart_Prix, NA ))
            ecart_interquart_neuf_ancien$REQTYPBIEN<- ifelse(ecart_interquart_neuf_ancien$REQTYPBIEN=="AP", "Appartements",
                                                             ifelse(ecart_interquart_neuf_ancien$REQTYPBIEN=="MA", "Maisons", NA ))
            
           
 ecart_interquart_neuf_ancien$typeVar <- paste0(ecart_interquart_neuf_ancien$REQTYPBIEN, ecart_interquart_neuf_ancien$REQ_ANC)
  ecart_interquart_neuf_ancien$blob <- ecart_interquart_neuf_ancien$REQ_ANC
            
            
  ggplot(ecart_interquart_neuf_ancien, aes(x=annee.x, y=ecart, group=typeVar)) +
    geom_line(aes(linetype=REQ_ANC,color=REQTYPBIEN,show.legend =F), size=1.5)+
              scale_linetype_manual(values=c("solid","dotted"))+
              scale_x_continuous(breaks = c(1996,1999,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
              theme_tmd()+
            labs(title = "Evolution des écarts interquartiles relatifs des prix immobiliers des biens neufs et anciens", 
                 subtitle = "Calculée sur la base des prix au m² pour les appartements, des prix absolus pour les maisons\nEffectifs redressés", x= NULL, y= NULL)+ 
              labs(caption = "Sources : BIEN\nRéalisation sous R avec le package ggplot2\nThibault Le Corre, Géographie-Cités, 2017")
            