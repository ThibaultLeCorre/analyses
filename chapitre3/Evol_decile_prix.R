library(tidyr)

  
  deciles<- c(seq(0.1, 1, 0.10))
  
  Prix_absolus = list(quantile(REQ_PRIX, deciles, na.rm=T)))
  
  evol_decile<- data_redresse%>% 
    filter(REQTYPBIEN=="AP")%>%
    group_by(annee.x)%>% 
    summarise(dec = list(sprintf("%1.0f%%", deciles*100)),
              Prix_m2 = list(quantile(REQ_PM2, deciles, na.rm=T)))%>% 
      unnest %>%
  arrange(annee.x,dec)%>%
    spread(annee.x,Prix_m2)
  
  evol_decile_segment<- App_BIEN_segments%>% 
    group_by(annee.x)%>% 
    summarise(dec = list(sprintf("%1.0f%%", deciles*100)),
              Prix_m2 = list(quantile(REQ_PM2, deciles, na.rm=T)))%>% 
    unnest %>%
    arrange(annee.x,dec)%>%
    spread(annee.x,Prix_m2)
  
  
  
  PRIX_tcam_appartement<- evol_decile%>%
    filter(dec!="100%")%>%
    group_by(dec)%>%
   mutate(`1.TCAM9603`=(((`2003`/`1996`)^(1/8)-(1))*100))%>%
    mutate(`2.TCAM0307`=(((`2007`/`2003`)^(1/5)-(1))*100))%>%
    mutate(`3.TCAM0712`=(((`2012`/`2007`)^(1/6)-(1))*100))
  PRIX_tcam_appartement<-gather(PRIX_tcam_appartement, "variables", "Values", 14:16)
  PRIX_tcam_appartement$Type_marche<- "Marché total"
  
  PRIX_tcam_appartement_segment<- evol_decile_segment%>%
    filter(dec!="100%")%>%
    group_by(dec)%>%
    mutate(`1.TCAM9603`=(((`2003`/`1996`)^(1/8)-(1))*100))%>%
    mutate(`2.TCAM0307`=(((`2007`/`2003`)^(1/5)-(1))*100))%>%
    mutate(`3.TCAM0712`=(((`2012`/`2007`)^(1/6)-(1))*100))
  
  PRIX_tcam_appartement_segment<-gather(PRIX_tcam_appartement_segment, "variables", "Values", 14:16)
  PRIX_tcam_appartement_segment$Type_marche<- "Marché segmenté"

  TCAM_appartement<- rbind(PRIX_tcam_appartement,PRIX_tcam_appartement_segment)
  val_med_total<-TCAM_appartement%>% filter(Type_marche=="Marché total")
  val_med_total<-mean(val_med_total$Values)
  
  TCAM_appartement_9603<- TCAM_appartement%>% filter (variables == "1.TCAM9603")
   
  val_med<-TCAM_appartement_9603%>% filter(Type_marche=="Marché total")
  val_med_9603<-mean(val_med$Values)
   TCAM_appartement_9603.plot<- ggplot(TCAM_appartement_9603,aes(variables, dec, group=Type_marche )) +
    geom_tile(aes(fill=Values))+
    theme_tmd() +
     facet_wrap(~Type_marche, scales="free") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = val_med_total ,  space = "Lab")
  
  
   
  TCAM_appartement_0307<- TCAM_appartement%>% filter (variables == "2.TCAM0307")
  

  val_med<-TCAM_appartement_0307%>% filter(Type_marche=="Marché total")
  val_med_0307<-mean(val_med$Values)
  TCAM_appartement_0307.plot<- ggplot(TCAM_appartement_0307, aes(variables, dec, group=Type_marche )) +
    geom_tile(aes(fill=Values))+
    theme_tmd() +
    facet_wrap(~Type_marche, scales="free") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = val_med_total ,  space = "Lab" )
  
  TCAM_appartement_0712<- TCAM_appartement%>% filter (variables == "3.TCAM0712")
  
  val_med<-TCAM_appartement_0712%>% filter(Type_marche=="Marché total")
  val_med_0712<-mean(val_med$Values)
  TCAM_appartement_0712.plot<- ggplot(TCAM_appartement_0712, aes(variables, dec, group=Type_marche )) +
    geom_tile(aes(fill=Values))+
    theme_tmd() +
    facet_wrap(~Type_marche, scales="free") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = val_med_total ,  space = "Lab" )
   
  
  
  TCAM_appartement_9603.plot + TCAM_appartement_0307.plot +TCAM_appartement_0712.plot 
    #pour les maisons
   evol_decile_maison<- orginals_data_BIEN%>% 
     filter(REQTYPBIEN=="MA")%>%
     group_by(annee.x)%>% 
     summarise(dec = list(sprintf("%1.0f%%", deciles*100)),
               Prix = list(quantile(REQ_PRIX, deciles, na.rm=T)))%>% 
     unnest %>%
     arrange(annee.x,dec)%>%
     spread(annee.x,Prix)
   
   evol_decile_segment<- MA_BIEN_segments%>% 
     group_by(annee.x)%>% 
     summarise(dec = list(sprintf("%1.0f%%", deciles*100)),
               Prix = list(quantile(REQ_PRIX, deciles, na.rm=T)))%>% 
     unnest %>%
     arrange(annee.x,dec)%>%
     spread(annee.x,Prix)
   
   PRIX_tcam_maison<- evol_decile%>%
     filter(dec!="100%")%>%
     group_by(dec)%>%
     mutate(`1.TCAM9603`=(((`2003`/`1996`)^(1/8)-(1))*100))%>%
     mutate(`2.TCAM0307`=(((`2007`/`2003`)^(1/5)-(1))*100))%>%
     mutate(`3.TCAM0712`=(((`2012`/`2007`)^(1/6)-(1))*100))
   PRIX_tcam_maison<-gather(PRIX_tcam_maison, "variables", "Values", 14:16)
   PRIX_tcam_maison$Type_marche<- "Marché total"
   
   PRIX_tcam_maisons_segment<- evol_decile_segment%>%
     filter(dec!="100%")%>%
     group_by(dec)%>%
     mutate(`1.TCAM9603`=(((`2003`/`1996`)^(1/8)-(1))*100))%>%
     mutate(`2.TCAM0307`=(((`2007`/`2003`)^(1/5)-(1))*100))%>%
     mutate(`3.TCAM0712`=(((`2012`/`2007`)^(1/6)-(1))*100))
   PRIX_tcam_maison_segment<-gather(PRIX_tcam_appartement_segment, "variables", "Values", 14:16)
   PRIX_tcam_maison_segment$Type_marche<- "Marché segmenté"
   
   TCAM_maison<- rbind(PRIX_tcam_maison,PRIX_tcam_maison)
   