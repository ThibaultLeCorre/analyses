library(dplyr)
library(tidyr)
PTZ<- BASE_PTZ_2016%>% 
  select (cins,iden,ccsp,vtto,vtpr,vtpz,vtpp,an,dept,tope,age, ctyp,napp,tysu,cepp,cecr,etpp,ccsp, stol, revl)%>%
  filter( dept==75 |dept==91 | dept==92| dept==93 | dept==95 | dept==95 | dept==77 | dept==78 , vtto<450000, vtpp>762 & vtpp<350000 )%>%
  filter(an==1996 | an==1999 | an>=2003 & an<=2012)%>%
# On ne garde que les prêts sur le marché de l'acquisition qui ad onné lieu à une transaction (hors auto-construction)
filter(tope==1|tope==4)

table(PTZ$stol,PTZ$an, PTZ$revl)
PTZ$napp,
table(PTZ$etpp)


PTZ$tope<- ifelse(PTZ$tope==1, "Achat dans le neuf",
                  ifelse(PTZ$tope==4, "Achat dans l'ancien", PTZ$tope))

PTZ$napp<- ifelse(PTZ$napp>=1 & PTZ$napp<=5 , "Prêt social",
                  ifelse(PTZ$napp==6 |PTZ$napp==7, "Prêt libre", "Autre prêt"))

PTZ$tysu<- ifelse(PTZ$tysu==0, "Aucune",
                  ifelse(PTZ$tysu==1, "Hypothèque", 
                         ifelse(PTZ$tysu==2, "Organisme de cautionnement", 
                                ifelse(PTZ$tysu==3, "Caution personnelle", "Autre sureté"))))



PTZ_type_credit<-PTZ %>%
  group_by(an,napp, tope)  %>%
  summarise(n_operation=n()) 

PTZ_type_credit<-PTZ %>%
  group_by(an,napp, tysu)  %>%
  summarise(n_operation=n()) %>%
  filter(napp!="Autre prêt")
PTZ_type_credit$geo<-"France"
PTZ_type_credit_IDF$geo<-"Région Île-de-France"

PTZ_type_credit<-rbind(PTZ_type_credit,PTZ_type_credit_IDF)

# PTZ_type_credit$napp<- ifelse(PTZ_type_credit$napp=="Prêt principal social",1,
#                   ifelse(PTZ_type_credit$napp=="Prêt principal hypothécaire et bancaire libre",2, NA))

PTZ_type_pret_IDF_France_plot<-ggplot(PTZ_type_credit, aes(x=napp, y=n_operation)) +
  geom_bar(stat = "identity", aes(fill=tysu))+
  theme_tmd() +
  facet_grid(geo~an, scale="free")+
labs(title = "Nature des prêts princiaux et des sûretés associées lors d'un achat avec PTZ dans le neuf et dans l'ancien en France et en Île-de-France", 
       subtitle = "L'effectif total concerne 258 033 transactions en accession avec un PTZ en IDF\nL'effectif total concerne 1 484 068transactions en accession avec un PTZ en France\nLes opérations d'auto-construction sont exclues de ce graphique\nLes autres prêts qui accompagnent éventuellement le prêt principal sont exclus\nLe prêt social concerne les prêts principaux réglementés par l'Etat (ex : PC, PAS, PEL, 1%)\nLe prêt libre concerne les prêts octroyés par le secteur bancaire et financier non réglementés (éligibles ou non au marché hypothécaire)", x= "Type de prêt principal", y= "Nombre d'acquisiotn avec PTZ")+ 
  labs(caption = "Sources : SGFGAS ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")
setwd("~/Sauvegarde_figures")
ggsave("PTZ_type_pret_IDF_France_plot.pdf",plot= PTZ_type_pret_IDF_France_plot,setwd("~/Sauvegarde_figures"), device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)





%>%
  spread(napp,n_operation,fill = 0)%>%
  gather(key = "Type", value="Value_type_credit", c(2:5))
PTZ_type_surete<-PTZ %>%
  group_by(an,tysu)  %>%
  summarise(n_operation=n()%>%
  spread(tysu,n_operation,fill = 0)%>%
  gather(key = "Type", value="Value_type_surete", c(2:6))


library(ggplot2)
PTZ_type_logement_plot<-ggplot(PTZ_type_logement, aes(an, Value_type_achat, group=Type_achat)) +
  geom_line(aes(color = Type_achat),size=1)+
  scale_x_continuous(breaks = c(1996,1999,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  theme_tmd() +
  labs(title = "Volume des achats avec PTZ dans le neuf et dans l'ancien en Ile-de-France", 
       subtitle = "L'effectif total concerne 258 033 transactions en accession avec un PTZ\n26 736 PTZ non pris en compte sur ces graphiques concernent des opérations d'auto-construction", x= "Année", y= "Nombre d'acquisiotn avec PTZ")+ 
  labs(caption = "Sources : SGFGAS ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")
ggsave("PTZ_type_logement_plot.pdf",plot= PTZ_type_logement_plot,setwd("~/Sauvegarde_figures"), device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)

#En outre, 26 736 PTZ non pris en compte sur ces graphiques concernent des opérations d'auto-construction 

#Les formes de crédit
PTZ %>%
  group_by(an,)  %>%
  summarise(n_operation=n()) %>%
  spread(tope,n_operation,fill = 0)%>%
  gather(key = "Type_achat", value= "Value_type_achat", c(2:3))


####Fourchette age

PTZ$age<- ifelse(PTZ$age>=18 & PTZ$age<25, "[18,25[",
                                   ifelse(PTZ$age>=25 & PTZ$age<30, "[25,30[",
                                          ifelse(PTZ$age>=30 & PTZ$age<35, "[30,35[",
                                                 ifelse(PTZ$age>=35 & PTZ$age<40, "[35,40[",
                                                        ifelse(PTZ$age>=40 & PTZ$age<45, "[40,45[",
                                                               ifelse(PTZ$age>=45 & PTZ$age<50, "[45,50[",
                                                                      ifelse(PTZ$age>=50 & PTZ$age<55, "[50,55[",
                                                                             ifelse(PTZ$age>=55 & PTZ$age<60, "[55,60[",
                                                                                    ifelse(PTZ$age>=60 , "[60+",PTZ$age)))))))))

# 
PTZ_age<-PTZ %>%
  filter(age!=17)%>%
  group_by(an,age)  %>%
  summarise(n_operation=n())

PTZ_age_plot<-ggplot(PTZ_age, aes(age, n_operation, group=an)) +
  geom_bar(stat = "identity")+
  theme_tmd() +
  facet_wrap(~an)+
  labs(title = "Volume des achats avec PTZ en Ile-de-France selon l'âge de l'emprunteur au sein du ménage", 
       subtitle = "L'effectif total concerne 258 033 transactions en accession avec un PTZ\n26 736 PTZ non pris en compte sur ces graphiques concernent des opérations d'auto-construction", x= "Tranches d'âge", y= "Nombre d'acquisition avec PTZ")+ 
  labs(caption = "Sources : SGFGAS ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")
ggsave("PTZ_age_plot.pdf",plot= PTZ_age_plot,setwd("~/Sauvegarde_figures"), device = "pdf", width = 380, height = 180, units = "mm", dpi = 330)

