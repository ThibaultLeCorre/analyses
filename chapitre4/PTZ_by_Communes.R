library(dplyr)
library(tidyr)
PTZ<- BASE_PTZ_2016%>% 
  select (cins,cpfl,iden,ccsp,vtto,vtpr,vtpz,vtpp,an,dept,tope,age, ctyp,napp,tysu,cepp,cecr,etpp,ccsp, stol, revl)%>%
  filter( dept==75 |dept==91 | dept==92| dept==93 | dept==94 | dept==95 | dept==77 | dept==78 , vtto<450000, vtpp>762 & vtpp<350000 )%>%
  filter(an==1996 | an==1999 | an>=2003 & an<=2012)%>%
  # On ne garde que les prêts sur le marché de l'acquisition qui ad onné lieu à une transaction (hors auto-construction)
  filter(tope==1|tope==4)


PTZ$tope<- ifelse(PTZ$tope==1, "Achat dans le neuf",
                  ifelse(PTZ$tope==4, "Achat dans l'ancien", PTZ$tope))

PTZ$napp<- ifelse(PTZ$napp>=1 & PTZ$napp<=5 , "Prêt social",
                  ifelse(PTZ$napp==6 |PTZ$napp==7, "Prêt libre", "Autre prêt"))

PTZ$tysu<- ifelse(PTZ$tysu==0, "Aucune",
                  ifelse(PTZ$tysu==1, "Hypothèque", 
                         ifelse(PTZ$tysu==2, "Organisme de cautionnement", 
                                ifelse(PTZ$tysu==3, "Caution personnelle", "Autre sureté"))))


PTZ$Periode<-ifelse(PTZ$an>=1996&PTZ$an<=2003, "Periode_96_2003",
                         ifelse(PTZ$an>=2004&PTZ$an<=2007, "Periode_04_2007", 
                                ifelse(PTZ$an>=2008&PTZ$an<=2012, "Periode_08_2012", NA)))


PTZ$cins<-ifelse(PTZ$cins==75056 & PTZ$cpfl>=75101,PTZ$cpfl,PTZ$cins)

PTZ_communes<-PTZ %>%
  group_by(Periode,cins)  %>%
  summarise(n_operation_PTZ=n(),
            Prets_libre=(length(which(napp=="Prêt libre"))/n_operation_PTZ)*100,
            Prets_Sociaux=(length(which(napp=="Prêt social"))/n_operation_PTZ)*100,
            Achat_neuf_PTZ= (length(which(tope=="Achat dans le neuf"))/n_operation_PTZ)*100,
            Achat_ancien_PTZ= (length(which(tope=="Achat dans l'ancien"))/n_operation_PTZ)*100)%>%
  filter(cins!=75056 & cins>75000, n_operation_PTZ>=5 )
PTZ_communes$DepCom<-PTZ_communes$cins

setwd("~/Projets/Realisation_ACP_communes/Realisation_ACP_communes")
write.csv2(x=PTZ_communes,file = "PTZ_communes.csv", row.names=FALSE, fileEncoding = "UTF-8")

















































































# 

PTZ$cpfl<- ifelse(PTZ$cpfl == 75001 , "75101",
                  ifelse(PTZ$cpfl == 75002 , "75102",
                         ifelse(PTZ$cpfl == 75003 , "75103",
                                ifelse(PTZ$cpfl == 75004 , "75104",
                                       ifelse(PTZ$cpfl == 75005 , "75105",
                                              ifelse(PTZ$cpfl == 75006 , "75106",
                                                     ifelse(PTZ$cpfl == 75007 , "75107",
                                                            ifelse(PTZ$cpfl == 75008 , "75108",
                                                                   ifelse(PTZ$cpfl == 75009 , "75109",
                                                                          ifelse(PTZ$cpfl == 75010 , "75110",
                                                                                 ifelse(PTZ$cpfl == 75011 , "75111",
                                                                                        ifelse(PTZ$cpfl == 75012 , "75112",
                                                                                               ifelse(PTZ$cpfl == 75013 , "75113",
                                                                                                      ifelse(PTZ$cpfl == 75014 , "75114",
                                                                                                             ifelse(PTZ$cpfl == 75015 , "75115",
                                                                                                                    ifelse(PTZ$cpfl == 75016 , "75116",
                                                                                                                           ifelse(PTZ$cpfl == 75017 , "75117",
                                                                                                                                  ifelse(PTZ$cpfl == 75018 , "75118",
                                                                                                                                         ifelse(PTZ$cpfl == 75019 , "75119",
                                                                                                                                                ifelse(PTZ$cpfl == 75020 , "75120",PTZ$cpfl))))))))))))))))))))

