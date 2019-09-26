
PTZ<- BASE_PTZ_2016%>% 
  select (cins,iden,ccsp,tegp, an, dept, dtpp, age)
PTZ<-PTZ %>% filter (tegp<16, 
                     ccsp!=10 & ccsp!=11 & ccsp!=12 & ccsp!=13 ,
                     dept==75 |dept==91 | dept==92| dept==93 | dept==95 | dept==95 | dept==77 | dept==78   )
unique(PTZ$an)

# Classification INSEE  
# 1 : Agriculteurs exploitants
# 2 : Artisans, commer?ants et chefs d'entreprise
# 3 : Cadres et professions intellectuelles sup?rieures
# 4 : Professions Interm?diaires
# 5 : Employ?s
# 6 : Ouvriers
# 7 : Retrait?s
# 8 : Autres personnes sans activit? professionnelle


PTZ$ccsp<-ifelse(PTZ$ccsp >= 30 & PTZ$ccsp < 40, "CPIS",
                       ifelse(PTZ$ccsp >= 40 & PTZ$ccsp < 50, "Prof_intermediaires",
                              ifelse(PTZ$ccsp >= 50 & PTZ$ccsp < 60, "Employes",
                                     ifelse(PTZ$ccsp >= 60 & PTZ$ccsp < 70, "Ouvriers",
                                            ifelse(PTZ$ccsp >= 70 & PTZ$ccsp < 80, "retraites",NA)))))

PTZ$age<-
  ifelse(PTZ$ccsp== "retraites", 60, PTZ$age )
# Box plot TEG

PTZ$age<- ifelse(PTZ$age>=18 & PTZ$age<30, "[18,30[",
                 ifelse(PTZ$age>=30 & PTZ$age<50, "[30,50[",
                        ifelse( PTZ$age>=50, "[50+",
                                ifelse(PTZ$ccsp== "retraites", PTZ$age=="[50+", NA))))




# ggplot(data = PTZ, aes(x=ccsp, y=tegp)) + 
#   geom_boxplot(aes(fill=ccsp),outlier.shape = NA) + facet_wrap( ~ an, scales="free")

TEG<- PTZ%>%
  group_by(ccsp,an,age)%>%
  filter(!is.na(age))%>%
  summarise(TEG_moyen=median(tegp),
            Premier_quartile = quantile(tegp, probs=0.25),
            Dernier_quartile = quantile(tegp, probs=0.75))%>%
  gather("Type", "value",c(4:6))%>%
  ggplot(., aes(x=an, y=value, fill=Type)) + 
  geom_line(aes(color=Type))+
  facet_grid(ccsp~age)+
  theme_tmd() +
  labs(title = "Taux effectif global (TEG) du prêt immobilier principal renseigné dans la base SGFGAS", x= "Année" , y= "Taux")+ 
  labs(caption = "Sources : SGFGAS ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2019")

setwd("~/Projets/PTZ/PTZ")
ggsave("TEG.png",plot= TEG, device = "png", width = 380, height = 180, units = "mm", dpi = 330)


Duree<-PTZ%>%
  group_by(ccsp, an,age)%>%
  filter(!is.na(age))%>%
  summarise(Durrée_moyenne=mean(dtpp),
            Premier_quartile = quantile(dtpp, probs=0.25),
            Dernier_quartile = quantile(dtpp, probs=0.75))%>%
  gather("Type", "value",c(4:6))%>%
  ggplot(., aes(x=an, y=value, fill=Type)) + 
  geom_line(aes(color=Type))+
  facet_grid(ccsp~age)+
  theme_tmd() +
  labs(title = "Nombre de mensualités pour le remboursement du prêt immobilier principal renseigné dans la base SGFGAS", x= "Année" , y= "Nombre de mois")+ 
  labs(caption = "Sources : SGFGAS ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2019")

setwd("~/Projets/PTZ/PTZ")
ggsave("Duree.png",plot= Duree, device = "png", width = 380, height = 180, units = "mm", dpi = 330)
