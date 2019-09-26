PTZ<- BASE_PTZ_2016%>% 
  select (cins,iden,ccsp,vtto,vtpr,vtpz,vtpp,an,dept,tegp,age)
PTZ<-PTZ %>% filter ( vtpr<450000 & vtpr>vtpp+vtpz, vtto>762 & vtto<350000 ,ccsp!=10 & ccsp!=11 & ccsp!=12 & ccsp!=13 )
# dept==75 |dept==91 | dept==92| dept==93 | dept==95 | dept==95 | dept==77 | dept==78   )
PTZ<-PTZ %>% filter (tegp<16, 
                     ccsp!=10 & ccsp!=11 & ccsp!=12 & ccsp!=13 ,
                     dept==75 |dept==91 | dept==92| dept==93 | dept==95 | dept==95 | dept==77 | dept==78   )%>%
  filter( dept==75 |dept==91 | dept==92| dept==93 | dept==94 | dept==95 | dept==77 | dept==78 , vtto<450000, vtpp>762 & vtpp<350000 )%>%
  filter(an==1996 | an==1999 |an>=2003 & an<=2012)


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
                                        ifelse(PTZ$ccsp >= 70 & PTZ$ccsp < 80, "retraites", NA)))))

  
  PTZ$age<-
    ifelse(PTZ$ccsp== "retraites", 60, PTZ$age )
  PTZ$age<- ifelse(PTZ$age>=18 & PTZ$age<30, "[18,30[",
                   ifelse(PTZ$age>=30 & PTZ$age<50, "[30,50[",
                          ifelse( PTZ$age>=50, "[50+", NA)))


# taux d'LTV'
PTZ$LTV<- (PTZ$vtpr /PTZ$vtto) *100
PTZ<- PTZ %>% filter(LTV<120)
# Box plot LTV

# ggplot(data = PTZ, aes(x=ccsp, y=LTV)) + 
 


LTV_PTZ_IDF<-as.data.frame(PTZ)%>%
  group_by(ccsp,an )%>%
  filter(!is.na(ccsp),an==1996 | an==1999 |an>=2003, !is.na(age))%>%
  ggplot(., aes(ccsp , LTV, fill=ccsp)) +
  geom_boxplot(aes(fill=ccsp),outlier.shape = NA) + facet_grid( age~ an, scales="fix")+
scale_color_manual(values = specificCol ) +
  scale_fill_manual(values= specificCol)+
   theme_tmd() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
  labs(title = "Boxplot du LTV total des ménages franciliens qui ont acquis un bien avec un PTZ", x= NULL , y= "LTV")+ 
  labs(subtitle = NULL)+
  labs(caption = "Sources : SGFGAS ; Réalisation : Thibault Le Corre, UMR Géographie-cités, 2018")
  
  setwd("~/Projets/PTZ/PTZ")
  ggsave("LTV_PTZ_IDF.pdf",plot= LTV_PTZ_IDF, device = "pdf", width = 380, height = 450, units = "mm", dpi = 330)
  ggsave("LTV_PTZ_IDF.png",plot= LTV_PTZ_IDF, device = "png", width = 380, height = 400, units = "mm", dpi = 330)
  
  
  
  # summarise(Capacite = mean(Capacite_total))%>%
  # 
  # geom_line()+
  # geom_path(aes(color = CSP))+
  # 
  # # scale_y_continuous(expand = c(0.01, 0),breaks = c(-10000,0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000,140000,150000,160000,170000))+
  # scale_x_continuous(breaks = c(1996, 1999,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)) +
  # theme_tmd() +
