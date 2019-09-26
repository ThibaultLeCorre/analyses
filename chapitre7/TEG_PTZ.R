library(dplyr)
library(tidyr)
library(ggplot2)

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


PTZ$ccsp<-
  ifelse(PTZ$ccsp == 10 , "agri_AC",
         ifelse(PTZ$ccsp >= 20 & PTZ$ccsp < 30, "com/art/ets_AC",
                
                ifelse(PTZ$ccsp >= 30 & PTZ$ccsp < 40, "cadres_AC",
                       ifelse(PTZ$ccsp >= 40 & PTZ$ccsp < 50, "prof_inter_AC",
                              ifelse(PTZ$ccsp >= 50 & PTZ$ccsp < 60, "employ_AC",
                                     ifelse(PTZ$ccsp >= 60 & PTZ$ccsp < 70, "ouvrier_AC",
                                            ifelse(PTZ$ccsp >= 70 & PTZ$ccsp < 80, "retraite_AC",
                                                   ifelse(PTZ$ccsp >= 80, "inactifs_AC", PTZ$ccsp ))))))))



# Box plot TEG

PTZ$age<- ifelse(PTZ$age>=18 & PTZ$age<30, "[18,30[",
                                   ifelse(PTZ$age>=30 & PTZ$age<50, "[30,50[",
                                          ifelse( PTZ$age>=50, "[50+",NA)))




ggplot(data = PTZ, aes(x=ccsp, y=tegp)) + 
  geom_boxplot(aes(fill=ccsp),outlier.shape = NA) + facet_wrap( ~ an, scales="free")

PTZ%>%
group_by(ccsp,an,age)%>%
  summarise(Taux_moyen=median(tegp),
Premier_quartile = quantile(tegp, probs=0.25),
Dernier_quartile = quantile(tegp, probs=0.75))%>%
  gather("Type", "value",c(4:6))%>%
  ggplot(., aes(x=an, y=value, fill=Type)) + 
  geom_line(aes(color=Type))+
  facet_grid(ccsp~age)


PTZ%>%
  group_by(ccsp, an,age)%>%
  summarise(Durree_moyen=mean(dtpp),
            Premier_quartile = quantile(dtpp, probs=0.25),
            Dernier_quartile = quantile(dtpp, probs=0.75))%>%
  gather("Type", "value",c(4:6))%>%
  ggplot(., aes(x=an, y=value, fill=Type)) + 
  geom_line(aes(color=Type))+
  facet_grid(ccsp~age)
  

# facet_wrap( ~ an, scales="free")

# coeff variation
 # moyenne
 
  for (year in unique(PTZ$an)) 
    show(year)
{
  TEGMean <-  PTZ%>%group_by(an, ccsp) %>%
  summarise(TEG_mean= mean(tegp))%>%
  arrange(an) 
    }
# visualisation ggline moyenne
TEGmen.plot<-ggplot(data = TEGMean, aes(x=an, y=TEG_mean)) + 
  geom_line(aes(color=ccsp)) 


# calcul coeff variation
# standart deviation
for (year in unique(PTZ$an)) 
  show(year)
{
  TEGStd <-  PTZ%>%group_by(an, ccsp) %>%
    summarise(TEG_sd= sd(tegp))%>%
    arrange(an) 
  }

# coeff variation
TEG<-left_join(TEGMean,TEGStd )
TEG$coef_var<- (TEG$TEG_sd / TEG$TEG_mean) *100

# Plot
par(mfrow=c(2,2))
coefvar.plot<-ggplot(data = TEG, aes(x=an, y=coef_var)) + 
  geom_line(aes(color=ccsp)) 

# coeff var global

for (year in unique(PTZ$an)) 
  show(year)
{
  TEGStd_global <-  PTZ%>%group_by(an) %>%
    summarise(TEG_sd= sd(tegp))%>%
    arrange(an) 
  TEGMean_global <-  PTZ%>%group_by(an) %>%
    summarise(TEG_mean= mean(tegp))%>%
    arrange(an) 
  }
TEG_global<-left_join(TEGMean_global,TEGStd_global )
TEG_global$coef_var<- (TEG_global$TEG_sd / TEG_global$TEG_mean) *100
coefvar_global.plot<-ggplot(data = TEG_global, aes(x=an, y=coef_var)) + 
  geom_line() 
# All plot
grid.arrange(TEGmen.plot,coefvar_global.plot, coefvar.plot,newpage = T)

