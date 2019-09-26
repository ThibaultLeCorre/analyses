library(dplyr)
library(tidyr)
coordPop

coordPop<- cbind(SpUnitt_with_cluster, res.acp.df)                


coordPop<-coordPop%>% 
  group_by(Periode, `cah$data.clust$clust`)%>% 
  summarise(CGdim1= mean(Dim.1),
            CGdim2= mean(Dim.2),
            CGdim3= mean(Dim.3),
            CGdim4= mean(Dim.4),
            CGdim5= mean(Dim.5))
coordPop$Periode<-ifelse(coordPop$Periode=="Periode_96_2003",1,
                         ifelse(coordPop$Periode=="Periode_04_2007",2,
                                ifelse(coordPop$Periode=="Periode_08_2012",3, NA)))
# gather(key = "Dimension", value = "Data", CGdim1:CGdim5) 
library(ggplot2)



coordPop[order(coordPop$Periode),]%>%
ggplot(aes(CGdim1, CGdim2, group=`cah$data.clust$clust`))+
  # theme_geometry(coordPop$CGdim1, coordPop$CGdim4,color = `cah$data.clust$clust`)+
  # geom_point(aes(fill=`cah$data.clust$clust`, color= `cah$data.clust$clust`), stat = "identity",size=2)+
  geom_text(aes(label=factor(`cah$data.clust$clust`)))+
  geom_path(aes(color =  `cah$data.clust$clust`, group = `cah$data.clust$clust`),arrow = arrow(type = , angle = 30, length = unit(0.1, "inches")))+
     # theme_tmd()+
  xlim(-5,11) + 
  ylim(-5,6) +
  geom_hline(yintercept=0,) + 
  geom_vline( xintercept=0) 

coordPop[order(coordPop$Periode),]%>%
  ggplot(aes(CGdim3, CGdim4, group=`cah$data.clust$clust`))+
  # theme_geometry(coordPop$CGdim1, coordPop$CGdim4,color = `cah$data.clust$clust`)+
  # geom_point(aes(fill=`cah$data.clust$clust`, color= `cah$data.clust$clust`), stat = "identity",size=2)+
  geom_text(aes(label=factor(`cah$data.clust$clust`)))+
  geom_path(aes(color =  `cah$data.clust$clust`, group = `cah$data.clust$clust`),arrow = arrow(type = , angle = 30, length = unit(0.1, "inches")))+
  # theme_tmd()+
  xlim(-5,11) + 
  ylim(-5,6) +
  geom_hline(yintercept=0,) + 
  geom_vline( xintercept=0) 

coordPop[order(coordPop$Periode),]%>%
  ggplot(aes(CGdim1, CGdim5, group=`cah$data.clust$clust`))+
  # theme_geometry(coordPop$CGdim1, coordPop$CGdim4,color = `cah$data.clust$clust`)+
  # geom_point(aes(fill=`cah$data.clust$clust`, color= `cah$data.clust$clust`), stat = "identity",size=2)+
  geom_text(aes(label=factor(`cah$data.clust$clust`)))+
  geom_path(aes(color =  `cah$data.clust$clust`, group = `cah$data.clust$clust`),arrow = arrow(type = , angle = 30, length = unit(0.1, "inches")))+
  # theme_tmd()+
  xlim(-5,11) + 
  ylim(-5,6) +
  geom_hline(yintercept=0,) + 
  geom_vline( xintercept=0) 
