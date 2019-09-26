#modele ggplot# à appliquer pour toute la these pour famille de police#
#sur word famille century gothic#

# theme_tmd <- function(){
# tmd <- theme(axis.text=element_text(size=6),
#              axis.title=element_text(size=7),
#              strip.text = element_text(size = 7),
#              plot.title=element_text(size =16,family = "Helvetica",face="bold",vjust = 1,hjust = -0.09,margin=margin(b=-15)),
#              legend.text = element_text(size = 7),
#              legend.title = element_text(size = 7),
#              legend.position="top",
#              legend.direction = "horizontal",
#              plot.margin = unit(x = c(0.25, 0.25, 0.25, 0.5), units = "cm"),
#              legend.box.spacing = unit(x = 0.25, units = "cm"),
#              legend.margin=margin(t = 0, r = 0, b = -0.2, l = 0,unit = "cm")
# )
# }

theme_tmd <- function(){
  tmd <-  theme(axis.text.x = element_text(size = 8, angle= 45,family = "Century Gothic"),
    axis.text.y = element_text(size = 8,family = "Century Gothic"),
    legend.title = element_text(size = 0, colour = NA,family = "Century Gothic"),
    legend.position = 'bottom',
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA),
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "gray92"),
    panel.grid.major = element_line(colour = "gray92", size = 1),
    legend.text = element_text(size = 10,family = "Century Gothic"),
    plot.margin = unit(x = c(0.25, 0.25, 0.25, 0.5), units = "cm"),
     plot.caption= element_text(size = 8,family = "Century Gothic"),
    legend.box.spacing = unit(x = 0.25, units = "cm"),
    legend.margin=margin(t = 0, r = 0, b = -0.2, l = 0,unit = "cm"),
    plot.title=element_text(size =12,family = "Century Gothic",face="bold",hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size =10,family = "Century Gothic"),
    plot.subtitle = element_text(size =10,family = "Century Gothic"))
}

# Modèle type :
#   
  famille police : century gothic
library(ggplot2)
library(extrafont)
loadfonts(device="win")
# 
#   sortie figure corps de texte: format large pleine page= 220*150 ; format long pleine page = 150*220
#                                format large 2/3 page= 145.2*99 ; format long 2/3 page = 99*145.2
#                               format large demi page= 110*75 ; format long 2/3 page = 75*110
#  sortie figure encadre: format large pleine page pout figure avec une seule ligne= 220*120 ; format long pleine page = 120*220
#   sortie figure encadre: format large pleine page pout figure avec plusieurs lignes= 220*150 ; format long pleine page = 150*220                 
#                   
#   source et réalisation :
#   labs(caption = "Sources : échantillon BD BIEN ; Réalisation : Thibault Le Corre, Géographie-Cités, 2017")

# install.packages("extrafont")
# library(extrafont)
# font_import()
# loadfonts(device="win")       #Register fonts for Windows bitmap output
# fonts()  


#couleurs

#  morphoCol <- unique(BIEN_comby$blob)
#  names(morphoCol) <- morphoCol
#  specificCol <- c("1.Agriculteurs" = "#E9DF00",     
# "2.Art/com/lib/Chef ets" = "#861388",        
# "3.CPIS"= "#067BC2" ,               
# "4.Prof.intermédiaires"="#5FAD41",
# "5.Employés"= "#FF8811",        
#  "6.Ouvriers"="#FF1D15",   
#  "7.Retraités"= "#77878B",
#  "8.Inactifs"="#8D775F",
#  "Personnes morales"= "#503D3F")

