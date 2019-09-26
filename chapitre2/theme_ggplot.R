library(extrafont)
font_import()
y
loadfonts(device="win")
loadfonts()
fonts()  

# loadfonts(device="win")

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


theme_tmd <- function(){
  tmd <-  theme(axis.text.x = element_text(size = 8, angle= 45,),
                axis.text.y = element_text(size = 8),
                legend.title = element_text(size = 0, colour = NA),
                legend.position = 'bottom',
                legend.direction = "horizontal",
                legend.background = element_rect(fill = NA),
                panel.background = element_rect(fill = "white"), 
                plot.background = element_rect(fill = "gray92"),
                panel.grid.major = element_line(colour = "gray92", size = 1),
                legend.text = element_text(size = 10),
                plot.margin = unit(x = c(0.25, 0.25, 0.25, 0.5), units = "cm"),
                plot.caption= element_text(size = 8),
                legend.box.spacing = unit(x = 0.25, units = "cm"),
                legend.margin=margin(t = 0, r = 0, b = -0.2, l = 0,unit = "cm"),
                plot.title=element_text(size =12,face="bold",hjust = 0.5, vjust = 0.5),
                axis.title = element_text(size =10),
                plot.subtitle = element_text(size =10))
}
