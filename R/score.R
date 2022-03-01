library(cowplot)
library(tidyverse)
library(broom)
library(ggpubr)
library(rgdal)
NHSBoards <- readOGR("D:\\Yacouba_New\\climte\\Data\\BFA_adm\\BFA_adm2.shp",use_iconv = TRUE,encoding = "UTF-8")
basepath= 'C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\code\\Crop_yield\\python\\dern\\'
#binary_precip$model
binary_precip= read.table(paste0(basepath, "mil_03.csv"), sep=",", dec=".", header=T)

binary_precip=binary_precip[order(binary_precip$province), ]


#d1=binary_precip[binary_precip$model=='RidgeCV',c('score_test','province')]
#model='RidgeCV'
score=function(model){
  d1=binary_precip[binary_precip$model==model,c('score_test')]
  d1=t(d1)
  d1=round(d1,digits = 2)
  d1=replace(d1, d1<=0, 0)
  NHSBoards_tidy <- tidy(NHSBoards)
  
  
  NHSBoards$id <- row.names(NHSBoards)
  NHSBoards_tidy <- left_join(NHSBoards_tidy, NHSBoards@data)
  
  dr=as.data.frame(NHSBoards@data)
  hospitalsSco <- data.frame(NAME_2  = sort(dr$NAME_2),
                             score = as.numeric(d1))
  NHSBoards_tidy <- left_join(NHSBoards_tidy, hospitalsSco)
  
  
  HBLabel <- NHSBoards_tidy %>%
    group_by(NAME_2) %>%
    summarise(label_long = mean(range(long)), label_lat = mean(range(lat)), score = mean(score))
  
  map <- ggplot(NHSBoards_tidy, aes(x = long, y = lat, group = group, fill = score)) +
    geom_polygon(color = "black", size = 0.1) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none")+
    labs(title = model) +
    theme(plot.title = element_text(margin = margin(t = 40, b = -40)))
  
  
  map=map +geom_text(data = HBLabel, mapping = aes(x = label_long, y = label_lat, label = score, group = NA)
                     , cex = 0, col = "white")
  return(map)
}

setwd(basepath)

RidgeCV=score('RidgeCV')
Lasso=score('Lasso')
RandomForest=score('RandomForest')
DecisionTree=score('DecisionTree')

png("map4_mil.png")
map=plot_grid(RidgeCV,  RandomForest,Lasso,DecisionTree,
          label_size = 11)
x11();map

dev.off()

