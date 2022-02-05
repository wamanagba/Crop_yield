

rm(list=ls())

library(cowplot)
library(tidyverse)
library(rgdal)
NHSBoards <- readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\BFA_adm\\BFA_adm2.shp")
basepath= 'C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\'

# Import aggregated data (years in rows and provinces in columns)
binary_precip= read.table(paste0(basepath, "bur_binaire.csv"), sep=",", dec=".", header=T)
names(binary_precip)= NULL

#glimpse(NHSBoards)

library(broom)


fig=list()

#x11();fig[1]


for (i in 1:39){
  

NHSBoards_tidy <- tidy(NHSBoards)

#x11();ggplot(NHSBoards_tidy, aes(x = long, y = lat, group = group)) +
#  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
#  coord_equal() +
#  theme_minimal()

NHSBoards$id <- row.names(NHSBoards)
NHSBoards_tidy <- left_join(NHSBoards_tidy, NHSBoards@data)

dr=as.data.frame(NHSBoards@data)
hospitalsSco <- data.frame(NAME_2  = sort(dr$NAME_2),
                           Hospitals = as.numeric(binary_precip[i,]))
NHSBoards_tidy <- left_join(NHSBoards_tidy, hospitalsSco)

#cols=colorRampPalette(c("lightgreen", "yellow","orange","red", "darkred"))(length(breakpoints)-1)

s=ggplot(NHSBoards_tidy, aes(x = long, y = lat, group = group, fill = Hospitals,colour= Hospitals)) +
  geom_polygon(color = "black", size = 1) +
  coord_equal() +
  theme_void() +
  #labs(title = paste0("year_", year)) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(margin = margin(t = 40, b = -40)))

fig[[i]]=s
}
var_names <- paste("",1983:2021, sep = "")
library(ggpubr)
x11();ggarrange(plotlist = fig, labels = var_names, nrow = 10,ncol = 4)



####################################################################################
####################################################################################
rm(list=ls())

library(cowplot)
library(tidyverse)
library(rgdal)
NHSBoards <- readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\gadm36_MRT_shp\\gadm36_MRT_2.shp")

basepath= 'C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\'

# Import aggregated data (years in rows and provinces in columns)
binary_precip= read.table(paste0(basepath, "mrt_binaire.csv"), sep=",", dec=".", header=T)
names(binary_precip)= NULL

#glimpse(NHSBoards)

library(broom)


fig=list()

#x11();fig[1]


for (i in 1:39){
  
  
  NHSBoards_tidy <- tidy(NHSBoards)
  
  #x11();ggplot(NHSBoards_tidy, aes(x = long, y = lat, group = group)) +
  #  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  #  coord_equal() +
  #  theme_minimal()
  
  NHSBoards$id <- row.names(NHSBoards)
  NHSBoards_tidy <- left_join(NHSBoards_tidy, NHSBoards@data)
  
  dr=as.data.frame(NHSBoards@data)
  province <- data.frame(NAME_2  = sort(dr$NAME_2),
                             Hospitals = as.numeric(binary_precip[i,]))
  NHSBoards_tidy <- left_join(NHSBoards_tidy, province)
  
  #cols=colorRampPalette(c("lightgreen", "yellow","orange","red", "darkred"))(length(breakpoints)-1)
  
  s=ggplot(NHSBoards_tidy, aes(x = long, y = lat, group = group, fill = Hospitals)) +
    geom_polygon(color = "black", size = 1) +
    coord_equal() +
    theme_void() +
    #labs(title = paste0("year_", year)) +
    theme(legend.position = "none")+
    theme(plot.title = element_text(margin = margin(t = 40, b = -40)))
  
  fig[[i]]=s
}
var_names <- paste("",1983:2021, sep = "")

x11();ggarrange(plotlist = fig, labels = var_names, nrow = 10,ncol = 4)






















x11();plot_grid(fig[1],fig[2],xlabels = c("2000","2001"), ncol = 3, nrow = 5,label_size=11)
x11();xx


n=paste0('x',6)
v=2
v<-get(n)

#par(new=TRUE)
z=ggplot(NHSBoards_tidy, aes(x = long, y = lat, group = group, fill = Hospitals)) +
  geom_polygon(color = "black", size = .11) +
  coord_equal() +
  theme_void() +
  #labs(title = paste0("year_", year)) +
  theme(legend.position = "none")+
  theme(plot.title = element_text(margin = margin(t = 40, b = -40)))
  


var_names <- paste("",1983:2021, sep = "")

x11();mylist

x11();ggarrange(plotlist = fig, labels = var_names, nrow = 10,ncol = 4)

xx <- plot_grid(vv,x,z,x,xlabels = c("2000","2001"), ncol = 3, nrow = 5,label_size=11)
x11();xx
ss=paste0("year_",'')

var_names <- paste("p",1, sep = "")
p_all <- mget(var_names, envir = globalenv(),ifnotfound = xx)
sample <- 10
rolling_means <- lapply(p_all, rollmean, sample)


## test mget
e1 <- new.env()
mget(letters, e1, ifnotfound = as.list(LETTERS))


x1<-1:3 
x2 <- seq(3.5,5.5,1) 
for (i in 1:2) { 
  sx<- (do.call("sin",list(c(get(paste('x',i,sep='',collapse='')))))) 
  cat(sx) 
}
