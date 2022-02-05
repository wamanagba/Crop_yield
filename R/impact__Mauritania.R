library("maps")

library(raster)
library(rgdal)
library(stringr)
library(rasterVis)
library(lubridate)
library(latticeExtra)
library(maptools)
shp=readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\gadm36_MRT_shp\\gadm36_MRT_2.shp")
basepath='C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\mrt\\'

mr.brick <- brick(file.choose())
mr.df <- as.data.frame(mr.brick[[14219]], xy=F)

dim(mr.brick)

#######################################
wkt=seq(as.Date('1983/01/01'),as.Date('2021/12/05'),by='day')
breakpoints=c(-Inf,seq(0,20,5),Inf)
my.brks=c(1:length(breakpoints))

myColorkey = list(at=my.brks, labels=list(cex=1.3,at=my.brks[-c(1,length(my.brks))], labels=c(breakpoints[2:(length(breakpoints)-1)])), space="bottom", height=0.98)

mapTheme = rasterTheme(panel.background=list(col="white"),region = colorRampPalette(c('white', "lightblue","blue","darkblue"))(length(breakpoints)-1))

btw=list(x=2,y=2) 
y.scale=list(at=seq(9,16,2), cex=1.3)
x.scale=list(at=seq(-6,3,2), cex=1.3)
#####################################################

l3=which(wkt %in% seq(as.Date('2020/08/01'),as.Date('2020/08/02'),by='day'))
l3_raster= subset(mr.brick,l3)
t=seq(as.Date('2020/08/01'),as.Date('2020/08/02'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_3'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l3_raster, shp),layout=c(2,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l4=which(wkt %in% seq(as.Date('2020/09/01'),as.Date('2020/09/13'),by='day'))
l4_raster= subset(mr.brick,l4)
t=seq(as.Date('2020/09/01'),as.Date('2020/09/13'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_4'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l4_raster, shp),layout=c(4,4),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l5=which(wkt %in% seq(as.Date('2020/09/01'),as.Date('2020/09/15'),by='day'))
l5_raster= subset(mr.brick,l5)
t=seq(as.Date('2020/09/01'),as.Date('2020/09/15'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_5'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l5_raster, shp),layout=c(5,3),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l6=which(wkt %in% seq(as.Date('2019/08/26'),as.Date('2019/08/27'),by='day'))
l6_raster= subset(mr.brick,l6)
t=seq(as.Date('2019/08/26'),as.Date('2019/08/27'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_6'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l6_raster, shp),layout=c(2,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l7=which(wkt %in% seq(as.Date('2018/09/14'),as.Date('2018/09/15'),by='day'))
l7_raster= subset(mr.brick,l7)
t=seq(as.Date('2018/09/14'),as.Date('2018/09/15'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_7'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l7_raster, shp),layout=c(2,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l8=which(wkt %in% seq(as.Date('2017/05/24'),as.Date('2017/05/24'),by='day'))
l8_raster= subset(mr.brick,l8)
#x11();levelplot(l8_raster) #storm
t=seq(as.Date('2017/05/24'),as.Date('2017/05/24'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_8'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l8_raster, shp),layout=c(1,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l9=which(wkt %in% seq(as.Date('2017/09/04'),as.Date('2017/09/06'),by='day'))
l9_raster= subset(mr.brick,l9)
t=seq(as.Date('2017/09/04'),as.Date('2017/09/06'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_9'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l9_raster, shp),layout=c(3,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()



l10=which(wkt %in% seq(as.Date('2013/08/02'),as.Date('2013/08/2'),by='day'))
l10_raster= subset(mr.brick,l10)
t=seq(as.Date('2013/08/02'),as.Date('2013/08/2'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_10'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l10_raster, shp),layout=c(1,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l11=which(wkt %in% seq(as.Date('2013/08/15'),as.Date('2013/08/15'),by='day'))
l11_raster= subset(mr.brick,l11)
t=seq(as.Date('2013/08/15'),as.Date('2013/08/15'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_11'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l11_raster, shp),layout=c(1,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l12=which(wkt %in% seq(as.Date('2013/08/5'),as.Date('2013/09/25'),by='day'))
l12_raster= subset(mr.brick,l12)
t=seq(as.Date('2013/08/5'),as.Date('2013/09/25'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_12'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l12_raster, shp),layout=c(11,5),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l13=which(wkt %in% seq(as.Date('2011/07/16'),as.Date('2011/07/17'),by='day'))
l13_raster= subset(mr.brick,l13)
t=seq(as.Date('2011/07/16'),as.Date('2011/07/17'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_13'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l13_raster, shp),layout=c(2,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l14=which(wkt %in% seq(as.Date('2010/01/01'),as.Date('2010/01/03'),by='day'))
l14_raster= subset(mr.brick,l14)
t=seq(as.Date('2010/01/01'),as.Date('2010/01/03'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_14'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l14_raster, shp),layout=c(3,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l15=which(wkt %in% seq(as.Date('2010/09/19'),as.Date('2010/09/21'),by='day'))
l15_raster= subset(mr.brick,l15)
t=seq(as.Date('2010/09/19'),as.Date('2010/09/21'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_15'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l15_raster, shp),layout=c(3,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l16=which(wkt %in% seq(as.Date('2009/08/28'),as.Date('2009/08/29'),by='day'))
l16_raster= subset(mr.brick,l16)
t=seq(as.Date('2009/08/28'),as.Date('2009/08/29'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_16'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l16_raster, shp),layout=c(2,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l17=which(wkt %in% seq(as.Date('2007/08/07'),as.Date('2007/09/05'),by='day'))
l17_raster= subset(mr.brick,l17)
t=seq(as.Date('2007/08/07'),as.Date('2007/09/05'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_17'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l17_raster, shp),layout=c(6,5),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l18=which(wkt %in% seq(as.Date('2007/08/31'),as.Date('2007/09/03'),by='day'))
l18_raster= subset(mr.brick,l18)
t=seq(as.Date('2007/08/31'),as.Date('2007/09/03'),by='day')
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_18'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l18_raster, shp),layout=c(2,2),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()




l19=which(wkt %in% seq(as.Date('2006/07/21'),as.Date('2006/07/21'),by='day'))
l19_raster= subset(mr.brick,l19)
x11();levelplot(l19_raster) #storm

l20=which(wkt %in% seq(as.Date('2006/08/02'),as.Date('2006/08/03'),by='day'))
l20_raster= subset(mr.brick,l20)
t=seq(as.Date('2006/08/02'),as.Date('2006/08/03'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_20'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l20_raster, shp),layout=c(2,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l21=which(wkt %in% seq(as.Date('2006/07/22'),as.Date('2006/07/24'),by='day'))
l21_raster= subset(mr.brick,l21)
t=seq(as.Date('2006/07/22'),as.Date('2006/07/24'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_21'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l21_raster, shp),layout=c(3,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l22=which(wkt %in% seq(as.Date('2006/08/03'),as.Date('2006/10/11'),by='day'))
l22_raster= subset(mr.brick,l22)
t=seq(as.Date('2006/08/03'),as.Date('2006/10/11'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_22'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l22_raster, shp),layout=c(10,7),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l23=which(wkt %in% seq(as.Date('2005/08/20'),as.Date('2005/08/21'),by='day'))
l23_raster= subset(mr.brick,l23)
t=seq(as.Date('2005/08/20'),as.Date('2005/08/21'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_23'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l23_raster, shp),layout=c(2,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l24=which(wkt %in% seq(as.Date('2005/09/27'),as.Date('2005/09/30'),by='day'))
l24_raster= subset(mr.brick,l24)
#x11();levelplot(l24_raster)
t=seq(as.Date('2005/09/27'),as.Date('2005/09/30'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_24'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l24_raster, shp),layout=c(2,2),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l25=which(wkt %in% seq(as.Date('2003/08/07'),as.Date('2003/08/20'),by='day'))
l25_raster= subset(mr.brick,l25)
t=seq(as.Date('2003/08/07'),as.Date('2003/08/20'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_25'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l25_raster, shp),layout=c(4,4),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l26=which(wkt %in% seq(as.Date('2002/01/09'),as.Date('2002/01/09'),by='day'))
l26_raster= subset(mr.brick,l26)
t=seq(as.Date('2002/01/09'),as.Date('2002/01/09'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_26'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l26_raster, shp),layout=c(1,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l27=which(wkt %in% seq(as.Date('2001/08/24'),as.Date('2001/08/24'),by='day'))
l27_raster= subset(mr.brick,l27)
t=seq(as.Date('2001/08/24'),as.Date('2001/08/24'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_27'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l27_raster, shp),layout=c(1,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l28=which(wkt %in% seq(as.Date('1999/09/01'),as.Date('1999/09/30'),by='day'))
l28_raster= subset(mr.brick,l28)
t=seq(as.Date('1999/09/01'),as.Date('1999/09/30'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_28'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l28_raster, shp),layout=c(6,5),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l29=which(wkt %in% seq(as.Date('1999/10/09'),as.Date('1999/10/09'),by='day'))
l29_raster= subset(mr.brick,l29)
t=seq(as.Date('1999/10/09'),as.Date('1999/10/09'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_29'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l29_raster, shp),layout=c(1,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l30=which(wkt %in% seq(as.Date('1999/10/16'),as.Date('1999/10/16'),by='day'))
l30_raster= subset(mr.brick,l30)
x11();levelplot(l30_raster) # storm


l31=which(wkt %in% seq(as.Date('1995/11/01'),as.Date('1995/11/30'),by='day'))
l31_raster= subset(mr.brick,l31)
t=seq(as.Date('1995/11/01'),as.Date('1995/11/30'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_31'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l31_raster, shp),layout=c(7,5),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()



l32=which(wkt %in% seq(as.Date('1991/06/02'),as.Date('1991/06/02'),by='day'))
l32_raster= subset(mr.brick,l32)
x11();levelplot(l32_raster) # storm

l33=which(wkt %in% seq(as.Date('1985/07/01'),as.Date('1985/07/31'),by='day'))
l33_raster= subset(mr.brick,l33)
x11();levelplot(l33_raster)


l34=which(wkt %in% seq(as.Date('1984/09/21'),as.Date('1984/09/21'),by='day'))
l34_raster= subset(mr.brick,l34)
t=seq(as.Date('1984/09/21'),as.Date('1984/09/21'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_34'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l34_raster, shp),layout=c(1,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()
