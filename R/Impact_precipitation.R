
library(raster)
library(rgdal)
library(stringr)
library(rasterVis)
library(lubridate)
library(latticeExtra)
library(maptools)

#library(ggplot2)
#rm(list=ls())
basepath='C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\Data\\climate\\'
shp=readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\BFA_adm\\BFA_adm2.shp")

#read netcdf as a raster brick
nc.brick <- brick(file.choose())
#x=crop(nc.brick,extent(shp))
#show dimensions of the raster brick
dim(nc.brick)
#dim(x)
#read the first layer in the brick as a data frame
nc.df <- as.data.frame(nc.brick[[dim(nc.brick)]], xy=F)

#xy <- as.data.frame(nc.brick[[1]], xy=T)[1:2]


wkt=seq(as.Date('1983/01/01'),as.Date('2021/11/30'),by='day')
l5=which(wkt %in% seq(as.Date('2020/04/19'),as.Date('2020/04/26'),by='day'))
l5_raster= subset(nc.brick,l5)
x11();levelplot(l5_raster)


breakpoints=c(-Inf,seq(0,20,5),Inf)
my.brks=c(1:length(breakpoints))

myColorkey = list(at=my.brks, labels=list(cex=1.3,at=my.brks[-c(1,length(my.brks))], labels=c(breakpoints[2:(length(breakpoints)-1)])), space="bottom", height=0.98)

mapTheme = rasterTheme(panel.background=list(col="white"),region = colorRampPalette(c('white', "lightblue","blue","darkblue"))(length(breakpoints)-1))

btw=list(x=2,y=2) 
y.scale=list(at=seq(9,16,2), cex=1.3)
x.scale=list(at=seq(-6,3,2), cex=1.3)
png(paste0(basepath,"Burkina_day_precip.png"), height=800, width=900, type="cairo")
P=levelplot(mask(l5_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=c('a','b','c','d','e','f','g','h'),  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(4,2))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


t=seq(as.Date('2020/04/19'),as.Date('2020/04/26'),by='day')
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='yacouba'
gra=function(name,t){
  png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
  P=levelplot(mask(l5_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(4,2))
  P+layer(sp.polygons(shp,lwd=1,col="grey30"))
  dev.off()
}


gra(name =name ,t)







t=seq(as.Date('2020/04/19'),as.Date('2020/04/26'),by='day')
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='yacouba'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l5_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(4,2))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()













####################################################
l6=which(wkt %in% seq(as.Date('2020/08/25'),as.Date('2020/09/10'),by='day'))
l6_raster= subset(nc.brick,l6)
#levelplot(l6_raster)

t=seq(as.Date('2020/08/25'),as.Date('2020/09/10'),by='day')
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='line6'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l6_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(5,4))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()
###################################################


l7=which(wkt %in% seq(as.Date('2018/07/24'),as.Date('2018/08/28'),by='day'))
l7_raster= subset(nc.brick,l7)

t=seq(as.Date('2018/07/24'),as.Date('2018/08/28'),by='day')
t=format(as.POSIXct(t), format = "%M:%D")

length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[00:]","")
name='line_7'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l7_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(6,6))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()
###################################

#l7=which(wkt %in% seq(as.Date('2018/07/24'),as.Date('2018/08/28'),by='day'))
#l7_raster= subset(nc.brick,l7)
#x11();levelplot(l7_raster)



l8=which(wkt %in% seq(as.Date('2017/05/01'),as.Date('2017/05/26'),by='day'))
l8_raster= subset(nc.brick,l8)

t=seq(as.Date('2017/05/01'),as.Date('2017/05/26'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")

length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='line_8'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l8_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(6,4))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()
#####################################################################


l9=which(wkt %in% seq(as.Date('2017/07/01'),as.Date('2017/08/31'),by='day'))
l9_raster= subset(nc.brick,l9)
t=seq(as.Date('2017/07/01'),as.Date('2017/08/31'),by='day')
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='line_9'
png(paste0(basepath,name,'.png'), height=1000, width=900, type="cairo")
P=levelplot(mask(l9_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(8,8))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

############################

l10=which(wkt %in% seq(as.Date('2016/05/13'),as.Date('2016/05/14'),by='day'))
l10_raster= subset(nc.brick,l10)
#x11();levelplot(l10_raster)

t=seq(as.Date('2016/05/13'),as.Date('2016/05/14'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")

length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='line_10'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l10_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(2,1))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()
#######################




l11=which(wkt %in% seq(as.Date('2016/06/21'),as.Date('2016/07/16'),by='day'))
l11_raster= subset(nc.brick,l11)
t=seq(as.Date('2016/06/21'),as.Date('2016/07/16'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='line_11'
png(paste0(basepath,name,'.png'), height=1000, width=900, type="cairo")
P=levelplot(mask(l11_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(5,6))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l12=which(wkt %in% seq(as.Date('2015/08/04'),as.Date('2015/08/07'),by='day'))
l12_raster= subset(nc.brick,l12)
t=seq(as.Date('2015/08/04'),as.Date('2015/08/07'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='line_12'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l12_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(2,2))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()



l13=which(wkt %in% seq(as.Date('2013/08/15'),as.Date('2013/08/17'),by='day'))
l13_raster= subset(nc.brick,l13)
t=seq(as.Date('2013/08/15'),as.Date('2013/08/17'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='line_13'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l13_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(3,1))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()



l14=which(wkt %in% seq(as.Date('2012/06/15'),as.Date('2012/09/05'),by='day'))
l14_raster= subset(nc.brick,l14)
x11();levelplot(l14_raster)


l15=which(wkt %in% seq(as.Date('2011/07/18'),as.Date('2011/07/18'),by='day'))
l15_raster= subset(nc.brick,l15)
t=seq(as.Date('2011/07/18'),as.Date('2011/07/18'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='line_15'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l15_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(1,1))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l16=which(wkt %in% seq(as.Date('2010/07/21'),as.Date('2010/09/30'),by='day'))
l16_raster= subset(nc.brick,l16)
x11();levelplot(l16_raster)



l17=which(wkt %in% seq(as.Date('2009/08/15'),as.Date('2009/08/15'),by='day'))
l17_raster= subset(nc.brick,l17)
t=seq(as.Date('2009/08/15'),as.Date('2009/08/15'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='line_17'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l17_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(1,1))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()




l18=which(wkt %in% seq(as.Date('2009/09/1'),as.Date('2009/09/1'),by='day'))
l18_raster= subset(nc.brick,l18)
t=seq(as.Date('2009/09/1'),as.Date('2009/09/1'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[-]","/")
name='line_18'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l18_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(1,1))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()

l19=which(wkt %in% seq(as.Date('2009/06/1'),as.Date('2009/07/31'),by='day'))
l19_raster= subset(nc.brick,l19)
x11();levelplot(l19_raster)

l20=which(wkt %in% seq(as.Date('2008/08/1'),as.Date('2008/09/30'),by='day'))
l20_raster= subset(nc.brick,l20)
x11();levelplot(l20_raster)

l21=which(wkt %in% seq(as.Date('2008/07/1'),as.Date('2008/07/31'),by='day'))
l21_raster= subset(nc.brick,l21)
t=seq(as.Date('2008/07/1'),as.Date('2008/07/31'),by='day')
t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[00:]","")
name='line_21'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l21_raster, shp),layout=c(6,6),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()



l22=which(wkt %in% seq(as.Date('2007/04/20'),as.Date('2007/04/20'),by='day'))
l22_raster= subset(nc.brick,l22)
#x11();levelplot(l22_raster) #orage et foudre
t=seq(as.Date('2007/04/20'),as.Date('2007/04/20'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "","")
name='line_22'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l22_raster, shp),layout=c(1,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l23=which(wkt %in% seq(as.Date('2007/07/26'),as.Date('2007/10/10'),by='day'))
l23_raster= subset(nc.brick,l23)
x11();levelplot(l23_raster) 



l24=which(wkt %in% seq(as.Date('2006/09/12'),as.Date('2006/09/12'),by='day'))
l24_raster= subset(nc.brick,l24)
t=seq(as.Date('2006/09/12'),as.Date('2006/09/12'),by='day')
#t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "","")
name='line_24'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l24_raster, shp),layout=c(1,1),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()



l25=which(wkt %in% seq(as.Date('2006/08/03'),as.Date('2006/10/11'),by='day'))
l25_raster= subset(nc.brick,l25)
t=seq(as.Date('2006/08/03'),as.Date('2006/10/11'),by='day')
t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[00:]","")
name='line_25'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l25_raster, shp),layout=c(7,10),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="")
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off() 


 


l26=which(wkt %in% seq(as.Date('2003/08/10'),as.Date('2003/10/19'),by='day'))
l26_raster= subset(nc.brick,l26)
t=seq(as.Date('2003/08/10'),as.Date('2003/10/19'),by='day')
t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[00:]","")
name='line_26'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l26_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(7,11))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off() 



l27=which(wkt %in% seq(as.Date('1999/08/1'),as.Date('1999/08/31'),by='day'))
l27_raster= subset(nc.brick,l27)
t=seq(as.Date('1999/08/1'),as.Date('1999/08/31'),by='day')
t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[00:]","")
name='line_27'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l27_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(6,6))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off() 


l29=which(wkt %in% seq(as.Date('1988/08/1'),as.Date('1988/08/31'),by='day'))
l29_raster= subset(nc.brick,l29)
t=seq(as.Date('1988/08/1'),as.Date('1988/08/31'),by='day')
t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[00:]","")
name='line_29'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l29_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(6,6))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l30=which(wkt %in% seq(as.Date('1985/06/11'),as.Date('1985/06/11'),by='day'))
l30_raster= subset(nc.brick,l30)
t=seq(as.Date('1985/06/11'),as.Date('1985/06/11'),by='day')
t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "/","/")
name='line_30'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l30_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(1,1))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off()


l31=which(wkt %in% seq(as.Date('1984/10/1'),as.Date('1984/10/31'),by='day'))
l31_raster= subset(nc.brick,l31)
t=seq(as.Date('1984/10/1'),as.Date('1984/10/31'),by='day')
t=format(as.POSIXct(t), format = "%M:%D")
length(t)
t=str_replace_all (str_replace_all(t, "-","/"), "[00:]","")
name='line_31'
png(paste0(basepath,name,'.png'), height=800, width=900, type="cairo")
P=levelplot(mask(l31_raster, shp),main =" Flood for each day" , colorkey=myColorkey,margin = F,scales=list(alternating=1),names.attr=t,  between=btw, par.strip.text=list(cex=1.8), par.settings=mapTheme, at=breakpoints, xlab="",ylab="",layout=c(6,6))
P+layer(sp.polygons(shp,lwd=1,col="grey30"))
dev.off() 




#l32=which(wkt %in% seq(as.Date('1977/9/1'),as.Date('1977/9/30'),by='day'))
#l32_raster= subset(nc.brick,l32)
#x11();levelplot(l32_raster) 


names(nc.df)
#examine the data frame
head(nc.df,10)
nc.df
write.csv(nc.df, file.choose())


dta=as.data.frame(matrix(nrow=1,ncol=45308 ))
wakat=as.data.frame(matrix(nrow=1,ncol=4)); colnames(wakat)=c("date","year","month","day")
data=cbind(wakat,dta)
View(data)


dim(nc.brick)[3]
##################
#nc.brick=x
#x=as.data.frame(x)

for (cpt in 11689:12054) {
  nc.df <- as.data.frame(nc.brick[[cpt]], xy=F)
  
  Time=as.Date( str_replace_all (str_replace_all(names(nc.df), "X",""), "[.]","/"))
  
  Temps=as.data.frame(matrix(nrow=length(Time),ncol=4)); colnames(Temps)=c("date","year","month","day")
  Temps$date=Time
  Temps$year=year(Temps$date)
  Temps$month=month(Temps$date)
  Temps$day=day(Temps$date)

  #q=as.vector(nc.df)
  #q=as.data.frame(q)
  q=as.data.frame(t(nc.df))
  q=cbind(Temps,q)
  data=rbind(data,q)
  
  
}
#D_2015=data
#D_2016_2021
#D_2005_2009
#tout=dttt
View(data)

#write.csv(data, file.choose())
#  
#        Save data
#
write.csv(dttt, here::here("C:/Users/Yacou/Desktop/Yacouba_New/Stage_Climate_Centre/Data/climate/","data.csv"),row.names = FALSE )
save(dttt,file="C:/Users/Yacou/Desktop/Yacouba_New/Stage_Climate_Centre/Data/climate/data.Rdata")

save(D_2016_2021,file="C:/Users/Yacou/Desktop/Yacouba_New/Stage_Climate_Centre/Data/climate/D_2016_2021.Rdata")

#dttt=data
#important=dttt
#ed_exp4 <- subset(education, Region == 2, select = c("State","Minor.Population","Education.Expenditures"))

#===================================================
l3 <- subset(D_2016_2021, year == 2021 & month%in%8:9)
l4 <- subset(D_2016_2021, year == 2020 & month==6)

l5= subset(D_2016_2021, year == 2020 & month==4 & day %in% 19:26)
View(l5)

l5_raster=rasterFromXYZ(cbind(xy,t(l5[-c(1:4)])))

l6= subset(D_2016_2021, year == 2020 & month==8 & day %in% 25:31)
l6_2 = subset(D_2016_2021, year == 2020 & month==9 & day %in% 1:10)

l7= subset(D_2016_2021, year == 2018 & month==7 & day %in% 24:31)
l7_2 = subset(D_2016_2021, year == 2018 & month==8 & day %in% 1:28)

l8= subset(D_2016_2021, year == 2017 & month==5 & day %in% 1:26)
l9= subset(D_2016_2021, year == 2017 & month%in%7:8)

l10= subset(D_2016_2021, year == 2016 & month==5 & day %in% 13:14)
l11= subset(D_2016_2021, year == 2016 & month==6 & day %in% 21:31)
l11_2 = subset(D_2016_2021, year == 2016 & month==7 & day %in% 1:16)

l12 = subset(D_2015, year == 2015 & month==8 & day %in% 4:7)

l13 = subset(dttt, year == 2013 & month==8 & day %in% 15:17)

l14= subset(dttt, year == 2012 & month==6 & day %in% 15:31)
l14_1= subset(dttt, year == 2012 & month==7)
l14_2 = subset(dttt, year == 2012 & month==9 & day %in% 1:5)

l15= subset(dttt, year == 2011 & month==7 & day==18)


l16= subset(dttt, year == 2010 & month==7 & day %in% 21:31)
l16_2 = subset(dttt, year == 2010 & month %in% 8:9)

l17= subset(dttt, year == 2009 & month==8 & day %in% 8:15)
l18= subset(dttt, year == 2009 & month==9 & day==1)
l19= subset(dttt, year == 2009 & month %in% 6:7)

l20= subset(dttt, year == 2008 & month %in% 7:9) #l21 also

l23= subset(dttt, year == 2007 & month==7 & day %in% 26:31)
l23_1= subset(dttt, year == 2007 & month %in% 8:9)
l23_2= subset(dttt, year == 2007 & month==10 & day %in% 1:10)

l24= subset(dttt, year == 2006 & month==9 & day ==12)
l25= subset(dttt, year == 2006 & month %in% 8:9)
l25_1= subset(dttt, year == 2006 & month==10 & day %in% 1:11)


l26= subset(dttt, year == 2003 & month==8 & day %in% 10:31)
l26_1= subset(dttt, year == 2003 & month==9)
l26_2= subset(dttt, year == 2003 & month==10 & day %in% 1:19)

l27= subset(dttt, year == 1999 & month==8)

#l28= subset(dttt, year == 1994)

l29= subset(dttt, year == 1988 & month==8)

l30= subset(dttt, year == 1985 & month==6 & day==11)

l31= subset(dttt, year == 1984 & month==10)

#l32= subset(dttt, year == 1977 & month==9)


View(l30)

c=rbind(l31,l30,l29)
View(ok)
ok=rbind(l3,l4,l5,l6,l6_2,l7 ,l7_2,l8,l9,l10,l11,l11_2,l12,l13,l14,l14_1,l14_2,l15,l16,l16_2,l17,l18,l19 ,l20 ,l23,l23_1,l23_2,l24,l25,l25_1,l26,l26_1,l26_2,l27,l29,l30,l31,l32)
DATA=ok(-c('date'))





