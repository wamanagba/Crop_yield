####This script calculates some rainfall characteristics

#### Data: CHIRPS, TAMSAT
#### Resolution: 0.25*0.25

rm(list=ls())
############ Load necessary libraries
suppressPackageStartupMessages({
library(sp)
library(rgdal)
#library(geosphere)
library(ncdf4)
library(lubridate)
library(stringr)
#library(tidyr)
#library(ggplot2)
#library(caTools)
#library(RMAWGEN)
#library(signal)
#library(forecast)
#library(futureheatwaves)
#library(RmarineHeatWaves)
#library(plyr)
#library(dplyr)
#library(vegan)
#require(IRanges)
#library(weathermetrics)
#library(FactoMineR)
#library(missMDA)
#library(TTR)
#library(pspline)
#library(sfsmisc)
library(raster)
#library(rasterVis)
#library(verification)
#library(SpecsVerification)
#library(s2dverification)
#library(easyVerification)
#library(PRROC)
#library(SpatialVx)
#library(extRemes)
#library(gridExtra)
#library("readxl")
})
set_Polypath(FALSE)

### Data directories and some parameters and functions

#basepath="/research/geog/data1/kg312/RCCC/Sierra_Leone_floods/" 
basepath="C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\Data\\climate\\"
#-----------------------------------------------------------------------------------------------------------------------------------------


### Shapefile

shp=readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\BFA_adm\\BFA_adm1.shp")
#shp=shapefile("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/TAMSAT/BFA_adm1.shp")

LZ_names=paste0("LZ",sprintf("%02d", 1:length(shp$NAME_1)))

#### Read Precip data
S= stack(paste0(basepath,"tamsat.nc") )#S= crop(stack(paste0(basepath,"Tamsat_complet_Precipitation_Burkina.nc"), extent(shp)) )
#x=crop(S,extent(shp))
B=as.data.frame(t(extract(S,shp,mean,na.rm=T))) 
A=B
colnames(A)= as.vector(LZ_names)

# Timetable
Time=as.Date( str_replace_all (str_replace_all(names(S), "X",""), "[.]","/"))

wakat=as.data.frame(matrix(nrow=length(Time),ncol=5)); colnames(wakat)=c("date","year","month","day","julian")
wakat$date=Time
wakat$year=year(wakat$date)
wakat$month=month(wakat$date)
wakat$day=day(wakat$date)
wakat$julian=yday(wakat$date)


######### Monthly  totals
A=subset(cbind(wakat,A), year %in% c(1983:2021))
B2= subset(A,month%in% 7:9)

# Cumule saisonnier

seas= ifelse(A$month %in% 7:9,1,0)
seas_clim = aggregate (A[-c(1:5)], list(seas,A$year),sum, na.rm=T)
seas_clim=seas_clim[seq(2,nrow(seas_clim),2),-1]
colnames(seas_clim)[2:ncol(seas_clim)]=shp$NAME_1
colnames(seas_clim)[1]=c("year")


# Exportation


write.csv(seas_clim, here::here("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\burk\\","7_9_region.csv"),row.names = FALSE )
save(B,file="C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\burk\\region.Rdata")


print("Well done")



