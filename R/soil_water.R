####This script calculates some rainfall characteristics

#### Data: CHIRPS, TAMSAT
#### Resolution: 0.25*0.25

rm(list=ls())


getwd()
#setwd("F:/INStech/Données")
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
basepath="C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Soil_Moisture/ERA5-Land/"
#-----------------------------------------------------------------------------------------------------------------------------------------


### Shapefile

shp=readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\BFA_adm\\BFA_adm2.shp")
#shp=shapefile("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/TAMSAT/BFA_adm1.shp")

LZ_names=paste0("LZ",sprintf("%02d", 1:length(shp$NAME_2)))

#### Read Precip data
S= stack(paste0(basepath,"soil_water.nc") )#S= crop(stack(paste0(basepath,"Tamsat_complet_Precipitation_Burkina.nc"), extent(shp)) )
#x=crop(S,extent(shp))
A=as.data.frame(t(extract(S,shp,mean,na.rm=T))) 
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
A=subset(cbind(wakat,A), year %in% c(2000:2017))
#mon_clim = aggregate (A[-c(1:5)], list(A$month,A$year),sum, na.rm=T)[-c(1,2)]/length(2000:2017)

mon_clim = aggregate (A[-c(1:5)], list(A$month,A$year),mean, na.rm=T)
colnames(mon_clim)[1:2]=c("month","year")
colnames(mon_clim)[3:47]=shp$NAME_2

# Cumule saisonnier

seas= ifelse(A$month %in% 7:9,1,0)
seas_clim = aggregate (A[-c(1:5)], list(seas,A$year),mean, na.rm=T)
seas_clim=seas_clim[seq(2,nrow(seas_clim),2),-1]
colnames(seas_clim)[2:ncol(seas_clim)]=shp$NAME_2
colnames(seas_clim)[1]=c("year")

print("Well done")


dir.create("data_output") # pour créer le dossier data_output
#setwd("data_output")

# Exportation
library(here)
write.csv(seas_clim, here::here("data_output","ERA5_soil_moisture_year.csv"),row.names = FALSE )
save(seas_clim,file="data_output/ERA5_soil_moisture_year.Rdata")

write.csv(mon_clim, here::here("data_output","ERA5_soil_moisture__month.csv"),row.names = FALSE )
save(mon_clim,file="data_output/ERA5_soil_moisture_month.Rdata")
