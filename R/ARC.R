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
basepath="C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/"
#-----------------------------------------------------------------------------------------------------------------------------------------


### Shapefile

shp=readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\BFA_adm\\BFA_adm2.shp")
#shp=shapefile("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/TAMSAT/BFA_adm1.shp")

LZ_names=paste0("LZ",sprintf("%02d", 1:length(shp$NAME_2)))

#### Read Precip data
S11= stack(paste0(basepath,"Precip_CHIRTS.nc") )#S= crop(stack(paste0(basepath,"Tamsat_complet_Precipitation_Burkina.nc"), extent(shp)) )
#x=crop(S,extent(shp))
Max=as.data.frame(t(extract(S1,shp,max,na.rm=T))) 
colnames(A1)= as.vector(LZ_names)


# Timetable
Time=as.Date( str_replace_all (str_replace_all(names(S1), "X",""), "[.]","/"))

wakat=as.data.frame(matrix(nrow=length(Time),ncol=5)); colnames(wakat)=c("date","year","month","day","julian")
wakat$date=Time
wakat$year=year(wakat$date)
wakat$month=month(wakat$date)
wakat$day=day(wakat$date)
wakat$julian=yday(wakat$date)


######### Monthly  totals
A22=subset(cbind(wakat,A1), year %in% c(2000:2018))
#mon_clim = aggregate (A[-c(1:5)], list(A$month,A$year),sum, na.rm=T)[-c(1,2)]/length(2000:2017)

mon_clim = aggregate (A[-c(1:5)], list(A$month,A$year),sum, na.rm=T)
colnames(mon_clim)[1:2]=c("month","year")
colnames(mon_clim)[3:47]=shp$NAME_2

colnames(A22)[6:356]=shp$NAME_3

# Cumule saisonnier

seas= ifelse(A$month %in% 4:10,1,0)
seas_clim = aggregate (A[-c(1:5)], list(seas,A$year),sum, na.rm=T)
seas_clim=seas_clim[seq(2,nrow(seas_clim),2),-1]
colnames(seas_clim)[2:ncol(seas_clim)]=shp$NAME_2
colnames(seas_clim)[1]=c("year")


pathss="C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/data_output/"
dir.create(pathss) # pour cr�er le dossier data_output


# Exportation
library(here)
write.csv(seas_clim, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/data_output/","chirps_Prec_year.csv"),row.names = FALSE )
save(seas_clim,file="C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/data_output/chirps_Prec_year.Rdata")


write.csv(mon_clim, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/data_output/","chirps_Prec_month.csv"),row.names = FALSE )
save(mon_clim,file="C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/data_output/chirps_Prec_month.Rdata")


print("Well done")

write.csv(A22, here::here("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\dta","chirps.csv"),row.names = FALSE )


