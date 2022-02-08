####This script merges all Berkely Tmax/Tmin temperatures between 1970 and 2019 and subset the Northern Tropical Africa domain

#### Data: BEST
#### Resolution: 1 x 1


############ Load necessary libraries
library(sp)
#library(rgdal)
#library(geosphere)
library(ncdf4)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
library(caTools)
library(RMAWGEN)
library(signal)
library(forecast)
#library(futureheatwaves)
#library(RmarineHeatWaves)
library(plyr)
library(dplyr)
library(vegan)
require(IRanges)
library(weathermetrics)

##Inputs
basepath="C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Temperature/BEST_Temperature_Berkeley_Earth/nn/"

LonW=-6;LonE=3;LatN=16;LatS=9  # Limits of the Northern Tropical Africa domain.

###### 1- Import anomalies 
for (Decade in seq(2000,2010,10)){
  for (Index in c("TMIN","TMAX")){
    print(paste(Index, Decade))
    
    S=nc_open(paste0(basepath,"Complete_",Index,"_Daily_LatLong1_",Decade,".nc"))
    
    ####attributing the name of the main variable to an object according to the result of the previous command#####
    code="temperature"
    
    #### Dimensions
    d1=(S$dim)$lon #Please make sure of the correct name from the model
    d2=(S$dim)$lat #Please make sure of the correct name from the model
    d3=(S$dim)$time #Please make sure of the correct name from the model
    
    lon=d1$vals;lon=ifelse(lon > 180, -360 + lon, lon) 
    lat=d2$vals
    Time=d3$vals
    
    lon_lat <- expand.grid(lon, lat) # in expand.grid the first factor varies the fastest: i.e. set lat, span all of lon values before to move on to the next value of lat.
    lon_lat=lon_lat %>% transform(Var1=as.vector(Var1), Var2 = as.vector(Var2))
    
    names_cols=paste(as.character(lon_lat$Var1),as.character(lon_lat$Var2),sep="||")
    
    # Timetable
    wakat=as.data.frame(matrix(nrow=length(Time),ncol=5)); colnames(wakat)=c("date","year","month","day","daymon")
    wakat$date=as.Date(Time-1,origin=paste0(Decade,"-01-01 00:00:00.0"))
    wakat$year=year(wakat$date)
    wakat$month=month(wakat$date)
    wakat$day=day(wakat$date)
    wakat$daymon=paste0(sprintf("%02d",as.integer(wakat$month)),sprintf("%02d",as.integer(wakat$day)))
    
    #### Get data
    VAR=ncvar_get(S,varid=code)
    VAR[VAR == (ncatt_get(S, code, "_FillValue"))$value]=NA 
    VAR=as.vector(VAR);VAR=as.data.frame(t(matrix(VAR, nrow = length(lon) * length(lat), ncol = length(Time))));colnames(VAR)=names_cols
    #### crop the NTA domain
    NTA_coord=subset(lon_lat, (Var1 %in% seq(LonW,LonE,0.001)& Var2 %in% seq(LatS,LatN,0.001)))
    lon_NTA=unique(NTA_coord$Var1);lat_NTA=unique(NTA_coord$Var2)
    VAR=VAR[,paste(NTA_coord$Var1,"||",NTA_coord$Var2,sep="")]
    VAR=cbind(wakat,VAR)
    
    assign(paste0(Index,"_",Decade),VAR)
  }}
#### Merge all times

TMIN=rbind(TMIN_2000,TMIN_2010) ; TMAX=rbind(TMAX_2000,TMAX_2010)

rm(VAR,TMIN_2000,TMIN_2010,TMAX_2000,TMAX_2010)

###### 2- Import climatology 

for (Index in c("TMIN","TMAX")){
  
  S=nc_open(paste0(basepath,"Complete_",Index,"_Daily_LatLong1_2010.nc"))
  
  ####attributing the name of the main variable to an object according to the result of the previous command#####
  code="climatology"
  
  #### Dimensions
  d1=(S$dim)$lon #Please make sure of the correct name from the model
  d2=(S$dim)$lat #Please make sure of the correct name from the model
  
  lon=d1$vals;lon=ifelse(lon > 180, -360 + lon, lon) 
  lat=d2$vals
  
  lon_lat <- expand.grid(lon, lat) # in expand.grid the first factor varies the fastest: i.e. set lat, span all of lon values before to move on to the next value of lat.
  lon_lat=lon_lat %>% transform(Var1=as.vector(Var1), Var2 = as.vector(Var2))
  
  names_cols=paste(as.character(lon_lat$Var1),as.character(lon_lat$Var2),sep="||")
  
  
  #### Get data
  VAR=ncvar_get(S,varid=code)
  VAR[VAR == (ncatt_get(S, code, "_FillValue"))$value]=NA 
  VAR=as.vector(VAR);VAR=as.data.frame(t(matrix(VAR, nrow = length(lon) * length(lat), ncol = 365)));colnames(VAR)=names_cols
  
  #### crop the NTA domain
  NTA_coord=subset(lon_lat, (Var1 %in% seq(LonW,LonE,0.001)& Var2 %in% seq(LatS,LatN,0.001)))
  lon_NTA=unique(NTA_coord$Var1);lat_NTA=unique(NTA_coord$Var2)
  VAR=VAR[,paste(NTA_coord$Var1,"||",NTA_coord$Var2,sep="")]
  
  #### Creating climatology for 29th February
  VAR=cbind(subset(wakat,year==2017)["daymon"],VAR)
  VAR[nrow(VAR)+1,]=NA ; VAR[nrow(VAR),1]="0229"
  VAR[nrow(VAR),-1]=(VAR[which(VAR$daymon=="0228"),-1]+VAR[which(VAR$daymon=="0301"),-1])/2 # the value on the 29th Feb is the avg of the 28th Feb and 01Mar
  VAR=VAR[order(VAR$daymon),] 
  
  #### Expand to the dimension of the data
  a=ncol(get(Index))+1
  b=ncol(get(Index))+ncol(VAR)-1
  CLIM=join(get(Index),VAR,by="daymon")[a:b]
  VAR=cbind(get(Index)[c(1:5)],CLIM+get(Index)[-c(1:5)])
  
  #### Export to NetCDF
  fillvalue=1e32
  londim=ncdim_def(d1$name,d1$units,lon_NTA) 
  latdim=ncdim_def(d2$name,d2$units,lat_NTA) 
  timedim=ncdim_def("time","days since 2000-01-01 00:00:00.0",(1:nrow(VAR))-1) 
  
  var_def=ncvar_def(paste0(toupper(substr(Index, 1, 1)), tolower(substr(Index, 2, nchar(Index)))),"Celsius",list(londim,latdim,timedim),fillvalue,paste0(tolower(substr(Index, 2, nchar(Index))),"imum temperature"),prec="double")
  
  ncout=nc_create(paste0(basepath,paste0(toupper(substr(Index, 1, 1)), tolower(substr(Index, 2, nchar(Index)))),".nc"),lapply(ls(pattern="_def"),get),force_v4=T)
  
  ncvar_put(ncout,var_def,c(t(VAR[-c(1:5)])))
  
  ncatt_put(ncout,d1$name,"axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,d2$name,"axis","Y")
  ncatt_put(ncout,"time","axis","T")
  nc_close(ncout)
}

print("Neerem zanga")

