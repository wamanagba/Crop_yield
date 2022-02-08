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
S= stack(paste0(basepath,"Precip_CHIRTS.nc") )#S= crop(stack(paste0(basepath,"Tamsat_complet_Precipitation_Burkina.nc"), extent(shp)) )

#x=crop(S,extent(shp))
A=as.data.frame(t(extract(S,shp,mean,na.rm=T))) 
prec#=A
maxi#=as.data.frame(t(extract(S,shp,max,na.rm=T))) 
A=prec
#A=as.data.frame(t(extract(S,shp,na.rm=T))) 
colnames(A)= as.vector(LZ_names)
#A=maxi

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

colnames(A)[6:50]=shp$NAME_2
#preci=A
#maxim=A
#mon_clim = aggregate (A[-c(1:5)], list(A$month,A$year),sum, na.rm=T)[-c(1,2)]/length(2000:2017)




# ================================================================================================
# =
# =                                    Climate indinces
# =
# =================================================================================================

#  RX1day, maximum one-day precipitation: highest precipitation amount in one-day period 
# ------------------------------------------------------------
B2= subset(maxim,month%in% 7:9)
RX1day = aggregate (B2[-c(1:5)], list(B2$year),max, na.rm=T)
write.csv(RX1day, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/ok_indices/","RX1day.csv"),row.names = FALSE )

# ------------------------------------------------------------

# RX5day, maximum five-day precipitation: highest precipitation amount in five-day period
library(zoo)
t=as.data.frame(sapply(A[-c(1:5)], function(x) rollmax(x,5,align = c("center"),na.pad = T)))
#d=rollapply(B1[-c(1:5)], 5, function(x) max(x)) # does not use rollmean
t=subset(cbind(wakat,t), year %in% c(2000:2017))
RX5day = aggregate (t[-c(1:5)], list(t$year),max, na.rm=T)
write.csv(RX5day, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/ok_indices/","RX5day.csv"),row.names = FALSE )

# ----------------------------------------------------------------

# SDII, simple daily intensity index: mean precipitation amount on a wet day
B1= subset(preci,month%in% 7:9)
BB=B1
BB[BB <= 1] <- NA
SDII = aggregate (BB[-c(1:5)], list(BB$year),mean, na.rm=T)
write.csv(SDII, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/ok_indices/","SDII.csv"),row.names = FALSE )

# --------------------------------------------------------------------

# === 20. R10mm, heavy precipitation days: count of days where RR (daily precipitation amount) ??? 10 mm
#A=preci
#colnames(A)[6:50]=shp$NAME_2

da = as.integer(B1[6]>=10)
for (cp in 7:50){
  d=as.integer(B1[cp] >= 10)
  da=cbind(da,d)
}

da=cbind(B1[1:5],da)
colnames(da)[6:50]=shp$NAME_2
R10mm = aggregate (da[-c(1:5)], list(da$year),sum, na.rm=T)
write.csv(R10mm, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/ok_indices/","R10mm.csv"),row.names = FALSE )

# ======================================================

#B1=subset(A,month%in% 7:9)

da = as.integer(B1[6]>=20)
for (cp in 7:50){
  d=as.integer(B1[cp] >= 20)
  da=cbind(da,d)
}

da=cbind(B1[1:5],da)
colnames(da)[6:50]=shp$NAME_2
R20mm = aggregate (da[-c(1:5)], list(da$year),sum, na.rm=T)

write.csv(R20mm, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/ok_indices/","R20mm.csv"),row.names = FALSE )

# ============================================================
# . Rnnmm: count of days where RR ??? user-defined threshold in mm


nn=5
da=0
da = as.integer(B1[6]>=nn)
for (cp in 7:50){
  d=as.integer(B1[cp] >= nn)
  da=cbind(da,d)
}

da=cbind(B1[1:5],da)
colnames(da)[6:50]=shp$NAME_2
R5mm = aggregate (da[-c(1:5)], list(da$year),sum, na.rm=T)

write.csv(R5mm, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/ok_indices/","R5mm.csv"),row.names = FALSE )


#==================================================

# CDD, consecutive dry days: maximum length of dry spell (RR < 1 mm)
cdd= 
# CWD, consecutive wet days: maximum length of wet spell (RR ??? 1 mm) 
# ==================================================================

# R95pTOT: precipitation due to very wet days (> 95th percentile) 


df=BB[6:50]

a=quant=as.data.frame(sapply(df, function(x) quantile(x, .95,na.rm= TRUE)))
quant = quant[,1]
#quant = quant[-1]
vect=c('Bale','Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')
quant=as.data.frame(cbind(vect,quant))
#dt=as.data.frame(vec,dt$`sapply(df, function(x) quantile(x, 1/3))`)
quant <- data.frame(quant[,-1], row.names = quant[,1])

cc=df
df[1][df[1] <= as.numeric(quant[1,])] <- NA
#da= as.integer(df[1] >= as.numeric(quant[1,]))
#BB[BB <= 1] <- NA


#vec=c('Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')

for (cp in 2:45){
  #d=as.integer(df[cp] >= as.numeric(quant[cp,]))
  df[cp][df[cp] <= as.numeric(quant[cp,])] <- NA
  #da=cbind(da,d)
}
#da=as.data.frame(da)
df=cbind(B1[1:5],df)
#colnames(df)[6:50]=shp$NAME_2

R95pTOT = aggregate (df[-c(1:5)], list(da$year),sum, na.rm=T)
colnames(R95pTOT)[1]=c("year")

write.csv(R95pTOT, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/ok_indices/","R95pTOT.csv"),row.names = FALSE )

# ==================================================================
# R99pTOT: precipitation due to extremely wet days (> 99th percentile) 

df=BB[6:50]

a=quant=as.data.frame(sapply(df, function(x) quantile(x, .99,na.rm= TRUE)))
quant = quant[,1]
#quant = quant[-1]
vect=c('Bale','Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')
quant=as.data.frame(cbind(vect,quant))
#dt=as.data.frame(vec,dt$`sapply(df, function(x) quantile(x, 1/3))`)
quant <- data.frame(quant[,-1], row.names = quant[,1])

cc=df
df[1][df[1] <= as.numeric(quant[1,])] <- NA
#da= as.integer(df[1] >= as.numeric(quant[1,]))
#BB[BB <= 1] <- NA


#vec=c('Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')

for (cp in 2:45){
  #d=as.integer(df[cp] >= as.numeric(quant[cp,]))
  df[cp][df[cp] <= as.numeric(quant[cp,])] <- NA
  #da=cbind(da,d)
}
#da=as.data.frame(da)
df=cbind(B1[1:5],df)
#colnames(df)[6:50]=shp$NAME_2

R99pTOT = aggregate (df[-c(1:5)], list(da$year),sum, na.rm=T)
colnames(R99pTOT)[1]=c("year")

write.csv(R99pTOT, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/ok_indices/","R99pTOT.csv"),row.names = FALSE )


# --------------------------------------------------------------

# PRCPTOT: total precipitation in wet days (> 1 mm)

PRCPTOT = aggregate (BB[-c(1:5)], list(da$year),sum, na.rm=T)
write.csv(PRCPTOT, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/ok_indices/","PRCPTOT.csv"),row.names = FALSE )

View(PRCPTOT)


# ==================================================================















# RX5day, maximum five-day precipitation: highest precipitation amount in five-day period
for( i in 1:length(A$day)){
  if (A$day[i] %in% 1:5) A$day[i]=0
  if (A$day[i] %in% 6:10) A$day[i]=1
  if (A$day[i] %in% 11:15) A$day[i]=2
  if (A$day[i] %in% 16:20) A$day[i]=3
  if (A$day[i] %in% 21:25) A$day[i]=4
  if (A$day[i] %in% 26:31) A$day[i]=5
}
B1=subset(A,month%in% 7:9)
da = aggregate (B1[-c(1:5)], list(B1$year, B1$month,B1$day),sum, na.rm=T)
RX5day = aggregate (da[-c(1:3)], list(da$Group.1),mean, na.rm=T)
colnames(RX5day)[1]='year'

write.csv(RX5day, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/","RX5day.csv"),row.names = FALSE )

# === 20. R10mm, heavy precipitation days: count of days where RR (daily precipitation amount) ??? 10 mm
A=prec
colnames(A)[6:50]=shp$NAME_2

B1=subset(A,month%in% 7:9)
da = as.integer(B1[6]>=10)
for (cp in 7:50){
  d=as.integer(B1[cp] >= 10)
  da=cbind(da,d)
}

da=cbind(B1[1:5],da)
colnames(da)[6:50]=shp$NAME_2
R10mm = aggregate (da[-c(1:5)], list(da$year),sum, na.rm=T)
write.csv(R10mm, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/","R10mm.csv"),row.names = FALSE )

# ==R20mm, very heavy precipitation days: count of days where RR ??? 20 mm


B1=subset(A,month%in% 7:9)

da = as.integer(B1[6]>=20)
for (cp in 7:50){
  d=as.integer(B1[cp] >= 20)
  da=cbind(da,d)
}

da=cbind(B1[1:5],da)
colnames(da)[6:50]=shp$NAME_2
R20mm = aggregate (da[-c(1:5)], list(da$year),sum, na.rm=T)

write.csv(R20mm, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/","R20mm.csv"),row.names = FALSE )


# R95pTOT: precipitation due to very wet days (> 95th percentile) 

df=B1[6:50]

a=quant=as.data.frame(sapply(df, function(x) quantile(x, .95)))
quant = quant[,1]
#quant = quant[-1]
#vect=c('Bale','Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')
quant=as.data.frame(cbind(vect,quant))
#dt=as.data.frame(vec,dt$`sapply(df, function(x) quantile(x, 1/3))`)
quant <- data.frame(quant[,-1], row.names = quant[,1])


da= as.integer(df[1] >= as.numeric(quant[1,]))


#vec=c('Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')

for (cp in 2:45){
  d=as.integer(df[cp] >= as.numeric(quant[cp,]))
  da=cbind(da,d)
}
da=as.data.frame(da)
da=cbind(B1[1:5],da)
colnames(da)[6:50]=shp$NAME_2

R95pTOT = aggregate (da[-c(1:5)], list(da$year),sum, na.rm=T)
colnames(R95pTOT)[1]=c("year")

write.csv(R95pTOT, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/","R95pTOT.csv"),row.names = FALSE )

###########  R99pTOT: precipitation due to extremely wet days (> 99th percentile) ############
                        ###########################


df=B1[6:50]

a=quant=as.data.frame(sapply(df, function(x) quantile(x, .99)))
quant = quant[,1]
#quant = quant[-1]
#vect=c('Bale','Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')
quant=as.data.frame(cbind(vect,quant))
#dt=as.data.frame(vec,dt$`sapply(df, function(x) quantile(x, 1/3))`)
quant <- data.frame(quant[,-1], row.names = quant[,1])


da= as.integer(df[1] >= as.numeric(quant[1,]))



for (cp in 2:45){
  d=as.integer(df[cp] >= as.numeric(quant[cp,]))
  da=cbind(da,d)
}
da=as.data.frame(da)
da=cbind(B1[1:5],da)
colnames(da)[6:50]=shp$NAME_2

R99pTOT = aggregate (da[-c(1:5)], list(da$year),sum, na.rm=T)
colnames(R99pTOT)[1]=c("year")

write.csv(R99pTOT, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/","R99pTOT.csv"),row.names = FALSE )

######## PRCPTOT: total precipitation in wet days (> 1 mm)



B1=subset(A,month%in% 7:9)

da = as.integer(B1[6]>=1)
for (cp in 7:50){
  d=as.integer(B1[cp] >= 1)
  da=cbind(da,d)
}

da=cbind(B1[1:5],da)
colnames(da)[6:50]=shp$NAME_2
PRCPTOT = aggregate (da[-c(1:5)], list(da$year),sum, na.rm=T)
write.csv(PRCPTOT, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/","PRCPTOT.csv"),row.names = FALSE )
















mon_clim = aggregate (A[-c(1:5)], list(A$month,A$year),sum, na.rm=T)
colnames(mon_clim)[1:2]=c("month","year")
colnames(mon_clim)[3:47]=shp$NAME_2

# Cumule saisonnier

seas= ifelse(A$month %in% 4:10,1,0)
seas_clim = aggregate (A[-c(1:5)], list(seas,A$year),sum, na.rm=T)
seas_clim=seas_clim[seq(2,nrow(seas_clim),2),-1]
colnames(seas_clim)[2:ncol(seas_clim)]=shp$NAME_2
colnames(seas_clim)[1]=c("year")


pathss="C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/data_output/"
dir.create(pathss) # pour créer le dossier data_output


# Exportation
library(here)
write.csv(seas_clim, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/data_output/","chirps_Prec_year.csv"),row.names = FALSE )
save(seas_clim,file="C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/data_output/chirps_Prec_year.Rdata")


write.csv(mon_clim, here::here("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/data_output/","chirps_Prec_month.csv"),row.names = FALSE )
save(mon_clim,file="C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/CHIRPS_rfe/complet/data_output/chirps_Prec_month.Rdata")


print("Well done")



