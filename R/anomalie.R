
rm(list=ls())


data <- read.csv("C:/Users/Yacou/Desktop/Yacouba_New/Stage_Climate_Centre/burk/tamsat_burk.csv")
#View(data)
#dta=as.data.frame(colMeans(data))

#data$Bale=data['Bale']-dta['Bale',]
#quantile(data$Banwa,1/4)
#dta=dta['BaleKossi#dta=dta['Bale',]


#yac <-apply(data,FUN=quantile,3,MARGIN=2)

df=data

a=quant=as.data.frame(sapply(df, function(x) quantile(x, 1/5)))
quant = quant[,1]
quant = quant[-1]
vect=c('Bale','Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')
quant=as.data.frame(cbind(vect,quant))
#dt=as.data.frame(vec,dt$`sapply(df, function(x) quantile(x, 1/3))`)
quant <- data.frame(quant[,-1], row.names = quant[,1])
#as.numeric(quant['Bale',])


da= as.integer(data['Bale'] <= as.numeric(quant['Bale',]))
#da[1,]
#colnames(da['V1'])="bale"
#da=data['Bale']-dta['Bale',]

vec=c('Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')

for (cp in vec){
  d=as.integer(data[cp] <= as.numeric(quant[cp,]))
  da=cbind(da,d)
}
da=as.data.frame(da)
da=cbind(data['year'],da)
da_b=da
colnames(da)[2:46]=vect
bur_binaire=da[2:46]
#write.csv(bur_binaire, here::here("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre","prov_bur_5_binaire.csv"),row.names = FALSE )


#########################

rm(list=ls())

data <- read.csv('C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre\\mrt\\mrt.csv',encoding = "UTF-8")


df=data

a=quant=as.data.frame(sapply(df, function(x) quantile(x, 1/5)))
quant = quant[,1]
quant = quant[-1]

vect=names(data)[2:45]
quant=as.data.frame(cbind(vect,quant))
quant <- data.frame(quant[,-1], row.names = quant[,1])


da= as.integer(data['Aoujeft'] <= as.numeric(quant["Aoujeft",]))

vec=vect[2:44]

for (cp in vec){
  d=as.integer(data[cp] <= as.numeric(quant[cp,]))
  da=cbind(da,d)
}

da=as.data.frame(da)
da=cbind(data['year'],da)

colnames(da)[2:45]=vect
da_mrt=da
da=da[2:45]
#write.csv(da, here::here("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\Stage_Climate_Centre","Prov_mrt_5_binaire.csv"),row.names = FALSE )


#######################################################################################


library(sf)
library(ggplot2)
library(rgdal)
library(rgeos)

#Reading the shapefiles
sf <- st_read(dsn="C:\\Users\\User\\Desktop\\cty_council_dist.shp", layer="cty_council_dist_haw")
shape <- readOGR(dsn="C:\\Users\\User\\Desktop\\cty_council_dist.shp", layer="cty_council_dist_haw")
shp=shapefile("C:/Users/Yacou/Desktop/Yacouba_New/climte/Data/Precipitation/TAMSAT/BFA_adm1.shp")
shape=shp=readOGR("C:\\Users\\Yacou\\Desktop\\Yacouba_New\\climte\\Data\\BFA_adm\\BFA_adm2.shp")


#To view the attributes
head(shape@da)
summary(sf)

#Plotting the shapefile
plot(shape)
plot(sf)

#Covert the coordinate system
sf_gcs <- st_transform(sf, crs = "EPSG:4326")
shape_gcs <- spTransform(shape, CRS=CRS("+init=EPSG:4326"))

#Plotting the shapefile
plot(shape)
plot(sf_gcs)

#Plotting the districts only
plot(shape["NAME_2"], axes = TRUE, main = "Province")
























da=data['Bale']-dta['Bale',]

vec=c('Banwa','Kossi',	'Mouhoun',	'Nayala',	'Sourou',	'Comoe'	,'Leraba',	'Boulgou',	'Koulpelogo',	'Kouritenga',	'Bam'	,'Namentenga',	'Sanmatenga',	'Boulkiemde',	'Sanguie'	,'Sissili',	'Ziro',	'Bazega',	'Nahouri',	'Zoundweogo',	'Kadiogo'	,'Gnagna',	'Gourma',	'Komandjoari',	'Kompienga'	,'Tapoa',	'Houet'	,'Kenedougou',	'Tuy'	,'Loroum',	'Passore',	'Yatenga',	'Zondoma',	'Ganzourgou',	'Kourweogo',	'Oubritenga',	'Oudalan',	'Seno',	'Soum',	'Yagha',	'Bougouriba',	'Ioba',	'Noumbiel',	'Poni')

for (cp in vec){
  d=data[cp]-dta[cp,]
  da=cbind(da,d)
  }
da=as.data.frame(da)
da=cbind(data['year'],da)

###################

da$colour <- ifelse(da$Bale < 0, "negative","positive")

x11();ggplot(da,aes(year,Bale,label=""))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(negative="firebrick1",positive="steelblue"))

for (cp in vec){
x11();ggplot(da,aes(year,Poni,label=""))+
  geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue"))
}
