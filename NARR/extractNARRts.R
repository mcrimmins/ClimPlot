# extract time series from NARR grids
# MAC 06/17/2020

library(raster)

# set rasteroptions
rasterOptions(progress = 'text')

# load processed rasters from processNARR.R
pwat<-stack("/scratch/crimmins/NARR/processed/PWAT_daily_NARR_WUS_1979_2019.grd") 
prcp<-stack("/scratch/crimmins/NARR/processed/PRCP_daily_NARR_WUS_1979_2019.grd") 
avgCAPE<-stack("/scratch/crimmins/NARR/processed/CAPE_daily_NARR_WUS_1979_2019.grd") 
maxCAPE<-stack("/scratch/crimmins/NARR/processed/maxCAPE_daily_NARR_WUS_1979_2019.grd") 

# extract point based on lat lon
#lat=33.4373; lon=-112.0078 # KPHX
lat=32.1145; lon=-110.9392 # KTUS

# extract time series
avgCAPEts<-t(raster::extract(avgCAPE, cellFromXY(avgCAPE, c(lon,lat))))
maxCAPEts<-t(raster::extract(maxCAPE, cellFromXY(maxCAPE, c(lon,lat))))
PRCPts<-t(raster::extract(prcp, cellFromXY(prcp, c(lon,lat))))
PWATts<-t(raster::extract(pwat, cellFromXY(pwat, c(lon,lat))))

# combine into data frame
dates<-as.Date(rownames(PRCPts), format="X%Y.%m.%d")
ALLts<-cbind.data.frame(dates,maxCAPEts,avgCAPEts,PRCPts,PWATts)

# write to file
write.csv(ALLts, file="./NARR/KTUS_NARR_1979_2019.csv", row.names = FALSE)

# make a plot
library(cowplot)

p1<-ggplot(ALLts, aes(dates,maxCAPEts))+
  geom_line(color="red")+
  ggtitle("NARR Daily max CAPE (J/kg) - KTUS")

p2<-ggplot(ALLts, aes(dates,avgCAPEts))+
  geom_line(color="orange")+
  ggtitle("NARR Daily avg CAPE (J/kg) - KTUS")

p3<-ggplot(ALLts, aes(dates,PWATts))+
  geom_line(color="blue")+
  ggtitle("NARR Daily avg PWAT (mm) - KTUS")

p4<-ggplot(ALLts, aes(dates,PRCPts))+
  geom_line(color="green")+
  ggtitle("NARR Daily total PRCP (mm) - KTUS")

plot_grid(p1, p2, p3, p4, ncol=1)


