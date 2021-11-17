# process NARR with crop and reproject
# MAC 03/07/2020

library(raster)
library(ncdf4)

# set rasteroptions
rasterOptions(progress = 'text')

# info about files
 #pathNARR<-"/scratch/crimmins/NARR/pressure/shum"
 #filesNC <- list.files(pathNARR,full.names = T) 
# 
#  ncin <- nc_open(filesNC[1])
#  print(ncin)
#  level <- ncvar_get(ncin, "level")
# 
# # level=17 is 500mb
# # level=7 is 850
#  pathNARR<-"/scratch/crimmins/NARR/pressure"
#  filesNC <- list.files(pathNARR,full.names = T) 
# # process reanalysis nc into raster stacks
# x <- stack()
# for(i in 1:length(filesNC)){
#     x <- stack( x , brick(filesNC[i], level=17) ) # 6=500mb 4=700mb,
#   print(filesNC[i])
# }
# 
# writeRaster(x, filename="/scratch/crimmins/NARR/processed/GH500_daily_NARR_1979_2020.grd", overwrite=TRUE)

# try also 
# fs <- list.files(path="F:\\MODIS\\Modis EVI\\HDF8 EVI", pattern = "tif$", full.names = TRUE)
# library(raster)
# s <- raster::stack(fs)
# writeRaster(s, "hdf8_EVI.TIF")
# also stackSave

#x1 <- brick(filesNC, level=17)

# reproject and clip NARR data Nauslar et al - USE for SOM STUDY
# -131W to 85W, 18N to 49.5N)
pathNARR<-"/scratch/crimmins/NARR/pressure"
filesNC <- list.files(pathNARR,full.names = T, pattern="^hgt")
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
e <- extent(-131, -85, 18, 49.5)
#
x <- stack()
for(i in 1:length(filesNC)){
  temp<-projectRaster(brick(filesNC[i], level=17), crs = geo.prj)
  x <- stack(x, crop(temp,e))
  print(filesNC[i])
}

writeRaster(x, filename="/scratch/crimmins/NARR/processed/GH500_daily_NARR_WUS_1979_2020.grd", overwrite=TRUE)

#ghNARR<-stack("/scratch/crimmins/NARR/processed/GH500_daily_NARR_WUS_1979_2019.grd")


# monolevel - USE for SOM STUDY
#pathNARR<-"/scratch/crimmins/NARR/monolevel"
#filesNC <- list.files(pathNARR,full.names = T, pattern="^monolevelacpcp")
pathNARR<-"/scratch/crimmins/NARR/monolevel"
filesNC <- list.files(pathNARR,full.names = T, pattern="^monolevelpr_wtr")

#filesNC <- list.files(pathNARR,full.names = T, pattern="^cape")
# pathPRES<-"/scratch/crimmins/NARR/monolevel/sfcpres"
# filesNC <- list.files(pathPRES,full.names = T, pattern="^pres.sfc.")
# 
# 
# reproject and clip NARR data Nauslar et al
# -131W to 85W, 18N to 49.5N)
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
e <- extent(-131, -85, 18, 49.5)

x <- stack()
for(i in 1:length(filesNC)){
  #temp<-projectRaster(brick(filesNC[i]), crs = geo.prj)
  beginCluster(6)
    temp<-projectRaster(brick(filesNC[i]), crs = geo.prj)
  endCluster()
  x <- stack(x, crop(temp,e))
  print(filesNC[i])
}

writeRaster(x, filename="/scratch/crimmins/NARR/processed/PWAT_daily_NARR_WUS_1979_2020.grd", overwrite=TRUE)


# # daily MAX monolevel 
# pathNARR<-"/scratch/crimmins/NARR/monolevel/CAPE/3hrly"
# #filesNC <- list.files(pathNARR,full.names = T, pattern="^monolevelacpcp") 
# filesNC <- list.files(pathNARR,full.names = T, pattern="^cape") 
# 
# # get daily max
# library(tidyr)
# 
# # reproject and clip NARR data Nauslar et al 
# # -131W to 85W, 18N to 49.5N)
# geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
# e <- extent(-131, -85, 18, 49.5) 
# 
# x <- stack()
# for(i in 1:length(filesNC)){
#   # get daily max
#   temp<-brick(filesNC[i])
#   layerNames<-as.data.frame(names(temp)); colnames(layerNames)<-"name"
#   dates<- layerNames %>% separate(name, c("year","month","day","hour",NA, NA))
#   dates$date<-as.Date(paste0(dates$year,"-",dates$month,"-",dates$day), format="X%Y-%m-%d")
#   # stackapply to dates
#   beginCluster(6)
#     temp <- raster::clusterR(temp, stackApply, args=list(indices = dates$date, fun = max, na.rm = TRUE))
#     #temp<- stackApply(temp, dates$date, fun = max)
#   endCluster()
#   # reproject and crop
#   beginCluster(6)
#     temp<-projectRaster(temp, crs = geo.prj)
#   endCluster()
#   names(temp)<-unique(dates$date)
#   x <- stack(x, crop(temp,e)) 
#   print(filesNC[i])
# }
# 
# writeRaster(x, filename="/scratch/crimmins/NARR/processed/maxCAPE_daily_NARR_WUS_1979_2019.grd", overwrite=TRUE)
# 

