# scratch code for NARR
# MAC 03/07/2020

library(raster)
library(ncdf4)

# set rasteroptions
rasterOptions(progress = 'text')

# info about files
pathNARR<-"/scratch/crimmins/NARR/pressure"
filesNC <- list.files(pathNARR,full.names = T) 
# ncin <- nc_open(filesNC[1])
# print(ncin)
# level <- ncvar_get(ncin, "level")

# level=17 is 500mb
# process reanalysis nc into raster stacks
# x <- stack()
# for(i in 1:length(filesNC)){
#     x <- stack( x , brick(filesNC[i], level=17) ) # 6=500mb 4=700mb
#   print(filesNC[i])
# }
# 
# writeRaster(x, filename="/scratch/crimmins/NARR/processed/GH500_daily_NARR_1979_2019.grd", overwrite=TRUE)

# try also 
# fs <- list.files(path="F:\\MODIS\\Modis EVI\\HDF8 EVI", pattern = "tif$", full.names = TRUE)
# library(raster)
# s <- raster::stack(fs)
# writeRaster(s, "hdf8_EVI.TIF")
# also stackSave

#x1 <- brick(filesNC, level=17)

# reproject and clip NARR data Nauslar et al 
# -131W to 85W, 18N to 49.5N)
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
e <- extent(-131, -85, 18, 49.5) 

x <- stack()
for(i in 1:length(filesNC)){
  temp<-projectRaster(brick(filesNC[i], level=13), crs = geo.prj)
  x <- stack(x, crop(temp,e)) # 6=500mb 4=700mb
  print(filesNC[i])
}

writeRaster(x, filename="/scratch/crimmins/NARR/processed/GH700_daily_NARR_WUS_1979_2019.grd", overwrite=TRUE)

#ghNARR<-stack("/scratch/crimmins/NARR/processed/GH500_daily_NARR_WUS_1979_2019.grd")


# monolevel 
pathNARR<-"/scratch/crimmins/NARR/monolevel"
filesNC <- list.files(pathNARR,full.names = T, pattern="^monolevelacpcp") 

# reproject and clip NARR data Nauslar et al 
# -131W to 85W, 18N to 49.5N)
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
e <- extent(-131, -85, 18, 49.5) 

x <- stack()
for(i in 1:length(filesNC)){
  temp<-projectRaster(brick(filesNC[i]), crs = geo.prj)
  x <- stack(x, crop(temp,e)) 
  print(filesNC[i])
}

writeRaster(x, filename="/scratch/crimmins/NARR/processed/PRCP_daily_NARR_WUS_1979_2019.grd", overwrite=TRUE)


# test mapping of NARR


