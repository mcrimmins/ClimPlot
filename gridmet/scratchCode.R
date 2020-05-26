# play around with GridMet data
# MAC 02/28/20

library(RCurl)
library(raster)
library(ncdf4)

# get grid - looking at 'permanent' versions
URL <- "https://www.northwestknowledge.net/metdata/data/permanent/2019/permanent_gridmet_20191225.nc"
download.file(URL, destfile = "./gridmet/permanent_gridmet_20191225.nc", method="curl")

# get info on gridmet
ncin <- nc_open("./gridmet/permanent_gridmet_20191225.nc")
print(ncin)
names(ncin$var)

gridmet<-raster("./gridmet/permanent_gridmet_20191225.nc", var="minimum_air_temperature")