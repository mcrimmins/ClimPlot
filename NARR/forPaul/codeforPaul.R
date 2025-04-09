# masking counties for Paul I
# 03/24/2021

library(raster)
library(ncdf4)

# set rasteroptions
rasterOptions(progress = 'text')

# info about files
pathNARR<-"/home/crimmins/RProjects/ClimPlot/NARR/forPaul"
filesNC <- list.files(pathNARR,full.names = T) 

ncin <- nc_open(filesNC[3])
print(ncin)
#ncvar_get(ncin, "var")
#level <- ncvar_get(ncin, "level")

test<-raster(filesNC[3])