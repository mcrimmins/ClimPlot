# download gridmet data
# MAC 03/03/2020

library(RCurl)
#library(raster)
#library(ncdf4)

# pr, pet, rmax, rmin, sph, srad, th, tmmn, tmmx, vpd, vs
#   erc, bi
filePrefix<-"fm1000_"

#Set years
yr1<-1979
yr2<-2020
# ---- download each year 
for(i in yr1:yr2){
  URL <- paste0("https://www.northwestknowledge.net/metdata/data/",filePrefix,i,".nc")
  fileName<-  paste0("/scratch/crimmins/gridmet/update_Aug2019/",filePrefix,i,".nc")
  download.file(URL, destfile = fileName)
  print(fileName)
}
  
  
# check out netcdf files
library(ncdf4)
ncfname<-"/scratch/crimmins/gridmet/update_Aug2019/rmin_2020.nc"
  ncin <- nc_open(ncfname)
  print(ncin)


