# download and procees 00Z NARR data
# MAC 08/14/2020

library(raster)
library(ncdf4)
library(RCurl)

#ncin <- nc_open("/home/crimmins/RProjects/ClimPlot/NARR/temp/temp.nc")
#print(ncin)
#level <- ncvar_get(ncin, "level")
#time <- ncvar_get(ncin, "time")

# set rasteroptions
rasterOptions(progress = 'text')
# pressure levels
# # ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/hgt.197901.nc

# reproject and clip NARR data Nauslar et al 
# -131W to 85W, 18N to 49.5N)
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
e <- extent(-131, -85, 18, 49.5)

months<-c("01","02","03","04","05","06","07","08","09","10","11","12")

# pressure level data
filePrefix<-"hgt."
#Set years
yr1<-1979
yr2<-2020
# ---- download each year
x<-stack()
for(i in yr1:yr2){
 for(j in 1:12) {
    # get data file
    URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/pressure/",filePrefix,i,months[j],".nc")
    #fileName<-  paste0("/scratch/crimmins/NARR/pressure/wind/",filePrefix,i,months[j],".nc")
    download.file(URL, destfile = "/home/crimmins/RProjects/ClimPlot/NARR/temp/temp.nc")
    # only process 00Z subset
    temp<-brick("/home/crimmins/RProjects/ClimPlot/NARR/temp/temp.nc", level=17) # 500mb=17
    temp<-temp[[seq(1,nlayers(temp),8)]]
    temp<-projectRaster(temp, crs=geo.prj)
    x <- stack(x, crop(temp,e)) # 6=500mb 4=700mb
    print(URL)
  }
}

writeRaster(x, filename="/scratch/crimmins/NARR/processed/00z/GH500_00z_NARR_WUS_1979_2020.grd", overwrite=TRUE)


# # # 3 hrly monolevel data   
#  filePrefix<-"pr_wtr."
# # #Set years
#  yr1<-1979
#  yr2<-2019
# # # ---- download each year
#  x<-stack()
#  for(i in yr1:yr2){
#    URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/",filePrefix,i,".nc")
#    download.file(URL, destfile = "/home/crimmins/RProjects/ClimPlot/NARR/temp/temp.nc")
#    # only process 00Z subset
#    temp<-brick("/home/crimmins/RProjects/ClimPlot/NARR/temp/temp.nc") #
#    temp<-temp[[seq(1,nlayers(temp),8)]]
#    temp<-projectRaster(temp, crs=geo.prj)
#    x <- stack(x, crop(temp,e)) # 6=500mb 4=700mb
#    print(URL)
#  }
# 
#  writeRaster(x, filename="/scratch/crimmins/NARR/processed/00z/PWAT_00z_NARR_WUS_1979_2019.grd", overwrite=TRUE)
 

