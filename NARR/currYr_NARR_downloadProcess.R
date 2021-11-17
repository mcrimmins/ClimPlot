# download and procees currYear for monsoon anomaly plots
# MAC 08/04/2020

library(raster)
library(ncdf4)
library(RCurl)

# set rasteroptions
rasterOptions(progress = 'text')

# download section
#####
# download current year data
# sfc pressure
filePrefix<-"pres.sfc."
i<-2020
  URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/",filePrefix,i,".nc")
  fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,".nc")
  download.file(URL, destfile = fileName)
# pwat
filePrefix<-"pr_wtr."
  i<-2020
  URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/",filePrefix,i,".nc")
  fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,".nc")
  download.file(URL, destfile = fileName)
# precip
filePrefix<-"apcp."
  i<-2020
  URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/",filePrefix,i,".nc")
  fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,".nc")
  download.file(URL, destfile = fileName)
# cape
filePrefix<-"cape."
  i<-2020
  URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/",filePrefix,i,".nc")
  fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,".nc")
  download.file(URL, destfile = fileName)
# cape
filePrefix<-"dpt.2m."
  i<-2020
  URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/",filePrefix,i,".nc")
  fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,".nc")
  download.file(URL, destfile = fileName)  
# u-flux
  filePrefix<-"wvuflx."
  i<-2020
  URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/",filePrefix,i,".nc")
  fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,".nc")
  download.file(URL, destfile = fileName)  
# v-flux
  filePrefix<-"wvvflx."
  i<-2020
  URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/",filePrefix,i,".nc")
  fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,".nc")
  download.file(URL, destfile = fileName) 
  
    
# pressure levels
  # # ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/hgt.197901.nc
  # 
  #months<-c("01","02","03","04","05","06","07","08","09","10","11","12")
  months<-c("01","02","03","04","05","06","07","08","09")  
  # 
  # # monolevel data   
   filePrefix<-"uwnd."
  # # ---- download each year 
  i=2020
   for(j in 1:12) {
      URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/",filePrefix,i,months[j],".nc")
      fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,months[j],".nc")
      download.file(URL, destfile = fileName)
      print(fileName)
    }
  filePrefix<-"vwnd."
  # # ---- download each year 
  i=2020
  for(j in 1:12) {
    URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/",filePrefix,i,months[j],".nc")
    fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,months[j],".nc")
    download.file(URL, destfile = fileName)
    print(fileName)
  }
  filePrefix<-"hgt."
  # # ---- download each year 
  i=2020
  for(j in 1:12) {
    URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/",filePrefix,i,months[j],".nc")
    fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,months[j],".nc")
    download.file(URL, destfile = fileName)
    print(fileName)
  }
  filePrefix<-"shum."
  # # ---- download each year 
  i=2020
  for(j in 1:12) {
    URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/",filePrefix,i,months[j],".nc")
    fileName<-  paste0("/scratch/crimmins/NARR/currYr/",filePrefix,i,months[j],".nc")
    download.file(URL, destfile = fileName)
    print(fileName)
  }
#####  
  
# process into grd files
  # reproject and clip NARR data Nauslar et al 
  # -131W to 85W, 18N to 49.5N)
  geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  e <- extent(-131, -85, 18, 49.5)
  
  # 850 SHUM
  pathNARR<-"/scratch/crimmins/NARR/currYr"
  filesNC <- list.files(pathNARR,full.names = T,pattern="^shum") 
  # 
  x <- stack()
  for(i in 1:length(filesNC)){
    temp<-projectRaster(brick(filesNC[i], level=7), crs = geo.prj)
    x <- stack(x, crop(temp,e)) # 6=500mb 4=700mb
    print(filesNC[i])
  }
  writeRaster(x, filename="/scratch/crimmins/NARR/currYr/processed/SH850_daily_NARR_WUS_2020.grd", overwrite=TRUE)
  # 500 GH
  pathNARR<-"/scratch/crimmins/NARR/currYr"
  filesNC <- list.files(pathNARR,full.names = T,pattern="^hgt") 
  # 
  x <- stack()
  for(i in 1:length(filesNC)){
    temp<-projectRaster(brick(filesNC[i], level=17), crs = geo.prj) #17=500mb
    x <- stack(x, crop(temp,e)) # 6=500mb 4=700mb
    print(filesNC[i])
  }
  writeRaster(x, filename="/scratch/crimmins/NARR/currYr/processed/GH500_daily_NARR_WUS_2020.grd", overwrite=TRUE)
  
  # monolevel 
  # cape
  filesNC <- list.files(pathNARR,full.names = T, pattern="^cape")
    temp<-projectRaster(brick(filesNC), crs = geo.prj)
    x <- crop(temp,e) # 6=500mb 4=700mb
    print(filesNC)
    writeRaster(x, filename="/scratch/crimmins/NARR/currYr/processed/CAPE_daily_NARR_WUS_2020.grd", overwrite=TRUE)
  # precip
  filesNC <- list.files(pathNARR,full.names = T, pattern="^apcp")
    temp<-projectRaster(brick(filesNC), crs = geo.prj)
    x <- crop(temp,e) # 6=500mb 4=700mb
    print(filesNC)
    writeRaster(x, filename="/scratch/crimmins/NARR/currYr/processed/PRCP_daily_NARR_WUS_2020.grd", overwrite=TRUE)
  # sfc dewpoint
  filesNC <- list.files(pathNARR,full.names = T, pattern="^dpt")
    temp<-projectRaster(brick(filesNC), crs = geo.prj)
    x <- crop(temp,e) # 6=500mb 4=700mb
    print(filesNC)
    writeRaster(x, filename="/scratch/crimmins/NARR/currYr/processed/DPT2m_daily_NARR_WUS_2020.grd", overwrite=TRUE)
  # sfc dewpoint
  filesNC <- list.files(pathNARR,full.names = T, pattern="^pr_wtr")
    temp<-projectRaster(brick(filesNC), crs = geo.prj)
    x <- crop(temp,e) # 6=500mb 4=700mb
    print(filesNC)
    writeRaster(x, filename="/scratch/crimmins/NARR/currYr/processed/PWAT_daily_NARR_WUS_2020.grd", overwrite=TRUE)
  # u flux
  filesNC <- list.files(pathNARR,full.names = T, pattern="^wvuflx")
    temp<-projectRaster(brick(filesNC), crs = geo.prj)
    x <- crop(temp,e) # 6=500mb 4=700mb
    print(filesNC)
    writeRaster(x, filename="/scratch/crimmins/NARR/currYr/processed/WVUFLX_daily_NARR_WUS_2020.grd", overwrite=TRUE)
  # v flux
  filesNC <- list.files(pathNARR,full.names = T, pattern="^wvvflx")
    temp<-projectRaster(brick(filesNC), crs = geo.prj)
    x <- crop(temp,e) # 6=500mb 4=700mb
    print(filesNC)
    writeRaster(x, filename="/scratch/crimmins/NARR/currYr/processed/WVVFLX_daily_NARR_WUS_2020.grd", overwrite=TRUE)
    