# download NARR data
# MAC 03/03/2020

library(RCurl)
#library(raster)
#library(ncdf4)

# get land/topo
# URL<-"ftp://ftp.cdc.noaa.gov/Datasets/NARR/time_invariant/hgt.sfc.nc"
#    fileName<-  paste0("/scratch/crimmins/NARR/hgt.sfc.nc")
#    download.file(URL, destfile = fileName)
# URL<-"ftp://ftp.cdc.noaa.gov/Datasets/NARR/time_invariant/land.nc"
#   fileName<-  paste0("/scratch/crimmins/NARR/land.nc")
#   download.file(URL, destfile = fileName)

   
# # # monolevel data   
filePrefix<-"pres.sfc."
# #Set years
yr1<-1979
yr2<-2019
# ---- download each year
for(i in yr1:yr2){
  URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/",filePrefix,i,".nc")
  fileName<-  paste0("/scratch/crimmins/NARR/monolevel/sfcpres/",filePrefix,i,".nc")
  download.file(URL, destfile = fileName)
  print(fileName)
}
  

# # 3 hrly monolevel data   
# filePrefix<-"pres.sfc."
# #Set years
# yr1<-1979
# yr2<-2019
# # ---- download each year
# for(i in yr1:yr2){
#   URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/",filePrefix,i,".nc")
#   fileName<-  paste0("/scratch/crimmins/NARR/monolevel/sfcpres/",filePrefix,i,".nc")
#   download.file(URL, destfile = fileName)
#   print(fileName)
# }


# # pressure level
# # ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/hgt.197901.nc
# 
# months<-c("01","02","03","04","05","06","07","08","09","10","11","12")  
# 
# # monolevel data   
# filePrefix<-"uwnd."
# #Set years
# yr1<-1979
# yr2<-2019
# # ---- download each year 
# for(i in yr1:yr2){
#  for(j in 1:12) {
#     URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/pressure/",filePrefix,i,months[j],".nc")
#     fileName<-  paste0("/scratch/crimmins/NARR/pressure/wind/",filePrefix,i,months[j],".nc")
#     download.file(URL, destfile = fileName)
#     print(fileName)
#   }
# }

