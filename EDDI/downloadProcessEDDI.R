# download/process EDDI data
# MAC 08/18/2020

library(RCurl)
library(raster)
#library(ncdf4)

# create date list of last date in month
dates<-seq(as.Date("1980-02-01"), as.Date("2020-01-01"), by="1 month") - 1
dateCode<-format(dates, "%Y%m%d")

# EDDI ftp dir
# ftp://ftp2.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/1980/
# ftp://ftp2.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/1980/EDDI_ETrs_01mn_19800101.asc

# # prefix  
filePrefix<-"06mn" # 03mn, 06mn, 01mn


x <- stack()
# download and build stacks
for(i in 1:length(dates)){
  URL<-paste0("ftp://ftp2.psl.noaa.gov/Projects/EDDI/CONUS_archive/data/",format(dates[i],"%Y"),"/EDDI_ETrs_",filePrefix,"_",dateCode[i],".asc")
  download.file(URL, destfile = "/home/crimmins/RProjects/ClimPlot/EDDI/temp/temp.asc")
  temp<-raster("/home/crimmins/RProjects/ClimPlot/EDDI/temp/temp.asc")
  x <- stack(x, temp)
  print(URL)
}

names(x)<-dates

writeRaster(x, filename="/scratch/crimmins/EDDI/EDDI_06mn_CONUS_1980_2019.grd", overwrite=TRUE)

#test<-stack("/scratch/crimmins/EDDI/EDDI_01mn_CONUS_1980_2019.grd")
