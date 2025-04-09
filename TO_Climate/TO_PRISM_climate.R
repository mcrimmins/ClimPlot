# extract ACIS PRISM data for Tohono O'odham Nation
# MAC 01/09/23
# info on the RCC-ACIS webservice can be found here https://www.rcc-acis.org/docs_webservices.html

# load necessary packages
library(RCurl)
library(jsonlite)
library(raster)    
library(rgdal)

#####
# Get TO Nation boundary for area of extent and zonal summaries
# get shapefile from https://catalog.data.gov/dataset/tiger-line-shapefile-2018-nation-u-s-current-american-indian-alaska-native-native-hawaiian-area

# download zip file to working directory
download.file("https://www2.census.gov/geo/tiger/TIGER2018/AIANNH/tl_2018_us_aiannh.zip", destfile = "tl_2018.zip", method="curl") # created subdir 'shapes'
unzip("tl_2018.zip",overwrite = TRUE)
# read in shapefile and subset to TO polygon
boundaries<-readOGR("tl_2018_us_aiannh.shp")
TO<-subset(boundaries, NAMELSAD=="Tohono O'odham Nation Reservation")
#####

#####
# set parameters for downloading PRISM data from RCC-ACIS

# Set date ranges - monthly data from 1895-present are available 
dateRangeStart="1895-01-01"
dateRangeEnd= "2022-12-31"

# generate date sequence -- keep with PRISM date
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),by="month")

# get bounding box from shapefile extent
bbox<-extent(TO)
  ACISbbox<-paste0(bbox@xmin,",",bbox@ymin,",",bbox@xmax,",",bbox@ymax)

# ACIS query - change elems to variable of interest
jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"mly_pcpn","meta":"ll,elev","output":"json"}') # or uid
#jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"2","elems":"pcpn","meta":"ll","output":"json"}') # or uid

out<-postForm("http://data.rcc-acis.org/GridData",
              .opts = list(postfields = jsonQuery,
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)
#####

#####
# convert matrix data to raster

# convert to list of matrices, flipud with PRISM
matrixList <- vector("list",length(out$data))
for(i in 1:length(out$data)){
  matrixList[[i]]<-apply(t(out$data[[i]][[2]]),1,rev)
}

# read into raster stack
rasterList<-lapply(matrixList, raster)
  gridStack<-stack(rasterList)
  gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
  gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
names(gridStack)<-allDates
# set -999/missing to NA
gridStack[gridStack == -999] <- NA
#####

#####
# plot some data - most recent month
plot(gridStack[[nlayers(gridStack)]])
plot(TO, add=TRUE)
#####

#####
# save raster stack to tif for use in ArcGIS
proj4string(gridStack) <- CRS("+init=epsg:4326") # set projection for ArcGIS
writeRaster(gridStack, file="TO_PRISM_monthly_precip_1895_2022.tif")
#####

#####
# extract a monthly time series using TO boundary
moPrecip <- t(extract(gridStack, TO, fun='mean', na.rm=TRUE, df=TRUE, weights = FALSE))
moPrecip <- moPrecip[2:nrow(moPrecip),] # drop that first ID row
moPrecip <- cbind.data.frame(allDates, moPrecip)
#####

#####
# plot a time series
library(ggplot2)
ggplot(moPrecip, aes(allDates,moPrecip))+
  geom_line()+
  ylab("Precip (in)")+
  xlab("date")+
  ggtitle("Monthly average precip for Tohono O'odham Nation - PRISM 1895-2022")
#####


