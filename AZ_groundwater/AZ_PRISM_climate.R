# extract ACIS PRISM data for AZ study area
# MAC 05/15/23
# info on the RCC-ACIS webservice can be found here https://www.rcc-acis.org/docs_webservices.html

# load necessary packages
library(RCurl)
library(jsonlite)
library(raster)    
library(rgdal)

#####
# read in shapefile
studyAreas<-readOGR("./AZ_groundwater/Union_Study_Areas.shp")
#####

#####
# set parameters for downloading PRISM data from RCC-ACIS

# Set date ranges - monthly data from 1895-present are available, download 1991-present to calculate 1991-2020 normals period 
dateRangeStart="1991-01-01"
dateRangeEnd= "2022-12-31"

# generate date sequence -- keep with PRISM date
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),by="month")

# get bounding box from shapefile extent
bbox<-extent(studyAreas)
  ACISbbox<-paste0(bbox@xmin,",",bbox@ymin,",",bbox@xmax,",",bbox@ymax)

# ACIS query - change elems to variable of interest
#jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"mly_pcpn","meta":"ll,elev","output":"json"}') # or uid
jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":[{"name":"mly_pcpn"},{"name":"mly_avgt"}],"meta":"ll,elev","output":"json"}') # or uid


out<-postForm("http://data.rcc-acis.org/GridData",
              .opts = list(postfields = jsonQuery,
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)
#####

#### process precip data from out ####
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
# set 0 and neg to NA
gridStack[gridStack < 0] <- NA
#####

#### process temperature data from out #####
matrixList <- vector("list",length(out$data))
for(i in 1:length(out$data)){
  matrixList[[i]]<-apply(t(out$data[[i]][[3]]),1,rev)
}

# read into raster stack
rasterList<-lapply(matrixList, raster)
tempStack<-stack(rasterList)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
tempStack<-setExtent(tempStack, gridExtent, keepres=FALSE, snap=FALSE)
names(tempStack)<-allDates
# set 0 and neg to NA
tempStack[tempStack <= -999] <- NA
#####

#####
# plot some precip data - most recent month
plot(gridStack[[nlayers(gridStack)]])
plot(studyAreas, add=TRUE)
# plot some temp data - most recent month
plot(tempStack[[nlayers(gridStack)]])
plot(studyAreas, add=TRUE)

#####

#####
# save raster stack to tif for use in ArcGIS
#proj4string(gridStack) <- CRS("+init=epsg:4326") # set projection for ArcGIS
#writeRaster(gridStack, file="TO_PRISM_monthly_precip_1895_2022.tif")
#####

#####
# extract a monthly precip time series using study area boundary
moPrecip <- t(extract(gridStack, studyAreas, fun='mean', na.rm=TRUE, df=TRUE, weights = FALSE))
moPrecip <- moPrecip[2:nrow(moPrecip),] # drop that first ID row
moPrecip <- cbind.data.frame(allDates, moPrecip)
colnames(moPrecip)<-c("date",as.character(studyAreas$Area_Name[1:5]),as.character(studyAreas$SUBBASIN_N[6:11])) # tried to add some names to the columns
colnames(moPrecip)[10]<-"DOUGLAS2"
# extract a monthly temp time series using study area boundary
moTemp <- t(extract(tempStack, studyAreas, fun='mean', na.rm=TRUE, df=TRUE, weights = FALSE))
moTemp <- moTemp[2:nrow(moTemp),] # drop that first ID row
moTemp <- cbind.data.frame(allDates, moTemp)
colnames(moTemp)<-c("date",as.character(studyAreas$Area_Name[1:5]),as.character(studyAreas$SUBBASIN_N[6:11])) # tried to add some names to the columns
colnames(moTemp)[10]<-"DOUGLAS2"
#####

#####
# plot a precip time series
temp<-tidyr::gather(moPrecip, area, precip, 2:12)
library(ggplot2)
ggplot(temp, aes(date,precip, color=area))+
  geom_line()+
  ylab("Precip (in)")+
  xlab("date")+
  ggtitle("Monthly average precip for study area sites - PRISM 1991-2022")
# plot a temp time series
temp<-tidyr::gather(moTemp, area, temp, 2:12)
library(ggplot2)
ggplot(temp, aes(date,temp, color=area))+
  geom_line()+
  ylab("Temp (F)")+
  xlab("date")+
  ggtitle("Monthly average temps for study area sites - PRISM 1991-2022")


#####


