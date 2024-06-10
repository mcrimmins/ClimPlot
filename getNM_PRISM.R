# get monsoon daily PRISM for New Mexico
# adapted from ClimPlot/PRISMPrecipPerc.R
# MAC 05/12/2023

library(RCurl)
library(jsonlite)
library(raster)

# loop through each and create growing stack of cumulative precip - does not work with webservice ----
# write to file
allPrecip <- stack()
for(year in 2010:2022){
  # create current date
  dateRangeStart=paste0(year,"-06-15")
  dateRangeEnd= paste0(year,"-09-30")
  
  # generate dates -- keep with PRISM date
  allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)
  
  # NM bbox
  ACISbbox<-"-109.237061,31.137603,-102.821045,37.151561"
  
  # ACIS query
  jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"pcpn","meta":"ll,elev","output":"json"}') # or uid
  #jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"2","elems":"pcpn","meta":"ll","output":"json"}') # or uid
  
  out<-postForm("http://data.rcc-acis.org/GridData",
                .opts = list(postfields = jsonQuery,
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  
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
  ## add dates as names, adjusted to day of precip
  names(gridStack)<-allDates-1
  
  # add to main stack
  allPrecip<-stack(allPrecip,gridStack)
  
  # cumPrecipAll <- stack(cumPrecipAll , tempGrid)
  print(year)
}
# ----

crs(allPrecip) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#writeRaster(allPrecip,filename=paste0("/home/crimmins/RProjects/precipPatterns/gridded/Tucson_PRISM_monsoon_2007_2022.grd"), overwrite=TRUE)

writeRaster(allPrecip, "PRISM_daily_precip_NewMexico_June15-Sept30_2010_2022.nc", overwrite=TRUE, format="CDF",     varname="precipitation", varunit="in", 
            longname="PRISM Daily Precipitation -- raster stack to netCDF", xname="Longitude",   yname="Latitude", zname="Time (day)")


