# develop precip percentiles from daily PRISM
# adapted from PRISMPrecipPerc.R
# MAC 09/9/2022

library(RCurl)
library(jsonlite)
library(raster)

# loop through each and create growing stack of cumulative precip - does not work with webservice ----
# write to file
cumPrecipAll <- stack()
for(year in 1983:2021){
  # create current date
  dateRangeStart=paste0(year,"-06-01")
  dateRangeEnd= paste0(year,"-09-30")
  
  # generate dates -- keep with PRISM date
  allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)
  
  # AZ/NM bbox -115.004883,31.184609,-102.524414,37.387617
  ACISbbox<-"-115,31,-102,38"
  
  print("downloading data")
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
  ##
  
  print("processing raster stack")
  # get cumulative total precip
  # parallell calc
  ptm <- proc.time()
  beginCluster(7)
  tempGrid <- clusterR(gridStack, calc, args=list(fun=cumsum))
  endCluster()
  proc.time() - ptm
  
  # write to file
  writeRaster(tempGrid,filename=paste0("/home/crimmins/RProjects/ClimPlot/monsoonCumPrecip/AZNM_PRISM_June_Sept_",year,"_cumPrecip.grd"), overwrite=TRUE)
  
  
  # cumPrecipAll <- stack(cumPrecipAll , tempGrid)
  print(year)
}
# ----

# test a raster
#test<-stack("/home/crimmins/RProjects/ClimPlot/monsoonCumPrecip/AZNM_PRISM_June_Sept_1981_cumPrecip.grd")

# load years into stack and reorder into days
#setwd("~/RProjects/ClimPlot/monsoonCumPrecip")
allCumSum <- do.call(stack, lapply(list.files(path = "~/RProjects/ClimPlot/monsoonCumPrecip", pattern = "*.grd", full.names=TRUE), stack))
writeRaster(allCumSum,filename=paste0("/home/crimmins/RProjects/ClimPlot/AZNM_PRISM_June_Sept_cumPrecip_1981_2021.grd"), overwrite=TRUE)

#####
#  process into daily climo

# load allCumSum
allCumSum<-stack("/home/crimmins/RProjects/ClimPlot/AZNM_PRISM_Monsoon_cumPrecip_1981_2018.grd")

# create date df
# allDates<-as.data.frame(seq(as.Date("1981-01-01"),as.Date("2021-12-31"),by="day"))
#   colnames(allDates)<-"dates"
# allDates$doy<-as.numeric(format(allDates$dates,"%j"))  
# subDays<-allDates[allDates$doy >= "2020-06-15" & allDates$doy <= "2020-09-30", ]

# get doy index
ndays<-length(seq(as.Date("1981-06-01"),as.Date("1981-09-30"),by="day"))
doyIdx<-rep(seq(1,ndays,1),nlayers(allCumSum)/ndays)

meanDailyCumPrecip<- stackApply(allCumSum, doyIdx, fun = mean)

# convert to mm
meanDailyCumPrecip<-meanDailyCumPrecip*25.4

# write to file
writeRaster(meanDailyCumPrecip,filename=paste0("/home/crimmins/RProjects/ClimPlot/AZNM_PRISM_June_Sept_meanDailyCumPrecip_1981_2021.grd"), overwrite=TRUE)


r <- calc(meanDailyCumPrecip, function(x) min(which(x > 1)))
plot(r, zlim=c(1,10))


