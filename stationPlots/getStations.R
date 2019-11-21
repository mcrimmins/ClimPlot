# Get station meta data from RCC ACIS
# MAC 11/18/19

# to do: figure out way to gauge missing days?, station types/threadex stations

# load libraries
library(RCurl)
library(jsonlite)

# get valid date ranges
jsonQuery='{"state":"NM","meta":"sids,name,valid_daterange,ll","elems":"pcpn"}'
out<-postForm("http://data.rcc-acis.org/StnMeta", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
outList<-fromJSON(out, flatten = FALSE) # 
# wrangle into dataframe
  dates<-data.frame(matrix(unlist(outList$meta$valid_daterange), nrow=nrow(outList$meta), byrow=T))
  sids<-as.data.frame(t(sapply(outList$meta$sids, '[', seq(max(sapply(outList$meta$sids, length))))))
  names<-outList$meta$name
  ll<-data.frame(matrix(unlist(outList$meta$ll), nrow=nrow(outList$meta), byrow=T))
  stations<-cbind(names,ll,dates,sids)
  colnames(stations)[2:5]<-c("lon","lat","beginYr","endYr")
  stations$beginYr<-as.Date(stations$beginYr, format="%Y-%m-%d")
  stations$endYr<-as.Date(stations$endYr, format="%Y-%m-%d")
  stations$obsN<-stations$endYr-stations$beginYr

# find stations with data in current year
stations<-stations[which(stations$endYr>=as.Date(paste0(format(Sys.Date(), "%Y"),"-01-01")) & stations$obsN/365>=30 ),]
  
# find station type,  find ThreadEx stations too
# test<-mapply(grepl,"US", stations)

# plot points on map
library(leaflet)

leaflet(data = stations) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(names), label = ~as.character(names))