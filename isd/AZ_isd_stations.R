# NOAA ISD stations
# MAC 12/30/2019

library(rnoaa)
library(leaflet)

## bounding box for AZ -114.81651	31.332177	-109.045223	37.00426
bbox <- c(-114.81651,31.332177,-109.045223,37.00426)
AZstations<-isd_stations_search(bbox = bbox)

# get por length
AZstations$begin<-as.Date(as.character(AZstations$begin), format="%Y%m%d")
AZstations$end<-as.Date(as.character(AZstations$end), format="%Y%m%d")
AZstations$porLength<-AZstations$end - AZstations$begin

# plot stations
leaflet() %>%
  addTiles() %>%
  addCircles(lng = AZstations$lon,
             lat = AZstations$lat,
             popup = AZstations$station_name) %>%
  clearBounds()

# get isd stations
# rappdirs::user_cache_dir("rnoaa/isd") # see cache

n<-which(AZstations$station_name=="TUCSON INTERNATIONAL AIRPORT")
station1 <- isd(usaf=AZstations$usaf[n], wban=AZstations$wban[n], year=2019, cleanup = TRUE,
                progress = TRUE)
