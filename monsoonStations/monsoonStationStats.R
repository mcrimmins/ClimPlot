# track precip at AZ/NM stations through monsoon season
# using PRISM and station data
# code adapted from plotMonsoonPRISM.R
# MAC 06/18/2019

library(RCurl)
library(jsonlite)
library(ggplot2)
library(scales)
library(reshape2)
library(raster)
library(rasterVis)
library(PBSmapping)
library(Hmisc)
library(rgdal)
library(readr)
library(xtable)
library(magick)
library(rmarkdown)
library(knitr)

# load station data
SWCities <- read_csv("/home/crimmins/RProjects/ClimPlot/monsoonStations/monsoonStations.csv")

# get shapefiles
all_states <- map_data("state")
all_counties<-map_data("county")

## ---- download PRISM data ----
# Manually set universal date range - ACIS PRISM current day-1, correct for LINUX UTC time
#dateRangeStart="2018-06-15"
#dateRangeEnd="2018-09-30"

# auto date range...start with 6-15 and run on 6-17 to get two days of data, end on 10/1
dateRangeStart="2019-06-15"
dateRangeEnd=as.Date(format(as.POSIXct(Sys.Date()),usetz=TRUE, tz="Etc/GMT+7")) # date on local time zone
if(dateRangeEnd<"2019-06-16" | dateRangeEnd>="2019-10-01"){
  stop()
}

# generate dates -- keep with PRISM date
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)
#allDates<-as.Date(format(allDates, format="%m-%d-%Y"),format="%m-%d-%Y")

# AZ/NM bbox -115.004883,31.184609,-102.524414,37.387617
ACISbbox<-"-115,31,-102,38"

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

# get elev grid
elevGrid<-apply(t(out[["meta"]][["elev"]]),1,rev) 
elevGrid<-raster(elevGrid)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
elevGrid<-setExtent(elevGrid, gridExtent, keepres=FALSE, snap=FALSE)
elevGrid[elevGrid < 0] <- NA

# read into raster stack
rasterList<-lapply(matrixList, raster)
gridStack<-stack(rasterList)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
names(gridStack)<-allDates
# set 0 and neg to NA
gridStack[gridStack < 0] <- NA
## ----

# create summary grids ----
# total precip
totalPrecipAll<-calc(gridStack, sum, na.rm=TRUE)
totalPrecipAll[totalPrecipAll < 0] <- NA
# percent of average
JJASppt<-stack("/home/crimmins/RProjects/ClimPlot/PRISM/JJASppt.grd")
JJASppt<-JJASppt/25.4
moStack<-as.numeric(format(allDates[length(allDates)], format='%m'))-5

if (moStack==1) {
  normal<-JJASppt[[moStack]]*(as.numeric(format(allDates[length(allDates)], format='%d'))/monthDays(allDates[length(allDates)]))
} else if ( moStack==2) {
  norm1<-JJASppt[[moStack]]*(as.numeric(format(allDates[length(allDates)], format='%d'))/monthDays(allDates[length(allDates)]))
  normal<-norm1+JJASppt[[(moStack-1)]]
} else {
  norm1<-JJASppt[[moStack]]*(as.numeric(format(allDates[length(allDates)], format='%d'))/monthDays(allDates[length(allDates)]))
  norm2<-calc(JJASppt[[1:(moStack-1)]], sum, na.rm=TRUE)
  normal<-norm1+norm2
}
percPrecip<-totalPrecipAll/normal

# perc days with rain >0.01"
rainDays <- calc(gridStack, fun=function(x){sum(x > 0.01, na.rm = TRUE)})
percRainDays<-(rainDays/length(allDates))*100
percRainDays[percRainDays <= 0] <- NA

# daily intensity index
sdii<-totalPrecipAll/rainDays

# max 1-day precip
maxRain <- calc(gridStack, fun=function(x){max(x, na.rm = TRUE)})
maxRain[maxRain < 0] <- NA

# loop through stations and get totals and perc of average
SWCities$totalPrecip<-NA
SWCities$percPrecip<-NA
SWCities$rainDays<-NA
SWCities$maxRain<-NA

for(j in 1:nrow(SWCities)){
  # get time series - BaseT0
  SWCities$totalPrecip[j]<-t(raster::extract(totalPrecipAll, cellFromXY(totalPrecipAll, c(SWCities$lon[j],SWCities$lat[j]))))
  SWCities$percPrecip[j]<-t(raster::extract(percPrecip, cellFromXY(percPrecip, c(SWCities$lon[j],SWCities$lat[j]))))
  SWCities$rainDays[j]<-t(raster::extract(rainDays, cellFromXY(rainDays, c(SWCities$lon[j],SWCities$lat[j]))))
  SWCities$maxRain[j]<-t(raster::extract(maxRain, cellFromXY(maxRain, c(SWCities$lon[j],SWCities$lat[j]))))
}
SWCities$percPrecip<-as.integer(SWCities$percPrecip*100)
SWCities$rainDays<-as.integer(SWCities$rainDays)
SWCities$elev_m<-SWCities$elev_m*3.28084

# map with locations on it
# load map boundary data
xlim = c(-115,-102)
ylim = c(31,38)
# state boundaries
colnames(all_states)<-c("X","Y","PID","POS","region","subregion")
states= clipPolys(all_states, xlim=xlim,ylim=ylim, keepExtra=TRUE)
# county boundaries
colnames(all_counties)<-c("X","Y","PID","POS","region","subregion")
all_states = clipPolys(all_counties, xlim=xlim,ylim=ylim, keepExtra=TRUE)

# PERCENT of AVG Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("darkgoldenrod4", "white", "darkgreen","blue1","deepskyblue"))(50)
precBreaks<-seq(0,400,50)
precLabs<-as.character(seq(0,400,50))
precLabs[9]<-">400"
precLabs[1]<-"0"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())
p<-gplot(percPrecip*100) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="% of avg", limits=c(0,400),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = c(-115,-102), ylim = c(31,38), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Percent of Average Precipitation (%): ",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))


p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = station_name), 
               size = 2, col = "black", fontface = "bold", nudge_y = 0.07)

# write out file
png("/home/crimmins/RProjects/ClimPlot/monsoonStations/SW_Monsoon_PercentPrecip_wStations.png", width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read("/home/crimmins/RProjects/ClimPlot/monsoonStations/SW_Monsoon_PercentPrecip_wStations.png")
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+510+2150")
# And overwrite the plot without a logo
image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/monsoonStations/SW_Monsoon_PercentPrecip_wStations.png")


# DEAL WITH PANDOC ERROR
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
# render table in markdown
render('/home/crimmins/RProjects/ClimPlot/monsoonStations/monsoonStationTable.Rmd', output_file='swus_monsoon_stations.html',
       output_dir='/home/crimmins/RProjects/ClimPlot/monsoonStations', clean=TRUE)

