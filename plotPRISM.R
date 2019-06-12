# Get Gridded data from ACIS
# MAC 09/19/2018

library(RCurl)
library(jsonlite)
library(ggplot2)
#library(ggmap)
library(scales)
library(reshape2)
library(raster)
library(rasterVis)
library(PBSmapping)

# Manually set universal date range
#dateRangeStart="2018-06-15"
#dateRangeEnd="2018-09-19"
#allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# Automated Universal date range
days<-35
dateRangeEnd<-as.Date(format(Sys.time(), "%Y-%m-%d"))-1
dateRangeStart<-as.Date(format(Sys.time(), "%Y-%m-%d"))-days
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# get shapefiles
all_states <- map_data("state")

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

# read into raster stack
rasterList<-lapply(matrixList, raster)
gridStack<-stack(rasterList)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
names(gridStack)<-allDates
# set 0 and neg to NA
gridStack[gridStack <= 0] <- NA

# plot all days
# plot map - fix lines http://cameron.bracken.bz/finally-an-easy-way-to-fix-the-horizontal-lines-in-ggplot2-maps
# plot limits "-115.004883,31.184609,-102.524414,37.387617"
xlim = c(-115,-102)
ylim = c(31,38)

colnames(all_states)<-c("X","Y","PID","POS","region","subregion")
all_states = clipPolys(all_states, xlim=xlim,ylim=ylim, keepExtra=TRUE)
#colnames(all_states)<-c("Lon","Lat","PID","POS","region","subregion")

theme_set(theme_bw())

p<-gplot(gridStack) + geom_tile(aes(fill = value)) +
  facet_wrap(~ variable, ncol = 7) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  scale_fill_distiller(palette = "Spectral", direction = -1, na.value="azure3", name="inches")+
  coord_equal()
p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA )+
  scale_x_continuous(breaks = c(-120,-140))+
  ggtitle("Daily Total Precip (inches) - PRISM")+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),"\nThe University of Arizona\nhttps://cals.arizona.edu/climate/"))


# write to image file
ptm <- proc.time()
#png(paste0("./",names(precip[[day1]]),"_",names(precip[[day2]]),"_Composite500mbPlot.png"), width = 7, height = 5, units = "in", res = 300L)
png("/home/crimmins/RProjects/ClimPlot/plots/AZNM_PRISMPrecip.png", width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()
proc.time() - ptm


