# extract ACIS PRISM data for Tribal lands
# MAC 10/20/20

# get monthly PRISM from RCC ACIS
library(RCurl)
library(jsonlite)
library(raster)    
# create current date
#dateRangeStart=paste0(year,"-06-15")
#dateRangeEnd= paste0(year,"-09-30")
dateRangeStart="1900-01-01"
dateRangeEnd= "2020-12-31"

# load tribal shapefile
library(rgdal)
tribes <- readOGR("/home/crimmins/RProjects/ClimPlot/tribes", "indlanp020")
tribes<-subset(tribes, NAME=="Hopi Indian Reservation")
extent(tribes)

# generate dates -- keep with PRISM date
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),by="month")

# AZ/NM bbox -115.004883,31.184609,-102.524414,37.387617
#extent(tribes)
ACISbbox<-"-113.3,35,-110,36.5" 

# ACIS query
jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"mly_pcpn","meta":"ll,elev","output":"json"}') # or uid
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
allDates<-as.data.frame(allDates)
allDates$month<-as.numeric(format(allDates$allDates, "%m"))
allDates$year<-as.numeric(format(allDates$allDates, "%Y"))

# water years
library(tidyverse)
library(lubridate)

#make sure R knows your dates are dates - you mention they're 'yyyy-mm-dd', so
allDates <- allDates %>% mutate(allDates = ymd(allDates)) 
         #in this script or another, define a water year function
         water_year <- function(date) {
           ifelse(month(date) < 10, year(date), year(date)+1)}
         
         #new wateryear column for your data, using your new function
         allDates <- allDates %>% 
           mutate(wateryear = water_year(allDates))

#idx<-which(allDates$month %in% c(7,8,9)) # grab only summer months
#allDates<-allDates[idx,]
#gridStack<-gridStack[[idx]]
sumSeas<-stackApply(gridStack, allDates$wateryear, fun = sum)
seasAvgPrecip<-cellStats(sumSeas, 'mean')
seasAvgPrecip<-cbind.data.frame(unique(allDates$wateryear),seasAvgPrecip)
# seasAvgPrecip$percRank<-perc.rank(seasAvgPrecip$seasAvgPrecip) 
# colnames(seasAvgPrecip)<-c("year","avgPrecip","percRank")
# # names
# seasAvgPrecip$anomName<-"normal"
# seasAvgPrecip$anomName[seasAvgPrecip$percRank<=0.33] <- "dry"
# seasAvgPrecip$anomName[seasAvgPrecip$percRank>=0.66] <- "wet"

# library(cowplot)   
# ggplot(seasAvgPrecip, aes(year,avgPrecip, fill=as.factor(seasAvgPrecip$anomName)) )+
#   geom_bar(stat = 'identity')+
#   ggtitle("Regional Average Total Precip (July-Aug-Sept)")+
#   geom_hline(yintercept=mean(seasAvgPrecip$avgPrecip), color="black")+
#   geom_hline(yintercept=median(seasAvgPrecip$avgPrecip), color="red")+
#   scale_fill_manual(values = c("saddlebrown", "grey", "forestgreen"), name="tercile")+
#   ylab("inches")

write.csv(seasAvgPrecip, file="Hopi_OctSep_totalPrecip.csv")

