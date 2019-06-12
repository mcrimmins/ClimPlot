# real time plotting of GH and Precip
# MAC 01/28/19

library(raster)
library(RCurl)
library(ncdf4)
library(rasterVis)
library(grid)

# map layers
states <- getData('GADM', country='United States', level=1)
us <- getData('GADM', country='United States', level=0)
mx <- getData('GADM', country='Mexico', level=0)
cn <- getData('GADM', country='Canada', level=0)

# global precip link
# https://www.esrl.noaa.gov/psd/data/gridded/data.cpc.globalprecip.html
# ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/precip.2019.nc

# get current year
currYr<-format(Sys.time(), "%Y")
prevYr<-as.numeric(format(Sys.time(), "%Y"))-1
# get US CPC precip
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/cpc_us_precip/RT/precip.V1.0.",prevYr,".nc")
  download.file(URL, destfile = paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/USprecip",prevYr,".nc"), method="curl")
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/cpc_us_precip/RT/precip.V1.0.",currYr,".nc")
  download.file(URL, destfile = paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/USprecip",currYr,".nc"), method="curl")
  # get hgts
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/hgt.",prevYr,".nc")
  download.file(URL, destfile = paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/hgt",prevYr,".nc"), method="curl")
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/hgt.",currYr,".nc")
  download.file(URL, destfile = paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/hgt",currYr,".nc"), method="curl")
 
# load data into rasters
precipPrev<-stack(paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/USprecip",prevYr,".nc"))
precipCurr<-stack(paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/USprecip",currYr,".nc"))

# load gh data
#gh<-stack("hgt2018.nc", level=6)
hgt500Prev <- brick(paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/hgt",prevYr,".nc"), level=4) # level 6 is 500 mb, 4 is 700mb
hgt500Curr <- brick(paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/hgt",currYr,".nc"), level=4)

# combine precip rasters
if(exists("precipCurr")==TRUE){
  precip<-stack(precipPrev,precipCurr)
}else{
  precip<-precipPrev
}

# combine heights
if(exists("hgt500Curr")==TRUE){
  hgt500<-stack(hgt500Prev,hgt500Curr)
}else{
  hgt500<-hgt500Prev
}

# trim to common length
layerN<-min(c(nlayers(precip), nlayers(hgt500)))
  precip<-precip[[1:layerN]]
  hgt500<-hgt500[[1:layerN]]
  
# get stacks to have same number of layers, pad to largest stack - prob hgts
# create empty raster
# empty<-precip[[nlayers(precip)]]
#   empty[empty >= 0] <- 1
# precip<-stack(precip, stack(replicate(nlayers(hgt500)-nlayers(precip), empty)))

# set to same number of layers
#names(precip)<-names(hgt500)

# ----- RasterVis plotting solution
# set 0 precip to NA
precip[precip <= 0] <- NA

# set number of days to plot
 days<-34 #34
   day1<-nlayers(precip)-days
   day2<-nlayers(precip)
# or set date range
#day1<-which(names(precip)=="X2018.12.01")
#day2<-which(names(precip)=="X2018.12.31")

# create color scheme for precip
#mycols <- rasterTheme(region=colorRampPalette(brewer.pal(9,'YlGnBu'))(100))
mycols <- rasterTheme(region=colorRampPalette(rev(brewer.pal(9,'Spectral')))(100))
cols.at <- seq(0, 50, 2)
# create contour cuts 5000 to 6000 m for gh
levs.at<-seq(2750,3250, 25)
# create levs and colors for pwat
pwat.cols <- rasterTheme(region=colorRampPalette(brewer.pal(9,'BuGn'))(100))
pwat.cols.at <- seq(0, 50, 5)

p<-levelplot(rotate(precip[[day1:day2]]), margin=FALSE, par.settings = mycols,layout=c(7,5),
          xlim=c(-130, -100), ylim=c(30, 50), at=cols.at, main="Daily Avg 700mb GH (m) & Total Precip (mm)")+
  contourplot(rotate(hgt500[[day1:day2]]),
              xlim=c(-130, -100), ylim=c(30, 50), at=levs.at,
              lwd = 0.4,
              labels = list(cex = 0.6),
              label.style = 'align')+
  # contourplot(rotate(pwat[[day1:day2]]),
  #             xlim=c(-150, -85), ylim=c(20, 60), par.settings=pwat.cols, at=pwat.cols.at,
  #             lwd = 0.1,
  #             labels = list(cex = 0.6),
  #             label.style = 'align',
  #             col='blue')+
  layer(sp.polygons(states, col = 'gray40', lwd=0.3))
  #layer(sp.polygons(mx, col = 'gray40', lwd=0.3))+
  #layer(sp.polygons(cn, col = 'gray40', lwd=0.3))

# write to image file
ptm <- proc.time()
  png("/home/crimmins/RProjects/ClimPlot/plots/USPrecip_700mbPlot.png", width = 16, height = 8, units = "in", res = 300L)
  #grid.newpage()
  print(p, newpage = FALSE)
  ## now add the text 
  grid.text(paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),"\nThe University of Arizona\nhttps://cals.arizona.edu/climate/"),
            x=.90, y=.03, 
            gp = gpar(col=1, 
                      fontfamily="Arial", cex=0.5)) 
dev.off()
proc.time() - ptm

# to do 
# create precip anom maps from long-term means
# add logos?
