# real time plotting of GH and Precip
# MAC 01/28/19

library(raster)
library(RCurl)
library(ncdf4)
library(rasterVis)
library(grid)

# to do 
# create precip anom maps from long-term means
# add logos?

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
# URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/cpc_us_precip/RT/precip.V1.0.",prevYr,".nc")
#   download.file(URL, destfile = paste0("./tempFiles/precip",prevYr,".nc"), method="curl")
# URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/cpc_us_precip/RT/precip.V1.0.",currYr,".nc")
#   download.file(URL, destfile = paste0("./tempFiles/precip",currYr,".nc"), method="curl")
  # get global precip
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/precip.",prevYr,".nc")
  download.file(URL, destfile = paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/precip",prevYr,".nc"), method="curl")
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/precip.",currYr,".nc")
  download.file(URL, destfile = paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/precip",currYr,".nc"), method="curl")
  # get hgts
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/hgt.",prevYr,".nc")
  download.file(URL, destfile = paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/hgt",prevYr,".nc"), method="curl")
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/hgt.",currYr,".nc")
  download.file(URL, destfile = paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/hgt",currYr,".nc"), method="curl")
# get pwat 
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/surface/pr_wtr.eatm.",prevYr,".nc")
  download.file(URL, destfile = paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/pwat",prevYr,".nc"), method="curl")
URL <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/surface/pr_wtr.eatm.",currYr,".nc")
  download.file(URL, destfile = paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/pwat",currYr,".nc"), method="curl")  
  
# load data into rasters
precipPrev<-stack(paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/precip",prevYr,".nc"))
precipCurr<-stack(paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/precip",currYr,".nc"))

# load gh data
#gh<-stack("hgt2018.nc", level=6)
hgt500Prev <- brick(paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/hgt",prevYr,".nc"), level=6) # level 6 is 500 mb
hgt500Curr <- brick(paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/hgt",currYr,".nc"), level=6)

# load pwat
pwatPrev <- stack(paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/pwat",prevYr,".nc"))
pwatCurr <- stack(paste0("/home/crimmins/RProjects/ClimPlot/tempFiles/pwat",currYr,".nc"))

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

# combine pwat
if(exists("pwatCurr")==TRUE){
  pwat<-stack(pwatPrev,pwatCurr)
}else{
  pwat<-pwatPrev
}

# trim to common length
layerN<-min(c(nlayers(precip), nlayers(hgt500), nlayers(pwat)))
  precip<-precip[[1:layerN]]
  hgt500<-hgt500[[1:layerN]]
  pwat<-pwat[[1:layerN]]
  
# get stacks to have same number of layers, pad to largest stack - prob hgts
# create empty raster
# empty<-precip[[nlayers(precip)]]
#   empty[empty >= 0] <- 1
# precip<-stack(precip, stack(replicate(nlayers(hgt500)-nlayers(precip), empty)))

# set to same number of layers
#names(precip)<-names(hgt500)

# ----- RasterVis plotting solution
# set 0 precip to NA
values(precip)[values(precip) <= 0] = NA

# set number of days to plot
 days<-34
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
levs.at<-seq(5000,6000, 100)
# create levs and colors for pwat
pwat.cols <- rasterTheme(region=colorRampPalette(brewer.pal(9,'BuGn'))(100))
pwat.cols.at <- seq(0, 50, 5)

p<-levelplot(rotate(precip[[day1:day2]]), margin=FALSE, par.settings = mycols,layout=c(7,5),
          xlim=c(-150, -85), ylim=c(20, 60), at=cols.at, main="Daily Avg 500mb GH (m) & Total Precip (mm)")+
  contourplot(rotate(hgt500[[day1:day2]]),
              xlim=c(-150, -85), ylim=c(20, 60), at=levs.at,
              lwd = 0.4,
              labels = list(cex = 0.6),
              label.style = 'align')+
  # contourplot(rotate(pwat[[day1:day2]]),
  #             xlim=c(-150, -85), ylim=c(20, 60), par.settings=pwat.cols, at=pwat.cols.at,
  #             lwd = 0.1,
  #             labels = list(cex = 0.6),
  #             label.style = 'align',
  #             col='blue')+
  layer(sp.polygons(us, col = 'gray40', lwd=0.3))+
  layer(sp.polygons(mx, col = 'gray40', lwd=0.3))+
  layer(sp.polygons(cn, col = 'gray40', lwd=0.3))

# write to image file
ptm <- proc.time()
  png("/home/crimmins/RProjects/ClimPlot/plots/Precip_500mbPlot.png", width = 16, height = 8, units = "in", res = 300L)
  #grid.newpage()
  print(p, newpage = FALSE)
  ## now add the text 
  grid.text(paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),"\nThe University of Arizona\nhttps://cals.arizona.edu/climate/"),
            x=.90, y=.03, 
            gp = gpar(col=1, 
                      fontfamily="Arial", cex=0.5)) 
dev.off()
proc.time() - ptm

# GH, pwat plot
pPW<-levelplot(rotate(pwat[[day1:day2]]),layout=c(7,5),
             xlim=c(-150, -85), ylim=c(20, 60), par.settings=pwat.cols, at=pwat.cols.at, margin=FALSE, 
             alpha.regions=0.5,
             main="Daily Avg 500mb GH (m) & Avg PWAT (mm)")+
  contourplot(rotate(hgt500[[day1:day2]]),
              xlim=c(-150, -85), ylim=c(20, 60), at=levs.at,
              lwd = 0.4,
              labels = list(cex = 0.6),
              label.style = 'align')+
  layer(sp.polygons(us, col = 'gray40', lwd=0.3))+
  layer(sp.polygons(mx, col = 'gray40', lwd=0.3))+
  layer(sp.polygons(cn, col = 'gray40', lwd=0.3))
# write to image file
ptm <- proc.time()
png("/home/crimmins/RProjects/ClimPlot/plots/PWAT_500mbPlot.png", width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(pPW, newpage = FALSE)
## now add the text 
grid.text(paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),"\nThe University of Arizona\nhttps://cals.arizona.edu/climate/"),
          x=.75, y=.03, 
          gp = gpar(col=1, 
                    fontfamily="Arial", cex=0.5)) 
dev.off()
proc.time() - ptm

# develop mean/stdev of gh heights, total precip
meanGH<-mean(hgt500[[day1:day2]], na.rm=TRUE)
sdGH<-overlay(hgt500[[day1:day2]], fun=sd, na.rm=TRUE)
totPrecip<-sum(precip[[day1:day2]],na.rm=TRUE)
  totPrecip[totPrecip <= 0] <- NA
meanPWAT<-mean(pwat[[day1:day2]], na.rm=TRUE)

prec.at <- seq(0, 250, 10)
sdlev.at <- seq(0, 250, 25)
pComp<-levelplot(rotate(totPrecip), margin=FALSE, par.settings = mycols,
             xlim=c(-150, -85), ylim=c(20, 60), at=prec.at, 
             main=paste0(names(precip[[day1]]),"-",names(precip[[day2]]), "Avg/Stdev 500mb GH(m), Prcp(mm) & PW(mm)"))+
  contourplot(rotate(meanGH),
              xlim=c(-150, -85), ylim=c(20, 60), at=levs.at,
              lwd = 0.4,
              labels = list(cex = 0.6),
              label.style = 'align')+
   contourplot(rotate(sdGH), at=sdlev.at,
               xlim=c(-150, -85), ylim=c(20, 60),
               lwd = 0.3,
               labels = list(cex = 0.6),
               label.style = 'align',
               col='red')+
  levelplot(rotate(meanPWAT),
            xlim=c(-150, -85), ylim=c(20, 60), par.settings=pwat.cols, at=pwat.cols.at, margin=FALSE, 
            alpha.regions=0.4)+
  layer(sp.polygons(us, col = 'gray40', lwd=0.3))+
  layer(sp.polygons(mx, col = 'gray40', lwd=0.3))+
  layer(sp.polygons(cn, col = 'gray40', lwd=0.3))

# write to image file
#png(paste0("./",names(precip[[day1]]),"_",names(precip[[day2]]),"_Composite500mbPlot.png"), width = 7, height = 5, units = "in", res = 300L)
png("/home/crimmins/RProjects/ClimPlot/plots/PeriodAvg500mbPlot.png", width = 7, height = 5, units = "in", res = 300L)
#grid.newpage()
print(pComp, newpage = FALSE)
## now add the text 
grid.text(paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),"\nThe University of Arizona\nhttps://cals.arizona.edu/climate/"),
          x=.90, y=.04, 
          gp = gpar(col=1, 
                    fontfamily="Arial", cex=0.5)) 
dev.off()

# 500mb GH plot ----

# create color scheme for GHs
#mycols <- rasterTheme(region=colorRampPalette(brewer.pal(9,'YlGnBu'))(100))
mycols <- rasterTheme(region=colorRampPalette(rev(brewer.pal(9,'Spectral')))(100))
cols.at <- seq(5000, 6000, 100)
# create contour cuts 5000 to 6000 m for gh
levs.at<-seq(5000,6000, 100)

p<-levelplot(rotate(hgt500[[day1:day2]]), margin=FALSE, par.settings = mycols,layout=c(7,5),
             xlim=c(-150, -85), ylim=c(20, 60), at=cols.at, main="Daily Avg 500mb GH (m)")+
  contourplot(rotate(hgt500[[day1:day2]]),
              xlim=c(-150, -85), ylim=c(20, 60), at=levs.at,
              lwd = 0.4,
              labels = list(cex = 0.6),
              label.style = 'align')+
  # contourplot(rotate(pwat[[day1:day2]]),
  #             xlim=c(-150, -85), ylim=c(20, 60), par.settings=pwat.cols, at=pwat.cols.at,
  #             lwd = 0.1,
  #             labels = list(cex = 0.6),
  #             label.style = 'align',
  #             col='blue')+
  layer(sp.polygons(us, col = 'gray40', lwd=0.3))+
  layer(sp.polygons(mx, col = 'gray40', lwd=0.3))+
  layer(sp.polygons(cn, col = 'gray40', lwd=0.3))

# write to image file
ptm <- proc.time()
png("/home/crimmins/RProjects/ClimPlot/plots/GH_500mbPlot.png", width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
## now add the text 
grid.text(paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),"\nThe University of Arizona\nhttps://cals.arizona.edu/climate/"),
          x=.90, y=.03, 
          gp = gpar(col=1, 
                    fontfamily="Arial", cex=0.5)) 
dev.off()
proc.time() - ptm

