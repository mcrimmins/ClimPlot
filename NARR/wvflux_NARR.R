# NARR Water Vapor Flux 
# MAC 06/22/20

library(raster)
library(ncdf4)

# set rasteroptions
rasterOptions(progress = 'text')

# map layers
states <- getData('GADM', country='United States', level=1)
az<-subset(states, NAME_1=="Arizona")
us <- getData('GADM', country='United States', level=0)
#mx <- getData('GADM', country='Mexico', level=0)

# reproject and clip NARR data Nauslar et al
# -131W to 85W, 18N to 49.5N)
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
e <- extent(-131, -85, 18, 49.5)

# info about files
pathWV<-"/scratch/crimmins/NARR/monolevel/wvflux"
filesU <- list.files(pathWV,full.names = T, pattern="^wvuflx")
filesV <- list.files(pathWV,full.names = T, pattern="^wvvflx")

# get level list
ncin <- nc_open(filesU[1])
print(ncin)
#level <- ncvar_get(ncin, "level")

fluxU<-stack(filesU[31])
fluxV<-stack(filesV[31])

# reproject
beginCluster(6)
  fluxU<-projectRaster(fluxU, crs = geo.prj)
  fluxV<-projectRaster(fluxV, crs = geo.prj)
endCluster()
# crop to smaller area of extent
fluxU<-crop(fluxU,extent(e))
fluxV<-crop(fluxV,extent(e))

# test plot
library(rasterVis)
library(grid)
uv<-stack(fluxU[[225]],fluxV[[225]])
at <- seq(250,800,50)
vectorplot(uv/8640, isField = 'dXY', narrows=500, region=TRUE, at=at,
           key.arrow = list(label = 'kg/m', size=10))+
  layer(sp.polygons(us))



