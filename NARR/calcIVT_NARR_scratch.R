# calculate daily average integrated vapor transport with NARR data
# MAC 06/19/2020

library(raster)
library(ncdf4)
library(pracma)

# set rasteroptions
rasterOptions(progress = 'text')

# functions
vertInt<-function(y){
  if (all(is.na(y))) {
    NA_real_
  } else {
    x<-trapz(level*-100,y)
    return(x)
  } 
}
# using sfc pressure to define levels, need to stack sfc.pres at bottom
vertIntSfc<-function(y){
  if (all(is.na(y[1:length(level)]))) {
    NA_real_
  } else {
    sfcpres<-(y[length(level)+1])/100
    levs<-level[which(abs(level-sfcpres)==min(abs(level-sfcpres))):length(level)]
    x<-trapz(levs*-100,y[which(abs(level-sfcpres)==min(abs(level-sfcpres))):length(level)])
    return(x)
  } 
}
####


#sfcpres<-886
#level[which(abs(level-sfcpres)==min(abs(level-sfcpres))):length(level)]
#####

# load NARR topo
#topo<-raster("/scratch/crimmins/NARR/hgt.sfc.nc")
#SP=MSLP'*exp(-9.81*elev/(287*(t2-0.0065/2*elev)))

# map layers
states <- getData('GADM', country='United States', level=1)
az<-subset(states, NAME_1=="Arizona")
us <- getData('GADM', country='United States', level=0)
mx <- getData('GADM', country='Mexico', level=0)

# dates
#dates<-as.data.frame(seq.Date(as.Date("1979-01-01"),as.Date("2019-12-31"),1))
#dates<-as.data.frame(seq.Date(as.Date("1979-01-01"),as.Date("2019-12-01"),by="1 month"))

# reproject and clip NARR data Nauslar et al
# -131W to 85W, 18N to 49.5N)
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
e <- extent(-131, -85, 18, 49.5)

# info about files
 pathSHUM<-"/scratch/crimmins/NARR/pressure/shum"
 filesSHUM <- list.files(pathSHUM,full.names = T) 
# vwind
 pathVWND<-"/scratch/crimmins/NARR/pressure/wind"
 filesVWND <- list.files(pathVWND,full.names = T, pattern="^vwnd") 
# uwind
 pathUWND<-"/scratch/crimmins/NARR/pressure/wind"
 filesUWND <- list.files(pathUWND,full.names = T, pattern="^uwnd") 
 # uwind
 pathPRES<-"/scratch/crimmins/NARR/monolevel/sfcpres"
 filesPRES <- list.files(pathPRES,full.names = T, pattern="^pres") 
 
 # get level list
  ncin <- nc_open(filesSHUM[1])
  #ncin <- nc_open(filesUWND[1])
  #print(ncin)
  level <- ncvar_get(ncin, "level")

# get sfc pressure for day to use as mask
  sfc.pres<-stack(filesPRES[28]) # 2009
    subPres<-sfc.pres[[311]] # Aug 13th
   
# put levels in stack
 level<-level[which(level>=300)] #1000 to 500mb

# out in loop  -- NOT EXTRACTING LEVELS IN SHUM 
 j=335 # Aug 2009
 shum<-stack()
 vwnd<-stack()
 uwnd<-stack()
 for(i in 1:length(level)){
   shum<-stack(shum, brick(filesSHUM[j], level=i))
   vwnd<-stack(vwnd, brick(filesVWND[j], level=i))
   uwnd<-stack(uwnd, brick(filesUWND[j], level=i))
   print(i)
 }
 
 # water vapor flux
#  uq<-uwnd*shum
#  vq<-vwnd*shum
 # reproject
 beginCluster(6)
 #  uq<-projectRaster(uq, crs = geo.prj)
#   vq<-projectRaster(vq, crs = geo.prj)
   subPres<-projectRaster(subPres, crs = geo.prj)
   vwnd<-projectRaster(vwnd, crs = geo.prj)
   uwnd<-projectRaster(uwnd, crs = geo.prj)
   shum<-projectRaster(shum, crs = geo.prj)
 endCluster()
 # crop to smaller area of extent [Crop to smaller extent first?]
#  uq<-crop(uq,extent(az))
#  vq<-crop(vq,extent(az))
  subPres<-crop(subPres,e)
  vwnd<-crop(vwnd,e)
  uwnd<-crop(uwnd,e)
  shum<-crop(shum,e)
  
# subset by day
  i=7
  subIDX<-seq(i,(nlayers(uwnd)-(nlayers(uwnd)/length(level)))+i,by=(nlayers(uwnd)/length(level)))  
  #temp_uq<-uq[[subIDX]]
  #temp_vq<-vq[[subIDX]]
  temp_vwnd<-vwnd[[subIDX]]
  temp_uwnd<-uwnd[[subIDX]]
  temp_shum<-shum[[subIDX]]
  # uq and vq of subset
    temp_uq<-temp_uwnd*temp_shum
    temp_vq<-temp_vwnd*temp_shum
  
  # vert int
  vInt<-calc(temp_vq, fun=vertInt)*(1/9.80665)
  uInt<-calc(temp_uq, fun=vertInt)*(1/9.80665)
  
  # vert int from sfc pressure
  vIntsfc<-calc(stack(temp_vq,subPres), fun=vertIntSfc)*(1/9.80665)
  uIntsfc<-calc(stack(temp_uq,subPres), fun=vertIntSfc)*(1/9.80665)
  
  
    
  # test plot
  library(rasterVis)
  uv<-stack(uInt,vInt)
  vectorplot(uv/10, isField = 'dXY')+
    layer(sp.polygons(us))

uv<-stack(uIntsfc,vIntsfc)
  vectorplot(uv/10, isField = 'dXY')+
    layer(sp.polygons(us))+
    layer(sp.polygons(mx))
  
  
  uv<-stack(temp_uq,temp_vq)
  vectorplot(uv, isField = 'dXY',
             uLayer=1:(nlayers(uv)/2),
             vLayer=((nlayers(uv)/2)+1):nlayers(uv))+
    layer(sp.polygons(az))
  
  uv<-stack(uInt,uIntsfc,vInt,vIntsfc)
  vectorplot(uv, isField = 'dXY',
             uLayer=1:2,
             vLayer=3:4)+
    layer(sp.polygons(az))
  
  
# look at vert integration values
  lat=32.1145; lon=-110.9392 # KTUS
  
  # extract time series
  Uts<-t(raster::extract(temp_uwnd, cellFromXY(temp_uwnd, c(lon,lat))))
  SHUMts<-t(raster::extract(temp_shum, cellFromXY(temp_shum, c(lon,lat))))
  presT<-t(raster::extract(subPres, cellFromXY(subPres, c(lon,lat))))
  uq<-Uts*SHUMts
  x<-trapz(level*-100,uq)
  

  # level=17 is 500mb
 # process reanalysis nc into raster stacks
 # x <- stack()
 # for(i in 1:length(filesNC)){
 #     x <- stack( x , brick(filesNC[i], level=17) ) # 6=500mb 4=700mb
 #   print(filesNC[i])
 # }