# calculate daily average integrated vapor transport with NARR data
# MAC 06/19/2020

library(raster)
library(ncdf4)
library(pracma)

# set rasteroptions
rasterOptions(progress = 'text')

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
 #print(ncin)
 level <- ncvar_get(ncin, "level")

 k=1
 m=1
 for(i in 1:length(filesPRES)){
   sfc.pres<-stack(filesPRES[i])
   print(i)
   for(j in 1:12){
     shum<-stack()
     vwnd<-stack()
     uwnd<-stack()
       for(l in 1:length(level)){
         shum<-stack(shum, brick(filesSHUM[k], level=l))
         vwnd<-stack(vwnd, brick(filesVWND[k], level=l))
         uwnd<-stack(uwnd, brick(filesUWND[k], level=l))
         #print(l)
       }
     beginCluster(6)
     #  uq<-projectRaster(uq, crs = geo.prj)
     #   vq<-projectRaster(vq, crs = geo.prj)
        subPres<-projectRaster(subPres, crs = geo.prj)
        vwnd<-projectRaster(vwnd, crs = geo.prj)
        uwnd<-projectRaster(uwnd, crs = geo.prj)
        shum<-projectRaster(shum, crs = geo.prj)
     endCluster()
     # crop to smaller area of extent [Crop to smaller extent first?]
       subPres<-crop(subPres,e)
       vwnd<-crop(vwnd,e)
       uwnd<-crop(uwnd,e)
       shum<-crop(shum,e)
      # loop through days
       # subset by day
         day=7
           subIDX<-seq(day,(nlayers(uwnd)-(nlayers(uwnd)/length(level)))+day,by=(nlayers(uwnd)/length(level)))  
           #temp_uq<-uq[[subIDX]]
           #temp_vq<-vq[[subIDX]]
           temp_vwnd<-vwnd[[subIDX]]
           temp_uwnd<-uwnd[[subIDX]]
           temp_shum<-shum[[subIDX]]
           # uq and vq of subset
           temp_uq<-temp_uwnd*temp_shum
           temp_vq<-temp_vwnd*temp_shum
           # vert int from sfc pressure
           vIntsfc<-calc(stack(temp_vq,subPres), fun=vertIntSfc)*(1/9.80665)
           uIntsfc<-calc(stack(temp_uq,subPres), fun=vertIntSfc)*(1/9.80665)
           
       
    k=k+1
   }
 }
 
 


# get sfc pressure for day to use as mask
  sfc.pres<-stack(filesPRES[28]) # 2009
    subPres<-sfc.pres[[311]] # Aug 13th
   
# put levels in stack
 level<-level[which(level>=300)] #1000 to 500mb

# out in loop  -- NOT EXTRACTING LEVELS IN SHUM 
 j=335 # Aug 2009

 
 # water vapor flux
#  uq<-uwnd*shum
#  vq<-vwnd*shum
 # reproject


  

  
  
    
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
  
  


  # level=17 is 500mb
 # process reanalysis nc into raster stacks
 # x <- stack()
 # for(i in 1:length(filesNC)){
 #     x <- stack( x , brick(filesNC[i], level=17) ) # 6=500mb 4=700mb
 #   print(filesNC[i])
 # }