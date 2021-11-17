# calc daily anoms
# adapted from NARR_monsoon_anoms.R
# MAC 08/01/20

library(raster)
library(lubridate)
library(rasterVis)

# set rasteroptions
rasterOptions(progress = 'text')

# functions
leap_every_year <- function(x) {
  ifelse(yday(x) > 59 & leap_year(x) == FALSE, yday(x) + 1, yday(x))
}

# # load pre-processed raster stacks from /ClimPlot/NARR/processNARR.R
# gh500<-stack("/scratch/crimmins/NARR/processed/GH500_daily_NARR_WUS_1979_2019.grd")
# pwat<-stack("/scratch/crimmins/NARR/processed/PWAT_daily_NARR_WUS_1979_2019.grd") 
# prcp<-stack("/scratch/crimmins/NARR/processed/PRCP_daily_NARR_WUS_1979_2019.grd") 
# avgCAPE<-stack("/scratch/crimmins/NARR/processed/CAPE_daily_NARR_WUS_1979_2019.grd")
# maxCAPE<-stack("/scratch/crimmins/NARR/processed/maxCAPE_daily_NARR_WUS_1979_2019.grd")
# sfcDP<-stack("/scratch/crimmins/NARR/processed/DPT2m_daily_NARR_WUS_1979_2019.grd")
# uFlux<-stack("/scratch/crimmins/NARR/processed/WVUFLX_daily_NARR_WUS_1979_2019.grd")
# vFlux<-stack("/scratch/crimmins/NARR/processed/WVVFLX_daily_NARR_WUS_1979_2019.grd")
# SH850<-stack("/scratch/crimmins/NARR/processed/SH850_daily_NARR_WUS_1979_2019.grd")
## 00z
 gh500<-stack("/scratch/crimmins/NARR/processed/00z/GH500_00z_NARR_WUS_1979_2019.grd")
 pwat<-stack("/scratch/crimmins/NARR/processed/00z/PWAT_00z_NARR_WUS_1979_2019.grd") 
 
 
# load pre-processed pentad stacks from NARR_monsoon_anoms.R
# dailyAvg_gh500<-stack("/scratch/crimmins/NARR/processed/dailyMean/GH500_dailyMean_NARR_WUS_1979_2019_pentMean.grd")
# dailyAvg_pwat<-stack("/scratch/crimmins/NARR/processed/dailyMean/PWAT_dailyMean_NARR_WUS_1979_2019_pentMean.grd") 
# dailyAvg_prcp<-stack("/scratch/crimmins/NARR/processed/dailyMean/PRCP_dailyMean_NARR_WUS_1979_2019_pentMed.grd") 
# dailyAvg_cape<-stack("/scratch/crimmins/NARR/processed/dailyMean/avgCAPE_dailyMean_NARR_WUS_1979_2019_pentMean.grd") 
# dailyAvg_maxcape<-stack("/scratch/crimmins/NARR/processed/dailyMean/maxCAPE_dailyMean_NARR_WUS_1979_2019_pentMean.grd") 
# dailyAvg_sfcDP<-stack("/scratch/crimmins/NARR/processed/dailyMean/DPT2m_dailyMean_NARR_WUS_1979_2019_pentMean.grd")
# dailyAvg_uFlux<-stack("/scratch/crimmins/NARR/processed/dailyMean/WVUFLX_dailyMean_NARR_WUS_1979_2019_pentMean.grd")
# dailyAvg_vFlux<-stack("/scratch/crimmins/NARR/processed/dailyMean/WVVFLX_dailyMean_NARR_WUS_1979_2019_pentMean.grd")
## 00z
 dailyAvg_gh500<-stack("/scratch/crimmins/NARR/processed/dailyMean/GH500_00zMean_NARR_WUS_1979_2019_pentMean.grd")
 dailyAvg_pwat<-stack("/scratch/crimmins/NARR/processed/dailyMean/PWAT_00zMean_NARR_WUS_1979_2019_pentMean.grd") 
 
# dates for layer selection
dates<-as.data.frame(seq.Date(as.Date("1979-01-01"),as.Date("2019-12-31"),1))
  colnames(dates)<-"date"
  dates$month<-as.numeric(format(dates$date, "%m"))
  dates$day<-as.numeric(format(dates$date, "%d"))
  dates$year<-as.numeric(format(dates$date, "%Y"))
  dates$doy<-as.numeric(format(dates$date, "%j"))
  dates$doy_ly<-leap_every_year(dates$date) # this day of year without leap day shifts

# #####
# # daily anomalies -- SLOW
# tempStack<-stack()
# for (i in 1:nlayers(gh500)){
#   tempLyr<-gh500[[i]]-dailyAvg_gh500[[dates$doy[i]]]
#   tempStack<-stack(tempStack, tempLyr) 
#   print(i/nlayers(gh500))
# }
# writeRaster(tempStack, filename= "/scratch/crimmins/NARR/processed/anom/GH500_daily_NARR_WUS_1979_2019_pentMean_anomaly.grd", overwrite=TRUE)  
# #####

yrs<-seq(1979,2019,1)
tempStack<-stack()
for (i in 1:length(yrs)){
  which(dates$year==yrs[i])
  temp<-gh500[[which(dates$year==yrs[i])]]-dailyAvg_gh500[[dates$doy[which(dates$year==yrs[i])]]]
  tempStack<-stack(tempStack, temp)
  print(yrs[i])
}
writeRaster(tempStack, filename= "/scratch/crimmins/NARR/processed/anom/GH500_00z_NARR_WUS_1979_2019_pentMean_anomaly.grd", overwrite=TRUE)  


