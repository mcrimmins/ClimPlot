# find doy mean daily precip exceeds threshold
# MAC 09/10/22
# data files derived from PRISMCumPrecip.R

# load packages
library(raster)

# load raster from local dir
meanDailyCumPrecip<-stack("/home/crimmins/RProjects/ClimPlot/AZNM_PRISM_June_Sept_meanDailyCumPrecip_1981_2021.grd")

# find first doy precip threshold in mm is met
thresh<-5 # cumulative daily precip threshold in mm
doyThresh <- calc(meanDailyCumPrecip, function(x) min(which(x > thresh)))

# check results
plot(doyThresh)
#plot(doyThresh, zlim=c(1,10))


