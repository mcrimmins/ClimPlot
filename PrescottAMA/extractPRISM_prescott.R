# extract and plot time series for Prescott AMA
# MAC 07/22/22
# adapted from SW_winter_summer_precip.R

library(raster)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rasterVis)
library(viridis)

# set rasteroptions
rasterOptions(progress = 'text')

prec<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_prec_1895_2020.grd")
tmean<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmean_1895_2020.grd")

# load data
ama<-rgdal::readOGR(dsn = "~/RProjects/ClimPlot/PrescottAMA", layer="AMA_and_INA")
ama<-ama[3,]

# extract zonal time series
# precTS<-as.data.frame(t(zonal(prec, SWhucs, 'mean'))) # rasterized polygons
precTS<-as.data.frame(t(raster::extract(prec, ama, 'mean'))) # polygons
colnames(precTS)<-"precip"

tempTS<-as.data.frame(t(raster::extract(tmean, ama, 'mean'))) # polygons
colnames(tempTS)<-"tmean"

precTS$date<-seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month")
precTS$month<-as.integer(format(precTS$date, "%m"))
precTS$year <- as.integer(format(precTS$date, "%Y"))

climTS<-cbind.data.frame(precTS, tempTS)

# annual summary
climTSann<-climTS %>%
  group_by(year) %>% 
  summarize(temp=mean(tmean, na.rm=TRUE),
            precip=sum(precip, na.rm = TRUE))

# convert units
climTSann$temp<-(climTSann$temp*(9/5))+32
climTSann$precip<-climTSann$precip/25.4


library(cowplot)
p1<-ggplot(climTSann,aes(x=year, y=precip))+
  geom_bar(stat='identity', fill='springgreen4', color='grey')+
  labs(y="precip (in.)", title='Prescott AMA Annual Total Precipitation (1895-2020)')+
  geom_hline(aes(yintercept=(mean(climTSann$precip, na.rm = TRUE))))+
  geom_text(aes(climTSann$year[1],(mean(climTSann$precip, na.rm = TRUE)),label ='average', vjust = -1)) +
  theme_bw()+
  theme(axis.text = element_text(size =14))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.y = element_text(size=14))

p2<-ggplot(climTSann,aes(x=year, y=temp))+
  geom_bar(stat='identity', fill='coral3', color='grey')+
  labs(y="degF", title='Prescott AMA Annual Average Temperature (1895-2020)')+
  geom_hline(aes(yintercept=(mean(climTSann$temp, na.rm = TRUE))))+
  geom_text(aes(climTSann$year[1],(mean(climTSann$temp, na.rm = TRUE)),label ='average', vjust = -1)) +
  theme_bw()+
  coord_cartesian(ylim=c(50,60))+
  theme(axis.text = element_text(size =14))+
  theme(axis.title = element_text(size=14))+
  theme(axis.text.x = element_text(size=14))+
  theme(axis.text.y = element_text(size=14))

plot_grid(
  p1, p2,
  #labels = "AUTO",
  ncol = 1
)





