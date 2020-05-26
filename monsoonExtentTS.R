# calculate daily extent of precip 
# MAC 08/27/19

library(raster)
library(plyr)
library(tidyr)
library(cowplot)
library(caTools)

us <- getData("GADM", country="USA", level=1)
# extract states (need to uppercase everything)
# state<-subset(us, NAME_1=="New Mexico")
state<-subset(us, NAME_1=="Arizona")
#state<-subset(us, NAME_1 %in% c("New Mexico","Arizona"))

dailyGT0<-NULL

# precip threshold
thresh<-0.01

for(year in 1981:2019){
  currYear<-stack(paste0("/home/crimmins/RProjects/ClimPlot/monsoonDailyData/AZNM_PRISM_Monsoon_",year,"_dailyPrecip.grd"))
  # mask to AZ
  currYearClip <- mask(currYear, state)
  # get counts
  temp1<-as.data.frame(cellStats(currYearClip, function(i, ...) sum(i>thresh, na.rm = TRUE)))
  # build time series  
  dailyGT0 = rbind(dailyGT0, temp1)
print(year) 
}

# get base count
tempGrid <- mask(currYear[[1]], state)
baseCount<-cellStats(tempGrid, function(i, ...) sum(i>=0, na.rm = TRUE))

# get dates from col names
colnames(dailyGT0)<-"counts"
dailyGT0$percExt<-(dailyGT0$counts/baseCount)*100
dailyGT0$dates<-rownames(dailyGT0)
dailyGT0<-separate(dailyGT0,dates, c(NA,"year",NA,"month",NA,"day"),sep=c(1,5,6,8,9,11), convert = TRUE)

# build date and doy
dailyGT0$date<-as.Date(paste0(dailyGT0$year,"-",dailyGT0$month,"-",dailyGT0$day), format="%Y-%m-%d")
dailyGT0$doy<-as.numeric(format(dailyGT0$date, "%j"))
# dummy date var
dailyGT0$dummyDate<-as.Date(paste0(2000,"-",dailyGT0$month,"-",dailyGT0$day), format="%Y-%m-%d")

# doy stats
# calculate stats by day
doyStats<- ddply(dailyGT0,.(dummyDate),summarise,
                 q25 = quantile(percExt,0.25,na.rm='TRUE'),
                 q50 = quantile(percExt,0.50,na.rm='TRUE'),
                 q75 = quantile(percExt,0.75,na.rm='TRUE'),
                 avg = mean(percExt,na.rm='TRUE'))
doyStats$q50smooth<-runmean(doyStats$q50, 15, align = "center", endrule = "mean")

# join stats to data table
doyStats$doy<-as.numeric(format(doyStats$dummyDate, "%j"))
tempGT0<-merge(dailyGT0, doyStats, by="doy")
tempGT0$abvQ50<-ifelse(tempGT0$percExt>=tempGT0$q50smooth,1,0) # monsoon days
# summarize by year
library(dplyr)
yearlyStats<-tempGT0 %>%
  group_by(year) %>%
  summarise(totalMonsoonDays = sum(abvQ50))

# plot monsoon days per year
ggplot(yearlyStats, aes(year,totalMonsoonDays))+
  geom_bar(stat = "identity")+
  ggtitle("Total days/season with abv median precip coverage (NM 81-19)")


# plot of daily monsoon extent by year
p<-ggplot(dailyGT0, aes(dummyDate,percExt))+
  geom_bar(stat = "identity", fill="darkgreen")+
  facet_wrap(~year, ncol = 5, nrow = 8)+
  xlab("Day of Year")+
  ylab("Percent Coverage - AZ")+
  ggtitle("Percent of Arizona observing >=0.01 inches during Monsoon Season (PRISM 1981-2019) ")
p<-p+geom_line(data=doyStats, aes(dummyDate,q50smooth))+
  geom_label(data = yearlyStats, aes(label=totalMonsoonDays), 
             x = -Inf, y = Inf, hjust=0, vjust=1,
             inherit.aes = FALSE)

png("/home/crimmins/RProjects/ClimPlot/AZMonsoonDays.png", width = 11, height = 8.5, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
## now add the text 
dev.off()

# plot single year
temp<-subset(dailyGT0, year==2019)
p<-ggplot(temp, aes(dummyDate,percExt))+
  geom_bar(stat = "identity", fill="darkgreen")+
  #facet_wrap(~year, ncol = 5, nrow = 8)+
  xlab("Day of Year")+
  ylab("Percent Coverage - AZ")+
  ylim(0,100)+
  ggtitle("Percent of Arizona observing >=0.01 inches during Monsoon Season (PRISM 1981-2019) ")
p<-p+geom_line(data=doyStats, aes(dummyDate,q50smooth))+
  geom_label(data = yearlyStats, aes(label=totalMonsoonDays), 
             x = -Inf, y = Inf, hjust=0, vjust=1,
             inherit.aes = FALSE)

png("/home/crimmins/RProjects/ClimPlot/AZMonsoonDays_2019.png", width = 11, height = 8.5, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
## now add the text 
dev.off()


# add climo plot
library(reshape)
doyStats <- doyStats[-c(7)]
doyStatsMelt<-melt(doyStats, id.vars = "dummyDate")
ggplot(doyStatsMelt, aes(dummyDate,value,color=variable))+
  geom_line()+
  xlab("Day of Year")+
  ylab("Percent Coverage - AZ")+
  ggtitle("Climatology of Percent of New Mexico observing >=0.05 (PRISM 1981-2019) ")+
  labs(color = "quantiles")

