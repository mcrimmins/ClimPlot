# download NCDC ISD-lite data
# ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite
# MAC 01/30/15
# notes: try out stargazer for summary stats tables, generate climo reports for records
# and ranks for years
# working dir
#setwd("C:/Users/Crimmins/Google Drive/MAC/r-stats/ncdc_isd")

## libraries
library(dplyr)
library(plyr)
library(reshape2)
library(chron)
library(ggplot2)
library(reshape)
library(scales)
library(stargazer)
library(weathermetrics)

# build download string
# ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt
stn<-"722740-23160" # SET station number stn<-"722728-03196" , 999999-23160, 722728-03196:Nogales, 722740-23160:Tucson 1973-2017, 699604-03145 YUMA MCAS (1988-2017)
#stn<-"699604-03145"
#stn<-"722780-23183"  # PHOENIX AIRPORT, AZ US (72278023183

(yrs<-1973:2022) # set range of years to download (yrs<-1973:2015) 1950:1972
baseurl<-"ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/"
suffix<-".gz"

colnames=c("year","month","day","hour","temp","dewpoint","mslp","wdir","wspd","sky","precip1hr",
           "precip6hr")


# loop through all years for station
for(i in 1:length(yrs)){
  url<-paste(baseurl,as.character(yrs[i]),'/',stn,'-',as.character(yrs[i]),suffix, sep="")  
  download.file(url,destfile="tmp.gz")
  temp <- read.table(gzfile("tmp.gz"))
  if(i==1){
    temp2<-temp
  }else{
    temp2<-rbind(temp2,temp)  
  }
}

# set column names
colnames(temp2)<-colnames
# replace 999 with na
temp2[temp2 == -9999] <- NA
# scale column values
temp2$temp<-temp2$temp*0.1
temp2$dewpoint<-temp2$dewpoint*0.1
temp2$dewpoint<-celsius.to.fahrenheit(temp2$dewpoint, round = 2)
temp2$mslp<-temp2$mslp*0.1
temp2$wspd<-temp2$wspd*0.1
temp2$precip1hr<-temp2$precip1hr*0.1
temp2$precip6hr<-temp2$precip6hr*0.1

# create date/time field and day ofyear; assumes all observations are present even if NA
temp2$dates<-as.POSIXct(strptime(paste(temp2$year, temp2$month, temp2$day, temp2$hour),"%Y %m %d %H"))
temp2$doy<-strptime(temp2$dates, "%Y-%m-%d")$yday+1  
temp2<-filter(temp2, month!=2 | day!=29) # remove leap days
rm(temp)

# summarizing by day..daily average 
dayGroup<- ddply(temp2,.(doy,year),summarise,
                 avgDayVar=mean(dewpoint,na.rm='TRUE'),
                 maxDayVar=max(dewpoint,na.rm='TRUE'),
                 minDayVar=min(dewpoint,na.rm='TRUE'))
dayGroup<- dayGroup[order(dayGroup$year,dayGroup$doy),]
currYear<-filter(dayGroup, year==2022) # CHOOSE YEAR to Plot

##########
# # trend analysis in first dewpoint day
# temp<-dayGroup
# temp$date<-as.Date(paste0(temp$doy,"-",temp$year), format = "%j-%Y")
# 
# subData<-subset(temp, doy>=152 & doy<=273)
# 
# firstDOY<-subData %>%
#   group_by(year) %>%
#   slice(match(TRUE, avgDayVar >= 54)) %>%
#   ungroup
# 
# library(ggplot2)
# 
# ggplot(subset(firstDOY,year>=1990), aes(year,doy))+
#   geom_point()+
#   #geom_smooth(method = "lm", se = FALSE)+
#   geom_smooth(method = "lm", alpha = .15)+
#   ggtitle("Day of year of first summer 50F dewpoint")
# 
# temp<-temp[,c(1,2,3,6)]
# colnames(temp)[3]<-"avg_dewpoint_F"
# write.csv(temp, file = "Tucson_daily_avg_dewpoint.csv")
# 
# #######


# # summarizing by day->daily avg RH calc
# dayRH<- ddply(temp2,.(doy,year),summarise,
#                  avgDayDP=mean(dewpoint,na.rm='TRUE'),
#                  avgDayT=mean(temp,na.rm='TRUE')
#                  )
# dayRH<- dayRH[order(dayRH$year,dayRH$doy),]
# library(weathermetrics)
# dayRH$RH<-dewpoint.to.humidity(dp = dayRH$avgDayDP, t = dayRH$avgDayT,
#                      temperature.metric = "celsius")
# write.csv(dayRH, file="NogalesRH.csv" )


## create 5-day window to increase observations in quantile calc
temp2p<-dayGroup
temp2p$doy<-temp2p$doy+2
temp1p<-dayGroup
temp1p$doy<-temp1p$doy+1
temp0<-dayGroup
temp1m<-dayGroup
temp1m$doy<-temp1p$doy-1
temp2m<-dayGroup
temp2m$doy<-temp2p$doy-2
dayGroup=rbind(temp2p,temp1p,temp0,temp1m,temp2m)
rm(temp0,temp1m,temp1p,temp2m,temp2p)
# find LT 1 and GT 365 
dayGroup$doy[dayGroup$doy < 0] <- 1
dayGroup$doy[dayGroup$doy > 365] <- 365
## comment out if don't want percentile windowing

# calculate stats by day
dayQuant<- ddply(dayGroup,.(doy),summarise,
                 q05 = quantile(avgDayVar,0.05,na.rm='TRUE'),
                 q25 = quantile(avgDayVar,0.25,na.rm='TRUE'),
                 q33 = quantile(avgDayVar,0.33,na.rm='TRUE'),
                 q50 = quantile(avgDayVar,0.50,na.rm='TRUE'),
                 q66 = quantile(avgDayVar,0.66,na.rm='TRUE'),
                 q75 = quantile(avgDayVar,0.75,na.rm='TRUE'),
                 q95 = quantile(avgDayVar,0.95,na.rm='TRUE'),
                 min = min(avgDayVar,na.rm='TRUE'),
                 max = max(avgDayVar,na.rm='TRUE'),
                 avg = mean(avgDayVar,na.rm='TRUE'))
# create date string from days of year
doyDates<-as.POSIXct(strptime(paste(2014, dayQuant$doy), format="%Y %j"))
currYear$date<-as.POSIXct(strptime(paste(2014, currYear$doy), format="%Y %j"))

# join tables, anomaly categories for area anomaly plot
currYear<-merge(currYear,dayQuant,by.x="doy",by.y="doy")
currYear$anom[currYear$avgDayVar >= currYear$avg]<-"Above Avg"
currYear$anom[currYear$avgDayVar < currYear$avg]<-"Below Avg"

# delete last day...incomplete obs will cause daily avg to be incorrect
currYear<-currYear[-nrow(currYear),]

# ggplot example
#molten <- melt(dayQuant ,  id = 'doy', variable_name = 'series')
#ggplot(molten, aes(doy,value)) + geom_line(aes(colour = series))

# geom_linerange example
barWidth<-2.5
p<-ggplot(dayQuant,aes(doyDates,q50))+
  theme_bw()+
  #theme(plot.background = element_blank(),
  #      panel.grid.minor = element_blank(),
  #      panel.grid.major = element_blank(),
  #      panel.border = element_blank(),
  #      panel.background = element_blank()) +
  geom_line(colour='grey',size=0.5)+
  geom_linerange(dayQuant, mapping=aes(x=doyDates, ymin=min, ymax=q33), colour = "tan",alpha=0.4, size=barWidth, show.legend = NA)+
  geom_linerange(dayQuant, mapping=aes(x=doyDates, ymin=q33, ymax=q66), colour = "gray0",alpha=0.2, size=barWidth)+
  geom_linerange(dayQuant, mapping=aes(x=doyDates, ymin=q66, ymax=max), colour = "palegreen4",alpha=0.4, size=barWidth)
p + geom_line(data=currYear,aes(date,avgDayVar, colour='2022'), size=1) +
  scale_colour_manual(values=c("red"),name='Year')+
  theme(legend.position=c(0.92,0.90),
        legend.title=element_blank(),
        legend.background = element_rect(fill=alpha('white', 0)))+
  scale_x_datetime(labels = date_format("%m/%d"), limits = as.POSIXct(c("2014-06-01","2014-09-30")))+
  labs(x='day of year', y='deg F',title='Daily Average Dewpoint Climatology (1973-2021) - TUcson International AP')+
  ylim(0,75)
  #geom_hline(yintercept = 70)




# geom_area plot example; doesn't work, search shading area between two lines
p<-ggplot(dayQuant,aes(doyDates,avg))+
  theme_bw()+
  geom_line()+
  geom_linerange(dayQuant, mapping=aes(x=doyDates, ymin=min, ymax=max), colour = "wheat2",alpha=0.4)
p + geom_line(data=currYear,aes(date,avgDayVar, colour='2017'), size=0.25) +
  scale_colour_manual(values=c("red"),name='Year')+
  theme(legend.position=c(0.92,0.92),legend.title=element_blank())+
  scale_x_datetime(labels = date_format("%m/%d"))+
  labs(x='day of year', y='deg C',title='Daily Average Dewpoint Climatology')

# heat map of dewpoints
ggplot(dayGroup,aes(x=doy,y=year,fill=avgDayVar))+
  geom_tile()+
  scale_fill_gradient2(low = muted("tan"), mid = "white", high = muted("palegreen4"), 
                       midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar")+
  xlim(121,304)
scale_x_date(date_labels = "%b %d")

#scale_x_datetime(labels = date_format("%m/%y"), limits = as.POSIXct(c("1998-01-01","2014-12-31")))


# group by day
#stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
#  daily_dewpoint<-aggregate(temp2$dewpoint ~ temp2$doy, temp2, mean) # creates mean for each day of year

# boxplot of daily values
#boxplot(temp2$wdir~month, data=temp2)


# create datelist, try as.Date() too...
# fullDates<-strptime(paste(temp2$intYear, temp2$doy), format="%Y %j")
# mos<-as.numeric(format(fullDates, "%m"))
# days<-as.numeric(format(fullDates, "%d"))
# temp2<-cbind(temp2,mos,days) # add in mos and days for summarizing...





