# script to build dataframe from NOAA ISD-lite dataset
# MAC 1/5/2020

# FROM isd_lite.R script ----
# build download string
# ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt
stn1<-"722740-23160" # Tucson 1973-2019
stn2<-"999999-23160" # Tucson 1946-1972

# set vars
yrs<-1948:2019 # set range of years to download (yrs<-1973:2015) 1950:1972
  switchYr<-1972
baseurl<-"ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/"
suffix<-".gz"

colnames=c("year","month","day","hour","temp","dewpoint","mslp","wdir","wspd","sky","precip1hr",
           "precip6hr")
datalist = list()

# loop through all years for station
for(i in 1:length(yrs)){
  stn<-ifelse(yrs[i]<=switchYr, stn2, stn1)
  url<-paste(baseurl,as.character(yrs[i]),'/',stn,'-',as.character(yrs[i]),suffix, sep="")  
  download.file(url,destfile="tmp.gz")
  datalist[[i]] <- read.table(gzfile("tmp.gz"))
}
# combine into dataframe
climData = do.call(rbind, datalist)

# set column names
colnames(climData)<-colnames
# replace 999 with na
climData[climData == -9999] <- NA
# scale column values
climData$temp<-climData$temp*0.1
climData$dewpoint<-climData$dewpoint*0.1
#climData$dewpoint<-celsius.to.fahrenheit(climData$dewpoint, round = 2)
climData$mslp<-climData$mslp*0.1
climData$wspd<-climData$wspd*0.1
climData$precip1hr<-climData$precip1hr*0.1
climData$precip6hr<-climData$precip6hr*0.1

# create date/time field and day ofyear; assumes all observations are present even if NA
climData$dates<-as.POSIXct(strptime(paste(climData$year, climData$month, climData$day, climData$hour),"%Y %m %d %H"))
climData$doy<-strptime(climData$dates, "%Y-%m-%d")$yday+1  
#climData<-filter(climData, month!=2 | day!=29) # remove leap days
#rm(dataList)

# add in pentads
# pentad<-as.data.frame(seq(1,365,by=1))
#   colnames(pentad)<-"doy"
# pentad$num<-rep(1:73, each=5)
pentad<-as.data.frame(seq(as.Date("2001-01-01"),as.Date("2001-12-31"), "day"))
colnames(pentad)<-"date"
  pentad$month<-as.numeric(as.character(format(pentad$date, "%m")))
  pentad$day<-as.numeric(as.character(format(pentad$date, "%d")))
  pentad$pentadNum<-rep(1:73, each=5)
  pentad <- pentad[,-1]
# add Feb 29th back in as pentad 12
pentad[366,]<-c(2,29,12)
# merge with climData
climData<-merge(climData,pentad, by=c("month","day"))

# annual temp trends from all data, min/max
library(dplyr)

annClimData<-climData %>% 
              group_by(year) %>% 
                summarize(meanTemp = mean(temp, na.rm = TRUE),
                          meanDP = mean(dewpoint, na.rm = TRUE),
                          sumPrecip = sum(precip1hr, na.rm = TRUE))


#library(ggplot2)
#ggplot(climData, aes(x=pentadNum, y=temp, group=pentadNum)) + 
#  geom_boxplot()
