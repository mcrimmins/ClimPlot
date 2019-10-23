# ACIS JSON station download and creation of seasonal summary plots
# MAC 10/16/19

# load libraries
library(RCurl)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(cowplot)
#library(reshape)
#library(ggmap)
#library(RColorBrewer)
#library(gridExtra)
#library(xtable)
#library(tidyr)
#library(SortableHTMLTables)

# ----- functions
wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}
#perc.rank <- function(x) trunc(rank(x,ties.method = "first"))/length(x)
#perc.rank <- function(x) (x)/sum(x, na.rm = TRUE)

# indices
# library(rsoi)
# library(rpdo)
# enso<-download_enso()
# pdo<-download_pdo()
# ----

# specify season
seas1mo<-6; seas1dy<-15
seas2mo<-9; seas2dy<-30

# WaterYear Lookup table
waterDates<-as.data.frame(seq(as.Date("1999/10/1"), as.Date("2000/9/30"), "days"))
colnames(waterDates)[1]<-"date"
  waterDates$month<-as.numeric(format(waterDates$date, "%m"))
  waterDates$day<-as.numeric(format(waterDates$date, "%d"))
  waterDates$year<-as.numeric(format(waterDates$date, "%Y"))
  waterDates$doy<-as.numeric(format(waterDates$date, "%j"))
  waterDates$waterYear<-wtr_yr(waterDates$date)
  waterDates <- waterDates %>%
    group_by(waterYear) %>% 
    mutate(wtr_day = (as.integer(difftime(date,ymd(paste0(waterYear - 1 ,'-09-30')), units = "days"))))
 # get water doy 
  seas1doy_wtr<-waterDates[which(waterDates$month==seas1mo & waterDates$day==seas1dy),7] 
  seas2doy_wtr<-waterDates[which(waterDates$month==seas2mo & waterDates$day==seas2dy),7]
  
# download data in JSON format and convert - extend 
stationID<-"028820"
jsonQuery=paste0('{"sid":"',stationID,'","meta":"name,ll,elev","sdate":"por","edate":"2020-12-31","elems":"1,2,43,4,10,11"}') # sid = station id, 029439=Winslow, arizona
out<-postForm("http://data.rcc-acis.org/StnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

# meta data
#out$meta

# wrangle data - get data from list
  data<-data.frame(out$data)
  colnames(data)<-c("date","t_max","t_min","t_mean","precip","snow","snowD")
  data$date<-as.Date(as.character(data$date))
  # replace T trace values
  data$precip<-recode(data$precip, "T" = "0.001")
  # convert columns to numeric
  unfactorize<-c("t_max","t_min","t_mean","precip","snow","snowD")
  data[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(data[,x])))

# add in month, day, years...
  data$month<-as.numeric(format(data$date, "%m"))
  data$day<-as.numeric(format(data$date, "%d"))
  data$year<-as.numeric(format(data$date, "%Y"))
  data$doy<-as.numeric(format(data$date, "%j"))
# dummy date
  data$dummyDate<-as.Date(paste0("2000-",data$month,"-",data$day), format="%Y-%m-%d")
  
# add rain days
#data$rain<-ifelse(data$snow==0,data$precip,0) 
#data$rain<-ifelse(is.na(data$snow) & !is.na(data$precip),data$precip,data$rain)
  data$precipDay<-ifelse(data$precip>=0.01,1,0) 

# freeze days
  data$frzDay<-ifelse(data$t_min<=32,1,0)

# find optimal period of record - longest period to present
  por<-na.contiguous(data$precip)
  por<-attr(por,"tsp")
    beginyr<-data$year[por[[1]]]
  # trim to por
  data<-subset(data, year>beginyr)
  
# add water year/days
  data$waterYear<-wtr_yr(data$date)
  data <- data %>%
     group_by(waterYear) %>% 
     mutate(wtr_day = (as.integer(difftime(date,ymd(paste0(waterYear - 1 ,'-09-30')), units = "days"))))

# SEASONAL SUMMARIES  
# calendar year vs water year switch
  if (seas1mo>seas2mo) {
    # crossing calendar year - water year
    dataSeas<-data[data$wtr_day>=seas1doy_wtr$wtr_day & data$wtr_day<=seas2doy_wtr$wtr_day,]
    # cumulative precip on season/yr using dplyr
    cumPrecip <- dataSeas %>% 
      group_by(waterYear, wtr_day) %>% # still doesn't quite work adjYear kicks off before adjDOY
      summarise(value = sum(precip, na.rm = T)) %>%
      mutate(csum = cumsum(value), cumPerc=csum/sum(value, na.rm=TRUE)*100)
    dataSeas$cumPrecip<-cumPrecip$csum
    dataSeas$cumPerc<-cumPrecip$cumPerc
    dataSeas$precipNA<-dataSeas$precip
    dataSeas$precipNA[dataSeas$precipNA == 0] <- NA
    dataSeas$precipNA[dataSeas$precipNA == 0.001] <- NA
    quantPrecip<-quantile(dataSeas$precipNA,  probs = c(33, 66)/100, na.rm = T)
    # average cumulative sum
    avgCumPrecip <- dataSeas %>%
      group_by(wtr_day) %>%
      summarise(meanCumPrecip = mean(cumPrecip, na.rm = T))
    # similar years matrix
    precipMatrix<-dataSeas[,c("wtr_day","waterYear","cumPrecip")]
    precipMatrix<-t(spread(precipMatrix, key = wtr_day,value = cumPrecip))
    # doy/year for plots
    dataSeas$doyX<-dataSeas$wtr_day
    dataSeas$yearX<-dataSeas$waterYear
    # adjust dummy date -- STOPPED HERE!!!
    dataSeas$dummyDate<-ifelse(dataSeas$dummyDate>=as.Date(paste0("2000-",seas1mo,"-",seas1dy)),
                        as.Date(paste0("1999-",dataSeas$month,"-",dataSeas$day), format="%Y-%m-%d"),
                        dataSeas$dummyDate)
    dataSeas$dummyDate<-as.Date(dataSeas$dummyDate, origin="1970-01-01")
    ####  
    #
    ## WATER YEAR
    seasSummary <- dataSeas %>% 
      group_by(waterYear) %>% # 
      summarise(totalPrecip = sum(precip, na.rm = T),
                totalRainDays = sum(precipDay, na.rm = T),
                meanTmin = mean(t_min, na.rm = T),
                meanTmax = mean(t_max, na.rm = T),
                meanTmean = mean(t_mean, na.rm = T),
                totalFrzDays = sum(frzDay, na.rm = T),
                totalSnow = sum(snow, na.rm = T),
                firstRain =wtr_day[min(which(precipDay==1))],
                lastRain =wtr_day[max(which(precipDay==1))],
                precip25 =wtr_day[min(which(cumPerc>=25))],
                precip50 =wtr_day[min(which(cumPerc>=50))],
                precip75 =wtr_day[min(which(cumPerc>=75))],
                maxDrySpell = max(rle(precipDay)$lengths),
                avgDrySpell = mean(rle(precipDay)$lengths, na.rm = T),
                lightRain = sum(precipNA<=quantPrecip[1], na.rm = T),
                modRain = sum(precipNA>quantPrecip[1] & precipNA<quantPrecip[2], na.rm = T),
                hvyRain = sum(precipNA>=quantPrecip[2], na.rm = T),
                precipNA = sum(is.na(precip)),
                tmeanNA = sum(is.na(t_mean))
      )
    
  }else{
  # within calendar year
    dataSeas<-data[data$dummyDate>=paste0("2000-",seas1mo,"-",seas1dy) & data$dummyDate<=paste0("2000-",seas2mo,"-",seas2dy),]
    # cumulative precip on season/yr using dplyr
    cumPrecip <- dataSeas %>% 
      group_by(year, doy) %>% # still doesn't quite work adjYear kicks off before adjDOY
      summarise(value = sum(precip, na.rm = T)) %>%
      mutate(csum = cumsum(value), cumPerc=csum/sum(value, na.rm=TRUE)*100)
    dataSeas$cumPrecip<-cumPrecip$csum
    dataSeas$cumPerc<-cumPrecip$cumPerc
    dataSeas$precipNA<-dataSeas$precip
    dataSeas$precipNA[dataSeas$precipNA == 0] <- NA
    dataSeas$precipNA[dataSeas$precipNA == 0.001] <- NA
    quantPrecip<-quantile(dataSeas$precipNA,  probs = c(33, 66)/100, na.rm = T)
    # average cumulative sum
    avgCumPrecip <- dataSeas %>%
      group_by(doy) %>%
      summarise(meanCumPrecip = mean(cumPrecip, na.rm = T))
    # similar years matrix
    precipMatrix<-dataSeas[,c("doy","year","cumPrecip")]
    precipMatrix<-t(spread(precipMatrix, key = doy,value = cumPrecip))
    # doy/year for plots
    dataSeas$doyX<-dataSeas$doy
    dataSeas$yearX<-dataSeas$year
     ## CALENDAR YEAR
    seasSummary <- dataSeas %>% 
      group_by(year) %>% # still doesn't quite work adjYear kicks off before adjDOY
      summarise(totalPrecip = sum(precip, na.rm = T),
                totalRainDays = sum(precipDay, na.rm = T),
                meanTmin = mean(t_min, na.rm = T),
                meanTmax = mean(t_max, na.rm = T),
                meanTmean = mean(t_mean, na.rm = T),
                totalFrzDays = sum(frzDay, na.rm = T),
                totalSnow = sum(snow, na.rm = T),
                firstRain =doy[min(which(precipDay==1))],
                lastRain =doy[max(which(precipDay==1))],
                precip25 =doy[min(which(cumPerc>=25))],
                precip50 =doy[min(which(cumPerc>=50))],
                precip75 =doy[min(which(cumPerc>=75))],
                maxDrySpell = max(rle(precipDay)$lengths),
                avgDrySpell = mean(rle(precipDay)$lengths, na.rm = T),
                lightRain = sum(precipNA<=quantPrecip[1], na.rm = T),
                modRain = sum(precipNA>quantPrecip[1] & precipNA<quantPrecip[2], na.rm = T),
                hvyRain = sum(precipNA>=quantPrecip[2], na.rm = T),
                precipNA = sum(is.na(precip)),
                precipNA = sum(is.na(precip)),
                tmeanNA = sum(is.na(t_mean))
      )
  }

# seasonal means accounting for missing vals 
  temp<-subset(seasSummary, tmeanNA<=30)
    seasMeans<-t(as.data.frame(colMeans(temp, na.rm = T)))

# daily quantiles  
  # temperatures
  dayTemps <- dataSeas %>% 
      group_by(dummyDate) %>% 
      summarise(doyX = min(doyX, na.rm = TRUE),
                maxTmax = max(t_max,na.rm='TRUE'),
                avgTmax = mean(t_max,na.rm='TRUE'),
                minTmin = min(t_min,na.rm='TRUE'),
                avgTmin = mean(t_min,na.rm='TRUE'))
  
# DEVELOP SEASONAL MEANS THAT CHANGE OVER TIME, interactive plotly versions?

# DEVELOP PLOTS
  currYear<-2019 # loop here for all plots
  if (seas1mo>seas2mo) {
    currYearData<-dataSeas[which(dataSeas$waterYear==currYear),]
    }else{
    currYearData<-dataSeas[which(dataSeas$year==currYear),]
  }

# deal with leap year-doy issue if needed
  if(nrow(avgCumPrecip) != nrow(currYearData)){
    avgCumPrecip<-avgCumPrecip[-nrow(avgCumPrecip),]
  }

# cumulative precip plot
  currYearData$avgCumPrecip<-avgCumPrecip$meanCumPrecip
  currYearData$diffAvg<-currYearData$cumPrecip-currYearData$avgCumPrecip
# grab temp data fram for stacked precip plot  
  temp<-currYearData[,c("date","avgCumPrecip","diffAvg")]
    temp$abvAvg<-temp$diffAvg
    temp$beloAvg<-temp$diffAvg
      temp$abvAvg[temp$abvAvg<0]<-0
      temp$beloAvg[temp$beloAvg>=0]<-0
      temp$avgCumPrecip<-temp$avgCumPrecip+temp$beloAvg
      temp$beloAvg<-abs(temp$beloAvg)
      temp$abvAvg<-abs(temp$abvAvg)
      temp<-temp[,-c(3)]
  temp<-gather(temp, "precipCat","value", -date)
  #temp$precipCat<-factor(temp$precipCat, c("avgCumPrecip","abvAvg", "beloAvg"))
  temp$precipCat<-factor(temp$precipCat, c("beloAvg","abvAvg","avgCumPrecip"))
  
# stacked bar precip plot
ggplot()+
    geom_bar(data=temp, aes(x=date,y=value, fill=as.factor(precipCat)),stat = "identity", width = 1,
             show.legend = FALSE)+
    scale_fill_manual(values = c("orange4", "darkgreen", "grey"))+
    geom_step(data=currYearData, aes(x=date,y=avgCumPrecip), color="black", position = position_nudge(x = -0.5))+
    geom_step(data=currYearData, aes(x=date,y=cumPrecip), color="blue", position = position_nudge(x = -0.5))+
    geom_bar(data=currYearData, aes(x=date,y=precip), stat = "identity",fill="blue",color="black")+
  xlab('Date') +
  ylab('Inches')+
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 month")

# similar years plot
#corrYears<-cor(precipMatrix[2:nrow(precipMatrix),], method = "pearson", use = "pairwise.complete.obs")
#corrYears<-cov(precipMatrix[2:nrow(precipMatrix),], method = "pearson", use = "pairwise.complete.obs")
  corrYears<-as.matrix(stats::dist(t(precipMatrix[2:nrow(precipMatrix),]), method = "euclidean"))
  topYears<-(cbind(corrYears[which(precipMatrix[1,]==currYear),],precipMatrix[1,]))
  topYears<-topYears[order(topYears[,1], decreasing = FALSE),]
  topYears<-topYears[1:3,2]
# plot top 3 years
  ggplot(subset(dataSeas, yearX %in% topYears)) + 
    geom_step(aes(dummyDate, cumPrecip, color=as.factor(yearX)))+
    scale_color_brewer(palette = "Set1")+
    ylab("inches")+
    xlab("date")+
    theme(legend.position = c(0.05, 0.8), legend.title = element_blank())

# Precip events intensity counts - bar plot
  temp<-seasSummary[which(seasSummary[,1]==currYear),16:18]
  temp<-gather(temp)
    temp$key<-factor(temp$key, levels=c("lightRain","modRain","hvyRain"),
                     labels = c("Light", "Moderate","Heavy"))
    temp$label<-c(paste0("<",quantPrecip[1]),paste0(quantPrecip[1],"-",quantPrecip[2]),paste0(">",quantPrecip[2]))
  ggplot(temp, aes(key,value, fill=key))+
    geom_bar(stat = "identity", color="black")+
    scale_fill_manual(values = c("deepskyblue2", "deepskyblue3", "deepskyblue4"))+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank())+
    ggtitle("Precip Events")+
    geom_text(data=temp,aes(x=key,y=value-3,label=label),vjust=0)
    
  # Precip Timing Gannt chart 
  temp<-seasSummary[which(seasSummary[,1]==currYear),11:13]
    temp<-currYearData$date[as.integer(temp)-(currYearData$doyX[1]-1)]
  temp2<-seasMeans[,11:13]
    temp2<-currYearData$date[as.integer(temp2)-(currYearData$doyX[1]-1)]
  temp<-as.data.frame(rbind(temp,temp2)) 
  colnames(temp)<-c("25%","50%","75%")
  temp$cat<-c(currYear,"Avg")
  temp<-gather(temp, key, value, 1:3)
  temp$value<-as.Date(temp$value, origin = "1970-01-01")
  # first/last dates
  temp3<-seasSummary[which(seasSummary[,1]==currYear),9:10]
    temp3<-currYearData$date[as.integer(temp3)-(currYearData$doyX[1]-1)]
  temp4<-seasMeans[,9:10]
    temp4<-currYearData$date[as.integer(temp4)-(currYearData$doyX[1]-1)]
  temp3<-as.data.frame(rbind(temp3,temp4)) 
  colnames(temp3)<-c("First","Last")
  temp3$cat<-c(currYear,"Avg")
  temp3<-gather(temp3, key, value, 1:2)
  temp3$value<-as.Date(temp3$value, origin = "1970-01-01")
    
  # plot chart
  ggplot()+
    geom_line(data=temp, aes(x=as.factor(cat),y=value, color=as.factor(key), group=as.factor(cat)), stat = "identity",
              size=10)+
     coord_flip()+
    scale_y_date(limits = c(currYearData$date[1],currYearData$date[nrow(currYearData)]),
                 date_breaks = "1 month", date_labels = "%b")+
    scale_color_manual(values = c("deepskyblue2", "deepskyblue3", "deepskyblue4"))+
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank())+
    geom_text(data=temp,aes(x=as.factor(cat),y=value,label=key),vjust=0)+
    geom_point(data=temp3, aes(x=as.factor(cat),y=value), shape=25, fill="red")+
    geom_text(data=temp3, aes(x=as.factor(cat), y=value, label=key), vjust=-0.5)
      
# Daily temperature plot
  dayTemps<-dayTemps[1:nrow(currYearData),]
  dayTemps$currDate<-currYearData$date
  ggplot()+
    geom_linerange(data=currYearData, aes(x=date, ymin=t_min, ymax=t_max),color="goldenrod2")+
    geom_line(data=dayTemps, aes(x=currDate,y=avgTmax), color="red")+
    geom_step(data=dayTemps, aes(x=currDate,y=maxTmax), color="red", size=0.1)+
    geom_line(data=dayTemps, aes(x=currDate,y=avgTmin), color="blue")+
    geom_step(data=dayTemps, aes(x=currDate,y=minTmin), color="blue", size=0.1)+
    geom_hline(yintercept=32, color='dodgerblue4', size=0.5, linetype=2)+
    scale_x_date(limits = c(currYearData$date[1],currYearData$date[nrow(currYearData)]),
                 date_breaks = "1 month", date_labels = "%b")+
    ylab("deg F")+
    theme(legend.position = "none",
          axis.title.x = element_blank())+
    annotate(geom="text", x=dayTemps$currDate[1], y=dayTemps$avgTmax[1]+4, label="T-max",
             color="red")+
    annotate(geom="text", x=dayTemps$currDate[1], y=dayTemps$avgTmin[1]-4, label="T-min",
             color="blue")+
    annotate(geom="text", x=dayTemps$currDate[5], y=30, 
             label=paste0("Freeze Days: ",as.integer(seasSummary[which(seasSummary[,1]==currYear),7]),
                          " (Avg:",as.integer(seasMeans[1,7]),")" ),
             color="dodgerblue4")+
    ylim(min(dayTemps$minTmin),max(dayTemps$maxTmax))
          
    
  # add records
  
  