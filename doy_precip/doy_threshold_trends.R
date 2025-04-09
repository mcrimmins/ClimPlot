##################################################3
# ACIS JSON Code for SEAS package
# download weather station data, format for SEAS package
# MAC 4/7/18

library(plyr)
library(RCurl)
library(jsonlite)
library(reshape)
library(dplyr)
library(tidyr)
library(seas)

# download data from RCC-ACIS in JSON format and convert, 028820 is Tucson
jsonQuery='{"sid":"028820,020287","sdate":"por","edate":"por","elems":"1,2,43,4,10,11"}'
out<-postForm("http://data.rcc-acis.org/StnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)


# get data from list
data<-data.frame(out$data)
colnames(data)<-c("date","t_max","t_min","t_mean","precip","snow","snowD")
data$date<-as.Date(as.character(data$date))
# replace T trace values
data$precip<-revalue(data$precip, c("T"="0.001"))
# convert columns to numeric
unfactorize<-c("t_max","t_min","t_mean","precip","snow","snowD")
data[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(data[,x])))

# add rain days
data$rain<-ifelse(data$snow==0,data$precip,0) 
data$rain<-ifelse(is.na(data$snow) & !is.na(data$precip),data$precip,data$rain)

# add attributes for plots
attr(data,"id")<-"028820"
attr(data,"name")<-"Tucson International Airport"
attr(data$t_max, "units") <- "deg F"
attr(data$t_max, "long.name") <- "Max Temp"
attr(data$t_min, "units") <- "deg F"
attr(data$t_min, "long.name") <- "Min Temp"  
attr(data$t_mean, "units") <- "deg F"
attr(data$t_mean, "long.name") <- "Avg Temp"
attr(data$precip, "units") <- "in"
attr(data$precip, "long.name") <- "total precip"
attr(data$snow, "units") <- "in"
attr(data$snow, "long.name") <- "total snowfall"
attr(data$snowD, "units") <- "in"
attr(data$snowD, "long.name") <- "total snowdepth"
attr(data$rain, "units") <- "in"
attr(data$rain, "long.name") <- "total rainfall"

# yearly climate plot
year.plot(data, precip.only = FALSE, na.cut=50) # set to TRUE to turn off snow in record

# precip norm plot
plot(precip.norm(data.ss, fun=mean))

# calculate dry spells
drySpells<-interarrival(data, var="precip",p.cut="0.01",  inv=FALSE) # p.cut sets precip threshold

##########################################################

# doy of first rain event

data$doy<-as.numeric(format(data$date,"%j")) 
data$year<-as.numeric(format(data$date,"%Y"))

subData<-subset(data, doy>=152 & doy<=273)

firstDOY<-subData %>%
          group_by(year) %>%
          slice(match(TRUE, precip >= 0.4)) %>%
          ungroup

library(ggplot2)

ggplot(subset(firstDOY,year>=2000), aes(year,doy))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)
  
