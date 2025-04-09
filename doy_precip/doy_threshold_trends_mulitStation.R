#####
# DOY precip threshold trend analysis
# adapting code from doy_threshold_trends.R & seasonalPlot.R
# MAC 6/22/22


library(plyr)
library(RCurl)
library(jsonlite)
library(reshape)
library(dplyr)
library(tidyr)
library(seas)


# # load from seasonal plots
# # load station list from getStations.R
# load("/home/crimmins/RProjects/StationPlots/stationList.RData")
# # subset if needed
# stationThin<-subset(stationThin, names %in% c("ANVIL RANCH","PICACHO 8 SE","GREEN VALLEY","TUCSON INTERNATIONAL AIRPORT"))
# 
# 
# jsonQuery=paste0('{"sid":"',stationID,'","meta":"name,ll,elev","sdate":"por","edate":"',endDate,'","elems":"1,2,43,4,10,11"}') # sid = station id, 029439=Winslow, arizona
# 
# out<-postForm("http://data.rcc-acis.org/StnData", 
#               .opts = list(postfields = jsonQuery, 
#                            httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
# 
# out<-fromJSON(out)

# download data from RCC-ACIS in JSON format and convert, 028820 is Tucson
jsonQuery='{"sids":["020287", "028820", "027530", "026513","023668","025924","028619","026132"],"sdate":"1900-01-01","edate":"2021-12-31","elems":"1,2,43,4,10,11"}'
out<-postForm("http://data.rcc-acis.org/MultiStnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))


out<-fromJSON(out)

# loop through out data to get common data frame
dataList<-list()
for(i in 1:length(out$data$data)){
  df<-as.data.frame(out$data$data[[i]])
  unfactorize<-c("V1","V2","V3","V4","V5","V6")
  df[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(df[,x])))
  df$name<-out$data$meta$name[i]
  df$date<-seq.Date(as.Date("1900-01-01"), as.Date("2021-12-31"),by='day')
  dataList[[i]]<-df
}

data = do.call(rbind, dataList)



# get data from list
#data<-data.frame(out$data)
colnames(data)<-c("t_max","t_min","t_mean","precip","snow","snowD","name","date")
#data$date<-as.Date(as.character(data$date))
# replace T trace values
#data$precip<-revalue(data$precip, c("T"="0.001"))
# convert columns to numeric
#unfactorize<-c("t_max","t_min","t_mean","precip","snow","snowD")
#data[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(data[,x])))

# add rain days
data$rain<-ifelse(data$snow==0,data$precip,0) 
data$rain<-ifelse(is.na(data$snow) & !is.na(data$precip),data$precip,data$rain)

# date vars
data$doy<-as.numeric(format(data$date,"%j")) 
data$year<-as.numeric(format(data$date,"%Y"))

# subset to summer
subData<-subset(data, doy>=152 & doy<=273)

firstDOY<-subData %>%
  group_by(year, name) %>%
  slice(match(TRUE, precip >= 0.4)) %>%
  ungroup

library(ggplot2)

ggplot(subset(firstDOY,year>=1900), aes(year,doy, color=name))+
  geom_point()+
  facet_wrap(.~name)+
  #geom_smooth(method = "lm", se = FALSE)+
  geom_smooth(method = "lm", alpha = .15, aes(fill = name))+
  ggtitle("Day of year of first summer 10mm precip event")




