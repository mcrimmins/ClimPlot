# download clim div and plot multiscale SPI/SPEI
# MAC 11/21/19

# load library
library(reshape2)
library(RCurl)
library(maps)
library(raster)
library(ggplot2)
library(tidyverse)
#library(zoo)
library(maptools)
library(SPEI)
library(weathermetrics)
#library(metR)
library(scales)

# ---- Functions ----
# capitalize county names
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
# ------

# ---- Local Data ----
# FIPS codes
data(county.fips)
countyDf <- map_data('county')
countyList<-unique(countyDf$subregion)

# polygons for centroids
countiesPoly<-getData('GADM', country='USA', level=2)
statesPoly<-getData('GADM', country='USA', level=1)
# load cdiv shapefile
cdivPoly<-readShapePoly("/home/crimmins/RProjects/ClimPlot/spiPlots/climDivShp/GIS.OFFICIAL_CLIM_DIVISIONS")

# state codes
stateCodes<-data.frame(code=c(seq(1,48,1),50,seq(101,110,1)),
                       name=c("Alabama","Arizona","Arkansas","California","Colorado",
                              "Connecticut","Delaware","Florida","Georgia","Idaho",
                              "Illinois","Indiana","Iowa","Kansas","Kentucky",
                              "Louisiana","Maine","Maryland","Massachusetts",
                              "Michigan","Minnesota","Mississippi","Missouri",
                              "Montana","Nebraska","Nevada","New Hampshire",
                              "New Jersey","New Mexico","New York","North Carolina",
                              "North Dakota","Ohio","Oklahoma","Oregon",
                              "Pennsylvania","Rhode Island","South Carolina",
                              "South Dakota","Tennessee","Texas","Utah",
                              "Vermont","Virginia","Washington","West Virginia",
                              "Wisconsin","Wyoming","Alaska","Northeast Region",
                              "East North Central Region","Central Region",
                              "Southeast Region","West North Central Region",
                              "South Region","Southwest Region",
                              "Northwest Region","West Region",
                              "National (contiguous 48 States)"
                       ))

# get boundary
us<-getData('GADM', country='USA', level=2)

# capitalize county names
# CapStr <- function(y) {
#   c <- strsplit(y, " ")[[1]]
#   paste(toupper(substring(c, 1,1)), substring(c, 2),
#         sep="", collapse=" ")
# }

# get county, div and state data
dataSets<-c("climdiv-pcpncy-v",
            "climdiv-pcpndv-v",
            "climdiv-pcpnst-v",
            "climdiv-tmincy-v",
            "climdiv-tmindv-v",
            "climdiv-tminst-v",
            "climdiv-tmaxcy-v",
            "climdiv-tmaxdv-v",
            "climdiv-tmaxst-v")
# -----


# ---- Get nClimDiv data ----
# container for all data
datalist = list()

# get directory listing and find most recent prcp file
  url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/'
  filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filelist<-unlist(strsplit(filenames,"\n"))

# loop through dataset    
for(i in 1:length(dataSets)){ 
  # download files and format into list
  tempName<-filelist[which((grepl(dataSets[i], filelist)) == TRUE)]
  url<-paste0("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",tempName)
  tempData<-read.table(url, colClasses = c("character","numeric",
                                           "numeric","numeric","numeric","numeric","numeric",
                                           "numeric","numeric","numeric","numeric","numeric",
                                           "numeric"))
  colnames(tempData)<-c("code",1:12)
  tempData$var<-dataSets[i]
  # add to container
  datalist[[i]] <- tempData
  print(paste0("Downloading ",dataSets[i]))
}
# combine into dataframe  
  allData = do.call(rbind, datalist)
  rm(datalist)
# ----
  
# ---- subset and wrangle data ----  
# county, division or state?
  region<-"cy" # cy,st, dv
    state <-"arizona"
      stCode<- stateCodes[which(stateCodes$name==CapStr(state)),1]
    cdiv  <- 7
    county<- "pima"  
      cyFIPS<-as.character(county.fips[which(county.fips$polyname==paste0(state,",",county)),1])
      cyFIPS<-as.numeric(ifelse(nchar(cyFIPS)==4, substr(cyFIPS,2,4), substr(cyFIPS,3,5)))
    tempData<-allData[which(grepl(region, allData$var)==TRUE),]
# parse code col
  if(region=="cy"){
    # parse into columns
    tempData$state<-(as.numeric(substr(tempData$code, 1,2)))
    tempData$div<-(as.numeric(substr(tempData$code, 3,5)))
    tempData$element<-(as.numeric(substr(tempData$code, 6,7)))
    tempData$year<-(as.numeric(substr(tempData$code, 8,11)))
    tempData<-subset(tempData, state==stCode & div==cyFIPS)
    # centroid
    subArea<-subset(countiesPoly,NAME_2==CapStr(county) & NAME_1==CapStr(state))
    centroid<-coordinates(subArea)
  }else if (region=="st"){
    tempData$state<-(as.numeric(substr(tempData$code, 1,3)))
    tempData$div<-(as.numeric(substr(tempData$code, 4,4)))
    tempData$element<-(as.numeric(substr(tempData$code, 5,6)))
    tempData$year<-(as.numeric(substr(tempData$code, 7,10)))
    tempData<-subset(tempData, state==stCode & div==0)
    # centroid
    subArea<-subset(statesPoly, NAME_1==CapStr(state))
    centroid<-coordinates(subArea)
  }else{
    tempData$state<-(as.numeric(substr(tempData$code, 1,2)))
    tempData$div<-(as.numeric(substr(tempData$code, 3,4)))
    tempData$element<-(as.numeric(substr(tempData$code, 5,6)))
    tempData$year<-(as.numeric(substr(tempData$code, 7,10)))
    tempData<-subset(tempData, state==stCode & div==cdiv)
    # centroid
    subArea<-subset(cdivPoly, STATE==CapStr(state) & CD_NEW==cdiv)
    centroid<-coordinates(subArea)
  }

  # melt data
  tempDataMelt<-melt(tempData, id.vars=c(14,18), measure.vars = 2:13)
  #tempDataMelt$date <- as.yearmon(paste(tempDataMelt$year, as.numeric(tempDataMelt$variable), sep = "-"))
  tempDataMelt$date <- as.Date(paste0(tempDataMelt$year,"-",as.numeric(tempDataMelt$variable),"-01"), format="%Y-%m-%d")
  tempDataMelt<-spread(tempDataMelt, var, value)
  # sort, -999 to NA
  tempDataMelt[tempDataMelt == -9.99] <- NA
  tempDataMelt[tempDataMelt == -99.9] <- NA
  # trim to 2018
  #allDataSubset<-allDataSubset[-(which(allDataSubset$year==2019)),]
  # standard colnames
  colnames(tempDataMelt)[4:6]<-c("precip","tmax","tmin")
  # calc tmean
  tempDataMelt$tmean<-(tempDataMelt$tmax+tempDataMelt$tmin)/2
# ----
  
# calculate indices ----
    ## Loop thru full SPI set
    dfSPI<-tempDataMelt[,1:3]
    for(i in 1:60){
      tempSPI <- spi(tempDataMelt$precip,i, na.rm = TRUE)
      dfSPI[[paste('SPI-',i,sep="")]] <-tempSPI$fitted
    }
    # remove rows with NAs
    dfSPI<-na.omit(dfSPI)
    #indexName="Standardized Precipitation Index"
    #indexNameShort="SPI"

    # # SPEI switch?
    #  PET <- thornthwaite(fahrenheit.to.celsius(data$t_mean,round=2), lat, na.rm = TRUE) 
    PET <- hargreaves(fahrenheit.to.celsius(tempDataMelt$tmin,round=2),fahrenheit.to.celsius(tempDataMelt$tmax,round=2),Ra=NA, as.numeric(centroid[1,2]), na.rm = TRUE) 
    dfSPEI<-tempDataMelt[,1:3]
    for(i in 1:60){
      tempSPI <- spei(inches_to_metric(tempDataMelt$precip,unit="mm",round=2)-PET,i, na.rm = TRUE)
      dfSPEI[[paste('SPEI-',i,sep="")]] <-tempSPI$fitted
    }
    # remove rows with NAs
    dfSPEI<-na.omit(dfSPEI)
    #indexName="Standardized Precipitation-Evapotranspiration Index"
    #indexNameShort="SPEI"

    # monthly anomalies - https://www.r-bloggers.com/visualize-monthly-precipitation-anomalies/
    tempDataMelt$PET<-PET/25.4
    tempDataMelt$P_PET<-tempDataMelt$precip-tempDataMelt$PET
    moAvg <- tempDataMelt %>%
      group_by(variable) %>%
      summarise(moAvgPrecip = mean(precip, na.rm=TRUE),
                moAvgTemp   = mean(tmean, na.rm=TRUE),
                moAvgPET    = mean(PET, na.rm=TRUE),
                moAvgP_PET  = mean(P_PET, na.rm=TRUE))
    tempDataMelt <- left_join(tempDataMelt, moAvg, by = "variable")
    tempDataMelt$precipAnom <- tempDataMelt$precip-tempDataMelt$moAvgPrecip
    tempDataMelt$tempAnom <- tempDataMelt$tmean-tempDataMelt$moAvgTemp
    tempDataMelt$PETAnom <- tempDataMelt$PET-tempDataMelt$moAvgPET
    tempDataMelt$P_PETAnom <- tempDataMelt$P_PET-tempDataMelt$moAvgP_PET
    # anom sign
    tempDataMelt$pAnomSign<-ifelse(tempDataMelt$precipAnom > 0, "pos", "neg")
    
    # plot variables 
    date1<-"1980-01-01" # by month
    date2<-"2019-10-01" # by month
    maxScale<-60+1 # max 60
      
    # SPI contou plot
    dfSPI<-melt(dfSPI, id.vars=c(1:3), measure.vars = 4:63)
    dfSPI$value<-as.numeric(dfSPI$value)
      colnames(dfSPI)[2]<-"month"
    # plotting
     ggplot(dfSPI, aes((date),as.numeric(variable) , fill = value))+
      geom_tile(width=31)+
      #scale_fill_gradient2(low = "brown", high = "green",mid = "white",
      #                    na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
      scale_fill_gradientn(colors=c("brown","yellow","white","green","darkgreen"),
                           na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
      scale_x_date(labels = date_format("%Y-%m"), breaks='1 years', expand=c(0,0),
                   limits = c(as.Date(date1),as.Date(date2)))+
      scale_y_continuous(limits=c(1,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
      theme_bw()+
      theme(legend.position="top")
    # precip anoms
     ggplot(tempDataMelt,aes(date, precipAnom, fill = pAnomSign)) + 
       geom_bar(stat = "identity", show.legend = FALSE) + 
       scale_x_date(date_breaks = "1 years", date_labels = "%b") +
       #scale_y_continuous(breaks = seq(-100, 100, 20)) +
       scale_fill_manual(values = c("brown", "darkgreen"))+
       theme_bw()
     
     
    
    # SPEI
    test2<-melt(dfSPEI, id.vars=c(1:3), measure.vars = 4:63)
    test2$value<-as.numeric(test2$value)
    colnames(test2)[2]<-"month"
    ggplot(test2, aes((date),as.numeric(variable) , fill = value))+
      geom_raster()+
      scale_fill_gradient2(low = "brown", high = "green",mid = "white",
                           na.value = "grey50", guide = "colourbar")
    
    # diff plot
    test2$diff<-test$value-test2$value
    ggplot(test2, aes((date),as.numeric(variable) , fill = diff))+
      geom_raster()+
      scale_fill_gradient2(low = "blue", high = "red",mid = "white",
                           na.value = "grey50", guide = "colourbar")    
    
    
    # metR contour plot
    #ggplot(test, aes((date),as.numeric(variable) , z = value))+
    #  geom_contour_fill()


