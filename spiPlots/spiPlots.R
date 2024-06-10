# download clim div and plot multiscale SPI/SPEI
# adapted from /climplots/spiPlots/plotClimDiv.R 
# MAC 11/21/19

# load library
library(reshape2)
library(RCurl)
library(maps)
library(raster)
#library(ggplot2)
library(cowplot)
library(tidyverse)
#library(zoo)
library(maptools)
library(SPEI)
library(weathermetrics)
#library(metR)
library(scales)
library(magick)
library(plotly)
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
# ggplot inset data
states4map <- map_data("state")

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
  
# create list of counties, states, divs and regions ----
  state.list<-statesPoly$NAME_1
    state.list<-state.list[-which(state.list=="Alaska")]
    state.list<-state.list[-which(state.list=="Hawaii")]
  county.list<-countiesPoly@data[,c(4,7)]
    county.list<-county.list[-which(county.list$NAME_1=="Alaska")]
    county.list<-county.list[-which(county.list$NAME_1=="Hawaii")]
    county.list<-paste0(county.list$NAME_1,"-",county.list$NAME_2)
  cdiv.list<-cdivPoly@data[,c(2,7,10)]
    cdiv.list<-paste0(cdiv.list$STATE,"-",cdiv.list$CD_NEW," (",cdiv.list$NAME,")")
  # fix regions
  region.list<-c("Watershed","Corn Belt","All USA")
    
# ---- subset and wrangle data ----  
# county, division or state? DEAL WITH SPECIAL REGIONS!!
  typePET<-"harg" # thornW or harg
  region<-"st" # cy,st, dv
    state <-"arizona"
      stCode<- stateCodes[which(stateCodes$name==CapStr(state)),1]
    cdiv  <- 1
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
    # build name string
    titleName<-paste0(CapStr(county)," County,",CapStr(state))
  }else if (region=="st"){
    tempData$state<-(as.numeric(substr(tempData$code, 1,3)))
    tempData$div<-(as.numeric(substr(tempData$code, 4,4)))
    tempData$element<-(as.numeric(substr(tempData$code, 5,6)))
    tempData$year<-(as.numeric(substr(tempData$code, 7,10)))
    tempData<-subset(tempData, state==stCode & div==0)
    # centroid
    subArea<-subset(statesPoly, NAME_1==CapStr(state))
    centroid<-coordinates(subArea)
    # build name string
    titleName<-paste0(CapStr(state))
  }else{
    tempData$state<-(as.numeric(substr(tempData$code, 1,2)))
    tempData$div<-(as.numeric(substr(tempData$code, 3,4)))
    tempData$element<-(as.numeric(substr(tempData$code, 5,6)))
    tempData$year<-(as.numeric(substr(tempData$code, 7,10)))
    tempData<-subset(tempData, state==stCode & div==cdiv)
    # centroid
    subArea<-subset(cdivPoly, STATE==CapStr(state) & CD_NEW==cdiv)
    centroid<-coordinates(subArea)
    # build name string
    titleName<-paste0(CapStr(state)," Climate Division ", cdiv)
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
  
# inset map ---- fix MI boundary
  insetmap<-ggplot() +
    geom_polygon(data = states4map, aes(x = long, y = lat, group = group), fill="lightgrey", color="grey",size=0.15)  + # get the state border back on top
    geom_polygon(data = subArea, aes(x = long, y = lat, group = group), fill="orange", color="red", size=0.15)  + # get the state border back on top
    coord_fixed(xlim=c(-125, -68), ylim = c(25,50), ratio = 1)+
    #coord_fixed(xlim=c(out$meta$ll[1]-3.5, out$meta$ll[1]+3.5), ylim=c(out$meta$ll[2]-3.5, out$meta$ll[2]+3.5), ratio = 1) +
    #geom_point(data = point, aes(x = V1, y = V2), size=1, color='red')+
    theme_bw(base_size=5)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())+
      theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))+
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  
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
    if (typePET=="thornW"){
      PET <- thornthwaite(fahrenheit.to.celsius(tempDataMelt$tmean,round=2), lat, na.rm = TRUE) 
        }else{
      PET <- hargreaves(fahrenheit.to.celsius(tempDataMelt$tmin,round=2),fahrenheit.to.celsius(tempDataMelt$tmax,round=2),Ra=NA, as.numeric(centroid[1,2]), na.rm = TRUE) 
      }
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
    moAvg[,2:5] <-round(moAvg[,2:5],2)
    
    tempDataMelt <- left_join(tempDataMelt, moAvg, by = "variable")
    tempDataMelt$precipAnom <- tempDataMelt$precip-tempDataMelt$moAvgPrecip
    tempDataMelt$tempAnom <- tempDataMelt$tmean-tempDataMelt$moAvgTemp
    tempDataMelt$PETAnom <- tempDataMelt$PET-tempDataMelt$moAvgPET
    tempDataMelt$P_PETAnom <- tempDataMelt$P_PET-tempDataMelt$moAvgP_PET
    # anom sign
    tempDataMelt$pAnomSign<-ifelse(tempDataMelt$precipAnom > 0, "pos", "neg")
    tempDataMelt$petAnomSign<-ifelse(tempDataMelt$P_PETAnom > 0, "pos", "neg")
    tempDataMelt$TAnomSign<-ifelse(tempDataMelt$tempAnom > 0, "pos", "neg")
    # round values
    tempDataMelt[,8:17] <-round(tempDataMelt[,8:17],2)
    
    # plot variables 
    date1<-"2010-01-01" # by month
    date2<-"2019-10-01" # by month
    maxScale<-60+1 # max 60
      
# SPI contour plot ----
    dfSPI<-melt(dfSPI, id.vars=c(1:3), measure.vars = 4:63)
    dfSPI$value<-as.numeric(dfSPI$value)
      colnames(dfSPI)[2]<-"month"
    # current heat map
      currDfSPI<-dfSPI[which(dfSPI$date==date2),]
    # plot  
     pCurr<- ggplot(currDfSPI, aes((date),as.numeric(variable) , fill = value))+
        geom_tile(width=1)+
        scale_fill_gradientn(colors=c("orange3","orange","yellow","white","green","green2","darkgreen"), name=" ",
                             na.value = "grey50", guide = FALSE, limits=c(-3, 3), oob=squish)+
        theme_bw()+
        scale_y_continuous(limits=c(0,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
        scale_x_date(labels = date_format("%b%Y"), expand=c(0,0))+
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.x=element_blank())+
       labs(title=" ")+
       theme(plot.margin = unit(c(5, 5, 0, 0), "pt"))
      
    # main plot  
    p1<-  ggplot(dfSPI, aes((date),as.numeric(variable) , fill = value))+
      geom_tile(width=31)+
      #scale_fill_gradient2(low = "brown", high = "green",mid = "white",
      #                    na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
      scale_fill_gradientn(colors=c("orange3","orange","yellow","white","green","green2","darkgreen"), name=" ",
                           na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
      scale_x_date(labels = date_format("%Y-%m"), breaks='2 years', expand=c(0,0),
                   limits = c(as.Date(date1),as.Date(date2)))+
      scale_y_continuous(limits=c(0,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
      theme_bw()+
      theme(legend.position="left")+
      theme(plot.title = element_text(face="bold"),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      guides(fill = guide_colourbar(barwidth = 1, barheight = 10))+
      ylab("Timescale (mos)")+
      labs(title=paste0(titleName," Standardized Precipitation Index (", format(as.Date(date1), "%b%Y"),
                        " - ",format(as.Date(date2), "%b%Y"),")"))+
      theme(plot.margin = unit(c(5, 0, 0, 0), "pt"))
    
    # precip anoms
    p2<- ggplot(tempDataMelt,aes(date, precipAnom, fill = pAnomSign)) + 
       geom_bar(stat = "identity", show.legend = FALSE) + 
       #scale_y_continuous(breaks = seq(-100, 100, 20)) +
       scale_fill_manual(values = c("orange4", "darkgreen"))+
       scale_x_date(labels = date_format("%Y"), breaks='2 years', expand=c(0,0),
                    limits = c(as.Date(date1),as.Date(date2)))+
       ylab("Precip Anom (in.)")+
       xlab("Month-Year")+
       theme_bw()+
       theme(axis.text.x = element_text(angle = 45, hjust = 1))+
       theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))+
       theme(panel.grid.minor = element_blank())
     
    # # trying to get alignments
    #    mainP<-plot_grid(p1, p2, ncol = 1, align = 'v', axis=c('l'),rel_heights = c(3.5,1))
    #    sideP<-plot_grid(pCurr, NULL, ncol = 1, rel_heights = c(3.5,1))
    #    plot_grid(mainP, sideP, nrow = 1, align='h',axis = c('tblr'), rel_widths = c(20,1))
    # # another solution
    #    plot_grid(p1,pCurr,p2,NULL, ncol = 2, nrow = 2, align = 'v',axis = 'b', rel_heights = c(10,10,1,1),
    #              rel_widths = c(20,1,20,1))
  
    # plotting grid using align
       mainCurr <- align_plots(p1, pCurr, align = 'h', axis = 'l')
       mainPrec <- align_plots(p1, p2, align = 'v', axis = 'b')
       
       mainP<-plot_grid(mainCurr[[1]], mainPrec[[2]], ncol = 1, align = 'v', axis=c('l'),rel_heights = c(3.5,1))
       sideP<-plot_grid(pCurr, NULL, ncol = 1, rel_heights = c(3.5,1))
       #plot_grid(mainP, sideP, nrow = 1, align='h',axis = c('tblr'), rel_widths = c(20,1))
       spiPlot<-plot_grid(mainP, sideP, nrow = 1, rel_widths = c(20,1))
  
       # add inset map
       spiPlot<-ggdraw(spiPlot)+draw_plot(insetmap, -0.315, 0.40, scale=0.14)
       
       # add margin
       spiPlot = spiPlot + theme(plot.margin = unit(c(0.25, 0.25, 0.7, 0.25), "in")) 
       # add caption
       captionString <- c( "Data from NOAA-NCEI",
                           "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",
                           paste0("Plot created: ", format(Sys.Date(), "%m-%d-%Y")),
                           "The University of Arizona",
                           "https://cals.arizona.edu/climate/")
       spiPlot<-ggdraw(spiPlot) + draw_text(captionString, x =0.125, 
                                              y = c(0.0625,0.0500,0.0375,0.0250,0.0125), hjust = 0,vjust=-0.25, size=8)
       
 
    # write high res to file ----
       png("/home/crimmins/RProjects/ClimPlot/spiPlots/spiPlots.png", width = 11, height = 8.5, units = "in", res = 300L)
       #grid.newpage()
       print(spiPlot, newpage = FALSE)
       dev.off()
       
       # add logos
       # Call back the plot
       plot <- image_read("/home/crimmins/RProjects/ClimPlot/spiPlots/spiPlots.png")
       # And bring in a logo
       #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png") 
       logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
       logo <- image_resize(logo_raw, geometry_size_percent(width=95,height = 95))
       # Stack them on top of each other
       #final_plot <- image_append((c(plot, logo)), stack = TRUE)
       #final_plot <- image_mosaic((c(plot, logo)))
       final_plot <- image_composite(plot, logo, offset = "+2235+2365")
       # And overwrite the plot without a logo
       image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/spiPlots/spiPlots.png")  
    # ----   
       
# PLOTLY SPI HEATMAP ----
         plot_ly(dfSPI, x = ~date, y = ~variable, z = ~value, colors='BrBG', type = "heatmap", zmin=-3, zmax=3) %>%
           layout(title = paste0(titleName," Standardized Precipitation Index (", format(as.Date(date1), "%b%Y"),
                                 " - ",format(as.Date(date2), "%b%Y"),")"),
                  xaxis=list(title="Month-Year",
                             range = c(as.Date(date1),as.Date(date2))),
                  yaxis=list(title="Timescale (mos)",
                             range = c(0,maxScale))
           )
# ----         

       
# SPEI contour plot ----
  dfSPEI<-melt(dfSPEI, id.vars=c(1:3), measure.vars = 4:63)
  dfSPEI$value<-as.numeric(dfSPEI$value)
  colnames(dfSPEI)[2]<-"month"
       # current heat map
       currDfSPEI<-dfSPEI[which(dfSPEI$date==date2),]
       # plot  
       pCurr<- ggplot(currDfSPEI, aes((date),as.numeric(variable) , fill = value))+
         geom_tile(width=1)+
         scale_fill_gradientn(colors=c("orange3","orange","yellow","white","green","green2","darkgreen"), name=" ",
                              na.value = "grey50", guide = FALSE, limits=c(-3, 3), oob=squish)+
         theme_bw()+
         scale_y_continuous(limits=c(0,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
         scale_x_date(labels = date_format("%b%Y"), expand=c(0,0))+
         theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               axis.title.x=element_blank())+
         labs(title=" ")+
         theme(plot.margin = unit(c(5, 5, 0, 0), "pt"))
       
       # main plot  
       p1<-  ggplot(dfSPEI, aes((date),as.numeric(variable) , fill = value))+
         geom_tile(width=31)+
         #scale_fill_gradient2(low = "brown", high = "green",mid = "white",
         #                    na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
         scale_fill_gradientn(colors=c("orange3","orange","yellow","white","green","green2","darkgreen"), name=" ",
                              na.value = "grey50", guide = "colourbar", limits=c(-3, 3), oob=squish)+
         scale_x_date(labels = date_format("%Y-%m"), breaks='2 years', expand=c(0,0),
                      limits = c(as.Date(date1),as.Date(date2)))+
         scale_y_continuous(limits=c(0,maxScale), expand=c(0,0), breaks=seq(0,60,6))+
         theme_bw()+
         theme(legend.position="left")+
         theme(plot.title = element_text(face="bold"),
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank())+
         guides(fill = guide_colourbar(barwidth = 1, barheight = 10))+
         ylab("Timescale (mos)")+
         labs(title=paste0(titleName," Standardized Precipitation-Evapotranspiration Index (", format(as.Date(date1), "%b%Y"),
                           " - ",format(as.Date(date2), "%b%Y"),")"))+
         theme(plot.margin = unit(c(5, 0, 0, 0), "pt"))
       
       # precip anoms
       p2<- ggplot(tempDataMelt,aes(date, P_PETAnom, fill = petAnomSign)) + 
         geom_bar(stat = "identity", show.legend = FALSE) + 
         #scale_y_continuous(breaks = seq(-100, 100, 20)) +
         scale_fill_manual(values = c("orange4", "darkgreen"))+
         scale_x_date(labels = date_format("%Y"), breaks='2 years', expand=c(0,0),
                      limits = c(as.Date(date1),as.Date(date2)))+
         ylab("P-PET Anom (in.)")+
         xlab("Month-Year")+
         theme_bw()+
         theme(axis.text.x = element_text(angle = 45, hjust = 1))+
         theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))+
         theme(panel.grid.minor = element_blank())
       
       # # trying to get alignments
       #    mainP<-plot_grid(p1, p2, ncol = 1, align = 'v', axis=c('l'),rel_heights = c(3.5,1))
       #    sideP<-plot_grid(pCurr, NULL, ncol = 1, rel_heights = c(3.5,1))
       #    plot_grid(mainP, sideP, nrow = 1, align='h',axis = c('tblr'), rel_widths = c(20,1))
       # # another solution
       #    plot_grid(p1,pCurr,p2,NULL, ncol = 2, nrow = 2, align = 'v',axis = 'b', rel_heights = c(10,10,1,1),
       #              rel_widths = c(20,1,20,1))
       
       # plotting grid using align
       mainCurr <- align_plots(p1, pCurr, align = 'h', axis = 'l')
       mainPrec <- align_plots(p1, p2, align = 'v', axis = 'b')
       
       mainP<-plot_grid(mainCurr[[1]], mainPrec[[2]], ncol = 1, align = 'v', axis=c('l'),rel_heights = c(3.5,1))
       sideP<-plot_grid(pCurr, NULL, ncol = 1, rel_heights = c(3.5,1))
       #plot_grid(mainP, sideP, nrow = 1, align='h',axis = c('tblr'), rel_widths = c(20,1))
       spiPlot<-plot_grid(mainP, sideP, nrow = 1, rel_widths = c(20,1))
       
       # add inset map
       spiPlot<-ggdraw(spiPlot)+draw_plot(insetmap, -0.315, 0.40, scale=0.14)
       
       # add margin
       spiPlot = spiPlot + theme(plot.margin = unit(c(0.25, 0.25, 0.7, 0.25), "in")) 
       # add caption
       captionString <- c( "Data from NOAA-NCEI",
                           "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/",
                           paste0("Plot created: ", format(Sys.Date(), "%m-%d-%Y")),
                           "The University of Arizona",
                           "https://cals.arizona.edu/climate/")
       spiPlot<-ggdraw(spiPlot) + draw_text(captionString, x =0.125, 
                                            y = c(0.0625,0.0500,0.0375,0.0250,0.0125), hjust = 0,vjust=-0.25, size=8)
       
       
       # write high res to file ----
       png("/home/crimmins/RProjects/ClimPlot/spiPlots/speiPlots.png", width = 11, height = 8.5, units = "in", res = 300L)
       #grid.newpage()
       print(spiPlot, newpage = FALSE)
       dev.off()
       
       # add logos
       # Call back the plot
       plot <- image_read("/home/crimmins/RProjects/ClimPlot/spiPlots/speiPlots.png")
       # And bring in a logo
       #logo_raw <- image_read("./logos/UA_CLIMAS_logos.png") 
       logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
       logo <- image_resize(logo_raw, geometry_size_percent(width=95,height = 95))
       # Stack them on top of each other
       #final_plot <- image_append((c(plot, logo)), stack = TRUE)
       #final_plot <- image_mosaic((c(plot, logo)))
       final_plot <- image_composite(plot, logo, offset = "+2235+2365")
       # And overwrite the plot without a logo
       image_write(final_plot, "/home/crimmins/RProjects/ClimPlot/spiPlots/speiPlots.png")  
       # ----   
       
       # PLOTLY SPI HEATMAP ----
       plot_ly(dfSPEI, x = ~date, y = ~variable, z = ~value, colors='BrBG', type = "heatmap", zmin=-3, zmax=3) %>%
         layout(title = paste0(titleName," Standardized Precipitation-Evapotranspiration Index (", format(as.Date(date1), "%b%Y"),
                               " - ",format(as.Date(date2), "%b%Y"),")"),
                xaxis=list(title="Month-Year",
                           range = c(as.Date(date1),as.Date(date2))),
                yaxis=list(title="Timescale (mos)",
                           range = c(0,maxScale))
         )
       # ----         
    
       # PLOTLY diff plot ----
       tempPlotlyDF<-as.data.frame(cbind(dfSPI$variable,dfSPI$value-dfSPEI$value))
        colnames(tempPlotlyDF)<-c("variable","value")
        tempPlotlyDF$date<-dfSPI$date
       plot_ly(tempPlotlyDF, x = ~date, y = ~variable, z = ~value, colors='PuOr', type = "heatmap", zmin=-3, zmax=3) %>%
          layout(title = paste0(titleName," SPI-SPEI (", format(as.Date(date1), "%b%Y"),
                                " - ",format(as.Date(date2), "%b%Y"),")"),
                 xaxis=list(title="Month-Year",
                            range = c(as.Date(date1),as.Date(date2))),
                 yaxis=list(title="Timescale (mos)",
                            range = c(0,maxScale))
          )  
       # ---- 
        
# interactive plots of temp, precip, PET, Anoms ----
       # temp Plotly
       tempPlotlyVars<-tempDataMelt[,c(3,5,6,7)]
        colnames(tempPlotlyVars)<-c("date","T-max(F)","T-mean(F)","T-min(F)")
       tempPlots<-tempPlotlyVars %>%
         tidyr::gather(variable,value,-date) %>%
         transform(id = as.integer(factor(variable))) %>%
         plot_ly(x = ~date, y = ~value, color = ~variable, colors = c("dodgerblue4","dimgray","firebrick"),
                 yaxis = ~paste0("y", id)) %>%
         add_lines()
       # temp Anom Plotly
       tempPlotlyVars<-tempDataMelt[,c(3,15)]
        colnames(tempPlotlyVars)<-c("date","T-mean Anom(F)")
       tempAnomPlots<-tempPlotlyVars %>%
         tidyr::gather(variable,value,-date) %>%
         transform(id = as.integer(factor(variable))) %>%
         plot_ly(x = ~date, y = ~value, color = ~variable, colors = "dimgray",
                 yaxis = ~paste0("y", id)) %>%
         add_lines()
       # precip Plotly
       tempPlotlyVars<-tempDataMelt[,c(3,4,14)]
         colnames(tempPlotlyVars)<-c("date","Precip(in)","PrecipAnom(in)")
       precipPlots<-tempPlotlyVars %>%
         tidyr::gather(variable,value,-date) %>%
         transform(id = as.integer(factor(variable))) %>%
         plot_ly(x = ~date, y = ~value, color = ~variable, colors = c("forestgreen","darkslateblue"),
                 yaxis = ~paste0("y", id)) %>%
         add_lines()
       # Precip Anom Plotly
       # tempPlotlyVars<-tempDataMelt[,c(3,14)]
       # precipAnomPlots<-tempPlotlyVars %>%
       #   tidyr::gather(variable,value,-date) %>%
       #   transform(id = as.integer(factor(variable))) %>%
       #   plot_ly(x = ~date, y = ~value, color = ~variable, colors = "darkgreen",
       #           yaxis = ~paste0("y", id)) %>%
       #   add_lines()
       # PET Plotly
       tempPlotlyVars<-tempDataMelt[,c(3,8)]
         colnames(tempPlotlyVars)<-c("date","PET(in)")
       PETPlots<-tempPlotlyVars %>%
         tidyr::gather(variable,value,-date) %>%
         transform(id = as.integer(factor(variable))) %>%
         plot_ly(x = ~date, y = ~value, color = ~variable, colors = "darkgoldenrod",
                 yaxis = ~paste0("y", id)) %>%
         add_lines()
       # PET_P Plotly
       tempPlotlyVars<-tempDataMelt[,c(3,9,17)]
         colnames(tempPlotlyVars)<-c("date","Precip-PET(in)","Precip-PETAnom(in)")
       PET_PPlots<-tempPlotlyVars %>%
         tidyr::gather(variable,value,-date) %>%
         transform(id = as.integer(factor(variable))) %>%
         plot_ly(x = ~date, y = ~value, color = ~variable, colors = c("darkorange","darkorchid4"),
                 yaxis = ~paste0("y", id)) %>%
         add_lines()
       # Precip Anom Plotly
       # tempPlotlyVars<-tempDataMelt[,c(3,17)]
       # PET_PAnomPlots<-tempPlotlyVars %>%
       #   tidyr::gather(variable,value,-date) %>%
       #   transform(id = as.integer(factor(variable))) %>%
       #   plot_ly(x = ~date, y = ~value, color = ~variable, colors = "darkgreen",
       #           yaxis = ~paste0("y", id)) %>%
       #   add_lines()
       # combine in subplots
       pSubPlot<-subplot(tempPlots, tempAnomPlots, precipPlots,
               PETPlots, PET_PPlots, nrows = 6, shareX = TRUE)
       pSubPlot<-layout(pSubPlot, title=paste0(titleName," Monthly Climate Data"))
       
       # test
       pSubPlot<-subplot(tempPlots, tempAnomPlots, precipPlots,
                         PETPlots, nrows = 4, shareX = TRUE)
       pSubPlot<-layout(pSubPlot, title=paste0(titleName," Monthly Climate Data"))
 # ----      
       
       
# climograph https://plot.ly/r/multiple-axes/
       ay <- list(
         tickfont = list(color = "red"),
         overlaying = "y",
         side = "right",
         title = "Temp(F)"
       )
       pClimo <- plot_ly() %>%
         add_lines(x = as.numeric(moAvg$variable), y = moAvg$moAvgPrecip, name = "Precip(in)") %>%
         add_lines(x = as.numeric(moAvg$variable), y = moAvg$moAvgPET, name = "PET(in)") %>%
         add_lines(x = as.numeric(moAvg$variable), y = moAvg$moAvgP_PET, name = "P-PET(in)") %>%
         add_lines(x = as.numeric(moAvg$variable), y = moAvg$moAvgTemp, name = "Temp(F)", yaxis = "y2") %>%
         layout(
           title = paste0(titleName," Monthly Average Climate"), yaxis2 = ay,
           xaxis = list(title="month",
                        range=c(1,12)),
           yaxis = list(title="inches")
         )
       
       