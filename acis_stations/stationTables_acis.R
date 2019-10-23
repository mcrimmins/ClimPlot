# ACIS JSON Monsoon summary tables
# MAC 07/18/15

# AZ precip example
library(RCurl)
library(jsonlite)
library(reshape)
#library(ggmap)
#library(RColorBrewer)
#library(gridExtra)
library(xtable)
library(tidyr)
library(SortableHTMLTables)

jsonQuery='{"state":"az,nm","sdate":"2019-06-15","edate":"2019-09-15","elems":[{"name":"pcpn","interval":"dly","duration":"dly","smry":{"reduce":"sum","add":"mcnt"},"smry_only":1},{"name":"pcpn","interval":"dly","duration":"dly","smry":"sum","smry_only":1,"normal":"departure"},{"name":"pcpn","interval":"dly","duration":"dly","smry":{"reduce":"max","add":"date"},"smry_only":1}]}'

out<-postForm("http://data.rcc-acis.org/MultiStnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

ll<-data.frame(matrix(unlist(out$data$meta$ll), nrow=length(out$data$meta$ll), byrow=T))
meta<-out$data$meta
#data<-cbind(meta$name,ll,out$data[2])
#colnames(data)<-c("name","lon","lat","temps")
#data$temps<-as.numeric(as.character(data$temps))

# get meta data formatted
temp<-out$data
temp2<-temp$meta

# get summary formatted
summary<- data.frame(matrix(unlist(out$data$smry), nrow=nrow(out$data), byrow=T))
colnames(summary)<-c("sum","missDays","depart","dayMax","maxDate")
summary<-cbind(temp2,summary)


# write out table
sortable.html.table(summary, './acis_stations/AZNMMonsoonPrecip_thru0915.html', page.title = '06/15-9/15 AZ/NM Precip')

## write html 
#print(xtable(data), type="html") # output omitted
#print.data.frame(data)