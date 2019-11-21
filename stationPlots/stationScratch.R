
# load libraries
library(RCurl)
library(jsonlite)
library(tidyr)
library(stringr)

# get valid date ranges
jsonQuery='{"state":"AZ","meta":"name,valid_daterange","elems":"pcpn"}'
out<-postForm("http://data.rcc-acis.org/StnMeta", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

nameRange <- data.frame(matrix(unlist(out$meta$valid_daterange), nrow=length(out$meta$valid_daterange), byrow=T))

nameRange$name<-out$meta$name


# get station name 
jsonQuery='{"state":"AZ","meta":"sids,name,valid_daterange","elems":"pcpn"}'
out<-postForm("http://data.rcc-acis.org/StnMeta", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

test<-data.frame(matrix((out$meta$sids), nrow=nrow(out$meta), byrow=T))
colnames(test)[1]<-"col1"



test<-gsub('c("', '', test, fixed = TRUE)

test2<-separate(data = test, col = col1, into = c("var1","var2","var3","var4","var5"), sep = ",",fill="left")



# get valid date ranges
jsonQuery='{"state":"AZ","meta":"sids,name,valid_daterange","elems":"pcpn"}'
out<-postForm("http://data.rcc-acis.org/StnMeta", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
outList<-fromJSON(out, flatten = TRUE)
# wrangle into dataframe
  dates<-data.frame(matrix(unlist(outList$meta$valid_daterange), nrow=nrow(outList$meta), byrow=T))
  sids<-as.data.frame(t(sapply(outList$meta$sids, '[', seq(max(sapply(outList$meta$sids, length))))))
  names<-outList$meta$name
stations<-cbind(names,dates,sids)

# find station type
test<-mapply(grepl,"US", stations)


  outList<-outList$meta
  # Hacky way to strip characters - I don't understand regex
  outList$valid_daterange <- sub("c\\(", "", outList$valid_daterange)
  outList$valid_daterange <- sub("\\(", "", outList$valid_daterange)
  outList$valid_daterange <- sub("\\)", "", outList$valid_daterange)
  outList$sids <- sub("c\\(", "", outList$sids)
  outList$sids <- sub("\\(", "", outList$sids)
  outList$sids <- sub("\\)", "", outList$sids) 
# seperate into cols
  outList<-separate(data = outList, col = valid_daterange, into = c("beginYr","endYr"), sep = ",",fill="left")
  outList<-separate(data = outList, col = sids, into = c("var1","var2","var3","var4","var5"), sep = ",",fill="left")
# convert to dates
  outList$beginYr<-trimws(outList$beginYr, whitespace = "[\\\\]")
  outList$beginYr<-as.Date(outList$beginYr, format="%Y-%m-%d")
  

  
  test<-gsub("[c]", "", outList)
  

