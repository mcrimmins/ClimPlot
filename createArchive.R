# copy files to monthly archive directory
# MAC

# create monthly archive plot if first of the month
if(as.numeric(format(Sys.time(), "%d"))==1){
  
  # clean out monthly directory
  moList = list.files("/home/crimmins/RProjects/ClimPlot/plots/monthly",
                        recursive = FALSE, full.names = TRUE)
  unlink(moList)
  
  # copy over files to archive
  fileList = list.files("/home/crimmins/RProjects/ClimPlot/plots",
                   recursive = FALSE, full.names = TRUE)
  
  fileList<-fileList[-grep("monthly",fileList)]
   
  file.copy(from = fileList, to = "/home/crimmins/RProjects/ClimPlot/plots/monthly/")
  
  fileListMO = list.files("/home/crimmins/RProjects/ClimPlot/plots/monthly",
                        recursive = FALSE, full.names = TRUE)
  
  sapply(fileListMO,FUN=function(eachPath){
    file.rename(from=eachPath,to=sub(pattern=".png",replacement=paste0("_",format(Sys.time(), "%m-%d-%Y"),".png"),eachPath))
  })
  
}else{}