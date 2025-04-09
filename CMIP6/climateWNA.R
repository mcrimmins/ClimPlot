# download, process CMIP6 downscaled data
# MAC 01/26/22
# https://adaptwest.databasin.org/pages/adaptwest-climatena/


URL <- "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/ensembles/ensemble_ssp370_2041_bioclim.zip"
fileName<-  paste0("/home/crimmins/RProjects/ClimPlot/CMIP6/ensemble_ssp370_2041_bioclim.zip")
download.file(URL, destfile = fileName)
print(fileName)

unzip("/home/crimmins/RProjects/ClimPlot/CMIP6/ensemble_ssp370_2041_bioclim.zip")

library(raster)

test<-raster("/home/crimmins/RProjects/ClimPlot/CMIP6/bioclim/ensemble_ssp370_2041_MAT.tif")