# Plot NDFD grids
# MAC 12/8/2019

# raster approach
library(raster)
library(leaflet)
#library(mapview)
#library(leafem)
#library(elevatr)
library(rgdal)
library(htmltools)

# get grib files
filename<-"./NDFDGrid/NDFDRTMAUnifiedTerrain2016.gb2"
#filename<-"./NDFDGrid/NDFD_2p5kmTerrain_2012.grib2"
ndfdDEM<-raster(filename)

# srtm elevation grid
zones <- readOGR("./NDFDGrid/z_10oc19", "z_10oc19")
# get TWC
zoneSelect<-zones[zones$CWA =="TWC",]
# reproject data
zoneSelect <- spTransform(zoneSelect,
                              crs(ndfdDEM))

# get elevation
#elev <- get_elev_raster(zoneSelect, z = 8)

# crop DEM grid down to CWA
e <- extent(zoneSelect)
ndfdDEMsub <- crop(ndfdDEM, zoneSelect)
#ndfdDEMsub<-projectRasterForLeaflet(ndfdDEMsub, method = "bilinear")
# develop mesh grid
ndfdGrid<-rasterToPolygons(ndfdDEMsub)
ndfdGrid <- spTransform(ndfdGrid, CRS("+init=epsg:4326")) # WGS
#ndfdGrid <- spTransform(ndfdGrid, CRS("+init=epsg:3857")) # leaflet proj

# get polygons
#plot(r, col=c(topo.colors(200)), axes=FALSE, box=FALSE)
#test<-rasterToPolygons(ndfdDEMsub)
# project for leaflet
#test2 <- spTransform(test, CRS("+init=epsg:4326"))


# pal <- colorNumeric(terrain.colors(3, 1, rev = FALSE), values(ndfdDEM),
#                     na.color = "transparent")
# leaflet() %>% addProviderTiles(providers$OpenTopoMap) %>%
#   addRasterImage(ndfdDEMsub, colors = pal, opacity = 0.8, layerId = "meters") %>%
#   addLegend(pal = pal, values = values(ndfdDEMsub),
#             title = "NDFD/RTMA 2.5km Grid") %>%
#   addMouseCoordinates() %>%
#   addImageQuery(ndfdDEMsub, type="mousemove", layerId = "meters", prefix = "", digits = 1)  %>%
#   #addPolygons(data = test2, weight = 0.1)
#   addPolygons(data=test2, color = "#444444", weight = 0.1, smoothFactor = 0.5,
#               opacity = 1.0, fillOpacity = 0.0,
#               highlightOptions = highlightOptions(color = "white", weight = 1,
#                                                   bringToFront = TRUE))
 

my_title <- tags$p(tags$style("p {color: black; font-size:12px}"),
                   tags$b("NDFD/RTMA 2.5km Forecast Grid (elevation in m)"))

leaflet() %>% addProviderTiles(providers$OpenTopoMap) %>%
  addPolygons(data=ndfdGrid, color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.1,
              highlightOptions = highlightOptions(color = "red", weight = 2,
                                                  bringToFront = TRUE),
              label=as.character(round(ndfdGrid$NDFDRTMAUnifiedTerrain2016),1),
              labelOptions = labelOptions(direction = "auto")) %>%
  addControl(my_title, position = "topright" )


# leaflet(spdf, options = leafletOptions(crs = epsg2163)) %>%
#   addPolygons(weight = 1, color = "#444444", opacity = 1,
#               fillColor = ~pal(pop_2014), fillOpacity = 0.7, smoothFactor = 0.5,
#               label = ~paste(name, pop_2014),
#               labelOptions = labelOptions(direction = "auto"))


# leaflet() %>% addProviderTiles(providers$OpenTopoMap) %>%
#   addRasterImage(elev, colors = pal, opacity = 1, layerId = "meters") %>%
#   addLegend(pal = pal, values = values(elev),
#             title = "SRTM Elev") %>%
#   addMouseCoordinates() %>%
#   addImageQuery(elev, type="mousemove", layerId = "meters", prefix = "", digits = 1)  


# saveWidget(leafMap, file="/home/crimmins/RProjects/ClimPlot/monsoonMaps/leafletMaps/SW_Monsoon_TotalPrecip.html", selfcontained = FALSE)
