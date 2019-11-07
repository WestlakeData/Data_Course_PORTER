#Library Calls
library(tidyverse)
library(ggmap)
library(rgdal)

key <- "AIzaSyAFtAiA0BypI0EnnaunaG9weqiV9Cl7s3k"

#Register Google Maps API Key
register_google(key = key)

#Install ColMaps####
install.packages("devtools")
devtools::install_github("nebulae-co/colmaps")

#Create Colombia Map ####
COL.center <- 
COL.box <- c(left = -79.302364, bottom = -4.709750, right = -64.682794, top = 13.225237)
map.COL <- get_map(location = COL.box, maptype= "roadmap", color = "bw", force = T)
gmap.COL <- get_googlemap(center = c(lon = -72.976339, lat = 4.299023),zoom = 5, maptype = "roadmap", source = "google", force = T)

ggmap(map.COL)
?get_googlemap
#Load SHP file data
library(rgdal)
shpData <- readOGR(dsn="./images/mpio_84.shp")
proj4string(shpData) # describes data’s current coordinate reference system
# to change to correct projection:
shpData <- spTransform(shpData,
                       CRS("+proj=longlat +datum=WGS84")) 
ogrListLayers(dsn = "./images/mpio_84.prj")
