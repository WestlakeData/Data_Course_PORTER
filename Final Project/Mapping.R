#Library Calls
library(tidyverse)
library(digest)
library(ggmap)
library(maps)
library(mapdata)

#Register Google Maps API Key
register_google(key = readLines("../../google_api.txt"))

#Create Colombia Map ####
#map.COL <- get_map(location = "Colombia", maptype= "roadmap", color = "bw", force = T, zoom = 5)
map.COL <- readRDS("./data/ColombiaMap.RDS")
ggmap(map.COL, borders(regions = state))
saveRDS(map.COL, "./data/ColombiaMap.RDS")

COL <- map_data("colombia")
help(package = "mapdata")
