#Library Calls
library(tidyverse)
library(ggmap)
library(colmaps)

install.packages("devtools")
devtools::install_github("nebulae-co/colmaps")

3#Register Google Maps API Key
register_google(key = "AIzaSyAFtAiA0BypI0EnnaunaG9weqiV9Cl7s3k")
COL.center <- 
COL.box <- c(left = -79.302364, bottom = -4.709750, right = -64.682794, top = 13.225237)
map.COL <- get_googlemap(center = c(lon = -72.976339, lat = 4.299023), maptype = "hybrid", source = "google", force = T)
ggmap(map.COL)
?get_googlemap
