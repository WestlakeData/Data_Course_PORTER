#Library Calls ####
library(tidyverse)
library(ggmap)
library(maps)
library(gganimate)
library(digest)
library(gapminder)

#devtools::install_github("dkahle/ggmap")

#Load in Data ####
voted <- read.csv("./data/All_Voted_SS.csv", stringsAsFactors = F)
names(voted) <- c("VoterID", "Address")

ss.voted <- read.csv("~/../Google Drive/City Council/Campaign 2019/Data/Target Voters/Archive/SS_Voted_2019-11-04.csv")

voted.join <- inner_join(sub, voted, by = "VoterID")







#Geocode Voter
register_google(key = api)
for(i in 1:nrow(voted.join)){
  # Print("Working...")
  
  result <- tryCatch(geocode(voted.join$Address[i], output = "latlona", source = "google"),
                     warning = function(w) data.frame(lon = NA, lat = NA, geoAddress = NA))
  voted.join$lon[i] <- as.numeric(result[1])
  voted.join$lat[i] <- as.numeric(result[2])
  voted.join$geoAddress[i] <- as.character(result[3])
}

write.csv(voted.join, "./data/geocoded_voters.csv", row.names = F)

#Create Maps
map <- get_map("Saratoga Springs, Utah", maptype = "roadmap", zoom = 12)
gmap.SS <- get_googlemap(center = c(-111.89,40.35), maptype = "roadmap", zoom = 12)
ggmap(gmap.COL) +
  geom_point(aes(x = lon, y = lat), data = voted.geocoded,
                 alpha = .5, size = 1)
  
ggmap(gmap.COL) + 
  stat_density2d(aes(x= lon, y = lat, fill = ..level..), alpha = 0.1, bins = 15,
                 size = 0.1, data = voted.geocoded, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red")

saveRDS(gmap.SS, "./data/Saratoga Springs Map.RDS")

#Just the Plot on the Map
ggmap(gmap.SS) +
  geom_point(aes(x = lon, y = lat, color = voted.join$VOTED), data = voted.join, 
             alpha = .5, size = 1) +
  scale_color_discrete(guide = 'none') +
  stat_density2d(aes(x= lon, y = lat, fill = ..level..), alpha = 0.15, bins = 15,
                 size = 0.1, data = voted.join, geom = "polygon") +
  scale_fill_gradient(breaks = c(50,475),
                      labels = c("Low", "High"),
                      low = "red", high = "green") +
  theme(legend.position = "right") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9,0.22)) +
  labs(title = "2019 Saratoga Springs Voter Turnout")
  
#Prepped for Animation
density.map <- ggmap(gmap.SS) +
  geom_point(aes(x = lon, y = lat, color = voted.join$VOTED), data = voted.join, 
             alpha = .5, size = 1)+ 
  stat_density2d(aes(x= lon, y = lat, fill = ..level..), alpha = 0.15, bins = 15,
                 size = 0.1, data = voted.join, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "Total Voter Turnout {frame_time}") + 
  transition_states(voted.join$VOTED)

gganimate(density.map)
dev.off()
