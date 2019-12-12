#Install necessary packages ####
#install.packages("ggmap") 
#install.packages("gganimate")
#install.packages("digest")
#install.packages("digest")
#install.packages("gifski")
#install.packages("ransformr")

#Library Calls ####
library(tidyverse)
library(ggmap)
library(gganimate)
library(digest)
library(gifski)
library(transformr)

#Load in Data ####
voted <- read.csv("./data/All_Voted_SS.csv", stringsAsFactors = F)
names(voted) <- c("VoterID", "Address")

ss.voted <- read.csv("~/../Google Drive/City Council/Campaign 2019/Data/Target Voters/Archive/SS_Voted_2019-11-04.csv")

voted.join <- inner_join(sub, voted, by = "VoterID")

#Geocode Voter Addresses ####
register_google(key = readLines("../../google_api.txt"))
for(i in 1:nrow(voted.join)){
  result <- tryCatch(geocode(voted.join$Address[i], output = "latlona", source = "google"),
                     warning = function(w) data.frame(lon = NA, lat = NA, geoAddress = NA))
  voted.join$lon[i] <- as.numeric(result[1])
  voted.join$lat[i] <- as.numeric(result[2])
  voted.join$geoAddress[i] <- as.character(result[3])
}

write.csv(voted.join, "./data/geocoded_voters.csv", row.names = F)

#Create Maps ####
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


##############################################################################################################################

#Plot Likely Voters ####
likely.voters <- readRDS("./data/Kmeans2_Likely_Voters.RDS")

jpeg("./images/Kmeans Likely Voters.jpg")
ggmap(gmap.SS) +
  geom_point(aes(x = lon, y = lat), data = likely.voters, 
             alpha = .25, size = 1) +
  scale_color_discrete(guide = 'none') +
  stat_density2d(aes(x= lon, y = lat, fill = ..level..), alpha = 0.25, bins = 15,
                 size = 0.1, data = likely.voters, geom = "polygon") +
  scale_fill_gradient(breaks = c(50,450),
                      labels = c("Low", "High"),
                      low = "red", high = "green") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9,0.22)) +
  labs(title = "2019 Saratoga Springs Likely Voters")
dev.off()

#Plot Actual Voters ####
actual.voters <- voter.score %>% filter(!is.na(voter.score$`X11.5.2019`))

jpeg("./images/Actual Voters.jpg")
ggmap(gmap.SS) +
  geom_point(aes(x = lon, y = lat), data = actual.voters, 
             alpha = .25, size = 1) +
  scale_color_discrete(guide = 'none') +
  stat_density2d(aes(x= lon, y = lat, fill = ..level..), alpha = 0.25, bins = 15,
                 size = 0.1, data = actual.voters, geom = "polygon") +
  scale_fill_gradient(breaks = c(50,450),
                      labels = c("Low", "High"),
                      low = "red", high = "green") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9,0.22)) +
  labs(title = "2019 Saratoga Springs Actual Voters")# + 
  #geom_point(aes(x = lon, y = lat), data = candidates, color = 'black', size = 2)
dev.off()

#Just the Plot on the  ####
gmap.SS <- readRDS("./data/Saratoga Springs Map.RDS") #Read saved Map

voted.join <- read.csv("./data/geocoded_voters.csv") #Read in geocoded Addresses
candidates <- read.csv("./data/Candidates.csv") #Read in geocoded candidate Addresses

voted.join$VOTED <- as.Date(voted.join$VOTED, format = '%m/%d/%Y')

ggmap(gmap.SS) +
  geom_point(aes(x = lon, y = lat), color = voted.join$VOTED, data = voted.join, 
             alpha = .5, size = 1) +
  scale_color_discrete(guide = 'none') +
  stat_density2d(aes(x= lon, y = lat, fill = ..level..), alpha = 0.15, bins = 15,
                 size = 0.1, data = voted.join, geom = "polygon") +
  scale_fill_gradient(breaks = c(50,450),
                      labels = c("Low", "High"),
                      low = "red", high = "green") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9,0.22)) +
  labs(title = "2019 Saratoga Springs Voter Turnout")# +
  geom_point(aes(x = lon, y = lat), data = candidate, color = 'black', size = 2)

#Prepped for Animation
density.map <- ggmap(gmap.SS) +
  geom_point(aes(x = lon, y = lat), data = animate.data, 
             alpha = .5, size = 1) +
  scale_color_discrete(guide = 'none') +
  stat_density2d(aes(x= lon, y = lat, fill = ..level..), alpha = 0.15, bins = 15,
                 size = 0.1, data = animate.data, geom = "polygon") +
  scale_fill_gradient(breaks = c(50,750),
                      labels = c("Low", "High"),
                      low = "red", high = "green") +
  theme(legend.position = "right") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9,0.22)) +
  ggtitle("2019 Saratoga Springs Voter Turnout", subtitle = "Date: {closest_state}") +
  geom_point(aes(x = lon, y = lat), data = candidate, color = 'black', size = 2) +
  transition_states(voted.join$VOTED, 
                    state_length = 5)

density.map

