#Library Calls
library(tidyverse); library(digest); library(ggmap); library(gganimate); library(digest); library(gifski); library(transformr)

#Register Google Maps API Key
register_google(key = readLines("../../google_api.txt"))

#Create Colombia Map ####
map.COL <- get_map(location = "Colombia", maptype= "roadmap", color = "bw", force = T, zoom = 6)
map.COL <- readRDS("./data/ColombiaMap.RDS")
ggmap(map.COL)
saveRDS(map.COL, "./data/ColombiaMap.RDS")

#Read in data ####
Colombia.Map <- readRDS("./data/ColombiaMap.RDS")
Colombia.Cases <- readRDS("./data/ColombiaZikaGeoCases.RDS")
Colombia.Cases <- na.omit(Colombia.Cases)# %>% filter(Colombia.Cases$report_date < "2016-03-01")
Colombia.Cases <- filter(Colombia.Cases, Colombia.Cases$report_date < "2016-03-01")

#2D Animated Density Map
ggmap(Colombia.Map) +
  stat_density2d(aes(x = lon, y = lat, fill = ..level..), alpha = 0.25, bins = 5,
                 size = 0.1, data = Colombia.Cases, geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red",
                      limits = c(0.025, 0.075),
                      breaks = c(.03,.07),
                      labels = c("Low", "High")) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9,0.22)) +
  labs(title = "Zika Virus in Colombia", subtitle = "Report Date: {closest_state}") +
  transition_states(Colombia.Cases$report_date, 
                    state_length = 3)

#Plot by number of cases
p <- ggmap(Colombia.Map) +
  geom_point(aes(lon, lat, size = Colombia.Cases$cases, color = Colombia.Cases$cases), alpha = 0.15, data = Colombia.Cases) +
  scale_color_gradient(low = "blue", high = "red",
                      limits = c(250, 1250)
                      ) +
  labs(title = "Zika Virus in Colombia", subtitle = "Report Date: {closest_state}") +
    transition_states(Colombia.Cases$report_date, 
                    state_length = 10,
                    transition_length = 4)

animate(p, nframes = 250)

#Mexico ####
#Get Map of Mexico
country.map <- get_map(location = "Panama", maptype= "roadmap", color = "bw", force = T, zoom = 3)
#saveRDS(country.map, "./data/MexicoMap.RDS")
#Read in data
country.map <- readRDS("./data/MexicoMap.RDS")
country.cases <- readRDS("./data/MexicoZikaGeoCases.RDS")
#country.cases <- country.cases %>% filter(Colombia.Cases$report_date < "2016-03-01")

#2D Animated Density Map
ggmap(country.map) +
  stat_density2d(aes(x = lon, y = lat, fill = ..level..), alpha = 0.25, bins = 10,
                 size = 0.1, data = country.cases, geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red",
                      #limits = c(0.005, 0.0125),
                      #breaks = c(.03,.07),
                      #labels = c("Low", "High")
                      ) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9,0.22)) +
  labs(title = "Zika Virus in Mexico", subtitle = "Report Date: {closest_state}") +
  transition_states(country.cases$report_date, 
                    state_length = 3)
country.cases <- rbind(Colombia.Cases, country.cases)

p <- ggmap(country.map) +
  geom_point(aes(lon, lat, size = country.cases$cases, color = country.cases$cases), alpha = 0.15, data = country.cases) +
  scale_color_gradient(low = "blue", high = "red",
                      limits = c(250, 1250)
                       ) +
  labs(title = "Zika Virus in Mexico", subtitle = "Report Date: {closest_state}") +
  transition_states(country.cases$report_date, 
                    state_length = 10,
                    transition_length = 4)

animate(p, nframes = 250)
