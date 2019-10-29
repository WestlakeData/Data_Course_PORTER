library(carData)
library(tidyverse)

data("MplsDemo")
data("MplsStops")

names(MplsDemo)
names(MplsStops)

full <- full_join(MplsStops, MplsDemo, by = "neighborhood")
anit <- anti_join(MplsStops, MplsDemo, by = "neighborhood")

crappy <- read.csv("../../Git_Repositories/Data_Course/Data/Bird_Measurements.csv")
names(crappy)
crappy2 <- crappy %>% 
  select(-ends_with("_N"))
crappyM <- crappy2 %>%
  select(-c(starts_with("F_"), starts_with("unsexed"), starts_with("Unsexed")))

crappyF <- crappy2 %>%
  select(-c(starts_with("M_"), starts_with("unsexed"), starts_with("Unsexed")))

crappyU <- crappy2 %>%
  select(-c(starts_with("F_"), starts_with("M_")))

#Create Gender columns for subsets
crappyM$Gender <- "Male"
crappyF$Gender <- "Female"
crappyU$Gender <- "Unsexed"
  
#  
names(crappyM) <- str_replace(names(crappyM), "M_","")
names(crappyF) <- str_replace(names(crappyF), "F_","")
names(crappyU) <- str_replace(names(crappyU), "unsexed","Unsexed")
names(crappyU) <- str_replace(names(crappyU), "Unsexed_","")

full <- rbind(crappyM, crappyF, crappyU)
full <- arrange(full, full$Family, full$Species_number)
