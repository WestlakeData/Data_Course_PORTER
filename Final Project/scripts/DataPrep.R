#Install Packages
#install.packages("raster")
#install.packages("rapportools")

#Library Calls
library(tidyverse)
library(raster)

#Read data
voter.train <- read.csv("../../AnonGeocode.csv")
candidate <- read.csv("./data/Candidates.csv")

#Remove observations that aren't municipal voters
voter.train <- filter(voter.train, voter.train$Precinct != "SR03:UN" & voter.train$Precinct != "UCFF")

#Candidate Matix
for (i in 1:6) {
  assign(paste("C",i, sep = ""), c(candidate$lon[i], candidate$lat[i]))
}
candidate.lonlat <- rbind(C1, C2, C3, C4, C5, C6)

#Calculate Distances of voters from candidates and record the minimum
for (i in 1:nrow(voter.train)) {
  v <- c(voter.train$lon[i], voter.train$lat[i])
  voter.train$min.c.dist[i] <- min(pointDistance(v,candidate.lonlat, lonlat = T))
  
}

#Create binary values for each election participation
election.list <- dplyr::select(voter.train, starts_with("X")) %>% names()

for(e in election.list){
    colnam <- paste("Voted_", e, sep = "")

  for(i in 1:nrow(voter.train)){  
    if(is.na(voter.train[i,e])) {
      voter.train[i,colnam] <- F
    }
    else{
      voter.train[i,colnam] <- T
    }
  }
}

saveRDS(voter.train, file = "./data/geocoded_voters_anon.RDS")