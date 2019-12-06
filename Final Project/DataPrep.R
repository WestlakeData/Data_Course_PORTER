#Install Packages
#install.packages("raster")
#install.packages("rapportools")

#Library Calls
library(tidyverse)
library(raster)
library(ggplot2)

#Read data
voter.train <- read.csv("../../AnonGeocode.csv")
candidate <- read.csv("./data/Candidates.csv")

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

m <- as.data.frame(na.omit(voter.train$min.c.dist)) %>% filter(`na.omit(voter.train$min.c.dist)` < 20000)
ggplot(m, aes(m$`na.omit(voter.train$min.c.dist)`)) +
  geom_histogram(bins = 100)

saveRDS(voter.train, file = "./data/geocoded_voters_anon.RDS")

#Create binary values for each election participation
election.list <- dplyr::select(voter.train, starts_with("X")) %>% names()
colnum <- which(names(voter.train) %in% election.list)

for (e in 1:length(colnum)) {
  election <- colnum[e]
  for (i in 1:nrow(voter.train)) {
    #print(voter.train[i,election])
    if(is.na(voter.train[i,election])){
     print("Is NA")
    }
    else{
      print("Has Value")
    }
  }
}

colnum[1:50]
voter.train[1,100]




#Clustering analysis
wss <- numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(cars.sub2, centers= k, nstart = 25)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

clu.cars <- kmeans(cars.sub2, 4, nstart = 25)
clu.cars
plot(cars.sub2, hp, qsec, color= "cluster")
cars.out <- as.data.frame(cbind(mtcars,clu.cars$cluster))