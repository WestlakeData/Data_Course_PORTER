#Library Calls
library(tidyverse)
library(ggplot2)
library(arules)
library(rpart)
library(rpart.plot)

#Read in data
voter.train <- readRDS("./data/geocoded_voters_anon.RDS")
voter.score <- read.csv("../../Post Election Score.csv")
colnames(voter.score) <- c("Voter.ID", "Precinct", "Full_Address", "X11.5.2019")

#Some initial plots to visualize data ####

#plot histogram of Voter Age
summary(voter.train$Age)

voter.train %>%
ggplot(aes(Age)) +
  geom_histogram(bins = 20) +
  labs(title = "Saratoga Springs Registerd Voters",
       subtitle = "Age Distribution")

#precinct characteristics
voter.train %>% filter(Precinct != "SR03:UN" & Precinct != "UCFF") %>%
  ggplot(aes(Precinct, Age)) +
  geom_boxplot() +
  labs(title = "Saratoga Springs Registered Voters",
       subtitle = "Age Distribution by Precinct")

voter.train %>% 
  ggplot(aes(Precinct, sum(as.numeric(voter.train$Voted_X11.7.2017)))) +
  geom_bar(stat = "identity") #Why are y values so large?


#plot histogram of distances to closest candidate
m <- as.data.frame(na.omit(voter.train$min.c.dist)) %>% filter(`na.omit(voter.train$min.c.dist)` < 6000)
ggplot(m, aes(m$`na.omit(voter.train$min.c.dist)`)) +
  geom_histogram(bins = 50) +
  labs(title = "Voter Distance from Closest Candidate",
       x = "Distance (m)")

summary(m)

#Association rules for data ####
voter.apriori <- voter.train[, 30:48]
rules <- apriori(voter.apriori, parameter = list (supp = 0.01, conf = 0.5, maxlen = 5))

#rules for municipal elections 
#Rules for Primary Elections
rules.P2017 <- apriori(voter.apriori[,1:15], parameter=list (supp=0.01,conf = 0.8), appearance = list (default="lhs",rhs="Voted_X8.15.2017"), control = list (verbose=F))
#Rules for General elections
rules.G2017 <- apriori(voter.apriori[,1:16], parameter=list (supp=0.05,conf = 0.5), appearance = list (default="lhs",rhs="Voted_X11.7.2017"), control = list (verbose=F))

#Examine Primary Rules
rules.P2017
rules_conf.P <- sort(rules.P2017, by="confidence", decreasing=T)
rules_supp.P <- sort(rules.P2017, by = "support", decreasing = T)
inspect(head(rules_conf.P, n = 10))
inspect(head(rules_supp.P, n = 10))

#Examine General Rules
rules.G2017
rules_conf.G <- sort(rules.G2017, by="confidence", decreasing=T)
rules_supp.G <- sort(rules.G2017, by = "support", decreasing = T)
inspect(head(rules_conf.G, n = 10))
inspect(head(rules_supp.G, n = 10))


#Clustering analysis ####

#Option 1 **Age & Voting History** ####
voter.cluster <- voter.train
cols <- sapply(voter.cluster, is.logical)
voter.cluster[,cols] <- lapply(voter.cluster[,cols], as.numeric)

voter.clust.data <- dplyr::select(voter.cluster, Age, starts_with("Voted_X"))

#Evaluate optimal k -Option 1
wss <- numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(voter.clust.data, centers= k, nstart = 25)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

#Cluster Data, k=3
clu.voters1 <- kmeans(voter.clust.data, centers = 3, nstart = 25)
voter.train.clust1 <- data.frame(voter.cluster, "Cluster" = clu.voters1$cluster)

clust1.centroid <- data.frame(clu.voters1$size, clu.voters1$centers)
clust1.centroid

#Option 2 **Age, and Apriori Rule selected Voting History ####
apriori.col <- c("Age", "Voted_X11.2.2010", "Voted_X11.6.2012", "Voted_X11.5.2013", "Voted_X11.4.2014", "Voted_X11.3.2015", "Voted_X11.8.2016", "Voted_X8.15.2017")
voter.clust.data2 <- dplyr::select(voter.train, Age, apriori.col)
voter.clust.data2$Age <- log10(voter.clust.data2$Age)

#Evaluate optimal k -Option 2
wss <- numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(voter.clust.data2, centers= k, nstart = 25)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

#Cluster Data, k=4
clu.voters2 <- kmeans(voter.clust.data2, centers = 4, nstart = 25)
voter.train.clust2 <- data.frame(voter.train, "Cluster" = clu.voters2$cluster)

clust2.centroid <- data.frame(clu.voters2$size, clu.voters2$centers)
clust2.centroid

likely.voters <- voter.train.clust2 %>% filter(Cluster == 3)
saveRDS(likely.voters, file = "./data/Kmeans2_Likely_Voters.RDS")

#Option 3 **Age, Voting History & Distance
voter.clust.data3 <- dplyr::select(voter.train, Age, apriori.col, min.c.dist) %>% filter(!is.nan(min.c.dist)) %>% filter(min.c.dist != 0) %>% filter(min.c.dist < 20000) %>% filter(Age < 110)
voter.clust.data3$min.c.dist <- log10(voter.clust.data3$min.c.dist)

summary(voter.clust.data3$min.c.dist)

#Evaluate optimal k -Option 3
wss <- numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(voter.clust.data3, centers= k, nstart = 25)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

#Cluster data, k=5
clu.voters3 <- kmeans(voter.clust.data3, centers = 5, nstart = 25)
voter.train.clust3 <- data.frame(voter.train, "Cluster" = clu.voters3$cluster)

clust3.centroid <- data.frame(clu.voters3$size, clu.voters3$centers)
clust3.centroid

#Evaluate Clustering
nrow(likely.voters)
nrow(actual.voters)

correct.kmeans <- semi_join(actual.voters, likely.voters, by = "Voter.ID")
nrow(correct.kmeans)

#Classification Tree #
voter.score <- voter.score[,c(1,4)]
voter.train.tree <- left_join(voter.train, voter.score, by = "Voter.ID")
colnam <- "Voted_X11.5.2019"

for(i in 1:nrow(voter.train.tree)){  
  if(is.na(voter.train.tree[i,"X11.5.2019"])) {
    voter.train.tree[i,colnam] <- 0
  }
  else{
    voter.train.tree[i,colnam] <- 1
  }
}
names(voter.train.tree)
var <- c(2:25, 28:48)

voter.tree <- rpart(voter.train.tree$Voted_X11.5.2019 ~ voter.train.tree$Permanent.Absentee + voter.train.tree$Registration.Date + voter.train.tree$Original.Registration.Date + voter.train.tree$Party + voter.train.tree$Precinct + voter.train.tree$Age + voter.train.tree$min.c.dist + voter.train.tree$Voted_X6.22.2010 + voter.train.tree$Voted_X11.2.2010 + voter.train.tree$Voted_X9.13.2011 + voter.train.tree$Voted_X11.8.2011 + voter.train.tree$Voted_X6.26.2012 + voter.train.tree$Voted_X11.6.2012 + voter.train.tree$Voted_X8.13.2013 + voter.train.tree$Voted_X11.5.2013 + voter.train.tree$Voted_X6.24.2014 + voter.train.tree$Voted_X11.4.2014 + voter.train.tree$Voted_X8.11.2015 + voter.train.tree$Voted_X11.3.2015 + voter.train.tree$Voted_X6.28.2016 + voter.train.tree$Voted_X11.8.2016 + voter.train.tree$Voted_X8.15.2017 + voter.train.tree$Voted_X11.7.2017 + voter.train.tree$Voted_X6.26.2018 + voter.train.tree$Voted_X11.6.2018 + voter.train.tree$Voted_X11.5.2019 , method = "class")
summary(voter.tree)
prp(voter.tree,extra=4, faclen=0, varlen=0)

write.csv(voter.train.tree, file = "./data/Tree Training Data", row.names = F)
