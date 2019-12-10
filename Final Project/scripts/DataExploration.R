#Library Calls
library(tidyverse)
library(ggplot2)
library(arules)

#Read in data
voter.train <- readRDS("./data/geocoded_voters_anon.RDS")

#Some initial plots to visualize data ####

#plot histogram of Voter Age
summary(voter.train$Age)
#Some erroneous data and so we will remove outliers
voter.train %>% filter(Age < 110) %>%
ggplot(aes(Age)) +
  geom_histogram(bins = 20)

#precinct characteristics
voter.train %>% filter(Age < 110) %>%
  ggplot(aes(Precinct, Age)) +
  geom_boxplot()

voter.train %>% filter(Age < 110) %>%
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
voter.apriori <- voter.train[, 29:46]
rules <- apriori(voter.apriori, parameter = list (supp = 0.01, conf = 0.5, maxlen = 5))

#rules for municipal elections
#Rules for Primary Elections
rules.P2017 <- apriori(voter.apriori[,1:15], parameter=list (supp=0.01,conf = 0.5), appearance = list (default="lhs",rhs="Voted_X8.15.2017"), control = list (verbose=F))
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

#Option 1 **Age & Voting History**
cols <- sapply(voter.train, is.logical)
voter.train[,cols] <- lapply(voter.train[,cols], as.numeric)

voter.clust.data <- dplyr::select(voter.train, Age, starts_with("Voted_X"))

#Evaluate optimal k -Option 1
wss <- numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(voter.clust.data, centers= k, nstart = 25)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

#Cluster Data, k=4
clu.voters1 <- kmeans(voter.clust.data1, centers = 4, nstart = 25)
voter.train.clust1 <- data.frame(voter.train, "Cluster" = clu.voters1$cluster)


#Option 2 **Age, Voting History & Distance

voter.clust.data2 <- dplyr::select(voter.train, Age, starts_with("Voted_X"), min.c.dist) %>% filter(!is.nan(min.c.dist)) %>% filter(min.c.dist != 0)
voter.clust.data2$min.c.dist <- log10(voter.clust.data2$min.c.dist)

summary(voter.clust.data2$min.c.dist)

#Evaluate optimal k -Option 2
wss <- numeric(15)
for (k in 1:15) wss[k] <- sum(kmeans(voter.clust.data2, centers= k, nstart = 25)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

#Cluster data, k=5
clu.voters2 <- kmeans(voter.clust.data, centers = 5, nstart = 25)
voter.train.clust <- data.frame(voter.train, "Cluster" = clu.voters$cluster)
