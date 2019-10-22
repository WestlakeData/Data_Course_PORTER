#Library Calls ####
library(tidyverse)

# Read in Data ####
mush <- read.csv("./data/mushroom_growth.csv")

#Data Exploration ####
summary(mush)

plot(mush$GrowthRate ~ mush$Humidity)
plot(mush$GrowthRate ~ as.factor(mush$Temperature))
plot(mush$GrowthRate ~ as.factor(mush$Nitrogen))
plot(mush$GrowthRate ~ as.factor(mush$Light))

#Model Creation ####
lm.model <- lm(mush$GrowthRate ~ mush$Light, data = mush)
summary(lm.model)

aov.model <- aov(mush$GrowthRate ~ mush$Species + mush$Light + mush$Humidity, data = mush)
summary(aov.model)

#Calculate MSE ####
mse.lm <- mean(sum((lm.model$residuals)^2))
mse.aov <- mean(sum((aov.model$residuals)^2))


max(mse.lm, mse.aov)


test <- 
aov.pred <- predict(aov.model, test)
