#Library Calls
library(tidyverse)
library(modelr)

#Task I
#Read in data

salaries <- read.csv("./data/salaries.csv")

#Convert to a usable tidy format so we can look at "Salary" as a dependent variable (10 points)
flat.salary <- salaries %>%
  gather(key = "Rank", value = "Salary", AssistProf, AssocProf, FullProf)

#Using ggplot, create boxplot of salary (y-axis) by University Tier (x-axis), filled by Faculty Rank (10 points)

plot1 <- flat.salary %>% ggplot(aes(Tier, Salary, fill = Rank)) +
  geom_boxplot() +
  ggtitle("Faculty Salaries - 1995")

 #Export this delightful boxplot to a file named "LASTNAME_exam2_plot1.jpeg" (10 points)

jpeg("./images/PORTER_exam2_plot1.jpeg")

plot1

dev.off()

#Task II. Linear modeling and predictions
#Read in atmosphere.csv (pretty clean data set)

atmo <- read.csv("./data/atmosphere.csv")

#Create three different linear models with Diversity as the dependent variable. The three models should have different
#predictors, or at least different numbers of predictors, with or without interaction terms. (10 points)

lin.mod1 <- lm(atmo$Diversity ~ atmo$Aerosol_Density + atmo$CO2_Concentration + atmo$Precip + atmo$Year, data = atmo)
summary(lin.mod1)

lin.mod2 <- lm(atmo$Diversity ~ atmo$Aerosol_Density + atmo$Precip + atmo$Year, data = atmo)
summary(lin.mod2)

lin.mod3 <- lm(atmo$Diversity ~ atmo$Aerosol_Density * atmo$Precip * atmo$Year, data = atmo)
summary(lin.mod3)

#Compare the residuals of the three models and somehow document which has best explanatory power for the data (10 points)

mean(abs(lin.mod1$residuals))
mean(abs(lin.mod2$residuals))
mean(abs(lin.mod3$residuals))

"Model 3 which incorporated interactions had the lowest mean residual when compared to the other 2 models"
"When comparing the 'Multiple R-squared' values for each model Model 3 had the highest value at 0.9828, meaning that this model explains 98.28% of the variation found in the data"

#Use all your models to predict Diversity values in the data set (10 points)

model.predictions <- gather_predictions(atmo, lin.mod1, lin.mod2, lin.mod3, .pred = "Diversity.pred")

#Make a plot showing actual Diversity values, along with the three models' predicted Diversity values.

ggplot(atmo, aes(atmo$Aerosol_Density*atmo$Precip*atmo$Year, atmo$Diversity)) +
  geom_point()
