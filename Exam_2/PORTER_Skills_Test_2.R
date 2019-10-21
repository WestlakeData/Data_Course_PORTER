#Library Calls ####
library(tidyverse)
library(modelr)

#Task I ####
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

#Task II. Linear modeling and predictions ####
#Read in atmosphere.csv (pretty clean data set)

atmo <- read.csv("./data/atmosphere.csv")

#Create three different linear models with Diversity as the dependent variable. The three models should have different
#predictors, or at least different numbers of predictors, with or without interaction terms. (10 points)

m1 <- lm(atmo$Diversity ~ atmo$Aerosol_Density + atmo$CO2_Concentration + atmo$Precip + atmo$Year, data = atmo)
summary(lin.mod1)

m2 <- lm(atmo$Diversity ~ atmo$Aerosol_Density + atmo$Precip + atmo$Year, data = atmo)
summary(lin.mod2)

m3 <- lm(atmo$Diversity ~ atmo$Aerosol_Density * atmo$Precip * atmo$Year, data = atmo)
summary(lin.mod3)

#Compare the residuals of the three models and somehow document which has best explanatory power for the data (10 points)

mean(abs(m1$residuals))
mean(abs(m2$residuals))
mean(abs(m3$residuals))

"Model 3 which incorporated interactions had the lowest mean residual when compared to the other 2 models"
"When comparing the 'Multiple R-squared' values for each model Model 3 had the highest value at 0.9828, meaning that this model explains 98.28% of the variation found in the data"

#Use all your models to predict Diversity values in the data set (10 points)

m.pred <- gather_predictions(atmo, m1, m2, m3, .pred = "Diversity.pred")

#Make a plot showing actual Diversity values, along with the three models' predicted Diversity values.

ggplot(atmo, aes(atmo$Aerosol_Density, atmo$Diversity)) +
  geom_point() +
  geom_point(data = m.pred, aes(m.pred$Diversity.pred, m.pred$Aerosol_Density, color = m.pred$model), alpha = 0.5)

# Write code to show the predicted values of Diversity for each model using the hypothetical new data found in hyp_data.csv (10 points)

hyp <- read.csv("./data/hyp_data.csv")
hyp.pred <- gather_predictions(hyp, lin.mod1, lin.mod2, lin.mod3, .pred = "Diversity.Pred")

#Export a text file that contains the summary output from *both* your models to "model_summaries.txt" (10 points)  ***(Hint: use the sink() function)***




# *Bonus* ####
#Add these hypothetical predicted values (from hypothetical data - Part II, Step 6) to a plot of actual data 
#and differentiate them by color. (10 bonus points possible for a pretty graph)




# *Bonus* ####
#Split the atmosphere.csv data into training and testing sets, randomly. Train your single best model on 50% of the data and 
#test it on the remaining 50% of the data. Find some way to show how well it fits the data.
#This is the only cross-validation part of the exam. (10 bonus points for proper code)