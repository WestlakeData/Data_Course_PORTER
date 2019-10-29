library(tidyverse)

df <- readRDS("../Data_Course_PORTER/clean_data.RDS")
names(df)[names(df)== "Days Alive"] <- "DaysAlive"
ggplot(df, aes(x= DaysAlive, y= IQ, color= Gender)) +
  geom_point()

data(iris)

ggplot(iris, aes(x= Petal.Length, y= Sepal.Length))+
  geom_point() + geom_smooth(method = "lm")

lm.mod <- lm(data = iris, Sepal.Length ~ Petal.Length)
lm.mod

mod2 <- lm(data = iris, Sepal.Length ~Petal.Length + Species)
summary(mod2)

mean(residuals(mod1)^2)