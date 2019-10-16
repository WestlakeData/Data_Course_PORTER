#Library Calls
library(tidyverse)

#Load the "iris" data set
data("iris")

#Duplicate the following 3 figures (on next 3 pages) and save them in your Assignment_5 directory as "iris_fig1.png", "iris_fig2.png", "iris_fig3.png", respectively.
#iris_fig1.png

#iris_fig1.png
png("./images/iris_fig1.png")

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, col=Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Sepal Length vs Petal Length",
       subtitle = "for three Iris species") +
  theme_light() +
    theme(panel.border = element_blank())

dev.off()    

#iris_fig2.png
png("./images/iris_fig2.png")

ggplot(iris) +
  geom_density(aes(Petal.Width, fill = Species), alpha = 0.5) +
  labs(title = "Sepal Length vs Petal Length",
       subtitle = "for three Iris species") +
  xlab("Petal Width") +
  theme_light() +
  theme(panel.border = element_blank())

dev.off()  

#iris_fig3.png
png("./images/iris_fig3.png")

ggplot(iris) +
  geom_boxplot(aes(Species, (Petal.Width/Sepal.Width), fill= Species)) +
  labs(title = "Sepal- to Petal-Width Ratio",
       subtitle = "for three Iris species") +
  ylab("Ratio of Sepal Width to Petal Width") +
  theme_light() +
  theme(panel.border = element_blank())

dev.off() 

#iris_fig4.png
msl <- mean(iris$Sepal.Length)
data <- mutate(iris, deviation = iris$Sepal.Length - msl)
data <- arrange(data,deviation)
data$ID <- row.names(data)
data$ID <- factor(data$ID, levels = data$ID)

png("./images/iris_fig4.png")

ggplot(data, aes(ID, deviation, fill = Species)) +
  geom_bar(stat = "identity") +
    theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Sepal Length deviation from the mean of all observations",
       caption = "Note: Deviance = Sepal Length - Mean(Sepal Length)") +
  ylab("Deviance from the Mean") +
  coord_flip()

dev.off()
           