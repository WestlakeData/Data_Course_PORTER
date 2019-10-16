#Install Packages
install.packages("ggplot2")
install.packages("png")

#Library Call
library(ggplot2)
library(png)
library(grid)

#read in data
data(iris)

image <- readPNG("./Purple Dragon/purple_dragon.PNG")

#Create Plot
ggplot(data = iris, aes(x= Petal.Length, y= Sepal.Length, color = Species)) +
  annotation_custom(rasterGrob(image, width = unit(1,"npc"), height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  geom_point(size = 25)+
  geom_smooth()+
  theme(plot.title = element_text(size = 36),
        legend.background = element_rect(fill = 'purple'),
        legend.box.background = element_rect(fill = '#9934eb'),
        legend.key = element_rect(fill = '#9934eb'),
        plot.background = element_rect(fill = 'purple', colour = 'purple'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Chart",
       x = "Length",
       y = "Length") +
  guides(fill = guide_legend("purple")) +
  scale_color_manual(values=c("#7434eb", "#8c34eb", "#b134eb"), 
                    name="Factors",
                    breaks=c("setosa", "versicolor", "virginica"),
                    labels=c("1", "2", "3"))
 