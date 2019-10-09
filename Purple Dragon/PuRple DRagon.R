#Library Call
library(ggplot2)

#read in data
data(iris)

#Create Plot
ggplot(data = iris, aes(x= Petal.Length, y= Sepal.Length, color = Species)) +
  geom_point(size = 25)+
  geom_smooth()+
  theme(plot.title = element_text(size = 36),
        legend.background = element_rect(fill = 'purple'),
        legend.box.background = element_rect(fill = '#9934eb'),
        legend.key = element_rect(fill = '#9934eb'),
        plot.background = element_rect(fill = 'purple', colour = 'purple'),
        panel.background = element_rect(fill = '#d634eb'),
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
