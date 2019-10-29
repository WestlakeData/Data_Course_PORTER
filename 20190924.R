dat <- DNA_Conc_by_Extraction_Date
library(dplyr)
library(ggplot2)

D_Upstairs <- dat%>%
  filter(Lab == "Upstairs") %>%
  select(starts_with("D"))

?summarise


Lab <- dat %>%
  group_by(Lab) %>%
  summarise(Ben = mean(DNA_Concentration_Ben),
             Katy = mean(DNA_Concentration_Katy))

Avg.GPA <- GradAd %>%
  filter(admit == 1) %>%
  group_by(rank)%>%
  summarise(Average_GPA = mean(gpa))

GradAd %>%
  filter(admit == 1)%>%
  ggplot(aes(x= as.factor(rank), y= gpa)) +
  geom_boxplot()
