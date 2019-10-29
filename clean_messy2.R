library(tidyverse)

a <- read.csv(file = "./Data/Messy_Take2/a_df.csv")
b <- read.csv(file = "./Data/Messy_Take2/b_df.csv")
c <- read.csv(file = "./Data/Messy_Take2/c_df.csv")
d <- read.csv(file = "./Data/Messy_Take2/d_df.csv")
e <- read.csv(file = "./Data/Messy_Take2/e_df.csv")
f <- read.csv(file = "./Data/Messy_Take2/f_df.csv")
g <- read.csv(file = "./Data/Messy_Take2/g_df.csv")
# Combine Files into single dataset
data <- rbind(a, b, c, d, e, f, g)
#Rename columns
colnam <- c("DOB_Male", "Days Alive_Male", "IQ_Male", "Pass_Male", "DOB_Female", "Days Alive_Female", "IQ_Female", "Pass_Female")
names(data) <- colnam

male <- data %>%
  select(ends_with("_Male"))

female <- data%>%
  select(ends_with("_Female"))

male <- male %>%mutate(Gender = "Male")
female <- female %>% mutate(Gender = "Female")

names(male) <- str_replace(names(male), "_Male", "")
names(female) <- str_replace(names(female), "_Female", "")

full <- rbind(male,female)

write.csv(full, "../Data_Course_PORTER/clean_data.csv", row.names = F, quote =F )

saveRDS(full, "../Data_Course_PORTER/clean_data.RDS")
