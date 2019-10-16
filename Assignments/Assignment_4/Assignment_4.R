#Assignment 4 BIOL 490R
#Load Data
df <- read.csv("./data/landdata-states.csv")

#Question 1
?read.csv

"1. What other stuff does read.csv() do automatically?"
"read.csv() takes a .csv file and parses it into a dataframe, it automatically uses a ',' as the default seperator and '.' as the decimal point."

#Question 2
?read.csv2

"2. How is it different from read.csv2()?"
"read.csv2() automatically uses ';' as the seperator, and ',' as the decimal point."

#Question 3
"3. Why does read.csv2() even exist?"
"read.csv2 exists for those crazy Europeans and their weird notation."

#Question 4
"4. How could I change the parameters of read.csv() to make it so the class of the 'State' column is 'character' instead of 'factor?'"
df <- read.csv("./data/landdata-states.csv", colClasses = c("State" = "character"))

#Question 5
"5. What command would give the summary stats for ONLY the Home.Value column?"
summary(df$Home.Value)

#Question 6
'*6. What value is returned by the command: names(df)[4] ?'
names(df)[4]
"Home.Value"

#Question 7
"7. What is happening when you add (...col=df\$region) to the above plotting code?  In other words, what happens when you run: plot(x=df\$Year,y=df\$Land.Value,col=df\$region)"

# histogram showing number of times each numeric value was seen in the vector "Land.Value"
hist(df$Land.Value)

# If you want to look at land value by region, you could do this:
plot(x=df$region,y=df$Land.Value)

# Land value by year
plot(x=df$Year,y=df$Land.Value)

plot(x=df$Year,y=df$Land.Value,col=df$region)

"This adds a third variable, 'Region' which is identified by color of the point on the plot"

#Assignment 4 Extra
library(tidyverse)

dat <- read.table("./data/ITS_mapping.csv", sep = "\t", header = T)
summary(dat)
head(dat)

#Plot Lat ~ Ecosystem
plot(dat$Ecosystem,dat$Lat, xlab= "Ecosystem", ylab="Lat")

png(filename = "./images/silly_boxplot.png")
plot(dat$Ecosystem,dat$Lat, xlab= "Ecosystem", ylab="Lat")
dev.off()
