#Library Calls
library(ggplot2)
library(lubridate)
library(tidyverse)

#Load in Data and modify classes
dna <- read.csv("DNA_Conc_by_Extraction_Date.csv")
dna$Year_Collected <- factor(dna$Year_Collected)
dna$Date_Collected <- as.POSIXct(dna$Date_Collected, tryFormats = "%Y-%m-%d")
colnames(dna)[4:5] <- c("Katy","Ben") 
dna_tidy <- gather(dna, key = "Lab_Tech", value = "DNA_Concentration", Katy, Ben)

#I. ####	
#Once you get the file loaded into an R object as a data frame, feel free to do some exploratory visualizations or summaries to get a feel for the data if you like.
#Your first task, though, is to create separate histograms of the DNA concentrations for Katy and Ben. Make sure to add nice labels to these (x-axis and main title).

#Create Histograms for Katy & Ben
hist(dna$Ben, xlab = "DNA Concentration", ylab = "Number of Samples", main = "Ben's Results")
hist(dna$Katy, xlab = "DNA Concentration", ylab = "Number of Samples", main = "Katy's Results")

#II. 	####
#Your second task is to look at DNA concentrations from the different extraction years. 
#One way to do this is a separate figure for each student is demonstrated in those two files:	ZAHN_Plot1.jpeg and ZAHN_Plot2.jpeg 
#Open those files in some image viewing program and take a look. I'd like you to re-create these exactly, including the labels.
#This is tricky, so I'll give a hint: the plot() function behaves differently depending on the classes of vectors that are given to it.

#Create Boxplots for Katy & Ben by Year
b <- dna_tidy %>% filter(Lab_Tech == "Ben")
plot(b$Year_Collected, b$DNA_Concentration, xlab = 'Year', ylab = "Concentration", main = "Ben's Extractions")
k <- dna_tidy %>% filter(Lab_Tech == "Katy")
plot(k$Year_Collected, k$DNA_Concentration, xlab = 'Year', ylab = "Concentration", main = "Katy's Extractions")

#III. ####
#Once you have your code for creating the figures correctly, you need to save those two images in YOUR Exam_1 directory. Name them similarly to how I named mine, but with your LASTNAME
#Make sure your code is saving the files. Don't do it manually with the mouse!

# Save Boxplots as Jpeg
jpeg("PORTER_Ben_Boxplot.jpg")
plot(b$Year_Collected, b$DNA_Concentration, xlab = 'Year', ylab = "Concentration", main = "Ben's Extractions")
dev.off()

jpeg("PORTER_Katy_Boxplot.jpg")
plot(k$Year_Collected, k$DNA_Concentration, xlab = 'Year', ylab = "Concentration", main = "Katy's Extractions")
dev.off()

#IV. ####
#Take a look at Ben's concentrations vs Katy's concentrations. You can do this however you like... with a plot or with summary stats or both.
#It looks like Ben had consistently higher DNA yields than Katy did...but surely it wasn't uniformly better, right? With some samples, he only had a marginal improvement over Katy.
#With other samples, he had a relatively massive improvement over her.
#Your task here is to write some code that tells us: in which extraction YEAR, was Ben's performance the lowest RELATIVE TO Katy's performance?
summary(dna$Ben)
summary(dna$Katy)

ggplot(dna_tidy, aes(Lab_Tech, DNA_Concentration))+
  geom_boxplot() +
  labs(title = "DNA Extraction Comparison",
       x = "Lab Tech",
       y= "DNA Concentration")

#Compare Ben & Katy Extractions by Year

df <- data.frame(Year_Collected=character(),DNA_Concentration_diff = numeric(), stringsAsFactors = F)
for(i in 1:12){
  df[i,2] <- mean(dna[dna$Year_Collected == levels(dna$Year_Collected)[i], "Ben"]) - mean(dna[dna$Year_Collected == levels(dna$Year_Collected)[i], "Katy"])
  df[i,1] <- levels(dna$Year_Collected)[i]
}

df$Year_Collected[df$DNA_Concentration_diff == min(df$DNA_Concentration_diff)]

#V. ####
#Do another subset of the data for me. Subset the data frame so it's just the "Downstairs" lab.
#Now, make a scatterplot of the downstairs lab data such that "Date_Collected" is on the x-axis and "DNA_Concentration_Ben" is on the y-axis. Save this scatterplot as "Ben_DNA_over_time.jpg" in your Exam_1 #directory. See the file "Downstairs.jpg" for an example of how yours should look. If it looks different, you might need to do some class conversions so the plot() function treats things correctly. HintHintHint: #POSIXct

downstairs <- dna[dna$Lab == "Downstairs",]
jpeg("Ben_DNA_over_time.jpg")
plot(downstairs$Date_Collected, downstairs$Ben)
dev.off()

#VI. ####
#For this final (BONUS) problem, let's just look at Ben's DNA concentration values. I think Katy messed up her PCRs, and at any rate, we can't use them for sequencing.
#Besides, our original purpose for this experiment was to see if DNA extractions sitting in a freezer degraded over time.
#To that end, I want you to make a new data frame (just using Ben's values) that has one column containing the years that DNA extractions were made, 
#and another column that contains the AVERAGE of the values within that year.  Just to be clear, this data frame should have only 12 rows (one for each year)! You will need to find a way to take the average of #Ben's DNA values in each separate year. A for-loop, or repeated subsetting, or some other way...
#Once you have this new data frame of averages by year, write some code that shows which extraction year has the highest average DNA concentration (and what that concentration is) and then save the 12-row #dataframe as a new csv file called "Ben_Average_Conc.csv"
df2 <- data.frame(Average_DNA_Concentration = numeric())
for(i in 1:12){
  df2[i,] <- mean(b$DNA_Concentration[b$Year_Collected == levels(b$Year_Collected)[i]])
}

write.csv(df2, file = "Ben_Average_Conc.csv", row.names = F)
