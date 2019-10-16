#Library Calls
library(dplyr)

#loads the mtcars data set
data("mtcars")

#subsets the mtcars dataframe to include only **automatic transmissions**

data <- mtcars %>%
  filter(am == 0)

#saves this new subset as a new file called "automatic_mtcars.csv" in your Assignment_5 directory

write.csv(data, "./automatic_mtcars.csv")

#plots the effect of horsepower on miles-per-gallon (update plot to have meaningful labels and title)

plot(data$hp, data$mpg, xlab = "Horsepower", ylab= "MPG", main = "MPG vs Horsepwer")

#saves this plot as a png image called "mpg_vs_hp_auto.png" in your Assignment_5 directory

png("mpg_vs_hp_auto.png")

plot(data$hp, data$mpg, xlab = "Horsepower", ylab= "MPG", main = "MPG vs Horsepwer")

dev.off()

#plots the effect of weight on miles-per-gallon (with improved labels, again)

plot(data$wt, data$mpg, main = "MPG vs Weight", xlab = "Weight", ylab = "MPG")

#saves this second plot as a **tiff** image called "mpg_vs_wt_auto.tiff" in your Assignment_5 directory

tiff("mpg_vs_wt_auto.tiff")

plot(data$wt, data$mpg, main = "MPG vs Weight", xlab = "Weight", ylab = "MPG")

dev.off()

#subsets the original mtcars dataframe to include only cars with displacements less than or equal to 200 cu.in.

data2 <- mtcars %>%
  filter(disp <= 200)

#saves that new subset as a csv file called mtcars_max200_displ.csv

write.csv(data, "mtcars_max200_dislp.csv")

#includes code to calculate the maximum horsepower for each of the three dataframes (original, automatic, max200)

max.o <- max(mtcars$hp)
max.a <- max(data$hp)
max.d <- max(data2$hp)

#prints these calculations (from task 10) in a readable format to a new plaintext file called hp_maximums.txt
filename <- "hp_maximums.txt"
cat(max.o,file= filename, sep = "\n")
cat(max.a,file= filename, sep = "\n", append = T)
cat(max.d,file= filename,append=TRUE)
