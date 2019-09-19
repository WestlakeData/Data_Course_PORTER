dna <- read.csv("DNA_Conc_by_Extraction_Date.csv")
attach(dna)

#Create Histograms for Katy & Ben
hist(DNA_Concentration_Ben, xlab = "DNA Concentration", ylab = "Number of Samples", main = "Ben's Results")
hist(DNA_Concentration_Katy, xlab = "DNA Concentration", ylab = "Number of Samples", main = "Katy's Results")

#Create Boxplots for Katy & Ben by Year
year <- as.factor(dna$Year_Collected)
dna <- as.data.frame()
ben.year <- dna[, col <- c("Year_Collected", "DNA_Concentration_Ben")] 
katy.year <- dna[, col <- c("Year_Collected", "DNA_Concentration_Katy")]
boxplot(ben.year, x= ben.year$Year_Collected, y= ben.year$DNA_Concentration_Ben, xlab = "YEAR", ylab = "DNA Concentration", main = "Ben's Extractions")
