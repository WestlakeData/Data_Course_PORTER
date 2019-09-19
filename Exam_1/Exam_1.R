dna <- read.csv("DNA_Conc_by_Extraction_Date.csv")
dna[,"Year_Collected"] <- factor(dna[,"Year_Collected"])
dna[, "Date_Collected"] <- as.Date(dna[, "Date_Collected"], format = %Y-%m-%d)
attach(dna)

#Create Histograms for Katy & Ben
hist(DNA_Concentration_Ben, xlab = "DNA Concentration", ylab = "Number of Samples", main = "Ben's Results")
hist(DNA_Concentration_Katy, xlab = "DNA Concentration", ylab = "Number of Samples", main = "Katy's Results")

#Create Boxplots for Katy & Ben by Year
ben.year <- dna[, col <- c("Year_Collected", "DNA_Concentration_Ben")] 
katy.year <- dna[, col <- c("Year_Collected", "DNA_Concentration_Katy")]
plot(ben.year, xlab = "YEAR", ylab = "DNA Concentration", main = "Ben's Extractions")
plot(katy.year, xlab = "YEAR", ylab = "DNA Concentration", main = "Katy's Extractions")

# Save Boxplots as Jpeg
jpeg("Ben_Boxplot.jpg")
plot(ben.year, xlab = "YEAR", ylab = "DNA Concentration", main = "Ben's Extractions")
dev.off()

jpeg("Katy_Boxplot.jpg")
plot(katy.year, xlab = "YEAR", ylab = "DNA Concentration", main = "Katy's Extractions")
dev.off()

#Compare Ben & Katy Extractions by Year
summary(DNA_Concentration_Ben)
summary(DNA_Concentration_Katy)

mean(dna[Year_Collected == "2001", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2001", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2002", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2002", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2003", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2003", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2004", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2004", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2005", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2005", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2006", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2006", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2007", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2007", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2008", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2008", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2009", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2009", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2010", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2010", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2011", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2011", "DNA_Concentration_Katy"])
mean(dna[Year_Collected == "2012", "DNA_Concentration_Ben"])/mean(dna[Year_Collected == "2012", "DNA_Concentration_Katy"])

#Downstairs
downstairs <- dna[Lab == "Downstairs",]
jpeg("Ben_DNA_over_time.jpg")
plot(as.POSIXct(Date_Collected), DNA_Concentration_Ben)
dev.off()

#Bonus
df <- mean(dna[Year_Collected == "2001", "DNA_Concentration_Ben"])

