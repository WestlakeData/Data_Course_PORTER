# Library Calls ####
library(tidyverse)

#Load Data ####
df <- read.csv("./data/cdc_zika.csv")

#Remove unused Columns and Rows
df <- select(df, -starts_with("time")) %>% filter(unit == "cases") %>% select(-matches("unit"))
df <- na.omit(df,target.colnames = "value")
#Remove problematic regional data reports
df <- filter(df, df$data_field_code != "BR0011" & df$location_type != "region")


#Convert values to nummeric and rename column
df$value <- as.numeric(df$value)
colnames(df)[colnames(df)=="value"] <- "cases"

#convert date to POSIXct
df$report_date <- as.Date(df$report_date, format = "%Y-%m-%d")

#Seperate Location into Country/State/Municipality
df <- separate(df, location, c("Country","State","Municipality"), sep = "-", extra = "merge")
df$Country <- as.factor(df$Country)
df$State <- as.factor(df$State)
df$Municipality <- as.factor(df$Municipality)

#Seperate into Countries ####
#Country.Names <- levels(df$Country)
#for (i in 1:nlevels(df$Country)) {
#  assign(Country.Names[i], filter(df, df$Country == Country.Names[i]))
#  fn<- names(Filter(is.factor, Country.Names[i]))
#}

#Process Each Country to standardize data format
#Argentina
names(Filter(is.factor, Argentina))
Argentina$Country <- factor(Argentina$Country)
ARG <- droplevels.factor(Argentina)
Argentina %>% filter(data_field_code == "AR0003" | data_field_code == "AR0001") %>%
  ggplot(aes(x= report_date, y = cases, color = data_field)) +
  geom_line() +
  facet_wrap()


#Columbia
