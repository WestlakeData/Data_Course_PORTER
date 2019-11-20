# Library Calls ####
library(tidyverse); library(plotly); library(tidyr)

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
Country.Names <- levels(df$Country)
for (i in 1:nlevels(df$Country)) {
  assign(Country.Names[i], filter(df, df$Country == Country.Names[i]))
  fn<- names(Filter(is.factor, Country.Names[i]))
}

#Process Each Country to standardize data format
#Argentina
names(Filter(is.factor, Argentina))
Argentina$Country <- factor(Argentina$Country)
ARG <- droplevels.factor(Argentina)
p1 <- Argentina %>% filter(data_field_code == "AR0003" | data_field_code == "AR0001") %>%
  ggplot(aes(x= report_date, y = cases, color = State)) +
  geom_point() +
  geom_smooth(se=F) +
  labs(title = "Argentina")

ggplotly(p1)

#Columbia
COL.colnames <- c("Country", "State", "Municipality")
COL.muni <- Colombia %>% select(COL.colnames) %>% unique()
COL.muni <- as.data.frame(paste(COL.muni$Municipality, COL.muni$State, COL.muni$Country, sep = ", "), stringsAsFactors = F)
colnames(COL.muni) <- "City"

#geocode Colombian Cities
register_google(key = readLines("../../google_api.txt"))
for(i in 1:nrow(COL.muni)){
  result <- tryCatch(geocode(COL.muni$City[i], output = "latlona", source = "google"),
                     warning = function(w) data.frame(lon = NA, lat = NA, geoAddress = NA))
  COL.muni$lon[i] <- as.numeric(result[1])
  COL.muni$lat[i] <- as.numeric(result[2])
  COL.muni$geoAddress[i] <- as.character(result[3])
} 
saveRDS(COL.muni, "./data/ColombiaCitiesGeocoded.RDS")

#Seperate City Names
COL.muni <- COL.muni %>% separate(City, c("City", "State", "Country"), sep = ", ")

#Join Disease data with geocoding
COL.muni.cases <- left_join(Colombia, COL.muni, by = c("Country" = "Country", "State" = "State", "Municipality" = "City"))
saveRDS(COL.muni.cases, "./data/ColombiaZikaGeoCases.RDS")

confirmed.COL <-c("CO0001", "CO0002")
COL <- Colombia %>% filter(data_field_code == confirmed.COL) #%>% group_by(report_date, State) %>% summarise(total_cases = sum(cases))
p2 <- ggplot(COL, aes(x= report_date, y = total_cases, color = State)) +
  geom_point() +
  geom_smooth(se=F) +
  labs(title = "Colombia")

ggplotly(p2)

#Dominican Republic
DR.codes <- distinct(Dominican_Republic, Dominican_Republic$data_field, Dominican_Republic$data_field_code )
DR <- Dominican_Republic %>% filter(location_type != "country") %>% filter(data_field_code == "DO0002") %>% group_by(report_date, State) %>% summarise(total_cases = sum(cases))
p3 <- ggplot(DR, aes(x= report_date, y = total_cases, color = State)) +
  geom_point() +
  geom_smooth(se=F) +
  labs(title = "Dominican Republic")

ggplotly(p3)
