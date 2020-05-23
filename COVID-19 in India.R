###################################################################################################################
#CLEARING THE ENVIRONMENT##########################################################################################
###################################################################################################################

rm(list = ls())

###################################################################################################################
#IMPORTING THE DATA################################################################################################
###################################################################################################################

library(lubridate)
covid_world <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19\\covid_19_data.csv")
head(covid_world)
str(covid_world)
covid_world$ObservationDate <- parse_date_time(covid_world$ObservationDate, c("%m/%d/%Y", "%m-%d-%Y"))
covid_world$ObservationDate <- as.Date(covid_world$ObservationDate)

###################################################################################################################
#SUBSETTING TOP 10 COUNTRIES WITH HIGHEST NUMBER OF CONFIRMED CASES################################################
###################################################################################################################

library(ggplot2)
library(reshape2)
library(dplyr)
compare_confirmed <- aggregate(covid_world["Confirmed"], by = covid_world[c("Country.Region", "ObservationDate")], FUN = "sum")
latest_date <- max(covid_world$ObservationDate)
compare_confirmed <- filter(compare_confirmed, ObservationDate == latest_date)
confirmed_world <- compare_confirmed[, c(1,3)]
compare_confirmed <- arrange(compare_confirmed, -compare_confirmed$Confirmed)
for_testing <- compare_confirmed
top_10 <- compare_confirmed[c(1:10),]
top_10$ObservationDate <- NULL
print(top_10)
options(scipen = 999)
ggplot(top_10, aes(x = Country.Region, y = Confirmed)) + geom_bar(stat = "identity") + ggtitle("Plotting Countries with highest Confirmed Cases") + xlab("Country") + ylab("No. of Confirmed Cases")

###################################################################################################################
#PLOTTING CONFIRMED NUMBERS OF TOP 10 COUNTRIES AND INDIA AGAINST TIME#############################################
###################################################################################################################

top_10_data <- subset(covid_world, Country.Region %in% top_10$Country.Region)
top_10_data <- aggregate(top_10_data["Confirmed"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data <- subset(covid_world, Country.Region == "India")
india_data <- aggregate(india_data["Confirmed"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
compare_confirmed_den <- rbind(top_10_data, india_data)
compare_confirmed_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
ggplot(compare_confirmed_den, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 1) + theme_bw() +
  ggtitle("Plotting Countries with highest Confirmed Cases and India") + xlab("Observed Dates") + ylab("No. of Confirmed Cases") + 
  labs(color = "Country")

###################################################################################################################
#COMPARING LAST 5 DAYS CONFIRMED NUMBERS OF EACH TOP 6 COUNTRY VS INDIA############################################
###################################################################################################################

covid_world_latest <- subset(covid_world, ObservationDate >= latest_date - 5)
top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[1])
top_10_data <- aggregate(top_10_data["Confirmed"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Confirmed <- top_10_data$Confirmed - top_10_data$Confirmed[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Confirmed"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Confirmed <- india_data$Confirmed - india_data$Confirmed[1]
india_data_den <- india_data
compare_confirmed_den <- rbind(top_10_data_den, india_data_den)
compare_confirmed_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed_den <- filter(compare_confirmed_den, ObservationDate > latest_date - 5)
ggplot(compare_confirmed_den, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Confirmed[i] <- top_10_data$Confirmed[i] - j
  j <- j + top_10_data$Confirmed[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Confirmed[i] <- india_data$Confirmed[i] - j
  j <- j + india_data$Confirmed[i] 
}
compare_confirmed <- rbind(top_10_data, india_data)
compare_confirmed %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed <- filter(compare_confirmed, ObservationDate > latest_date - 5)
ggplot(compare_confirmed, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Cases in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[2])
top_10_data <- aggregate(top_10_data["Confirmed"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Confirmed <- top_10_data$Confirmed - top_10_data$Confirmed[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Confirmed"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Confirmed <- india_data$Confirmed - india_data$Confirmed[1]
india_data_den <- india_data
compare_confirmed_den <- rbind(top_10_data_den, india_data_den)
compare_confirmed_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed_den <- filter(compare_confirmed_den, ObservationDate > latest_date - 5)
ggplot(compare_confirmed_den, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Confirmed[i] <- top_10_data$Confirmed[i] - j
  j <- j + top_10_data$Confirmed[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Confirmed[i] <- india_data$Confirmed[i] - j
  j <- j + india_data$Confirmed[i] 
}
compare_confirmed <- rbind(top_10_data, india_data)
compare_confirmed %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed <- filter(compare_confirmed, ObservationDate > latest_date - 5)
ggplot(compare_confirmed, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Cases in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[3])
top_10_data <- aggregate(top_10_data["Confirmed"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Confirmed <- top_10_data$Confirmed - top_10_data$Confirmed[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Confirmed"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Confirmed <- india_data$Confirmed - india_data$Confirmed[1]
india_data_den <- india_data
compare_confirmed_den <- rbind(top_10_data_den, india_data_den)
compare_confirmed_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed_den <- filter(compare_confirmed_den, ObservationDate > latest_date - 5)
ggplot(compare_confirmed_den, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Confirmed[i] <- top_10_data$Confirmed[i] - j
  j <- j + top_10_data$Confirmed[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Confirmed[i] <- india_data$Confirmed[i] - j
  j <- j + india_data$Confirmed[i] 
}
compare_confirmed <- rbind(top_10_data, india_data)
compare_confirmed %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed <- filter(compare_confirmed, ObservationDate > latest_date - 5)
ggplot(compare_confirmed, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Cases in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[4])
top_10_data <- aggregate(top_10_data["Confirmed"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Confirmed <- top_10_data$Confirmed - top_10_data$Confirmed[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Confirmed"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Confirmed <- india_data$Confirmed - india_data$Confirmed[1]
india_data_den <- india_data
compare_confirmed_den <- rbind(top_10_data_den, india_data_den)
compare_confirmed_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed_den <- filter(compare_confirmed_den, ObservationDate > latest_date - 5)
ggplot(compare_confirmed_den, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Confirmed[i] <- top_10_data$Confirmed[i] - j
  j <- j + top_10_data$Confirmed[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Confirmed[i] <- india_data$Confirmed[i] - j
  j <- j + india_data$Confirmed[i] 
}
compare_confirmed <- rbind(top_10_data, india_data)
compare_confirmed %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed <- filter(compare_confirmed, ObservationDate > latest_date - 5)
ggplot(compare_confirmed, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Cases in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[5])
top_10_data <- aggregate(top_10_data["Confirmed"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Confirmed <- top_10_data$Confirmed - top_10_data$Confirmed[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Confirmed"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Confirmed <- india_data$Confirmed - india_data$Confirmed[1]
india_data_den <- india_data
compare_confirmed_den <- rbind(top_10_data_den, india_data_den)
compare_confirmed_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed_den <- filter(compare_confirmed_den, ObservationDate > latest_date - 5)
ggplot(compare_confirmed_den, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Confirmed[i] <- top_10_data$Confirmed[i] - j
  j <- j + top_10_data$Confirmed[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Confirmed[i] <- india_data$Confirmed[i] - j
  j <- j + india_data$Confirmed[i] 
}
compare_confirmed <- rbind(top_10_data, india_data)
compare_confirmed %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed <- filter(compare_confirmed, ObservationDate > latest_date - 5)
ggplot(compare_confirmed, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Cases in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[6])
top_10_data <- aggregate(top_10_data["Confirmed"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Confirmed <- top_10_data$Confirmed - top_10_data$Confirmed[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Confirmed"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Confirmed <- india_data$Confirmed - india_data$Confirmed[1]
india_data_den <- india_data
compare_confirmed_den <- rbind(top_10_data_den, india_data_den)
compare_confirmed_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed_den <- filter(compare_confirmed_den, ObservationDate > latest_date - 5)
ggplot(compare_confirmed_den, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Confirmed[i] <- top_10_data$Confirmed[i] - j
  j <- j + top_10_data$Confirmed[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Confirmed[i] <- india_data$Confirmed[i] - j
  j <- j + india_data$Confirmed[i] 
}
compare_confirmed <- rbind(top_10_data, india_data)
compare_confirmed %>% 
  subset(select = c("ObservationDate", "Country.Region", "Confirmed")) %>%
  arrange(by = ObservationDate)
compare_confirmed <- filter(compare_confirmed, ObservationDate > latest_date - 5)
ggplot(compare_confirmed, aes(x = ObservationDate, y = Confirmed)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Cases in Country with high Confirmed cases and India") + xlab("Last five observed dates") + ylab("No. of Confirmed Cases per day") + 
  labs(color = "Country")

###################################################################################################################
#SUBSETTING TOP 10 COUNTRIES WITH HIGHEST NUMBER OF DEATH CASES + INDIA############################################
###################################################################################################################

compare_deaths <- aggregate(covid_world["Deaths"], by = covid_world[c("Country.Region", "ObservationDate")], FUN = "sum")
compare_deaths <- filter(compare_deaths, ObservationDate == latest_date)
death_world <- compare_deaths[, c(1,3)]
compare_deaths <- arrange(compare_deaths, -compare_deaths$Deaths)
top_10 <- compare_deaths[c(1:10),]
top_10$ObservationDate <- NULL
print(top_10)
ggplot(top_10, aes(x = Country.Region, y = Deaths)) + geom_bar(stat = "identity") +
  ggtitle("Plotting Countries with highest no. of Deaths") + xlab("Country") + ylab("No. of Deaths")

###################################################################################################################
#COMPARING DEATH RATE OF THESE COUNTRIES AND INDIA#################################################################
###################################################################################################################

world_rates <- merge(x = confirmed_world, y = death_world, by.x = "Country.Region", by.y = "Country.Region")
world_rates$Death_Rate <- (world_rates$Deaths / world_rates$Confirmed) * 100
compare_rates_top10 <- filter(world_rates, world_rates$Country.Region %in% top_10$Country.Region)
rate_india <- filter(world_rates, world_rates$Country.Region == "India")
compare_rates <- rbind(compare_rates_top10, rate_india)
ggplot(compare_rates, aes(x = Country.Region, y = Death_Rate)) + geom_bar(stat = "identity") + 
  ggtitle("Plotting Death Rates of Countries with highest no. of Deaths and India") + xlab("Country") + ylab("Death Rate")

###################################################################################################################
#PLOTTING DEATH NUMBERS OF TOP 10 COUNTRIES AND INDIA AGAINST TIME#################################################
###################################################################################################################

top_10_data <- subset(covid_world, Country.Region %in% top_10$Country.Region)
top_10_data <- aggregate(top_10_data["Deaths"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data <- subset(covid_world, Country.Region == "India")
india_data <- aggregate(india_data["Deaths"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
compare_deaths_den <- rbind(top_10_data, india_data)
compare_deaths_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
ggplot(compare_deaths_den, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 1) + theme_bw() + 
  ggtitle("Plotting Countries with highest Deaths and India") + xlab("Observed Dates") + ylab("No. of Deaths") + 
  labs(color = "Country")

###################################################################################################################
#COMPARING LAST 5 DAYS DEATH NUMBERS OF EACH TOP 6 COUNTRY VS INDIA################################################
###################################################################################################################

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[1])
top_10_data <- aggregate(top_10_data["Deaths"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Deaths <- top_10_data$Deaths - top_10_data$Deaths[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Deaths"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Deaths <- india_data$Deaths - india_data$Deaths[1]
india_data_den <- india_data
compare_deaths_den <- rbind(top_10_data_den, india_data_den)
compare_deaths_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths_den <- filter(compare_deaths_den, ObservationDate > latest_date - 5)
ggplot(compare_deaths_den, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Deaths[i] <- top_10_data$Deaths[i] - j
  j <- j + top_10_data$Deaths[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Deaths[i] <- india_data$Deaths[i] - j
  j <- j + india_data$Deaths[i] 
}
compare_deaths <- rbind(top_10_data, india_data)
compare_deaths %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths <- filter(compare_deaths, ObservationDate > latest_date - 5)
ggplot(compare_deaths, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Deaths in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[2])
top_10_data <- aggregate(top_10_data["Deaths"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Deaths <- top_10_data$Deaths - top_10_data$Deaths[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Deaths"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Deaths <- india_data$Deaths - india_data$Deaths[1]
india_data_den <- india_data
compare_deaths_den <- rbind(top_10_data_den, india_data_den)
compare_deaths_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths_den <- filter(compare_deaths_den, ObservationDate > latest_date - 5)
ggplot(compare_deaths_den, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Deaths[i] <- top_10_data$Deaths[i] - j
  j <- j + top_10_data$Deaths[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Deaths[i] <- india_data$Deaths[i] - j
  j <- j + india_data$Deaths[i] 
}
compare_deaths <- rbind(top_10_data, india_data)
compare_deaths %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths <- filter(compare_deaths, ObservationDate > latest_date - 5)
ggplot(compare_deaths, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Deaths in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[3])
top_10_data <- aggregate(top_10_data["Deaths"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Deaths <- top_10_data$Deaths - top_10_data$Deaths[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Deaths"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Deaths <- india_data$Deaths - india_data$Deaths[1]
india_data_den <- india_data
compare_deaths_den <- rbind(top_10_data_den, india_data_den)
compare_deaths_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths_den <- filter(compare_deaths_den, ObservationDate > latest_date - 5)
ggplot(compare_deaths_den, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Deaths[i] <- top_10_data$Deaths[i] - j
  j <- j + top_10_data$Deaths[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Deaths[i] <- india_data$Deaths[i] - j
  j <- j + india_data$Deaths[i] 
}
compare_deaths <- rbind(top_10_data, india_data)
compare_deaths %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths <- filter(compare_deaths, ObservationDate > latest_date - 5)
ggplot(compare_deaths, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Deaths in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[4])
top_10_data <- aggregate(top_10_data["Deaths"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Deaths <- top_10_data$Deaths - top_10_data$Deaths[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Deaths"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Deaths <- india_data$Deaths - india_data$Deaths[1]
india_data_den <- india_data
compare_deaths_den <- rbind(top_10_data_den, india_data_den)
compare_deaths_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths_den <- filter(compare_deaths_den, ObservationDate > latest_date - 5)
ggplot(compare_deaths_den, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Deaths[i] <- top_10_data$Deaths[i] - j
  j <- j + top_10_data$Deaths[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Deaths[i] <- india_data$Deaths[i] - j
  j <- j + india_data$Deaths[i] 
}
compare_deaths <- rbind(top_10_data, india_data)
compare_deaths %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths <- filter(compare_deaths, ObservationDate > latest_date - 5)
ggplot(compare_deaths, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Deaths in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[5])
top_10_data <- aggregate(top_10_data["Deaths"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Deaths <- top_10_data$Deaths - top_10_data$Deaths[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Deaths"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Deaths <- india_data$Deaths - india_data$Deaths[1]
india_data_den <- india_data
compare_deaths_den <- rbind(top_10_data_den, india_data_den)
compare_deaths_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths_den <- filter(compare_deaths_den, ObservationDate > latest_date - 5)
ggplot(compare_deaths_den, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Deaths[i] <- top_10_data$Deaths[i] - j
  j <- j + top_10_data$Deaths[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Deaths[i] <- india_data$Deaths[i] - j
  j <- j + india_data$Deaths[i] 
}
compare_deaths <- rbind(top_10_data, india_data)
compare_deaths %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths <- filter(compare_deaths, ObservationDate > latest_date - 5)
ggplot(compare_deaths, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Deaths in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[6])
top_10_data <- aggregate(top_10_data["Deaths"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Deaths <- top_10_data$Deaths - top_10_data$Deaths[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Deaths"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Deaths <- india_data$Deaths - india_data$Deaths[1]
india_data_den <- india_data
compare_deaths_den <- rbind(top_10_data_den, india_data_den)
compare_deaths_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths_den <- filter(compare_deaths_den, ObservationDate > latest_date - 5)
ggplot(compare_deaths_den, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Deaths[i] <- top_10_data$Deaths[i] - j
  j <- j + top_10_data$Deaths[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Deaths[i] <- india_data$Deaths[i] - j
  j <- j + india_data$Deaths[i] 
}
compare_deaths <- rbind(top_10_data, india_data)
compare_deaths %>% 
  subset(select = c("ObservationDate", "Country.Region", "Deaths")) %>%
  arrange(by = ObservationDate)
compare_deaths <- filter(compare_deaths, ObservationDate > latest_date - 5)
ggplot(compare_deaths, aes(x = ObservationDate, y = Deaths)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Deaths in Country with high Deaths and India") + xlab("Last five observed dates") + ylab("No. of Deaths per day") + 
  labs(color = "Country")

###################################################################################################################
#SUBSETTING TOP 10 COUNTRIES WITH HIGHEST NUMBER OF RECOVERED CASES################################################
###################################################################################################################

compare_cured <- aggregate(covid_world["Recovered"], by = covid_world[c("Country.Region", "ObservationDate")], FUN = "sum")
compare_cured <- filter(compare_cured, ObservationDate == latest_date)
cured_world <- compare_cured[, c(1,3)]
compare_cured <- arrange(compare_cured, -compare_cured$Recovered)
top_10 <- compare_cured[c(1:10),]
top_10$ObservationDate <- NULL
print(top_10)
ggplot(top_10, aes(x = Country.Region, y = Recovered)) + geom_bar(stat = "identity") + 
  ggtitle("Plotting Countries with highest no. of Recovered Cases") + xlab("Country") + ylab("No. of Recovered Cases")

###################################################################################################################
#COMPARING RECOVERY RATES OF THESE COUNTRIES AND INDIA#############################################################
###################################################################################################################

world_rates <- merge(x = world_rates, y = cured_world, by.x = "Country.Region", by.y = "Country.Region")
world_rates$Recovery_Rate <- (world_rates$Recovered / world_rates$Confirmed) * 100
compare_rates_top10 <- filter(world_rates, world_rates$Country.Region %in% top_10$Country.Region)
rate_india <- filter(world_rates, world_rates$Country.Region == "India")
compare_rates <- rbind(compare_rates_top10, rate_india)
ggplot(compare_rates, aes(x = Country.Region, y = Recovery_Rate)) + geom_bar(stat = "identity") + 
  ggtitle("Plotting Recovery Rates of Countries with highest no. of Recovered Cases and India") + xlab("Country") + ylab("Recovery Rate")

###################################################################################################################
#PLOTTING RECOVERY NUMBERS OF TOP 10 COUNTRIES AND INDIA AGAINST TIME##############################################
###################################################################################################################

top_10_data <- subset(covid_world, Country.Region %in% top_10$Country.Region)
top_10_data <- aggregate(top_10_data["Recovered"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data <- subset(covid_world, Country.Region == "India")
india_data <- aggregate(india_data["Recovered"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
compare_cured_den <- rbind(top_10_data, india_data)
compare_cured_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
ggplot(compare_cured_den, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 1) + theme_bw() +
  ggtitle("Plotting Countries with highest no. of Recovered Cases and India") + xlab("Observed Dates") + ylab("No. of Recovered Cases") + 
  labs(color = "Country")

###################################################################################################################
#COMPARING LAST 5 DAYS RECOVERY NUMBERS OF EACH TOP 6 COUNTRY VS INDIA#############################################
###################################################################################################################

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[1])
top_10_data <- aggregate(top_10_data["Recovered"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Recovered <- top_10_data$Recovered - top_10_data$Recovered[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Recovered"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Recovered <- india_data$Recovered - india_data$Recovered[1]
india_data_den <- india_data
compare_cured_den <- rbind(top_10_data_den, india_data_den)
compare_cured_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured_den <- filter(compare_cured_den, ObservationDate > latest_date - 5)
ggplot(compare_cured_den, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Recovered[i] <- top_10_data$Recovered[i] - j
  j <- j + top_10_data$Recovered[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Recovered[i] <- india_data$Recovered[i] - j
  j <- j + india_data$Recovered[i] 
}
compare_cured <- rbind(top_10_data, india_data)
compare_cured %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured <- filter(compare_cured, ObservationDate > latest_date - 5)
ggplot(compare_cured, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Recoveries in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[2])
top_10_data <- aggregate(top_10_data["Recovered"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Recovered <- top_10_data$Recovered - top_10_data$Recovered[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Recovered"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Recovered <- india_data$Recovered - india_data$Recovered[1]
india_data_den <- india_data
compare_cured_den <- rbind(top_10_data_den, india_data_den)
compare_cured_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured_den <- filter(compare_cured_den, ObservationDate > latest_date - 5)
ggplot(compare_cured_den, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Recovered[i] <- top_10_data$Recovered[i] - j
  j <- j + top_10_data$Recovered[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Recovered[i] <- india_data$Recovered[i] - j
  j <- j + india_data$Recovered[i] 
}
compare_cured <- rbind(top_10_data, india_data)
compare_cured %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured <- filter(compare_cured, ObservationDate > latest_date - 5)
ggplot(compare_cured, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Recoveries in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[3])
top_10_data <- aggregate(top_10_data["Recovered"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Recovered <- top_10_data$Recovered - top_10_data$Recovered[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Recovered"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Recovered <- india_data$Recovered - india_data$Recovered[1]
india_data_den <- india_data
compare_cured_den <- rbind(top_10_data_den, india_data_den)
compare_cured_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured_den <- filter(compare_cured_den, ObservationDate > latest_date - 5)
ggplot(compare_cured_den, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Recovered[i] <- top_10_data$Recovered[i] - j
  j <- j + top_10_data$Recovered[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Recovered[i] <- india_data$Recovered[i] - j
  j <- j + india_data$Recovered[i] 
}
compare_cured <- rbind(top_10_data, india_data)
compare_cured %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured <- filter(compare_cured, ObservationDate > latest_date - 5)
ggplot(compare_cured, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Recoveries in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[4])
top_10_data <- aggregate(top_10_data["Recovered"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Recovered <- top_10_data$Recovered - top_10_data$Recovered[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Recovered"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Recovered <- india_data$Recovered - india_data$Recovered[1]
india_data_den <- india_data
compare_cured_den <- rbind(top_10_data_den, india_data_den)
compare_cured_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured_den <- filter(compare_cured_den, ObservationDate > latest_date - 5)
ggplot(compare_cured_den, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Recovered[i] <- top_10_data$Recovered[i] - j
  j <- j + top_10_data$Recovered[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Recovered[i] <- india_data$Recovered[i] - j
  j <- j + india_data$Recovered[i] 
}
compare_cured <- rbind(top_10_data, india_data)
compare_cured %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured <- filter(compare_cured, ObservationDate > latest_date - 5)
ggplot(compare_cured, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Recoveries in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[5])
top_10_data <- aggregate(top_10_data["Recovered"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Recovered <- top_10_data$Recovered - top_10_data$Recovered[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Recovered"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Recovered <- india_data$Recovered - india_data$Recovered[1]
india_data_den <- india_data
compare_cured_den <- rbind(top_10_data_den, india_data_den)
compare_cured_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured_den <- filter(compare_cured_den, ObservationDate > latest_date - 5)
ggplot(compare_cured_den, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Recovered[i] <- top_10_data$Recovered[i] - j
  j <- j + top_10_data$Recovered[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Recovered[i] <- india_data$Recovered[i] - j
  j <- j + india_data$Recovered[i] 
}
compare_cured <- rbind(top_10_data, india_data)
compare_cured %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured <- filter(compare_cured, ObservationDate > latest_date - 5)
ggplot(compare_cured, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Recoveries in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases per day") + 
  labs(color = "Country")

top_10_data <- subset(covid_world_latest, Country.Region == top_10$Country.Region[6])
top_10_data <- aggregate(top_10_data["Recovered"], by = top_10_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
top_10_data$Recovered <- top_10_data$Recovered - top_10_data$Recovered[1]
top_10_data_den <- top_10_data
india_data <- subset(covid_world_latest, Country.Region == "India")
india_data <- aggregate(india_data["Recovered"], by = india_data[c("Country.Region", "ObservationDate")], FUN = "sum" )
india_data$Recovered <- india_data$Recovered - india_data$Recovered[1]
india_data_den <- india_data
compare_cured_den <- rbind(top_10_data_den, india_data_den)
compare_cured_den %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured_den <- filter(compare_cured_den, ObservationDate > latest_date - 5)
ggplot(compare_cured_den, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing the trend in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases") + 
  labs(color = "Country")
j <- 0
for (i in 1:6) {
  top_10_data$Recovered[i] <- top_10_data$Recovered[i] - j
  j <- j + top_10_data$Recovered[i] 
}
j <- 0
for (i in 1:6) {
  india_data$Recovered[i] <- india_data$Recovered[i] - j
  j <- j + india_data$Recovered[i] 
}
compare_cured <- rbind(top_10_data, india_data)
compare_cured %>% 
  subset(select = c("ObservationDate", "Country.Region", "Recovered")) %>%
  arrange(by = ObservationDate)
compare_cured <- filter(compare_cured, ObservationDate > latest_date - 5)
ggplot(compare_cured, aes(x = ObservationDate, y = Recovered)) + geom_line(aes(color = Country.Region), size = 0.7) + 
  ggtitle("Comparing per-day Recoveries in Country with high no. of Recovered cases and India") + xlab("Last five observed dates") + ylab("No. of Recovered Cases per day") + 
  labs(color = "Country")

###################################################################################################################
#IMPORTING THE DATA################################################################################################
###################################################################################################################

covid_india <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India New\\nation_level_daily.csv")
head(covid_india)
str(covid_india)
covid_india <- covid_india[, c(1:4)]
names(covid_india) <- c('Date', 'Confirmed', 'Death', 'Recovered')
covid_india$Date <- as.Date(covid_india$Date, "%d %b")
covid_india$Date <- factor(covid_india$Date)

###################################################################################################################
#PLOTTING INDIA'S COVID-19 STATS AGAINST TIME######################################################################
###################################################################################################################

covid <- as.data.frame(melt(covid_india))
covid$Date <- as.Date(covid$Date)
ggplot(covid, aes(x = Date, y = value)) + geom_line(aes(color = variable), size = 0.7) + xlab("Observed Days") + ylab("No of Cases") +
  ggtitle("Plotting India's COVID-19 stats against time") + labs(color = "")

###################################################################################################################
#PLOTTING INDIA'S STATEWISE COVID-19 STATS#########################################################################
###################################################################################################################

india_state_stats <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India New\\state_level_latest.csv")  
india_state_stats <- india_state_stats[-1, c(1, 4:7)]
names(india_state_stats) <- c('State', 'Confirmed', 'Active', 'Death', 'Recovered')
covid <- as.data.frame(melt(india_state_stats))  
ggplot(covid, aes(x = State, y = value)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("No. of Cases") + ggtitle("Plotting State-wise COVID-19 stats") + labs(fill = "")
state_stat <- arrange(india_state_stats, desc(india_state_stats$Confirmed))[c(1:10),]
state_data <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India New\\complete.csv")
state_data$Date <- as.Date(state_data$Date, "%Y-%m-%d")
state_data <- state_data[, c(1, 2, 8, 9)]
names(state_data) <- c('Date', 'State', 'Death', 'Confirmed')
state_data <- subset(state_data, State %in% state_stat$State)
state_data <- aggregate(state_data["Confirmed"], by = state_data[c("State", "Date")], FUN = "sum" )
state_data %>% 
  subset(select = c("Date", "State", "Confirmed")) %>%
  arrange(by = Date)
ggplot(state_data, aes(x = Date, y = Confirmed)) + geom_line(aes(color = State), size = 1) + theme_bw() +
  ggtitle("Plotting States with Highest no. of Confirmed Cases") + xlab("Observed Dates") + ylab("No. of Confirmed Cases") + 
  labs(color = "State")

###################################################################################################################
#PLOTTING DEATH RATE AND RECOVERY RATE OF EACH STATE###############################################################
###################################################################################################################

india_state_stats$Death_Rate <- (india_state_stats$Death / india_state_stats$Confirmed) * 100
india_state_stats$Recovery_Rate <- (india_state_stats$Recovered / india_state_stats$Confirmed) * 100
ggplot(india_state_stats, aes(x = State, y = Death_Rate)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("Death Rate") + ggtitle("Comparing the Death Rates of different States")
ggplot(india_state_stats, aes(x = State, y = Recovery_Rate)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("Recovery Rate") + ggtitle("Comparing the Recovery Rates of different States")

###################################################################################################################
#COMPARING GLOBAL DEATH AND RECOVERY RATES WITH INDIA##############################################################
###################################################################################################################

global_rate <- filter(world_rates, Country.Region == "India")
global_rate$Country.Region <- as.character(global_rate$Country.Region)
global_rate <- rbind(global_rate, c("World", sum(world_rates$Confirmed), sum(world_rates$Deaths), (sum(world_rates$Deaths)/ sum(world_rates$Confirmed))* 100,
                                    sum(world_rates$Recovered), (sum(world_rates$Recovered)/ sum(world_rates$Confirmed))* 100))
global_rate$Country.Region <- factor(global_rate$Country.Region)
global_rate$Death_Rate <- round(as.numeric(global_rate$Death_Rate), 2)
global_rate$Recovery_Rate <- round(as.numeric(global_rate$Recovery_Rate), 2)
ggplot(global_rate, aes(x = Country.Region, y = Death_Rate)) + geom_bar(stat = "identity") + 
  xlab("") + ylab("Death Rate") + ggtitle("Comparing Global Death Rate with that of India")
ggplot(global_rate, aes(x = Country.Region, y = Recovery_Rate)) + geom_bar(stat = "identity") + 
  xlab("") + ylab("Recovery Rate") + ggtitle("Comparing Global Recovery Rate with that of India")

###################################################################################################################
#AGE GROUP ANALYSIS################################################################################################
###################################################################################################################

india_age <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India New\\patients_data.csv")
india_age$age_bracket <- as.numeric(as.character(india_age$age_bracket))
str(india_age)
india_age$AgeGroup[india_age$age_bracket < 10] <- "0-9"
india_age$AgeGroup[india_age$age_bracket >= 10 & india_age$age_bracket < 20] <- "10-19"
india_age$AgeGroup[india_age$age_bracket >= 20 & india_age$age_bracket < 30] <- "20-29"
india_age$AgeGroup[india_age$age_bracket >= 30 & india_age$age_bracket < 40] <- "30-39"
india_age$AgeGroup[india_age$age_bracket >= 40 & india_age$age_bracket < 50] <- "40-49"
india_age$AgeGroup[india_age$age_bracket >= 50 & india_age$age_bracket < 60] <- "50-59"
india_age$AgeGroup[india_age$age_bracket >= 60 & india_age$age_bracket < 70] <- "60-69"
india_age$AgeGroup[india_age$age_bracket >= 70 & india_age$age_bracket < 80] <- "70-79"
india_age$AgeGroup[india_age$age_bracket >= 80] <- "80<="
library(Hmisc)
india_age$AgeGroup <- impute(india_age$AgeGroup, "Missing")
india_age$count <- 1
india_age_group <- aggregate(india_age["count"], by = india_age["AgeGroup"], FUN = "sum")
names(india_age_group) <- c("AgeGroup", "TotalCases")
print(india_age_group)
india_age_group <- india_age_group[c(1:9), ]
library(scales)
india_age_group$label <- percent(india_age_group$TotalCases/sum(india_age_group$TotalCases))  
india_age_group$Percentage <- round((india_age_group$TotalCases / sum(india_age_group$TotalCases))* 100, 2)
ggplot(india_age_group, aes(x = AgeGroup, y = TotalCases)) + geom_bar(stat = "identity") +
  xlab("Age Group") + ylab("No. of Cases") + ggtitle("Showing the no. of Confirmed Cases among different Age Groups(excluding missing values)")
india_age_group <- arrange(india_age_group, desc(AgeGroup))
library(ggrepel)
library(forcats)
library(scales)
ggplot(india_age_group) +
  geom_bar(aes(x = "", y = Percentage, fill = AgeGroup), width = 1.5, stat = "identity") +
  coord_polar("y", start = 0) + theme_void() +
  geom_label_repel(aes(x = 1.3, y = cumsum(Percentage) - Percentage/2, label = label), size=4, show.legend = F, nudge_x = 0.55) +
  guides(fill = guide_legend(reverse = T)) + ggtitle("Pie Chart showing Age Group distribution of Confirmed Cases(excluding missing values).")

india_age_dec <- subset(india_age, current_status == "Deceased")
india_age_group <- aggregate(india_age_dec["count"], by = india_age_dec["AgeGroup"], FUN = "sum")
names(india_age_group) <- c("AgeGroup", "TotalCases")
print(india_age_group)
india_age_group <- india_age_group[c(1:9), ]
india_age_group$label <- percent(india_age_group$TotalCases/sum(india_age_group$TotalCases))  
india_age_group$Percentage <- round((india_age_group$TotalCases / sum(india_age_group$TotalCases))* 100, 2)
ggplot(india_age_group, aes(x = AgeGroup, y = TotalCases)) + geom_bar(stat = "identity") +
  xlab("Age Group") + ylab("No. of Cases") + ggtitle("Showing the no. of Deaths among different Age Groups(excluding missing values)")
india_age_group <- arrange(india_age_group, desc(AgeGroup))
ggplot(india_age_group) +
  geom_bar(aes(x = "", y = Percentage, fill = AgeGroup), width = 1.5, stat = "identity") +
  coord_polar("y", start = 0) + theme_void() +
  geom_label_repel(aes(x = 1.3, y = cumsum(Percentage) - Percentage/2, label = label), size=4, show.legend = F, nudge_x = 0.7) +
  guides(fill = guide_legend(reverse = T)) + ggtitle("Pie Chart showing Age Group distribution of Death Cases(excluding missing values).")

###################################################################################################################
#GENDER ANALYSIS IN INDIA##########################################################################################
###################################################################################################################

india_gender <- aggregate(india_age["count"], by = india_age["gender"], FUN = "sum")
names(india_gender) <- c("Gender", "TotalCases")
print(india_gender)
india_gender <- india_gender[c(2:4), ]
india_gender$label <- percent(india_gender$TotalCases/sum(india_gender$TotalCases))  
india_gender$Percentage <- round((india_gender$TotalCases / sum(india_gender$TotalCases))* 100, 2)
ggplot(india_gender, aes(x = Gender, y = TotalCases)) + geom_bar(stat = "identity") +
  xlab("Gender") + ylab("No. of Cases") + ggtitle("Showing the no. of Confirmed Cases among different Genders(excluding missing values)")
india_gender <- arrange(india_gender, desc(Gender))
ggplot(india_gender) +
  geom_bar(aes(x = "", y = Percentage, fill = Gender), width = 1.5, stat = "identity") +
  coord_polar("y", start = 0) + theme_void() +
  geom_label_repel(aes(x = 1.3, y = cumsum(Percentage) - Percentage/2, label = label), size=4, show.legend = F, nudge_x = 0.7) +
  guides(fill = guide_legend(reverse = T)) + ggtitle("Pie Chart showing Gender distribution of Confirmed Cases(excluding missing values).")

india_gender_dec <- subset(india_age, current_status == "Deceased")
india_gender <- aggregate(india_gender_dec["count"], by = india_gender_dec["gender"], FUN = "sum")
names(india_gender) <- c("Gender", "TotalCases")
print(india_gender)
india_gender <- india_gender[c(2:3), ]
india_gender$label <- percent(india_gender$TotalCases/sum(india_gender$TotalCases))  
india_gender$Percentage <- round((india_gender$TotalCases / sum(india_gender$TotalCases))* 100, 2)
ggplot(india_gender, aes(x = Gender, y = TotalCases)) + geom_bar(stat = "identity") +
  xlab("Gender") + ylab("No. of Cases") + ggtitle("Showing the no. of Deaths among different Genders(excluding missing values)")
india_gender <- arrange(india_gender, desc(Gender))
ggplot(india_gender) +
  geom_bar(aes(x = "", y = Percentage, fill = Gender), width = 1.5, stat = "identity") +
  coord_polar("y", start = 0) + theme_void() +
  geom_label_repel(aes(x = 1.3, y = cumsum(Percentage) - Percentage/2, label = label), size=4, show.legend = F, nudge_x = 0.7) +
  guides(fill = guide_legend(reverse = T)) + ggtitle("Pie Chart showing Gender distribution of Death Cases(excluding missing values).")

india_gender_state <- india_age[, c(6,9,23)]
india_gender_state <- subset(india_gender_state, gender %in% c("M", "F", "Non-Binary"))
india_gender_state <- aggregate(india_gender_state["count"], by = india_gender_state[c("detected_state", "gender")], FUN = "sum")
ggplot(india_gender_state, aes(x = detected_state, y = count)) + geom_bar(aes(fill = gender), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("No. of Cases") + ggtitle("Plotting State-wise Gender distribution of Confirmed Cases(excluding missing values)") + labs(fill = "")

###################################################################################################################
#TESTING NUMBERS IN INDIA##########################################################################################
###################################################################################################################

india_tests <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India New\\tests_daily.csv")
str(india_tests)
india_tests$updatetimestamp <- parse_date_time(india_tests$updatetimestamp, "%d/%m/%Y %H:%M:%S")
india_tests$updatetimestamp <- as.Date(india_tests$updatetimestamp)
india_tests <- india_tests[length(india_tests$updatetimestamp), ]
test_pos <- covid_india[, c(1,2)]
test_pos$Date <- as.Date(test_pos$Date, "%Y-%m-%d")
test_pos <- subset(test_pos, Date == min(india_tests$updatetimestamp, max(test_pos$Date)))
india_tests$totalpositivecases <- test_pos$Confirmed
india_test_prop <- data.frame("Positive Cases", india_tests$totalpositivecases, stringsAsFactors = F)
names(india_test_prop) <- c("Outcome", "Total_Cases")
india_test_prop <- rbind(india_test_prop, c("Negative Cases", india_tests$totalsamplestested - india_tests$totalpositivecases))
india_test_prop$Outcome <- factor(india_test_prop$Outcome)
india_test_prop$Total_Cases <- as.numeric(india_test_prop$Total_Cases)
india_test_prop$Percent <- round(india_test_prop$Total_Cases/sum(india_test_prop$Total_Cases) * 100, 2) 
india_test_prop$label <- india_test_prop$Percent
ggplot(india_test_prop) +
  geom_bar(aes(x = "", y = Percent, fill = Outcome), width = 1.5, stat = "identity") +
  coord_polar("y", start = 0) + theme_void() +
  geom_text(aes(x = 1, y = cumsum(Percent) - Percent/2, label = label)) +
  guides(fill = guide_legend(reverse = T)) +
  ggtitle("Showing the percentages of Positive and Negative outcomes from Tests performed")

india_pop <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India\\population_india_census2011.csv")
india_pop_prop <- data.frame("Yes", india_tests$totalsamplestested, stringsAsFactors = F)
names(india_pop_prop) <- c("Tested", "Number_of_Samples")
india_pop_prop <- rbind(india_pop_prop, c("No", sum(india_pop$Population) - india_tests$totalsamplestested))
india_pop_prop$Tested <- factor(india_pop_prop$Tested)
india_pop_prop$Number_of_Samples <- as.numeric(india_pop_prop$Number_of_Samples)
india_pop_prop$Percent <- round(india_pop_prop$Number_of_Samples/sum(india_pop_prop$Number_of_Samples) * 100, 2) 
india_pop_prop$label <- india_pop_prop$Percent
ggplot(india_pop_prop) +
  geom_bar(aes(x = "", y = Percent, fill = Tested), width = 1.5, stat = "identity") +
  coord_polar("y", start = 1) + theme_void() +
  geom_text(aes(x = 1, y = cumsum(Percent) - Percent/2, label = label)) +
  guides(fill = guide_legend(reverse = T)) +
  ggtitle("Showing the percentage of Samples tested for COVID-19 against the Indian Population")

###################################################################################################################
#STATE-WISE TESTING NUMBERS IN INDIA###############################################################################
###################################################################################################################

india_state_test <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India New\\tests_latest_state_level.csv")
india_state_test$updatedon <- as.Date(india_state_test$updatedon, "%d/%m/%Y")
str(india_state_test)
india_state_test[is.na(india_state_test)] <- 0
latest_state_test <- aggregate(india_state_test[c("totaltested", "positive")], by = india_state_test["state"], FUN = "max")
state_pop <- india_pop[, c(2,3)]
names(state_pop)
latest_state_test <- merge(x = latest_state_test, y = state_pop, by.x = "state", by.y = "State...Union.Territory")
latest_state_test$Positive_Rate <- round((latest_state_test$positive/latest_state_test$totaltested)* 100, 2)
latest_state_test$Percent_Tested <- round((latest_state_test$totaltested/latest_state_test$Population)* 100, 2)
latest_state_test$Tests_per_One_Million_of_Population <- floor((latest_state_test$totaltested/latest_state_test$Population)* 1000000)
latest_state_test$Positive_Outcomes_per_One_Million_of_Population <- floor((latest_state_test$positive/latest_state_test$Population)* 1000000)
state_test <- as.data.frame(melt(latest_state_test[, c(1:3)]))
ggplot(state_test, aes(x = state, y = value)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("No. of Samples") + ggtitle("Showing the Testing stats of different States") + labs(fill = "")
ggplot(latest_state_test, aes(x = state, y = Positive_Rate)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("Positive Rate") + ggtitle("Comparing the Positive Rates of different States")
ggplot(latest_state_test, aes(x = state, y = Percent_Tested)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("Percent of Samples against population") + ggtitle("Showing the percentage of Samples tested for COVID-19 against the population of different States")
ggplot(latest_state_test, aes(x = state, y = totaltested/10000)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("No. of Tests performed") + ggtitle("Showing the no. of Tests performed per 10,000 people")
state_test <- as.data.frame(melt(latest_state_test[, c(1,7,8)]))
ggplot(state_test, aes(x = state, y = value)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State") + ylab("No. of people") + ggtitle("Showing Testing Stats per one million of Population of the different States in India") + labs(fill = "")

###################################################################################################################
#COMPARING INDIA'S TEST NUMBERS WITH THE REST OF THE WORLD#########################################################
###################################################################################################################

world_test <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 World\\testing data.csv")
world_test <- world_test[, c(2, 8)]
names(world_test) <- c("Country", "Tested")
world_test$Country <- as.character(world_test$Country)
world_test$Country[world_test$Country == "United States"] <- "US"
world_test$Country[world_test$Country == "United Kingdom"] <- "UK"
world_test$Country <- factor(world_test$Country)
for_testing <- for_testing[, c(1,3)]
world_test <- merge(x = world_test, y = for_testing, by.x = "Country", by.y = "Country.Region")
names(world_test) <- c("Country", "Tested", "Positive")
world_test <- arrange(world_test, -world_test$Tested)
compare_tests <- world_test[c(1:10), ]
world_pop <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\World Population\\pop_worldometer_data.csv")
world_pop <- world_pop[, c(1:2)]
names(world_pop) <- c("Country", "Population")
world_pop$Country <- as.character(world_pop$Country)
world_pop$Country[world_pop$Country == "United States"] <- "US"
world_pop$Country[world_pop$Country == "United Kingdom"] <- "UK"
world_pop$Country <- factor(world_pop$Country)
compare_tests <- merge(x = compare_tests, y = world_pop, by.x = "Country", by.y = "Country")
str(compare_tests)
compare_tests$PositiveRate <- round((compare_tests$Positive/compare_tests$Tested)* 100, 2)
compare_tests$TestsPer10000Persons <- round((compare_tests$Tested/10000), 2)
compare_tests$TestsPerOneMillionOfPopulation <- round((compare_tests$Tested/compare_tests$Population)* 1000000, 2)
compare_tests$PositivesPerOneMillionOfPopulation <- round((compare_tests$Positive/compare_tests$Population)* 1000000, 2)
compare_tests$PercentOfPopulationTested <- round((compare_tests$Tested/compare_tests$Population)* 100, 2)
country_test <- as.data.frame(melt(compare_tests[, c(1,2,3)]))
ggplot(country_test, aes(x = Country, y = value)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
  xlab("Country") + ylab("No. of people") + ggtitle("Showing Testing Stats for Countries with highest Testing numbers and India") + labs(fill = "")
ggplot(compare_tests, aes(x = Country, y = PositiveRate)) + geom_bar(stat = "identity") +
  xlab("Country") + ylab("Positive Rate") + ggtitle("Comparing the Positive Rates of these Countries")
ggplot(compare_tests, aes(x = Country, y = TestsPer10000Persons)) + geom_bar(stat = "identity") + 
  xlab("Country") + ylab("No. of Tests") + ggtitle("Comparing the no. of Tests performed per 10,000 persons")
country_test <- as.data.frame(melt(compare_tests[, c(1,7,8)]))
ggplot(country_test, aes(x = Country, y = value)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Country") + ylab("No. of people") + ggtitle("Showing Testing Stats per one million of Population of Countries with highest Testing numbers and India") + labs(fill = "")
ggplot(compare_tests, aes(x = Country, y = PercentOfPopulationTested)) + geom_bar(stat = "identity") +
  xlab("Country") + ylab("Percent of Population") + ggtitle("Comparing the percentage of Population tested in these Countries")

###################################################################################################################
#INSIGHTS ON THE CORONAVIRUS ZONES IN INDIA########################################################################
###################################################################################################################

zones <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India New\\zones.csv")
district <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India New\\district_level_latest.csv")
zones <- zones[, c(1, 5, 7)]
district <- district[, c(3:7)]
zones <- merge(x = zones, y = district, by.x = "district", by.y = "district")
active_state <- aggregate(zones["active"], by = zones["state"], FUN = "sum")
names(active_state) <- c("State", "Active")
zones$count <- 1
zone_state <- aggregate(zones["count"], by = zones[c("state", "zone")], FUN = "sum")
ggplot(zone_state, aes(x = state, y = count)) + geom_bar(aes(fill = zone), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1) ) +
  xlab("State or Union Territory") + ylab("No. of Districts") + ggtitle("Showing the distribution of different types of Zones in different States") + labs(fill = "Type of Zone") +
  scale_fill_manual(values = c("Red" = "red", "Green" = "green", "Orange" = "orange", " " = "black"))
red_zone <- subset(zones, zone == "Red")
red_zone <- as.data.frame(melt(red_zone[, c(2, 4, 5, 6)]))
ggplot(red_zone, aes(x = state, y = value)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1) ) +
  xlab("State or Union Territory") + ylab("No. of Cases") + ggtitle("Showing the COVID-19 Stats in the Red Zones across the different States") + labs(fill = " ")
orange_zone <- subset(zones, zone == "Orange")
orange_zone <- as.data.frame(melt(orange_zone[, c(2, 4, 5, 6)]))
ggplot(orange_zone, aes(x = state, y = value)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1) ) +
  xlab("State or Union Territory") + ylab("No. of Cases") + ggtitle("Showing the COVID-19 Stats in the Orange Zones across the different States") + labs(fill = " ")

###################################################################################################################
#INSIGHTS ON HOSPITALS AND AVAILABLE BEDS IN INDIA#################################################################
###################################################################################################################

covid_india <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India\\covid_19_india.csv")
head(covid_india)
str(covid_india)
covid_india$State.UnionTerritory[covid_india$State.UnionTerritory == "Chattisgarh"] <- "Chhattisgarh"
covid_india$State.UnionTerritory[covid_india$State.UnionTerritory == "Nagaland#"] <- "Nagaland"
covid_india$State.UnionTerritory[covid_india$State.UnionTerritory == "Jharkhand#"] <- "Jharkhand"
covid_india$Date <- as.Date(covid_india$Date, "%d/%m/%y")

india_hos_bed <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India\\HospitalBedsIndia.csv")
str(india_hos_bed)
india_hptls <- merge(x = india_hos_bed[, c(2,7,8)], y = state_pop, by.x = "State.UT", by.y = "State...Union.Territory")
names(india_hptls) <- c("State.UT", "Hospitals", "Hospital_Beds", "Population")
compare_confirmed <- aggregate(covid_india["Confirmed"], by = covid_india["State.UnionTerritory"], FUN = "max")
compare_deaths <- aggregate(covid_india["Deaths"], by = covid_india["State.UnionTerritory"], FUN = "max")
india_hptls <- merge(x = merge(x = india_hptls, y = compare_confirmed, by.x = "State.UT", by.y = "State.UnionTerritory"), y = compare_deaths, by.x = "State.UT", by.y = "State.UnionTerritory") 
india_hptls$Average_Beds_per_Hospital <- floor(india_hptls$Hospital_Beds/india_hptls$Hospitals)
india_hptls$No_of_Persons_per_Bed <- floor(india_hptls$Population/india_hptls$Hospital_Beds)
india_hptls$Beds_per_10000_Persons <- round(india_hptls$Hospital_Beds/10000, 2)
india_hptls$Beds_per_one_million_population <- floor((india_hptls$Hospital_Beds/india_hptls$Population)*1000000)
india_hptls <- merge(x = india_hptls, y = active_state, by.x = "State.UT", by.y = "State")
i <- length(india_hos_bed$State.UT)
india_hptls <- rbind(india_hptls, c("All India", india_hos_bed$TotalPublicHealthFacilities_HMI[i], india_hos_bed$NumPublicBeds_HMIS[i],
                                    world_pop$Population[2], rate_india$Confirmed, rate_india$Deaths, floor(india_hos_bed$NumPublicBeds_HMIS[i]/india_hos_bed$TotalPublicHealthFacilities_HMI[i]),
                                    floor(world_pop$Population[2]/india_hos_bed$NumPublicBeds_HMIS[i]), round(india_hos_bed$NumPublicBeds_HMIS[i]/10000, 2),
                                    floor((india_hos_bed$NumPublicBeds_HMIS[i]/world_pop$Population[2])*1000000), sum(active_state$Active)))
str(india_hptls)
india_hptls[, c(2:11)] <- sapply(india_hptls[, c(2:11)], as.numeric)
india_hptls[-30,] %>% ggplot(aes(x = State.UT, y = Hospitals)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("No. of Hospitals") + ggtitle("Showing the number of Hospitals in different States")
india_hptls[-30,] %>% ggplot(aes(x = State.UT, y = Hospital_Beds)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("No. of Hospital Beds") + ggtitle("Showing the number of Hospital Beds in different States")
india_hptls[30,] %>% ggplot(aes(x = State.UT, y = Hospital_Beds)) + geom_bar(stat = "identity")  +
  xlab(element_blank()) + ylab("No. of Hospital Beds") + ggtitle("Showing the total number of Hospital Beds in India")
india_hptls[-30,] %>% ggplot(aes(x = State.UT, y = Average_Beds_per_Hospital)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("No. of Hospital Beds") + ggtitle("Showing the Average number of Hospital Beds per Hospital in different States")
india_hptls[-30,] %>% ggplot(aes(x = State.UT, y = No_of_Persons_per_Bed)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("No. of Persons") + ggtitle("Showing the number of Persons per Hospital Bed in different States")
india_hptls[-30,] %>% ggplot(aes(x = State.UT, y = Beds_per_10000_Persons)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("No. of Hospital Beds") + ggtitle("Showing the number of Hospital Beds per 10,000 Persons in different States")
india_hptls[30,] %>% ggplot(aes(x = State.UT, y = Beds_per_10000_Persons)) + geom_bar(stat = "identity") +
  xlab(element_blank()) + ylab("No. of Hospital Beds") + ggtitle("Showing the number of Hospital Beds per 10,000 Persons in India")
india_hptls[-30,] %>% ggplot(aes(x = State.UT, y = Beds_per_one_million_population)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab("No. of Hospital Beds") + ggtitle("Showing the number of Hospital Beds per One million Population in different States")
india_hptls[30,] %>% ggplot(aes(x = State.UT, y = Beds_per_one_million_population)) + geom_bar(stat = "identity") +
  xlab(element_blank()) + ylab("No. of Hospital Beds") + ggtitle("Showing the number of Hospital Beds per One million Population in India")
arrange_hptls <- arrange(india_hptls, by = desc(Confirmed))
compare_hptls <- as.data.frame(melt(arrange_hptls[-1, c(1,3,11)]))
ggplot(compare_hptls, aes(x = State.UT, y = value)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("State or Union Territory") + ylab(element_blank()) + ggtitle("Comparing the number of Hospital Beds with number of Active Cases in different States") + labs(fill = "")
compare_hptls <- as.data.frame(melt(arrange_hptls[1, c(1,3,11)]))
ggplot(compare_hptls, aes(x = State.UT, y = value)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity") +
  xlab(element_blank()) + ylab(element_blank()) + ggtitle("Comparing the number of Hospital Beds with total Active Cases in all India") + labs(fill = "")

###################################################################################################################
#INSIGHTS ON THE NUMBER OF TEST LABS IN INDIA######################################################################
###################################################################################################################

india_labs <- read.csv("C:\\Users\\Shekhar Lamba\\Documents\\Datasets\\COVID-19 India\\ICMRTestingLabs.csv")
india_labs$count <- 1
india_labs[, c(1:4)] <- NULL
str(india_labs)
state_labs <- aggregate(india_labs["count"], by = india_labs["state"], FUN = "sum")
state_type_labs <- aggregate(india_labs["count"], by = india_labs[c("state", "type")], FUN = "sum")
type_labs <- aggregate(india_labs["count"], by = india_labs["type"], FUN = "sum")
ggplot(state_labs, aes(x = state, y = count)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1) ) +
  xlab("State or Union Territory") + ylab("No. of Testing Labs") + ggtitle("Showing the number of Testing Labs in different States")
ggplot(type_labs, aes(x = type, y = count)) + geom_bar(stat = "identity") +
  xlab("Type of Testing Labs") + ylab("No. of Testing Labs") + ggtitle("Showing the number of different types of Testing Labs in India")
ggplot(state_type_labs, aes(x = state, y = count)) + geom_bar(aes(fill = type), stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1) ) +
  xlab("State or Union Territory") + ylab("No. of Testing Labs") + ggtitle("Showing the distribution of different types of Testing Labs in different States") + labs(fill = "Type of Testing Lab")
state_labs <- merge(x = state_labs, y = latest_state_test, by.x = "state", by.y = "state")
state_labs$Tests_per_Lab <- state_labs$totaltested/state_labs$count
ggplot(state_labs, aes(x = state, y = Tests_per_Lab)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1) ) +
  xlab("State or Union Territory") + ylab("No. of Tests") + ggtitle("Showing the number of Tests done per Testing Labs in different States") 

