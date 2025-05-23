install.packages("ggplot2")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("DT")
install.packages("scales")

library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)


colors <- c("#CC1011", "#665555", "#05a399", "#cfcaca", 
            "#f5e840", "#0683c9", "#e075b0")

library(readr)

apr_data<- read_csv("/Users/urvigandhi/Desktop/uber/uber-raw-data-apr14.csv")
View(apr_data)



jun_data <- read_csv("/Users/urvigandhi/Desktop/uber/uber-raw-data-jun14.csv")
View(jun_data)


jul_data <- read_csv("/Users/urvigandhi/Desktop/uber/uber-raw-data-jul14.csv")
View(jul_data)

aug_data <- read_csv("/Users/urvigandhi/Desktop/uber/uber-raw-data-aug14.csv")
View(aug_data)



data_2014 <- rbind(apr_data,jun_data,jul_data,aug_data)

cat("The dimensions of the data are:", dim(data_2014))

head(data_2014)


colnames(data_2014)[which(colnames(data_2014) == "Date/Time")] <- "Date.Time"

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Date.Time


data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Time


data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
data_2014$Date.Time


data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$day 


data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$month



data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$year


data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))
data_2014$dayofweek


data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$hour


data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$minute


data_2014$second <- factor(second(hms(data_2014$Time)))
data_2014$second

hour_data <- data_2014 %>% 
  group_by(hour) %>% 
  summarise(Total = n())
datatable(hour_data)

ggplot(hour_data, aes(hour,Total)) +
  geom_bar(stat = "identity", fill ="steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

month_hour <- data_2014 %>% 
  group_by(month, hour) %>% 
  summarise(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels= comma)

day_group <- data_2014 %>% 
  group_by(day) %>% 
  summarise(Total = n())
datatable(day_group)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


day_month_group <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


month_group <- data_2014 %>% 
  group_by(month) %>% 
  summarise(Total = n())
datatable(month_group)

ggplot(month_group, aes(month, Total, fill=month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips Every Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)

day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")

