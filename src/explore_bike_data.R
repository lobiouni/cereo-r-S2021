# Explore the bike data characteristics
#
# Explore the bike data to look at the relationship between 
# temperature and number of riders.
#
# Step 1: Load the bike data and look at the metadata

library(tidyverse)

### Read the data ---- 
bike <- read_csv("data/daily_bike_data.csv")

head(bike)
str(bike)
sort(names(bike)) #print in alphabetic order

# Time trend of ridership
ggplot(data = bike)+
  geom_line(aes(x = dteday, y = cnt))


ggplot(data = bike)+
  geom_point(aes(x= temp, y = cnt))


### Data Cleaning ----
# dplyr verbs for data transformations
# select: select columns that we want ot keep
# filters: select rows that we want to keep
# mutate: transforms data while keeping other columns
# transmute: creates new columns and does not keep old columns
# %>%: "pipe" pipes the output from one command as the input for the next command

bike %>%
  select(dteday, season, weathersit, temp, cnt)

# One way of selecting  spring records and just a few cols
spring_bike <- filter(bike, season == "spring")

# Select just the temperature and counts
spring_bike_temp_cnt <- select(spring_bike, temp, cnt)

# Other way
spring_bike_temp_cnt2 <- bike %>%
  filter(season=="spring") %>%
  select(temp, cnt)


## Exercise: select weathersit and count for all winter records
spring_bike_wthr_cnt <- bike %>%
  filter(season=="winter") %>%
  select(weathersit, cnt)

### Mutate and Transmute with factors and dates

## Converting to factors

summary(bike$weathersit)
unique(bike$weathersit)

# Can reference the data documentation:
# https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

bike2 <- bike %>%
  dplyr::mutate(
    weather_fac=factor(weathersit, levels=c(1, 2, 3, 4),
                       labels = c("Clear", "Cloudy", "Rainy", "Heavy Rain")))

unique(bike2$weather_fac)

bike2 %>% select(dteday, weathersit, weather_fac)

## Converting to and from dates

bike_dates <- bike %>% transmute(
  instant, 
  date = dteday,
  date_num = as.numeric(dteday),
  date_char = as.character(dteday))

bike_dates %>% transmute(
    instant, 
    date,
    date_num = as.Date(date_num, origin="1970-01-01"),
    date_char= as.Date(date_char))


### Additional, filtering, and selecting
bike %>% select(dteday, cnt)
bike %>% select(dteday, cnt, temp)%>%select(-temp)

keep_vars <- c("dteday", "cnt", "temp")
bike %>% select(all_of(keep_vars))

### Filtering 
bike %>% filter(season == "spring")
bike %>% filter(season != "spring")

bike %>%
  filter(season != "spring") %>%
  select(season) %>%
  distinct

# slecting multiple options at once
bike %>%
filter(season== "summer" | season == "winter")

seasons <- c("summer", "winter")
bike %>%
  filter(season %in% seasons)
