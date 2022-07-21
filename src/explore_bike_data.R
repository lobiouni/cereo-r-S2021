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

library(here)
?here
here::here("data/daily_bike_data.csv")

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

# selecting multiple options at once
bike %>%
filter(season== "summer" | season == "winter")

seasons <- c("summer", "winter")
bike %>%
  filter(season %in% seasons)


### More dyplyr verbs
# summarize: summary multiple rows for a col/variable
# group_by: perform and operation separately for each group

bike2 %>% 
  filter (season == "summer") %>%
  summarize(
  temp_mean = mean(temp),
  cnt_mean = mean (cnt),
  cnt_sum = sum(cnt))

bike2 %>%
  group_by(season) %>%
  summarize(
    temp_mean = mean(temp),
    cnt_mean = mean (cnt),
    cnt_sum = sum(cnt))

# What are the season definitions?
sort(names(bike))
bike %>% select (season, mnth) %>% distinct()

# Create a new season 
bike3 <- bike2 %>%
  mutate(
    season2 = 1* (mnth %in% c("December", "January", "February"))+
      2 * (mnth %in% c("March", "Apr", "May")) +
      3 * (mnth %in% c("June", "July", "August")) +
      4 * (mnth %in% c("September", "October", "November"))) %>%
  mutate(season2 = factor(season2, levels = 0:4, 
                          labels = c("Unknown", "Winter", "Spring", "Summer", "Fall")))

bike3 %>%
  group_by(season2) %>%
  summarize(
    temp_mean = mean(temp),
    cnt_mean = mean (cnt),
    cnt_sum = sum(cnt))


## Facetting in ggplot
ggplot(data = bike3) +
  geom_point(aes(x=temp, y=cnt))+
  geom_smooth(aes(x=temp, y=cnt), method = "lm") +
  facet_wrap(~season2)

ggplot(data = bike3) +
  geom_point(aes(x=temp, y=cnt))+
  geom_smooth(aes(x=temp, y=cnt), 
              method = "lm", 
              formula = y ~ poly(x, 2)) +
  facet_wrap(~season2)

### Pivoting wider to long and longer to wide
# Long to wide: data in multiple columns ( e.g. temp1, temp2, temp3, ..)
# Wide to long: data in one column, classifier in other columns
# tidyr is the package that allows transformations

months <- c("January", "February", "March", "April",
            "May", "June", "July", "August",
            "September", "October", "November", "December")


tidybike <- bike3 %>% # long format data
  select (yr, mnth, temp, cnt) %>%
  mutate(month = factor(mnth, 
                        levels= months, 
                        labels = months)) %>%
  group_by(yr, month) %>%
  summarize(temp_mean = mean(temp), rides = sum(cnt))


# Going from long to wide
# pivot_wider (new sintax)

tidybike %>% 
  select(-rides) %>%
  pivot_wider(values_from=temp_mean, 
              names_from=month, 
              names_prefix = "temp_")

rides <- tidybike %>% 
  select(-temp_mean) %>%
  pivot_wider(values_from=rides, 
              names_from=month, 
              names_prefix = "rides_") %>%
  rename_with(tolower) %>%
  rename(year = yr)

# Going from wide to long
# pivot_longer / gather
rides %>% gather(key = "month", value = "rides", -year)

rides %>% 
  select (year, rides_january, rides_february) %>%
  pivot_longer(names_to = "month", cols = c("rides_january", "rides_february")) %>%
  mutate(month = substr(month, 7, nchar(month))) %>%
  mutate(month = toupper(month))


