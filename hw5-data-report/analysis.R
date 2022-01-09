# Loading the `dplyr` package
library("dplyr")
library("stringr")

# read csv file (ms = mass shooting)
ms <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

# Use the View function to look at the loaded data
View(ms)

# Basic dimensional info of data set
year <- substring(ms[1, 1], nchar(ms[1, 1]) - 3, nchar(ms[1, 1]))
n_shootings <- nrow(ms)
n_cols <- ncol(ms)

# Sum of total deaths
total_death <- sum(ms$num_killed)

# Most impacted city Pt. 1 (total deaths and injuries)
city_impact <- ms %>%
  group_by(city) %>%
  summarize(
    sum_kill = sum(num_killed, na.rm = TRUE),
    sum_inj = sum(num_injured, na.rm = TRUE)
  )

# Most impacted city Pt. 2 (max of those totals)
city_impact$sum <- city_impact$sum_kill + city_impact$sum_inj
city_most_impacted <- city_impact %>%
  filter(sum == max(sum, na.rm = TRUE)) %>%
  pull(city)

# State with the least incidents
state_least <- ms %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  filter(count == min(count, na.rm = TRUE)) %>%
  pull(state)

# Date with the most incidents
date_most <- ms %>%
  group_by(date) %>%
  summarize(count = n()) %>%
  filter(count == max(count, na.rm = TRUE)) %>%
  pull(date)

###########################ig the top two could be changed into a function??
###############################add title!!!!!!!!!!!


## use functions?
## structure so that one can easily update entire report

# Creating dataset for Figure 1
figure_1 <- ms %>%
  group_by(state) %>%
  summarize(
    total_killed = sum(num_killed, na.rm = TRUE),
    total_injured = sum(num_injured, na.rm = TRUE)
  )

figure_1$impact <- figure_1$total_killed + figure_1$total_injured
figure_1 <- arrange(figure_1, -impact)

# Getting values for Thousand Oaks Pt 1
# Finding exact incident and getting a data set with only those values: I
# deemed that date and address was enough to find the unique incident
thous_oak <- ms %>%
  filter(address == "99 Rolling Oaks Dr") %>%
  filter(date == "November 7, 2018")

# Getting values for Thousand Oaks Pt 2
# Getting specific values for text: city, state, number killed and injured, and
# date
thous_city <- thous_oak$city
thous_state <- thous_oak$state
thous_date <- thous_oak$date
thous_n_killed <- thous_oak$num_killed
thous_n_injured <- thous_oak$num_injured

# Interactive map
library("leaflet")

## Created data frame for relevant facts
info <- data.frame(
  label = c(ms$address),
  latitude = c(ms$lat),
  longitude = c(ms$long),
  size = c(ms$num_killed + ms$num_injured),
  description = paste("City:", ms$city, "<br>",
                      "Deaths:", ms$num_killed, "<br>",
                      "Injured:", ms$num_injured)
)

## Draw actual map with plots
figure_2 <- leaflet(data = info) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -100, lat = 38, zoom = 4) %>%
  addCircles(
    lat = ~latitude,
    lng = ~longitude,
    popup = ~description,
    radius = ~size * 5000,
    stroke = FALSE
  )

# Plot to answer month vs ratio quesiton
library("ggplot2")
library("tidyr")

## Create dataframe with relevant information
get_month_val <- ms %>%
  mutate(numeric_date = as.Date(date, format = "%B %d, %Y")) %>%
  mutate(month = substring(numeric_date, 6, 7)) %>%
  group_by(month) %>%
  summarize(
    killed = sum(num_killed, na.rm = TRUE),
    injured = sum(num_injured, na.rm = TRUE)
  )

## Wrangle data into a more efficient plot creation
month_ratio <- get_month_val %>%
  select(month, killed, injured) %>%
  gather(key = killedinjured, value = count, -month)

## Create a fill postiton plot
figure_3 <- ggplot(month_ratio) +
  geom_col(
    mapping = aes(x = month, y = count, fill = killedinjured),
    position = "fill"
  )
figure_3 <- figure_3 +
  ggtitle("Ratio of Killed/Injured by Month") +
  xlab("Month (numerical representation)") +
  ylab("Count (pecentage out of 1.0)")
