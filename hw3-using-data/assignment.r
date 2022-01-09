# Assignment 3: Using Data
#
# Before you get started:
# - Set your working directory to "source file location" using the Session menu
# - Run the following line of code to delete all variables in your workspace
#     (This will make it easier to test your script)
rm(list = ls())


### Built in R Data ###########################################################

# In this section, you'll work with the variable `Titanic`, a data set which is
# built into the R environment.
# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table.
check <- is.data.frame(Titanic)

# Create a variable `titanic_df` by converting `Titanic` into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`
# Hint: Be sure to **not** treat strings as factors!
titanic_df <- data.frame(Titanic, stringsAsFactors = FALSE)

# It's important to understand the _meaning_ of each column before analyzing it
# Using comments below, describe what information is stored in each column
# For categorical variables, list all possible values

# Class: the economic status of each person on the ship: 1st, 2nd, 3rd, or crew
# Sex: the gender identity of each person on the ship: male or female
# Age: how old each passenger/crew member is: child or adult
# Survived: binary of whether they survived: yes or no
# Freq: the number of passenger with the same values for each of the previous
#       variables: not catagorical, but range from 0-670

# Create a variable `children` that is a data frame containing only the rows
# from `titanic_df` with information about children on the Titanic
# Hints:
# - Filter rows using a vector of boolean values (like vector filtering)
# - See chapter 10.2.3
children <- titanic_df[titanic_df$Age == "Child", ]

# Create a variable `num_children` that is the total number of children.
# Hint: Remember the `sum()` function!
num_children <- sum(children$Freq, na.rm = TRUE)

# Create a variable `most_lost` that is the *row* from `titanic_df` with the
# largest absolute number of losses (people who did not survive)
# You can use multiple lines of code if you find that helpful
# to create this variable
# Hint: Filter for those who did not survive, then look for the row
most_lost <- titanic_df[titanic_df$Freq == max(titanic_df$Freq
[titanic_df$Survived == "No"], na.rm = TRUE), ]

# Define a function called `survival_rate()` that takes in two arguments which
# must be in *the following order*:
# - a ticket class (e.g., "1st", "2nd"), and
# - the dataframe itself (it's good practice to explicitly pass in data frames)
#
# This function should return a sentence that states the *survival rate*
# (# survived / # in group) of adult men and "women and children" in that
# ticketing class.
# It should read (for example):
# >"Of Crew class, 87% of women and children survived and 22% of men survived."
#
# This is a complicated function! We recommend the following approach:
# - Filter for all rows representing the given ticketing class and save the
#   new data frame to a variable
# - Using this data frame, filter for all rows representing Adult Males
# - Find the total number of men and total number of male survivors to
#   calculate the survival rate
# - Likewise, use the data frame to filter for all Children and Adult Females
# - Perform the above calculation for this group as well
#
# Other approaches are also acceptable, please comment to explain what you do!
survival_rate <- function(ticket, dataframe) {
  ticket_frame <- dataframe[dataframe$Class == ticket, ]
  adult_frame <- ticket_frame[ticket_frame$Age == "Adult", ]

  male_frame <- adult_frame[adult_frame$Sex == "Male", ]

  am_percent <- round((male_frame[male_frame$Survived == "Yes", "Freq"] /
    sum(male_frame$Freq)), 2) * 100

  female_frame <- adult_frame[adult_frame$Sex == "Female", ]
  ch_frame <- ticket_frame[ticket_frame$Age == "Child", ]

  survive <- female_frame[female_frame$Survived == "Yes", "Freq"] +
    sum(ch_frame[ch_frame$Survived == "Yes", "Freq"])
  total <- sum(female_frame$Freq) + sum(ch_frame$Freq)

  afch_percent <- round((survive / total), 2) * 100

  return(paste0(
    "Of ", ticket, " class, ", afch_percent,
    "% of women and children survived and ", am_percent,
    "% of men survived."
  ))
}

# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class and the `titanic_df` data frame
# to your `survival_rate` function
first_survived <- survival_rate("1st", titanic_df)
second_survived <- survival_rate("2nd", titanic_df)
third_survived <- survival_rate("3rd", titanic_df)
crew_survived <- survival_rate("Crew", titanic_df)

# What notable differences do you observe in the survival rates across classes?
# Note at least 2 observations.
# There is a tendancy for people of higher class to survive as the highest
# percentages were both in 1st class. However, the trend isn't followed with
# crew, where it is higher than the 3rd class passengers.

# What notable differences do you observe in the survival rates between the
# women and children versus the men in each group?
# Note at least 2 observations.
# There is a much higher tendancy for women and children to survive than
# adult males. The highest difference was in 2nd class: it would have been
# a good prediction that the highest difference not be in 1st class regardless
# of it having the highest survival for womena and children because there was
# already an observed increase in survival for men for the highest class.


### Reading in Data ###########################################################

# In this section, you'll work with .csv data of life expectancy by country
# First, download the csv file of `Life Expectancy` data from GapMinder:
# https://www.gapminder.org/data/
# You should save the .csv file into your `data` directory


# Before getting started, explore the GapMinder website to better understand
# the *original* source of the data (e.g., who calculated these estimates)
# Place a brief summary of the each data source here (e.g., 1 - 2 sentences
# per data source)
# Mattias Lindgren, IHME, and UN data/authors were used: Lindgren complied 100
# sources and they used their own version as well. IHME from the next period
# was their main source from the Global Burden of Disease Study in 2017. The
# next period was from the UN's forcasts for the world population in 2019.

# Using the `read.csv` function, read the life_expectancy_years.csv file into
# a variable called `life_exp`. Make sure not to read strings as factors
life_exp <- read.csv("data/life_expectancy_years.csv",
  stringsAsFactors = FALSE
)

# Write a function `get_col_mean()` that takes a column name and a data frame
# and returns the mean of that column. Make sure to properly handle NA values
# Hint: `mean()` takes in an argument called `na.rm`
get_col_mean <- function(col_name, dataframe) {
  return(mean(dataframe[, col_name], na.rm = TRUE))
}

# Create a list `col_means` that has the mean value of each column in the
# data frame (except the `Country` column). You should use your function above.
# Hint: Use an `*apply` function (lapply, sapply, etc.)
names <- colnames(life_exp[-1])
col_means <- lapply(names, get_col_mean, life_exp)

# Create a variable `avg_diff` that is the difference in average country life
# expectancy between 1800 and 2018
avg_diff <- mean(life_exp$X2018, na.rm = TRUE) - mean(life_exp$X1800,
  na.rm = TRUE
)

# Create a column `life_exp$change` that is the change in life
# expectancy from 2000 to 2018. Increases in life expectancy should
# be *positive*
life_exp$change <- life_exp$X2018 - life_exp$X2000

# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy. Make sure to filter NA values
# Hint: `max()` takes in an argument called `na.rm`
most_improved <- life_exp$country[life_exp$change == max(life_exp$change,
  na.rm = TRUE
) & !is.na(life_exp$change)]

# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved less than 1 year between 2000 and 2018
# Make sure to filter NA values
# Hint: Lookup `is.na()`
num_small_gain <- nrow(life_exp[life_exp$change < 1 & life_exp$change > 0 &
  is.na(life_exp$change) == TRUE, ])

# Write a function `country_change()` that takes in a country's name,
# two years as numbers (not strings), and the `life_exp` data frame
# Parameters should be written *in the above order*
# It should return the phrase:
# "Between YEAR1 and YEAR2, the life expectancy in COUNTRY went DIRECTION by
# SOME_YEARS years".
# Make sure to properly indictate the DIRECTION as "up" or "down"
# Hint: Use an if/else statement to help compute DIRECTION
country_change <- function(name, year_1, year_2, life_exp) {
  country_frame <- life_exp[life_exp$country == name, ]
  one <- paste0("X", year_1)
  two <- paste0("X", year_2)
  difference <- country_frame[, two] - country_frame[, one]
  direction <- ""
  if (difference > 0) {
    direction <- "up"
  } else {
    direction <- "down"
  }
  return(paste0(
    "Between ", year_1, " and ", year_2,
    ", the life expectancy in ", name, " went ", direction, " by ",
    abs(difference), " years"
  ))
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 1990 in Sweden
sweden_change <- country_change("Sweden", 1960, 1990, life_exp)

# Write a function `compare_change()` that takes in two country names and your
# `life_exp` data frame as parameters, and returns a sentence that describes
# their change in life expectancy from 2000 to 2018 (the `change` column)
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=6.9),
#  whose life expectancy grew by 0.6 years more than Bolivia's (gain=6.3)."
# Make sure to round your numbers to one digit (though only after calculations)
# Hint: Use an if/else statement to paste the countries in the correct order
compare_change <- function(country_1, country_2, life_exp) {
  first <- life_exp[life_exp$country == country_1, "change"]
  second <- life_exp[life_exp$country == country_2, "change"]
  bigger <- ""
  smaller <- ""
  big_value <- 0
  small_value <- 0
  if (first - second > 0) {
    bigger <- country_1
    big_value <- round(first, 1)
    smaller <- country_2
    small_value <- round(second, 1)
  } else {
    bigger <- country_2
    big_value <- round(second, 1)
    smaller <- country_1
    small_value <- round(first, 1)
  }
  difference <- big_value - small_value
  return(paste0(
    "The country with the bigger change in life expectancy was ",
    bigger, " (gain=", big_value,
    "), whose life expectancy grew by ", difference,
    " years more than ", smaller, "'s (gain=", small_value, ")."
  ))
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)
usa_or_france <- compare_change("United States", "France", life_exp)

# Write your `life_exp` data.frame to a new .csv file to your
# data/ directory with the filename `life_exp_with_change.csv`.
# Make sure not to write row names.
write.csv(life_exp, "data/life_exp_with_change.csv", row.names = FALSE)
