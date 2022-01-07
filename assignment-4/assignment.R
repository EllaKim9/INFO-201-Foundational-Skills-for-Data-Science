# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

# Loading and Exploring Data -------------------------------- (**29 points**)

# First, search online for a dplyr cheatsheet and put the link to one you
# like in the comments here (it's ok if the link makes the line too long):
# https://d33wubrfki0l68.cloudfront.net/db69c3d03699d395475d2ac14d64f611054fa9a4/e98f3/wp-content/uploads/2018/08/data-transformation.png

# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
library("dplyr")

# If your computer isn't in English, you made need to use this line of code
# to get the csv to load correctly (if the data gets messed up a few rows in):
# Sys.setlocale("LC_ALL", "English")

# Load your data, making sure to not interpret strings as factors.
ks <- read.csv("data/ks-projects-201801.csv", stringsAsFactors = FALSE)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
# - How many rows is the data frame?
# - How many columns are in the data frame?
colum_name <- colnames(ks)
number_rows <- nrow(ks)
number_col <- ncol(ks)

# Use the `summary` function to get some summary information
average_goal_and_pledged <- summarize(
  ks,
  mean_goal = mean(goal, na.rm = TRUE),
  mean_pledged = mean(usd_pledged_real, na.rm = TRUE)
)

min_goal_and_pledged <- summarize(
  ks,
  min_goal = min(goal, na.rm = TRUE),
  min_pledged = min(usd_pledged_real, na.rm = TRUE)
)

max_goal_and_pledged <- summarize(
  ks,
  max_goal = max(goal, na.rm = TRUE),
  max_pledged = max(usd_pledged_real, na.rm = TRUE)
)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type
get_col_info <- function(col_name, dataframe) {
  unique <- unique(dataframe[, col_name])
  if (typeof(dataframe[, col_name]) == "double") {
    return(
      list_info <- list(
        min = min(dataframe[, col_name],  na.rm = TRUE),
        max = max(dataframe[, col_name], na.rm = TRUE),
        mean = mean(dataframe[, col_name], na.rm = TRUE)
      )
    )
  } else if (length(unique) < 10) {
    return(
      list_info <- list(
        n_values = length(unique),
        unique_values = unique
      )
    )
  } else {
    return(
      list_info <- list(
        n_values = length(unique),
        sample_values = sample(unique, 10)
      )
    )
  }
}

# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name
check_col_info_num <- get_col_info("goal", ks)
check_col_info_char1 <- get_col_info("state", ks)
check_col_info_char2 <- get_col_info("deadline", ks)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(dataframe) {
  list_info <- lapply(colnames(dataframe), get_col_info, dataframe)
  names(list_info) <- c(colnames(dataframe))
  return(list_info)
}

# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
check_summary_info <- get_summary_info(ks)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# I found it interesting that the state had such a variety besides just failed,
# succeeded or some form to show that it is onging. It indirectly gave more
# detailed information. I also found it interesting that the minimum goal was
# $.01, it makes me wonder whether that was the min they had to state? Adding
# to that, it makes me wonder why there is such a difference from the max in
# the same category: if the goal was that small it would seem like it succeded
# but it could be because of a small goal not that many people supported, etc.

# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!
# Note: For questions about goals and pledged, use the usd_pledged_real
# and the usd_goal_real columns, since they standardize the currancy.


# What was the name of the project(s) with the highest goal?
name_highest_goal <- ks %>%
  filter(usd_goal_real == max(usd_goal_real, na.rm = TRUE)) %>%
  pull(name)

# What was the category of the project(s) with the lowest goal?
category_lowest_level <- ks %>%
  filter(usd_goal_real == min(usd_goal_real, na.rm = TRUE)) %>%
  pull(category)

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find
number_deadline_2018 <- ks %>%
  filter(substring(deadline, 1, 4) == "2018") %>%
  nrow()

# What proportion of projects weren't marked successful (e.g., failed or live)?
# Your result can be a decimal
proportion_unsuccessful <- ks %>%
  filter(state != "successful") %>%
  nrow() / nrow(ks)

# What was the amount pledged for the project with the most backers?
amount_pledged_most_back <- ks %>%
  filter(backers == max(backers, na.rm = TRUE)) %>%
  pull(usd_pledged_real)

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
failed_highest_pledged <- ks %>%
  filter(state == "failed") %>%
  filter(usd_pledged_real == max(usd_pledged_real, na.rm = TRUE)) %>%
  pull(name)

# How much total money was pledged to projects that weren't marked successful?
total_pledge_unsuccessful <- ks %>%
  filter(state != "successful") %>%
  pull(usd_pledged_real) %>%
  sum(na.rm = TRUE)

# Performing analysis by *grouped* observations ----------------- (31 Points)

# Which category had the most money pledged (total)?
category_most_pledged <- ks %>%
  group_by(category) %>%
  summarize(sum_pledged = sum(usd_pledged_real, na.rm = TRUE)) %>%
  filter(sum_pledged == max(sum_pledged, na.rm = TRUE)) %>%
  pull(category)

# Which country had the most backers?
country_most_backers <- ks %>%
  group_by(country) %>%
  summarize(sum_backers = sum(backers, na.rm = TRUE)) %>%
  filter(sum_backers == max(sum_backers, na.rm = TRUE)) %>%
  pull(country)

# Which year had the most money pledged (hint: you may have to create a new
# column)?
# Note: To answer this question you can choose to get the year from either
# deadline or launched dates.
year_most_pledged <- ks %>%
  mutate(deadline_year = (substring(deadline, 1, 4))) %>%
  group_by(deadline_year) %>%
  summarize(sum_pledged = sum(usd_pledged_real, na.rm = TRUE)) %>%
  filter(sum_pledged == max(sum_pledged, na.rm = TRUE)) %>%
  pull(deadline_year)

# Write one sentance below on why you chose deadline or launched dates to
# get the year from:
# Deadline would be the most appropriate year because pledges by the "crowd"
# can be made after the launched dates (at least to my knowledge), so to
# accurately idenify the total money pledged it would have to be at the end/
# deadline of the startup.

# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_3_categorties <- ks %>%
  mutate(deadline_year = (substring(deadline, 1, 4))) %>%
  group_by(main_category) %>%
  filter(deadline_year == "2018") %>%
  summarize(sum_backed = sum(backers, na.rm = TRUE)) %>%
  arrange(-sum_backed) %>%
  top_n(3) %>%
  pull(main_category)

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
most_day_launch <- ks %>%
  mutate(day_of_week = (weekdays(as.Date(launched)))) %>%
  group_by(day_of_week) %>%
  summarize(count_of_day = n()) %>%
  filter(count_of_day == max(count_of_day, na.rm = TRUE)) %>%
  pull(day_of_week)

# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were marked successful )? This might require creative problem solving...
# Hint: Try googling "r summarize with condition in dplyr"
least_successful_day_launch <- ks %>%
  mutate(day_of_week = (weekdays(as.Date(launched)))) %>%
  group_by(day_of_week) %>%
  summarize(rate = sum(state == "successful") / n()) %>%
  filter(rate == min(rate, na.rm = TRUE)) %>%
  pull(day_of_week)
