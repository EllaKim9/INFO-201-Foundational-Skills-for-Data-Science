library(dplyr)


data_df <- read.csv("data/insitution-data-cleaned.csv", stringsAsFactors = FALSE)
majors_df <- read.csv("data/Most-Recent-Field-Data-Elements.csv",
  stringsAsFactors = FALSE
)


col_mean <- function(data_df, col) {
  ret <- data_df %>%
    filter(
      !!as.name(col) != "NULL",
      !!as.name(col) != "PrivacySuppressed"
    ) %>%
    summarise(paste0("$", prettyNum(round(
      mean(as.numeric(!!as.name(col)))), ","))) %>%
    pull()
  ret
}


summary_info <- function(data_df, majors_df) {
  ret <- list()

  # Average cost of university in USA
  ret$avg_cost <- col_mean(data_df, "COST")

  # Amount of schools in the study
  ret$total_schools <- prettyNum(nrow(data_df), ",")

  # Average school debt in the USA per School
  ret$avg_debt <- col_mean(data_df, "GRAD_DEBT_MDN")


  # Best major and worse majors by revenue
  exp_major <- majors_df %>%
    select(CIPDESC, DEBTMEAN, MD_EARN_WNE) %>%
    filter(DEBTMEAN != "NULL", DEBTMEAN != "PrivacySuppressed") %>%
    filter(MD_EARN_WNE != "Null", MD_EARN_WNE != "PrivacySuppressed") %>%
    group_by(CIPDESC) %>%
    mutate(revenue = as.numeric(MD_EARN_WNE) - as.numeric(DEBTMEAN)) %>%
    summarize(
      avg_debt = mean(as.numeric(DEBTMEAN)),
      mean_rev = mean(revenue)
    )

  ret$max_rev <- exp_major %>%
    filter(mean_rev == max(mean_rev)) %>%
    summarize(
      max = paste0("$", prettyNum(mean_rev, ",")),
      major = CIPDESC
    )

  ret$min_rev <- exp_major %>%
    filter(mean_rev == min(mean_rev)) %>%
    summarize(
      min = prettyNum(mean_rev, ","),
      major = CIPDESC
    )

  #gets the negative sign before the dollar sign if it's a negative number
  if (startsWith(ret$min_rev$min, "-")) {
    ret$min_rev$min <- paste0("-$", substring(ret$min_rev$min, 2))
  }


  # Most expensive school
  min_max <- data_df %>%
    filter(COST != "NULL", COST != "PrivacySuppressed") %>%
    group_by(NAME) %>%
    summarize(cost = mean(as.numeric(COST), na.rm = T))

  ret$most_expensive <- min_max %>%
    filter(cost == max(cost)) %>%
    summarize(
      cost = paste0("$", prettyNum(cost, ",")),
      name = NAME
    )
  ret$least_expensive <- min_max %>%
    filter(cost == min(cost)) %>%
    summarize(
      cost = paste0("$", prettyNum(cost, ",")),
      name = NAME
    )

  ret
}