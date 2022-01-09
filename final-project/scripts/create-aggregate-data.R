library("dplyr")
library("ggplot2")
library("plotly")
library("styler")
library("lintr")

# Students will often go to "prestige" college even if the cost is much higher
# Does the potential earnings of "prestige" colleges out weigh the cost?

# read in data
data <- read.csv("data/insitution-data-cleaned.csv", stringsAsFactors = FALSE)

# convert null adm_rate to 1
data$ADM_RATE[data$ADM_RATE == "NULL"] <- 1

# select adm, cost, earnings
# filter out nulls and privacy surpressed
# adj cost for years in college
explore <- data %>%
  filter(HIGHDEG != "NULL" &
           COST != "NULL" &
           MN_EARN_WNE_P6 != "NULL" &
           MN_EARN_WNE_P6 != "PrivacySuppressed" &
           MN_EARN_WNE_P8 != "NULL" &
           MN_EARN_WNE_P8 != "PrivacySuppressed" &
           MN_EARN_WNE_P10 != "NULL" &
           MN_EARN_WNE_P10 != "PrivacySuppressed") %>%
  mutate(cost = as.numeric(COST) / (4 / as.numeric(HIGHDEG))) %>%
  select(adm = ADM_RATE,
         earn_6 = MN_EARN_WNE_P6,
         earn_8 = MN_EARN_WNE_P8,
         earn_10 = MN_EARN_WNE_P10,
         cost)

# convert columns to numeric
explore$adm <- as.numeric(explore$adm)
explore$earn_6 <- as.numeric(explore$earn_6)
explore$earn_8 <- as.numeric(explore$earn_8)
explore$earn_10 <- as.numeric(explore$earn_10)
explore$cost <- as.numeric(explore$cost)

# subtract cost
explore$earn_6 <- explore$earn_6 - explore$cost
explore$earn_8 <- explore$earn_8 - explore$cost
explore$earn_10 <- explore$earn_10 - explore$cost


# plot earnings - cost vs. adm rate
g_earnings <- ggplot(data = explore, mapping = aes(x = adm),
                     show.legend = TRUE) +
  geom_smooth(mapping = aes(y = earn_6, color = "6 Years"),
              method = "loess",
              formula = y ~ x,
              show.legend = TRUE) +
  geom_smooth(mapping = aes(y = earn_8, color = "8 Years"),
              method = "loess",
              formula = y ~ x,
              show.legend = TRUE) +
  geom_smooth(mapping = aes(y = earn_10, color = "10 Years"),
              method = "loess",
              formula = y ~ x,
              show.legend = TRUE) +
  scale_x_reverse() +
  labs(x = "College Admission Rate (%)",
       y = "Student Earnings ($)",
       title = "Yearly Student Mean Earnings - Yearly Cost vs. College Admission Rate",
       subtitle = "(6, 8, and 10 years after College Enrollment)")