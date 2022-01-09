library("dplyr")
library("tidyr")
library("ggplot2")
library("lintr")

# Depending on the type of institution the school is, the price for tuition can
# vary widely, potentially affecting those with lower incomes more.
# With these varieties in tuition rates, do different income levels have
# varying amounts of debt?

# read data
raw <- read.csv("data/insitution-data-cleaned.csv", stringsAsFactors = FALSE)

# select type, and low, med, and high debt medians
# filter NULL and PrivacySupressed values
# group rows by type
# summarize the mean for each income level for each type
final_chart <- raw %>%
  select(
    type = TYPE,
    grad_debt = GRAD_DEBT_MDN,
    lo_inc = MN_EARN_WNE_INC1_P10,
    md_inc = MN_EARN_WNE_INC2_P10,
    hi_inc = MN_EARN_WNE_INC3_P10
  ) %>%
  filter(
    grad_debt != "NULL" &
    grad_debt != "PrivacySuppressed" &
    lo_inc != "NULL" &
    lo_inc != "PrivacySuppressed" &
    md_inc != "NULL" &
    md_inc != "PrivacySuppressed" &
    hi_inc != "NULL" &
    hi_inc != "PrivacySuppressed"
  ) %>%
  group_by(type) %>%
  summarize(
    avg = mean(as.numeric(grad_debt)),
    max = max(as.numeric(grad_debt)),
    low = mean(as.numeric(lo_inc)),
    medium = mean(as.numeric(md_inc)),
    high = mean(as.numeric(hi_inc))
  ) %>%
  mutate(type = replace(type, type == 1, "public")) %>%
  mutate(type = replace(type, type == 2, "private")) %>%
  mutate(type = replace(type, type == 3, "private_for_profit"))

# set type of institution in a custom order
final_chart$type <- factor(final_chart$type, levels = c("public", "private",
                                                        "private_for_profit"))

# wangle data: select income columns
# gather into dollars and income columns
income_type <- final_chart %>%
  select(type, avg, max, low, medium, high) %>%
  gather(key = debt_and_income, value = amount, -type)

# set level of income in a custom order (for visual chart and legend)
income_type$debt_and_income <- factor(income_type$debt_and_income,
                                      levels = c("max", "avg", "low", "medium",
                                                            "high"))

# plot dollar earned or borrowed by debt_and_income vs. type of institution
g_income_type <- ggplot(income_type) +
  geom_col(
    mapping = aes(x = type, y = amount, fill = debt_and_income),
    position = "dodge"
  ) +
  labs(
    x = "Type of Institution",
    y = "Dollars ($)",
    title = "Debt/Earning for Income Levels vs. Types of Institution",
    subtitle = "(Income level from FAFSA:
    low = $0-$30,000, medium = $30,001-$75,000, high = $75,001+)"
  )