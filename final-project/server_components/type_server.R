library("dplyr")
library("tidyr")
library("ggplot2")

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
    avg_debt = mean(as.numeric(grad_debt)),
    low_earning = mean(as.numeric(lo_inc)),
    medium_earning = mean(as.numeric(md_inc)),
    high_earning = mean(as.numeric(hi_inc))
  ) %>%
  mutate(type = replace(type, type == 1, "public")) %>%
  mutate(type = replace(type, type == 2, "private")) %>%
  mutate(type = replace(type, type == 3, "private_for_profit"))

# set type of institution in a custom order
final_chart$type <- factor(final_chart$type, levels = c("public", "private",
                                                        "private_for_profit"))

# plot dollar earned or borrowed by debt_and_income vs. type of institution
type_plot <- function(input) {
  p_income_type <- renderPlotly({
    g_income_type <- ggplot(data = final_chart) +
    geom_col(
      mapping = aes_string(x = "type", y = input$diff_type, fill = "type")
    ) +
    labs(
      x = "Type of Institution",
      y = "Dollars ($)",
      title = paste("Debt/Earning of Income Level vs. Type of Institution")
    )
  p_type <- ggplotly(g_income_type)

  return(p_type)
  })

  p_income_type
}