library("dplyr")
library("ggplot2")
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
    MN_EARN_WNE_P7 != "NULL" &
    MN_EARN_WNE_P7 != "PrivacySuppressed" &
    MN_EARN_WNE_P8 != "NULL" &
    MN_EARN_WNE_P8 != "PrivacySuppressed" &
    MN_EARN_WNE_P9 != "NULL" &
    MN_EARN_WNE_P9 != "PrivacySuppressed" &
    MN_EARN_WNE_P10 != "NULL" &
    MN_EARN_WNE_P10 != "PrivacySuppressed") %>%
  mutate(cost = as.numeric(COST) / (4 / as.numeric(HIGHDEG))) %>%
  select(
    adm = ADM_RATE,
    earn_6 = MN_EARN_WNE_P6,
    earn_7 = MN_EARN_WNE_P7,
    earn_8 = MN_EARN_WNE_P8,
    earn_9 = MN_EARN_WNE_P9,
    earn_10 = MN_EARN_WNE_P10,
    cost
  )

# convert columns to numeric
explore$adm <- as.numeric(explore$adm)
explore$earn_6 <- as.numeric(explore$earn_6)
explore$earn_7 <- as.numeric(explore$earn_7)
explore$earn_8 <- as.numeric(explore$earn_8)
explore$earn_9 <- as.numeric(explore$earn_9)
explore$earn_10 <- as.numeric(explore$earn_10)
explore$cost <- as.numeric(explore$cost)

# subtract cost
explore$earn_6 <- explore$earn_6 - explore$cost
explore$earn_7 <- explore$earn_7 - explore$cost
explore$earn_8 <- explore$earn_8 - explore$cost
explore$earn_9 <- explore$earn_9 - explore$cost
explore$earn_10 <- explore$earn_10 - explore$cost

earnings_plot <- function(input) {
  p <- renderPlotly({
    # plot earnings - cost vs. adm rate
    g_earnings <- ggplot(
      data = explore, mapping = aes(x = adm),
      show.legend = TRUE
    ) +
      geom_smooth(
        mapping = aes_string(y = paste0("earn_", input$year)),
        method = "loess",
        formula = y ~ x,
        show.legend = TRUE
      ) +
      labs(
        x = "Uni Admission Rate (%)",
        y = "Student Earnings ($)",
        title = "Yearly Student Revenue (Mean Earnings - Cost)
                    vs. Uni Admission Rate",
        subtitle = "(6, 8, and 10 years after Uni Enrollment)"
      )

    p_earnings <- ggplotly(g_earnings)

    return(p_earnings)
  })

  p
}