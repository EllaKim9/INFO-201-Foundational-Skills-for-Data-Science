library(plotly)
source("ui_components/analysis_ui.R")


# for earnings
sidebar_content <- sidebarPanel(
  sliderInput(
    inputId = "year",
    label = "Years after Enrollment", value = 6, min = 6, max = 10
  )
)

# for earnings
main_content <- mainPanel(
  plotlyOutput("earnings"),
)


# earnings page
earnings_panel <- tabPanel(
  "Earnings Graph",
  earnings_intro,

  # creates two columns
  sidebarLayout(
    # side
    sidebar_content,
    # main
    main_content
  ),
  earnings_analysis
)