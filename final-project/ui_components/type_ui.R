library(plotly)
source("ui_components/analysis_ui.R")


# for type
checkbox_content <- sidebarPanel(
  selectInput(
    inputId = "diff_type",
    label = "Type of Institute",
    # choiceNames = c("Average Debt",
    # "Low Income Earnings",
    # "Medium Income Earnings",
    # "High Income Earnings"),
    choices = c("avg_debt", "low_earning", "medium_earning", "high_earning")
  )
)

# for type
main_content <- mainPanel(
  plotlyOutput("type"),
)


# type page
type_panel <- tabPanel(
  "Institution/Income Graph",
  type_intro,
  # creates two columns
  sidebarLayout(
    # side
    checkbox_content,
    # main
    main_content
  ),
  type_analysis
)