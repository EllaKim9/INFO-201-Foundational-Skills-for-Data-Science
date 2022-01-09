# map ui

library("styler")
library("lintr")

library(shiny)
source("ui_components/analysis_ui.R")
map_panel <- tabPanel(
  "Map",
  titlePanel("How does the cost of college change throughout the US?"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("map_data", "Map Data",
        choiceNames = c(
          "Average Graduation Debt",
          "Average Tuition Cost",
          "Average Default Rate"
        ),
        choiceValues = c("debt", "cost", "default"),
        selected = "debt"
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "map")
    )
  ),
  map_analysis
)