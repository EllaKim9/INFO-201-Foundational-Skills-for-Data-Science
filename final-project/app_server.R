# Load libraries so they are available
library("shiny")
library("ggplot2")

source("server_components/earnings_server.R")
source("server_components/map_server.R")
source("server_components/type_server.R")

# Define a server function
server <- function(input, output) {
  # returns earnings plot
  output$earnings <- earnings_plot(input)
  output$map <- map_plot(input)
  output$type <- type_plot(input)
}