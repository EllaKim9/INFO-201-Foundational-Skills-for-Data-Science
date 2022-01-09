# Load app
library("shiny")

# load ui and server
source("app_ui.R")
source("app_server.R")

# launch app
shinyApp(ui = ui, server = server)