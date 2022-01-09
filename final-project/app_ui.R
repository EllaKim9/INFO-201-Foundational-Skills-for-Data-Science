# Load libraries so they are available
library("shiny")

# load earnings ui components
source("ui_components/earnings_ui.R")


source("ui_components/intro_ui.R")
source("ui_components/conclusion_ui.R")
source("ui_components/map_ui.R")
source("ui_components/type_ui.R")

ui <- navbarPage(theme = "styles.css",
                 "Are Universities Still Worth It?",
                 intro,
                 earnings_panel,
                 map_panel,
                 type_panel,
                 conclusion)