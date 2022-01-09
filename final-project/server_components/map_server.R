library("dplyr")
library("ggplot2")
library("plotly")

# for map_data() and coor_map()
library("maps")
library("mapproj")

# read in data
data <- read.csv("data/insitution-data-cleaned.csv", stringsAsFactors = FALSE)

# Load a shapefile of U.S. states using ggplot's `map_data()` function
state_shape <- map_data("state")


# Organize data by state, calc mean debt
plot_data <- data %>%
  select(
    state = STATE,
    debt = GRAD_DEBT_MDN,
    cost = COST,
    default = CDR3
  ) %>%
  filter(debt != "PrivacySuppressed", debt != "NULL",
         cost != "NULL", default != "NULL") %>%
  group_by(state) %>%
  summarize(
    debt = mean(as.numeric(debt)),
    cost = mean(as.numeric(cost)),
    default = mean(as.numeric(default))
  ) %>%
  mutate(state = tolower(state.name[match(state, state.abb)])) %>%
  filter(!is.na(state))

# Join state shapes and data
state_shape <- map_data("state") %>% # load state shapefile
  rename(state = region) %>% # rename for joining
  left_join(plot_data, by = "state") # join eviction data


# Theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

# get title/legend title for map
titles <- list(
  test = "test",
  debt = c(
    "Average Student Debt by State",
    "Average Graduation Debt ($)"
  ),
  cost = c(
    "Average Tuition Cost by State",
    "Average Tuition Cost ($)"
  ),
  default = c(
    "Average Cohort Default Rate by State",
    "Average Default Rate"
  )
)


# creates map of U.S.
map_plot <- function(input) {
  p <- renderPlotly({
    g_us <- ggplot(state_shape) +
      geom_polygon(
        mapping = aes_string(x = "long", y = "lat",
                             group = "group", fill = input$map_data),
        color = "white", # show state outlines
        size = .1 # thinly stroked
      ) +
      coord_map() + # use a map-based coordinate system
      scale_fill_continuous(low = "blue", high = "red") +
      labs(
        fill = titles[[input$map_data]][2],
        title = titles[[input$map_data]][1]
      ) +
      blank_theme # theme

    p_map <- ggplotly(g_us)
    return(p_map)
  })
  p
}