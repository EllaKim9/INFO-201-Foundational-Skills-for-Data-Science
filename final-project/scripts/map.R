library("dplyr")
library("ggplot2")
library("plotly")
library("styler")
library("lintr")
# for map_data() and coor_map()
library("maps")
library("mapproj")

# read in data
data <- read.csv("data/insitution-data-cleaned.csv", stringsAsFactors = FALSE)

# Load a shapefile of U.S. states using ggplot's `map_data()` function
state_shape <- map_data("state")


# Organize data by state, calc mean debt
plot_data <- data %>%
  select(state = STATE,
         debt = GRAD_DEBT_MDN) %>%
  filter(debt != "PrivacySuppressed", debt != "NULL") %>%
  group_by(state) %>%
  summarize(debt = mean(as.numeric(debt))) %>%
  mutate(state = tolower(state.name[match(state, state.abb)])) %>%
  filter(!is.na(state))

# Join state shapes and data
state_shape <- map_data("state") %>% # load state shapefile
  rename(state = region) %>% # rename for joining
  left_join(plot_data, by = "state") # join eviction data


# Theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# creates map of U.S.
g_us <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = debt),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "lightgrey", high = "Red") +
  labs(fill = "Average Graduation Debt ($)",
       title = "Average Student Debt By State") +
  blank_theme # theme



p_us <- ggplotly(g_us)