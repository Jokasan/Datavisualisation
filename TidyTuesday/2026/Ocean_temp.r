######################################
## Author: Nils Indreiten           ##
## Date: 2026-03-31                 ##
## Description: This script         ##
## explores ocean temperatures,     ##
## part of TidyTuesday 2026-03-31   ##
## dataset.                         ##
######################################

# Load libraries and fonts:

library(tidyverse)
library(showtext)


font_add_google("Domine", family = "domine")
font_add_google("Rethink Sans", family = "rethink")
showtext_auto()
showtext_opts(dpi = 300)

# Retrieve and wrangle data:

ocean_temperature <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature.csv"
)

# Filter to last 4 years and prepare variables
plot_data <- ocean_temperature %>%
  mutate(year = year(date)) %>%
  filter(year >= 2022, year <= 2026) %>%
  mutate(
    day_of_year = as.Date(
      paste0("2000-", format(date, "%m-%d")),
      format = "%Y-%m-%d"
    ),
    depth_fct = fct_rev(factor(
      paste0(sensor_depth_at_low_tide_m, " m"),
      levels = paste0(sort(unique(sensor_depth_at_low_tide_m)), " m")
    )),
    year_fct = factor(year)
  ) |>
  complete(day_of_year, depth_fct, year_fct)

# Plot:

heatmap <- ggplot(
  plot_data,
  aes(x = day_of_year, y = depth_fct, fill = mean_temperature_degree_c)
) +
  geom_tile(
    data = ~ filter(.x, is.na(mean_temperature_degree_c)),
    fill = "lightgrey",
    alpha = 0.3,
    width = 1,
    height = 0.9
  ) +
  geom_tile(width = 1, height = 0.9) +

  facet_wrap(~year_fct, ncol = 1, strip.position = "top") +

  scale_fill_gradientn(
    colours = c(
      "#1B2A49",
      "#1D6996",
      "#73AF48",
      "#EDAD08",
      "#E17C05",
      "#CC503E"
    ),
    na.value = "transparent",
    name = "Mean Temp (°C)",
    breaks = seq(0, 25, 5),
    labels = scales::label_number(suffix = "°C"),
    guide = guide_colourbar(
      barwidth = 15,
      barheight = 0.4,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +

  scale_x_date(
    breaks = as.Date(paste0("2000-", sprintf("%02d", 1:12), "-01")),
    date_labels = "%b",
    expand = expansion(mult = c(0.01, 0.01))
  ) +

  labs(
    title = "4 Years Beneath the Surface",
    subtitle = "Daily mean ocean temperature at Birchy Head, Nova Scotia, by ocean sensor depth.\nWarmer temperatures seep to ocean depths and are lasting longer into the year. Light grey\nareas indicate missing data.",
    x = NULL,
    y = NULL,
    caption = "Data: Centre for Marine Applied Research · Coastal Monitoring Program | #TidyTuesday 2026-03-31"
  ) +

  theme_minimal(base_family = "rethink", base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(
      family = "domine",
      size = 18,
      face = "bold",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 11,
      colour = "grey40",
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 8,
      colour = "grey55",
      margin = margin(t = 10)
    ),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text.x.top = element_text(
      angle = 0,
      face = "bold",
      size = 12,
      family = "rethink",
      colour = "#2C3E50",
      hjust = 0
    ),
    panel.grid = element_blank(),
    panel.spacing.y = unit(1.2, "lines"),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    plot.margin = margin(15, 15, 10, 15),
    plot.background = element_rect(fill = "#FAFAFA", colour = NA)
  )

ggsave("ocean_temp.png", heatmap, width = 7, height = 10, dpi = 300)
