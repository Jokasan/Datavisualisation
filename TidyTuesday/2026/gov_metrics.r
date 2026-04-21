######################################
## Author: Nils Indreiten           ##
## Date: 2026-04-21                 ##
## Description: This script         ##
## explores health spending in the  ##
## G7 part of TidyTuesday           ##
## 2026-04-21 dataset.              ##
######################################

# Load libraries and fonts:
library(tidyverse)
library(ggplot2)
library(showtext)

font_add_google("Domine", "domine")
font_add_google("Rethink Sans", "rethink")
showtext_auto()

health_spending <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-21/health_spending.csv'
)

# Prep the data:

g7_countries <- c(
  "Canada",
  "France",
  "Germany",
  "Italy",
  "Japan",
  "United Kingdom",
  "United States of America"
)

g7_labels <- c(
  "Canada" = "Canada",
  "France" = "France",
  "Germany" = "Germany",
  "Italy" = "Italy",
  "Japan" = "Japan",
  "United Kingdom" = "UK",
  "United States of America" = "USA"
)

g7_colours <- c(
  "Canada" = "#D62828", # red
  "France" = "#003189", # deep blue
  "Germany" = "#FFBE00", # gold
  "Italy" = "#009246", # green
  "Japan" = "#BC002D", # crimson
  "UK" = "#CF142B", # red (distinguished via linetype)
  "USA" = "#3C3B6E" # navy
)

time_points <- c(2000, 2005, 2010, 2015, 2019, 2021, 2023)

# Wrangle data:
bump_data <- health_spending |>
  filter(
    indicator_code == "gghed_che",
    country_name %in% g7_countries,
    year %in% time_points
  ) |>
  mutate(
    short_name = g7_labels[country_name]
  ) |>
  group_by(year) |>
  mutate(rank = rank(-value, ties.method = "first")) |>
  ungroup()

# Endpoint label frames
left_labels <- bump_data |>
  filter(year == min(time_points))

right_labels <- bump_data |>
  filter(year == max(time_points))

# Midpoint annotation data: COVID years for a bracket annotation
covid_years <- tibble(
  xmin = 2019,
  xmax = 2021,
  ymin = 0.35,
  ymax = 7.65
)

# plot the data:

p1 <- ggplot(
  bump_data,
  aes(x = year, y = rank, group = short_name, colour = short_name)
) +

  annotate(
    "rect",
    xmin = 2018.8,
    xmax = 2021.2,
    ymin = 0.4,
    ymax = 7.6,
    fill = "indianred",
    alpha = 0.2,
    colour = NA
  ) +
  annotate(
    "text",
    x = 2020,
    y = 0.3,
    label = "COVID-19 (2019–2021)",
    family = "rethink",
    size = 5,
    colour = "grey45",
    fontface = "bold"
  ) +

  geom_line(
    linewidth = 1.8,
    alpha = 0.8,
    lineend = "round",
    linejoin = "round"
  ) +

  geom_point(size = 8, colour = "white") +

  geom_point(size = 8, alpha = 0.92) +

  geom_text(
    aes(label = rank),
    colour = "white",
    size = 6,
    fontface = "bold",
    family = "rethink"
  ) +

  geom_text(
    aes(label = paste0(round(value, 0), "%"), colour = short_name),
    size = 6,
    family = "rethink",
    vjust = 5.2,
    fontface = "bold"
  ) +

  geom_text(
    data = left_labels,
    aes(
      x = min(time_points),
      y = rank,
      label = short_name,
      colour = short_name
    ),
    hjust = 1,
    nudge_x = -0.8,
    size = 8,
    fontface = "bold",
    family = "rethink",
    inherit.aes = FALSE
  ) +

  geom_text(
    data = right_labels,
    aes(
      x = max(time_points),
      y = rank,
      label = paste0(short_name, "  ", round(value, 0), "%"),
      colour = short_name
    ),
    hjust = 0,
    nudge_x = 0.8,
    size = 8,
    fontface = "bold",
    family = "rethink",
    inherit.aes = FALSE
  ) +

  scale_colour_manual(values = g7_colours) +

  scale_x_continuous(
    breaks = time_points,
    expand = expansion(mult = c(0.18, 0.20))
  ) +

  scale_y_reverse(
    breaks = 1:7,
    expand = expansion(add = 0.65)
  ) +

  labs(
    title = "Domestic health spending as a percentage of current health expenditure in the G7",
    subtitle = "Japan consistently remains committed to government health financing\nwhile the USA's share is consistently lowest",
    x = NULL,
    y = NULL,
    caption = "Source: WHO Global Health Expenditure Database (GHED) · #TidyTuesday 2026-04-21"
  ) +

  theme_minimal(base_family = "rethink", base_size = 20) +
  theme(
    plot.title = element_text(
      family = "domine",
      size = 30,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 5)
    ),
    plot.title.position = "plot",
    plot.subtitle = element_text(
      size = 28,
      colour = "grey40",
      lineheight = .4,
      hjust = 0.5,
      margin = margin(b = -12)
    ),
    plot.background = element_rect(fill = "#FAFAF8", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      size = 20,
      face = "bold",
      colour = "grey30"
    ),
    axis.text.y = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0.5)
  )

ggsave(
  "g7_bump_chart.png",
  plot = p1,
  width = 7,
  height = 6,
  dpi = 300,
  bg = "#FAFAF8"
)
