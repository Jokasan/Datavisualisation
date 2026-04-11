######################################
## Author: Nils Indreiten           ##
## Date: 2026-03-31                 ##
## Description: This script         ##
## explores ocean temperatures,     ##
## part of TidyTuesday 2026-03-31   ##
## dataset.                         ##
######################################

# Load libraries and fonts:

pacman::p_load(
  tidyverse,
  ggplot2,
  showtext,
  glue,
  scales
)

font_add_google("Domine", "domine")
font_add_google("Rethink Sans", "rethink")
showtext_auto()

# Load data & wrangle:
repairs <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-07/repairs.csv'
)

# Palette:
palette_outcome <- c(
  "Yes" = "#4a7c59", # deep green
  "Partial" = "#e8a838", # amber
  "No" = "#c0392b" # terracotta red
)

# Custom theme:
theme_repair <- function() {
  theme_minimal(base_family = "rethink", base_size = 11) +
    theme(
      plot.title = element_text(
        family = "domine",
        size = 16,
        face = "bold",
        color = "#2c2c2c",
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        family = "rethink",
        size = 11,
        color = "#555555",
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        family = "rethink",
        size = 8,
        color = "#888888",
        hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = element_rect(fill = "#faf8f4", color = NA),
      panel.background = element_rect(fill = "#faf8f4", color = NA),
      panel.grid.major = element_line(color = "#e0dbd2", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#444444"),
      legend.position = "top",
      legend.title = element_blank()
    )
}

# Some useful wrangling for plot:

top_countries <- repairs |>
  filter(!is.na(country)) |>
  count(country, sort = TRUE) |>
  slice_head(n = 6) |>
  pull(country)

plot3_data <- repairs |>
  filter(country %in% top_countries, !is.na(repair_date)) |>
  mutate(
    year = year(repair_date),
    country = factor(country, levels = rev(top_countries)) # top country at bottom
  ) |>
  filter(year >= 2015, year <= 2025) |>
  count(country, year)

# We use a line + ribbon area chart in small multiples (cleaner than ridgelines for count data), and annotate:
country_totals <- repairs |>
  filter(country %in% top_countries) |>
  count(country) |>
  mutate(label = glue("{country}\n({scales::comma(n)} total)"))

country_label_map <- setNames(country_totals$label, country_totals$country)

plot_data <- plot_data |>
  mutate(
    country_label = factor(
      country_label_map[as.character(country)],
      levels = country_label_map[rev(top_countries)]
    )
  )

plot_data <- plot_data |>
  mutate(country_label = forcats::fct_rev(country_label))

# Create plot:
repair_plot <- ggplot(
  plot_data,
  aes(x = year, y = n, fill = country_label, color = country_label)
) +
  geom_area(alpha = 0.25, linewidth = 0) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~country_label, ncol = 2, scales = "free_y", ) +
  scale_x_continuous(breaks = c(2015, 2018, 2021, 2024)) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(
    values = colorRampPalette(c(
      "#4a7c59",
      "#a8c5a0",
      "#e8a838",
      "#c0392b",
      "#2c5f8a",
      "#8e6bbf",
      "#d4845a",
      "#5c9e9e"
    ))(8)
  ) +
  scale_color_manual(
    values = colorRampPalette(c(
      "#4a7c59",
      "#a8c5a0",
      "#e8a838",
      "#c0392b",
      "#2c5f8a",
      "#8e6bbf",
      "#d4845a",
      "#5c9e9e"
    ))(8)
  ) +
  labs(
    title = "The Repair Café movement is surging",
    subtitle = "Annual number of logged repairs for the 6 most active countries · 2015–2025",
    x = NULL,
    y = "Number of repairs",
    caption = "Source: Repair Monitor | #TidyTuesday 2026-04-07"
  ) +
  theme_repair() +
  theme(
    legend.position = "none",
    strip.text = element_text(
      family = "rethink",
      size = 9.5,
      face = "bold",
      color = "#333333"
    ),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1.2, "lines")
  )

ggsave(
  "repair_growth_by_country.png",
  repair_plot,
  width = 7,
  height = 10,
  dpi = 300,
  bg = "#faf8f4"
)
