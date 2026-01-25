######################################
## Author: Nils Indreiten           ##
## Date: 2026-01-13                 ##
## Description: This script         ##
## explores NBA data using the API  ##
## and creates a heatmap.           ##
######################################

# Load packages:
# devtools::install_github("ys-xue/ggbasketball")
pacman::p_load(hoopR, skimr, tidyverse, sportyR, devtools, ggbasketball, patchwork, magick)

# Retrieve data:
nba_team_box <- hoopR::load_nba_team_box(2024)

# NBA player box data, 2022-2024 seasons:
nba_player_box <- hoopR::load_nba_player_box(2025)

# NBA play by play data, 2022-2024 seasons:

nba_pbp <- hoopR::load_nba_pbp(2025) %>%
  filter(athlete_id_1 %in% c(3975, 1966), shooting_play == TRUE)

nba_pbp %>%
  select(athlete_id_1, coordinate_x_raw, coordinate_y_raw, scoring_play) %>%
  mutate(
    scoring_play = as.integer(scoring_play),
    loc_x = coordinate_x_raw - 25, 
    loc_y = coordinate_y_raw - 44
  ) %>% 
  select(
    athlete_id_1,
    loc_x,
    loc_y,
    shot_made_flag = scoring_play
  ) -> curry_shots

nba_pbp %>%
  skimr::skim()

# Some visualisations:

# simple shot chart
ggshotchart(
  curry_shots |> filter(athlete_id_1 == 3975),
  x = "loc_x",
  y = "loc_y",
  result = shot_made_flag
)
# Now with sports verse package:

ggcourt(orientation = "wide")

# Draw a regulation NBA basketball court

# Density plot:
geom_basketball("nba", display_range = "offense", rotation = 270,
                color_updates = list(
                  plot_background = NULL,
                  defensive_half_court = "#ffffff",
                  offensive_half_court = "#ffffff",
                  court_apron = "#ffffff",
                  center_circle_fill = "#ffffff",
                  two_point_range = "#ffffff",
                  painted_area = "#ffffff",
                  free_throw_circle_fill = "#ffffff")) +
  stat_density_2d(
    data = curry_shots |>
      filter(athlete_id_1 == 3975),
    aes(x = loc_x, y = loc_y, fill = after_stat(level)),
    geom = "polygon",
    alpha = 0.2,
    bins = 10,           # Number of contour levels (higher = more detail)
    h = c(10, 10)          # Bandwidth for smoothing (lower = less smooth, higher = more smooth)
  ) +
  scale_fill_gradient(
    low = "yellow", 
    high = "red",
    name = "Density",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(50, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom"
    )
  ) +
  theme(legend.position = "none")

# Made not made:

# Download Curry image
curry_img_url <- "https://cdn.nba.com/headshots/nba/latest/1040x760/201939.png"
curry_img <- magick::image_read(curry_img_url) |>
  magick::image_scale("200x200")

# Create title area with text
title_plot <- ggplot() +
  annotate("text", x = 0.08, y = 0.6, label = "Steph Curry Shot Concentration", 
           size = 6, fontface = "bold", hjust = 0) +
  annotate("text", x = 0.08, y = 0.4, label = "2024 season", 
           size = 4, hjust = 0) +
  xlim(-0.1, 1) +
  ylim(0, 1) +
  theme_void()

# Create image as grob
img_grob <- grid::rasterGrob(curry_img, interpolate = TRUE)

# Create image plot
img_plot <- ggplot() + 
  annotation_custom(img_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void()

# Create the main plot (without title)
curry_plot <- geom_basketball("nba", display_range = "offense", rotation = 270,
                color_updates = list(
                  plot_background = NULL,
                  defensive_half_court = "#ffffff",
                  offensive_half_court = "#ffffff",
                  court_apron = "#ffffff",
                  center_circle_fill = "#ffffff",
                  two_point_range = "#ffffff",
                  painted_area = "#ffffff",
                  free_throw_circle_fill = "#ffffff")) +
  stat_density_2d(
    data = curry_shots |>
      filter(athlete_id_1 == 3975),
    aes(x = loc_x, y = loc_y, fill = as_factor(shot_made_flag), alpha = after_stat(level)),
    geom = "polygon",
    bins = 40,
    h = c(10, 10)
  ) +
  scale_fill_manual(
    values = c("0" = "grey", "1" = "orange"),
    labels = c("0" = "Missed", "1" = "Made"),
    name = "Shot Result",
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      keyheight = unit(1, units = "mm"),
      keywidth = unit(5, units = "mm")
    )
  ) +
  scale_alpha_continuous(range = c(0.1, 0.6), guide = "none") +
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(t = -90, r = 0, b = 0, l = 0)
  ) +
  labs(caption = "Source: NBA via hoopR package")

# Combine title with image, then stack with main plot
(title_plot + img_plot + plot_layout(widths = c(4, 3))) / curry_plot + 
  plot_layout(heights = c(1, 8)) 

#########
## FIN ##
#########
