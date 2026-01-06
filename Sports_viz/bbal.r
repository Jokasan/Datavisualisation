pacman::p_load(hoopR, skimr, tidyverse, sportyR, devtools)
devtools::install_github("ys-xue/ggbasketball")
library(ggbasketball)

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
    loc_x = coordinate_x_raw - 25, # Center x coordinate (convert 0-50 to -25 to 25)
    loc_y = coordinate_y_raw - 44
  ) %>% # Keep y as is (already in feet)
  select(
    athlete_id_1,
    loc_x,
    loc_y,
    shot_made_flag = scoring_play
  ) -> curry_shots

write.csv(
  curry_shots,
  "/Users/joka/Documents/JS/bbal_viz/src/data/curry_shots.csv",
  row.names = FALSE
)

nba_pbp %>%
  skimr::skim()

# Some visualisation:

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
# Heatmap showing shooting percentage by location
geom_basketball("nba", rotation = 270) +
  stat_summary_2d(
    data = curry_shots |>
      filter(athlete_id_1 == 1966),
    aes(x = loc_x, y = loc_y, z = shot_made_flag),
    fun = mean,
    bins = 50,
    alpha = 0.8
  ) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#fee08b",
    high = "#1a9850",
    midpoint = 0.5,
    name = "FG%",
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  theme_void() +
  theme(legend.position = "right")


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

# plotly::ggplotly(p)

# Made not made:

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
  # labs(
  #   title = "Steph Curry shot result concentration",
  #   subtitle = "2024 season"
#  ) +
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(t = -30, r = 0, b = 0, l = 0)
    # plot.title = element_text(hjust = 0,vjust = 1.5, face = "bold", margin = margin(l = 32)),
    # plot.subtitle = element_text(hjust = 0, margin = margin(l = 32))
  )

