######################################
## Author: Nils Indreiten           ##
## Date: 2026-04-14                 ##
## Description: This script         ##
## explores bird sea sightings,     ##
## part of TidyTuesday 2026-04-14   ##
## dataset.                         ##
######################################

# Load libraries and fonts:

pacman::p_load(
  tidyverse,
  ggplot2,
  showtext,
  glue,
  scales,
  rnaturalearth
)

font_add_google("Domine", "domine")
font_add_google("Rethink Sans", "rethink")
showtext_auto()
showtext_opts(dpi = 300)

# Load data:
beaufort_scale <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/beaufort_scale.csv"
)
birds <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/birds.csv"
)
sea_states <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/sea_states.csv"
)
ships <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/ships.csv"
)


# Do some wrangling:

bird_ship <- birds |>
  filter(!is.na(species_common_name)) |>
  inner_join(ships, by = "record_id")

nz_coast <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(admin %in% c("New Zealand", "Australia"))

top_observers <- ships |>
  filter(!is.na(observer)) |>
  count(observer, sort = TRUE) |>
  slice_head(n = 6) |>
  pull(observer)

observer_tracks <- bird_ship |>
  filter(
    observer %in% top_observers,
    !is.na(latitude),
    !is.na(longitude),
    count < 99999
  ) |>
  arrange(observer, date, time) |>
  group_by(record_id, observer, date, time, latitude, longitude) |>
  summarise(total_birds = sum(count, na.rm = TRUE), .groups = "drop")

# Prepare for plot:

# Colour palette: 6 muted sea-inspired tones
obs_colours <- c(
  "#E07A5F",
  "#3D85C6",
  "#81B29A",
  "#F2CC8F",
  "#6A4C93",
  "#48BFE3"
)
names(obs_colours) <- top_observers

p2 <- ggplot() +
  geom_sf(
    data = nz_coast,
    fill = "#1a1a2e",
    colour = "#334455",
    linewidth = 0.3
  ) +
  geom_path(
    data = observer_tracks,
    aes(x = longitude, y = latitude, group = observer),
    colour = "white",
    alpha = 0.08,
    linewidth = 0.3
  ) +
  geom_point(
    data = observer_tracks,
    aes(x = longitude, y = latitude, size = total_birds),
    colour = "#E07A5F",
    alpha = 0.35,
    shape = 16
  ) +
  scale_size_continuous(
    range = c(1, 8),
    breaks = c(10, 100, 500),
    name = "Total birds per record",
    labels = scales::comma,
    guide = guide_legend(
      override.aes = list(alpha = 0.8)
    )
  ) +
  coord_sf(
    xlim = c(148, 180),
    ylim = c(-52, -24),
    crs = 4326
  ) +
  facet_wrap(~observer, ncol = 2) +
  labs(
    title = "Six Observers, Six Voyages",
    subtitle = "Each panel maps one observer's sighting locations across the Tasman Sea\nand Southern Ocean. Point size reflects total bird count per observation.",
    caption = "Data: Te Papa Tongarewa — At-Sea Observations of Seabirds 1969–1990 | #TidyTuesday 2026-04-14"
  ) +
  theme_minimal(base_family = "rethink") +
  theme(
    plot.background = element_rect(fill = "#0f1b2d", colour = NA),
    panel.background = element_rect(fill = "#0f1b2d", colour = NA),
    strip.background = element_blank(),
    strip.text = element_text(
      colour = "#C8D6E5",
      family = "rethink",
      face = "bold",
      size = 35
    ),
    panel.grid = element_line(colour = "#1a2940", linewidth = 0.15),
    panel.spacing = unit(0.5, "lines"),
    text = element_text(colour = "#C8D6E5"),
    axis.text.x = element_text(
      colour = "#556677",
      size = 30,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(colour = "#556677", size = 30),
    axis.title = element_blank(),
    plot.title = element_text(
      family = "domine",
      size = 52,
      face = "bold",
      colour = "#F7ECB5",
      margin = margin(b = 2)
    ),
    plot.title.position = "plot",
    plot.subtitle = element_text(
      colour = "#8899AA",
      size = 32,
      lineheight = 0.25,
      margin = margin(b = 4)
    ),
    plot.caption = element_text(
      colour = "#556677",
      size = 20,
      margin = margin(t = 0, r = 50)
    ),
    legend.title = element_text(colour = "#8899AA", size = 35),
    legend.text = element_text(colour = "#8899AA", size = 35),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(8, 0, 2, 0)
  )

ggsave(
  "plot2_observer_voyages.png",
  p2,
  width = 7,
  height = 10,
  dpi = 300,
  bg = "#0f1b2d"
)
