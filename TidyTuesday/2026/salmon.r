######################################
## Author: Nils Indreiten           ##
## Date: 2026-03-17                 ##
## Description: This script         ##
## explores salmon losses and       ##
## mortality.                       ##
###################################### 

# Load in the fonts:

library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(showtext)

tuesdata <- tidytuesdayR::tt_load(2026, week = 11)

monthly_mortality_data <- tuesdata$monthly_mortality_data |> mutate(date = ymd(date))

font_add_google("Domine", "domine")
font_add_google("Rethink Sans", "rethink")
showtext_auto()

# Do some wrangling and preparation:

salmon_county_yr <- monthly_mortality_data |>
  filter(species == "salmon", geo_group == "county") |>
  mutate(year = year(date)) |>
  group_by(region, year) |>
  summarise(avg_median = mean(median, na.rm = TRUE),
            .groups = "drop")

# Mapping for the regions:
county_lookup <- tribble(
  ~map_name,            ~data_region,
  "Agder",              "Agder & Rogaland",
  "Rogaland",           "Agder & Rogaland",
  "Vestland",           "Vestland",
  "Hordaland",          "Vestland",
  "Sogn og Fjordane",   "Vestland",
  "Møre og Romsdal",    "Møre og Romsdal",
  "Trøndelag",          "Trøndelag",
  "Sør-Trøndelag",      "Trøndelag",
  "Nord-Trøndelag",     "Trøndelag",
  "Nordland",           "Nordland",
  "Troms",              "Troms",
  "Troms og Finnmark",  "Troms",
  "Finnmark",           "Finnmark"
)

# Get the Norway shapefile with the region names & join:
norway_adm1 <- ne_states(country = "Norway", returnclass = "sf") |>
  left_join(county_lookup, by = c("name" = "map_name"))

years_df <- tibble(year = sort(unique(salmon_county_yr$year)))

norway_yearly <- norway_adm1 |>
  cross_join(years_df) |>
  left_join(salmon_county_yr,
            by = c("data_region" = "region", "year" = "year"))

bbox <- st_bbox(c(xmin = 4, xmax = 32, ymin = 57.5, ymax = 71.5),
                crs = st_crs(4326))

val_range <- range(salmon_county_yr$avg_median, na.rm = TRUE)

# Plot the data & some annotations:

region_centroids <- norway_adm1 |>
  filter(!is.na(data_region)) |>
  group_by(data_region) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  mutate(centroid = st_centroid(geometry)) |>
  mutate(
    x = st_coordinates(centroid)[, 1],
    y = st_coordinates(centroid)[, 2]
  ) |>
  st_drop_geometry()


anno <- salmon_county_yr |>
  slice_max(avg_median, n = 1, by = year) |>
  left_join(region_centroids, by = c("region" = "data_region")) |>
  mutate(
    label  = paste0("Highest mortality:\n", region, " (", scales::percent(avg_median, accuracy = 0.1), ")"),

    x_end  = 18,
    y_end  = 65,

    anno_colour = scales::col_numeric(
      palette = c("#2A9D8F", "#E9C46A", "#E76F51", "#8B1A1A"),
      domain  = c(0, 1)
    )(avg_median)
  )


p3b <- ggplot(norway_yearly) +
  geom_sf(aes(fill = avg_median), colour = "white", linewidth = 0.2) +


  geom_curve(
    data = anno,
    aes(x = x_end+4.5, y = y_end-0.5, xend = x, yend = y),
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    curvature = -0.25,
    colour = "grey20", 
    linewidth = 0.2,
    inherit.aes = FALSE,
    alpha=0.5) +
  geom_text(
    data = anno,
    aes(x = x_end, y = y_end, label = label, colour = anno_colour),
    size = 5.5, family = "rethink", fontface = "bold",
    hjust = 0, vjust = 0.5, lineheight = 0.3,
    inherit.aes = FALSE,
  ) +
 
  facet_wrap(~ year, nrow = 2) +
 
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = FALSE) +
 
  scale_fill_gradientn(
    colours  = c("#2A9D8F", "#E9C46A", "#E76F51", "#8B1A1A"),
    na.value = "grey92",
    limits   = c(0.20, 1),
    breaks   = c(seq(0.20, 0.80, by = 0.20), 1.0),
    labels   = scales::percent_format(accuracy = 1, scale = 100),
    name     = "Average monthly Salmon mortality (%)",
    guide    = guide_colorbar(
      barwidth       = 15,
      barheight      = 0.2,
      title.position = "top",
      title.hjust    = 0.5,
      ticks.linewidth = 0.1,
      frame.colour   = NA
    )
  ) +
  scale_colour_identity() +
 
  labs(
    title    = "Where Mortality Hits Hardest",
    subtitle = "Salmon quality of life in aquaculture farming has received more attention over the years in Norway.\nIncreased average mortality is higher in the west coast of the country, particularly in the Vestland\nregion, where the mortality is highest 3 out of the 6 years in the dataset. Grey zones have no aquaculture data.",
    caption  = "Source: Norwegian Veterinary Institute \u2022 #TidyTuesday: 2026-03-17"
  ) +
  theme_minimal(base_family = "rethink", base_size = 15) +
  theme(
    plot.title.position = "plot",
    plot.title    = element_text(
      size =30,
      family = "domine", face = "bold",
      hjust  = 0.5),
      plot.subtitle = element_text(
        size = 25,
        colour = "grey40",
        family = "rethink",
        hjust = 0.5,
        lineheight = 0.4,
        margin = margin(t = 1, b = 8)
      ),

    plot.caption  = element_text(
      size   = 20,
      colour = "grey55",
      family = "rethink",
      hjust  = 0.5,
      margin = margin(t = 6)
    ),
    strip.text    = element_text(
      family = "rethink", face = "bold",
      size   = 20,
      margin = margin(b = 2, t = 2)
    ),
    axis.text     = element_blank(),
    axis.ticks    = element_blank(),
    axis.title    = element_blank(),
    panel.grid    = element_blank(),
    panel.spacing = unit(0.3, "lines"),
    legend.position      = "bottom",
    legend.justification = "center",
    legend.title         = element_text(size = 20, face = "bold"),
    legend.text          = element_text(size = 20),
    legend.margin        = margin(t = 4, b = 0),
    legend.box.margin    = margin(t = 4, b = 0),
    plot.margin          = margin(8, 6, 6, 6)
  )
 
ggsave("salmon_by_year.png", p3b, width = 7, height = 6, dpi = 300, bg = "white")