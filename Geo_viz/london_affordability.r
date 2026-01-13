###########################################################
## Author: Nils Indreiten                                ##
## Date: 2026-01-13                                      ##
## Description: This script creates a map visualizing    ##
## housing affordability in London boroughs. It explores ##
## two different plot types.                             ##
###########################################################

# Load packages:
pacman::p_load(tidyverse,tmap.cartogram,cowplot,ggspatial,showtext,tmap,sf,ggmap,ggpp,emojifont,biscale)

# Load geospatial data for London boroughs:
geographr::boundaries_utla21-> tt

# Some other useful stuff:
# geographr::boundaries_utla21-> tt
# geographr::lookup_ltla20_utla20

# Basic plot to check data:
tm_shape(tt) +
  tm_borders()

# London borioughs only:
ggplot(tt |> 
  filter(utla21_name %in% c("Barking and Dagenham",
"Barnet",
"Bexley",
"Brent",
"Bromley",
"Camden",
"City of London",
"Croydon",
"Ealing",
"Enfield",
"Greenwich",
"Hackney",
"Hammersmith and Fulham",
"Haringey",
"Harrow",
"Havering",
"Hillingdon",
"Hounslow",
"Islington",
"Kensington and Chelsea",
"Kingston upon Thames",
"Lambeth",
"Lewisham",
"Merton",
"Newham",
"Redbridge",
"Richmond upon Thames",
"Southwark",
"Sutton",
"Tower Hamlets",
"Waltham Forest",
"Wandsworth",
"Westminster"))
  )+
  geom_sf()+
  theme_minimal()

# Read in auxiliary data:
readxl::read_xls(path = 'Geo_viz/earnings-workplace-borough.xls',sheet = 'FT workers annual Median') -> earnings
readxl::read_xls(path = 'Geo_viz/land-registry-house-prices-borough.xls',sheet = 'Mean') -> house_prices

# Clean auxiliary data:
house_prices |> 
select(Area, `Year ending Dec 2017`) |> 
  na.omit() |> 
  janitor::clean_names() -> house_prices_clean

earnings |> 
  select(Area, `2025`) |>
  mutate(`2025`=replace_na(`2025`,mean(earnings$`2025`))) |>
  na.omit()-> earnings_clean

# Scale the data to make them comparable:
tt |> 
  filter(utla21_name %in% c("Barking and Dagenham",
"Barnet",
"Bexley",
"Brent",
"Bromley",
"Camden",
"City of London",
"Croydon",
"Ealing",
"Enfield",
"Greenwich",
"Hackney",
"Hammersmith and Fulham",
"Haringey",
"Harrow",
"Havering",
"Hillingdon",
"Hounslow",
"Islington",
"Kensington and Chelsea",
"Kingston upon Thames",
"Lambeth",
"Lewisham",
"Merton",
"Newham",
"Redbridge",
"Richmond upon Thames",
"Southwark",
"Sutton",
"Tower Hamlets",
"Waltham Forest",
"Wandsworth",
"Westminster")) |> 
left_join(earnings_clean,by = c("utla21_name"="Area")) |>
left_join(house_prices_clean,by = c("utla21_name"="area")) |>
mutate(
    # Scale features (z-score normalization)
    income_scaled = scale(`2025`)[,1],
    house_price_scaled = scale(year_ending_dec_2017)[,1],
    # Calculate affordability: higher values = less affordable
    affordability = house_price_scaled - income_scaled) -> to_plot

# Create diverging choropleth map:
font_add_google("Montserrat", "montserrat")
font_add_google("Roboto", "roboto")
font_add_google("Lato", "lato")

# Get basemap using ggmap
bbox <- st_bbox(to_plot)
basemap <- get_stadiamap(
  bbox = c(left = -0.5097014, bottom = 51.2867587, 
           right = 0.3340242 , top = 51.6918756 ),
  maptype = "alidade_smooth", 
  zoom = 11)

# Points of interest
poi <- tibble(
  name = c("Heathrow Airport", "London Eye", "Paddington Station", "London City Airport"),
  lon = c(-0.4543, -0.1195, -0.1764, 0.0553),
  lat = c(51.4700, 51.5033, 51.5154, 51.5048),
  shape = c(24, 21, 22, 24) 
)

# Create the final map
affordability_map <- ggmap(basemap) +
  geom_sf(data = to_plot, aes(fill = affordability), 
          color = "white", linewidth = 0.5, alpha = 0.7, inherit.aes = FALSE) +
  geom_point(data = poi, aes(x = lon, y = lat, shape = factor(name)), 
             size = 5, color = "black", fill = "black", stroke = 1.5) +
  scale_shape_manual(values = c("Heathrow Airport" = 24, "London Eye" = 21, "Paddington Station" = 22, "London City Airport" = 24),
                     guide = "none") +
  geom_text(data = poi, aes(x = lon, y = lat, label = name), 
            size = 3, family = "roboto", fontface = "bold", 
            hjust = 0, nudge_x = 0.02, color = "black",
            bg.colour = "white", bg.r = 0.15) +
  scale_fill_gradient2(
    low = "#2166ac",      # Blue for affordable
    mid = "#f7f7f7",      # White for neutral
    high = "#b2182b",     # Red for unaffordable
    midpoint = 0,
    name = NULL,
    labels = c("More Affordable", "Less Affordable"),
    breaks = c(min(to_plot$affordability, na.rm = TRUE), 
               max(to_plot$affordability, na.rm = TRUE)),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(60, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      frame.colour = "black",
      frame.linewidth = 0.5,
      ticks.colour = "black",
      ticks.linewidth = 0.5,
      raster = FALSE
    )
  ) +
  labs(
    title = "London Housing Affordability by Borough",
    subtitle = "Relative affordability based on house prices and income",
    caption = "Income data: 2025 | House prices: December 2017"
  ) +
  annotation_north_arrow(
    location = "br",
    pad_x = unit(1.2, "in"),
    pad_y = unit(0.4, "in"),
    style = north_arrow_fancy_orienteering,
    height = unit(4, "cm"), width = unit(4, "cm")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(family = "montserrat", size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(family = "roboto", size = 12, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(family = "roboto", size = 9, color = "#666666", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(family = "roboto", size = 12, face = "bold"),
    legend.margin = margin(5, 5, 5, 5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

showtext_auto()
affordability_map

# The bivariate color map:

data <- bi_class(to_plot, x = `2025`, y = `year_ending_dec_2017`, style = "quantile", dim = 3)

# Plot the map:

font_add_google("Montserrat", "montserrat")
font_add_google("Roboto", "roboto")
font_add_google("Lato", "lato")
showtext_auto()

map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 10, alpha = 0.5) +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.4, show.legend = FALSE, alpha = 0.7) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "How do house prices and income vary together in London?*",
    caption = "*Income data based on 2025, house prices on December 2017"
  ) +
  #--- add north arrow ---#
  annotation_north_arrow(
    location = "br",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.4, "in"),
    style = north_arrow_fancy_orienteering,
    height = unit(5, "cm"), width = unit(5, "cm")
  )+
  bi_theme() +
  theme(
    plot.title = element_text(family = "montserrat", size = 20),
    # plot.subtitle = element_text(family = "montserrat", size = 16, color = "#666666", hjust = 0.46),
    plot.caption = element_text(family = "montserrat", size = 12, color = "#999999", hjust = 0),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Higher Income ",
                    ylab = "Higher House Prices",
                    size = 11) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.03, .05, 0.2, 0.2) +
  # Add commentary labels
  annotate(
    "text",
    x = 0.08, y = 0.24,
    label = "Most\nUnaffordable",
    size = 3,
    hjust = 0,
    family = "roboto",
    color = "#333333",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.209, y = 0.078,
    label = "Most\nAffordable",
    size = 3,
    hjust = 1,
    family = "roboto",
    color = "#333333",
    # make bold
    fontface = "bold"
  )

finalPlot


#########
## FIN ##
#########