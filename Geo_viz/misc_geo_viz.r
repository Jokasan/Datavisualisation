pacman::p_load(tidyverse,tmap.cartogram,cowplot,ggspatial,showtext)
# install.packages("tmap.cartogram")

consump <- read_csv("/Users/joka/Documents/R_projects/data_exp/country_data.csv") |>
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "value"
  )

country_metadata <- read_csv("/Users/joka/Documents/R_projects/data_exp/country_metadata.csv")

consump |> 
  select(-...70) |> 
  filter(year %in% c("2022")) -> tt

tt |> 
  left_join(country_metadata, join_by(`Country Code`)) |> 
  janitor::clean_names() |> 
  select(-income_group,-special_notes, -table_name, -x6) |>
  mutate(country_name = ifelse(country_name == "United States","United States of America",country_name))-> tt

# A map of africa:

# Get the shape file of Africa, see how on
# https://r-graph-gallery.com/168-load-a-shape-file-into-r.html

# I stored the data on a DATA folder and read it from there
library(sf)
wrld_simpl <- read_sf("DATA/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp")
afr <- wrld_simpl[wrld_simpl$REGION == 2, ]

# We can visualize the region's boundaries with the plot function
plot(st_geometry(afr))


pacman::p_load(tmap,sf)

Africa <- World |> 
  filter(continent == "Africa")

tm_shape(Africa) +
  tm_borders()

plot(st_geometry(Africa))

Africa |> 
  left_join(tt, by = c("name"="country_name")) -> to_plot


# Palette of 30 colors
library(RColorBrewer)
my_colors <- brewer.pal(9, "Reds")
my_colors <- colorRampPalette(my_colors)(30)


class_of_country <- cut(to_plot$value, 30)
my_colors <- my_colors[as.numeric(class_of_country)]

# Make the plot
plot(st_geometry(to_plot),
  xlim = c(-20, 60), ylim = c(-40, 40), col = my_colors)


# Cartogram, Africa:

library(cartogram)

# construct a cartogram using the population in 2005
# Impute NA values with median before creating cartogram
# to_plot <- to_plot |> 
#   mutate(value = ifelse(is.na(value), median(value, na.rm = TRUE), value))

# need first to "change" the projection to Mercator (AKA Google Maps): EPSG: 3857
afr_merc <- st_transform(to_plot, 3857)

afr_cartogram <- cartogram_cont(afr_merc, "value", itermax = 5)

# And back to the previous projection
afr_cartogram <- st_transform(afr_cartogram, st_crs(to_plot))

# This is a new geospatial object, we can visualise it!
plot(st_geometry(afr_cartogram))

ggplot(afr_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds")) +
  theme_void()

# Make pretty:

ggplot(afr_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  theme_void() +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Reds"),
    name = "Consumption",
    # breaks = c(250, 500, 750, 1000),
    # labels = c("250", "500", "750", "1,000"),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(50, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom"
    )
  ) +
  labs(title = "Africa 2005 Population") +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f4", color = NA),
    panel.background = element_rect(fill = "#f5f5f4", color = NA),
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(
      size = 22, hjust = 0.5,
      color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 2,
        unit = "cm"
      )
    ),
    legend.position = c(0.2,0.26)
  ) -> p1


# Cartogram Asia:

Asia <- World |> 
  filter(continent == "Asia")

tm_shape(Asia) +
  tm_borders()

plot(st_geometry(Asia))

Asia |> 
  left_join(tt, by = c("name"="country_name")) -> to_plot



  # Impute NA values with median before creating cartogram
  to_plot <- to_plot |> 
    mutate(value = ifelse(is.na(value), median(value, na.rm = TRUE), value))

  # need first to "change" the projection to Mercator (AKA Google Maps): EPSG: 3857
  asia_merc <- st_transform(to_plot, 3857)

  asia_cartogram <- cartogram_cont(asia_merc, "value", itermax = 5)

  # And back to the previous projection
  asia_cartogram <- st_transform(asia_cartogram, st_crs(to_plot))

# This is a new geospatial object, we can visualise it!
plot(st_geometry(asia_cartogram))

ggplot(asia_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds")) +
  theme_void()


# Make pretty:

ggplot(asia_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  theme_void() +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Reds"),
    name = "Consumption",
    # breaks = c(250, 500, 750, 1000),
    # labels = c("250", "500", "750", "1,000"),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(50, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom"
    )
  ) +
  labs(title = "Asia 2005 Population") +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f4", color = NA),
    panel.background = element_rect(fill = "#f5f5f4", color = NA),
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(
      size = 22, hjust = 0.5,
      color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 2,
        unit = "cm"
      )
    ),
    legend.position = c(0.2,0.26)
  ) -> p1



# Cartogram Europe:

Europe <- World |> 
  filter(continent == "Europe")

tm_shape(Europe) +
  tm_borders()

plot(st_geometry(Europe))

Europe |> 
  left_join(tt, by = c("name"="country_name")) -> to_plot



  # Impute NA values with median before creating cartogram
  # to_plot <- to_plot |> 
  #   mutate(value = ifelse(is.na(value), median(value, na.rm = TRUE), value))

  # need first to "change" the projection to Mercator (AKA Google Maps): EPSG: 3857
  europe_merc <- st_transform(to_plot, 3857)

  europe_cartogram <- cartogram_cont(europe_merc, "value", itermax = 5)

  # And back to the previous projection
  europe_cartogram <- st_transform(europe_cartogram, st_crs(to_plot))

# This is a new geospatial object, we can visualise it!
plot(st_geometry(europe_cartogram))

ggplot(europe_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds")) +
  theme_void()


# Make pretty:

ggplot(europe_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  coord_sf(xlim = c(-30, 40), ylim = c(35, 72)) +
  theme_void() +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Reds"),
    name = "Consumption",
    labels = scales::label_dollar(prefix = "$ "),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(60, units = "mm"),
      title.position = "top",
      title.hjust = 0,
      label.position = "bottom"
    )
  ) +
  labs(title = "Europe 2005 Population") +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f4", color = NA),
    panel.background = element_rect(fill = "#f5f5f4", color = NA),
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(
      size = 22, hjust = 0.5,
      color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 2,
        unit = "cm"
      )
    ),
    legend.position = c(0.15,0.26)
  ) -> p3


# Cartogram SA+NA:

South_america <- World |> 
  filter(continent %in% c("South America","North America"))

tm_shape(South_america) +
  tm_borders()

plot(st_geometry(South_america))

South_america |> 
  left_join(tt, by = c("name"="country_name")) -> to_plot



  # Impute NA values with median before creating cartogram
  to_plot <- to_plot |> 
    mutate(value = ifelse(is.na(value), median(value, na.rm = TRUE), value))

  # need first to "change" the projection to Mercator (AKA Google Maps): EPSG: 3857
  sa_merc <- st_transform(to_plot, 3857)

  sa_cartogram <- cartogram_cont(sa_merc, "value", itermax = 5)

  # And back to the previous projection
  sa_cartogram <- st_transform(sa_cartogram, st_crs(to_plot))

# This is a new geospatial object, we can visualise it!
plot(st_geometry(sa_cartogram))

ggplot(sa_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds")) +
  theme_void()


# Make pretty:

ggplot(sa_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  # coord_sf(xlim = c(-30, 40), ylim = c(35, 72)) +
  theme_void() +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Reds"),
    name = "Consumption",
    labels = scales::label_dollar(prefix = "$"),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(60, units = "mm"),
      title.position = "top",
      title.hjust = 0,
      label.position = "bottom"
    )
  ) +
  labs(title = "NA & SA Consumption") +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f4", color = NA),
    panel.background = element_rect(fill = "#f5f5f4", color = NA),
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(
      size = 22, hjust = 0.5,
      color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 2,
        unit = "cm"
      )
    ),
    legend.position = c(0.75,0.26)
  )->p4


# Oceania:

Oceania<- World |> 
  filter(continent %in% c("Oceania"))

tm_shape(Oceania) +
  tm_borders()

plot(st_geometry(Oceania))

World |> 
  left_join(tt, by = c("name"="country_name")) -> to_plot



  # Impute NA values with median before creating cartogram
  # to_plot <- to_plot |> 
  #   mutate(value = ifelse(is.na(value), median(value, na.rm = TRUE), value))

  # need first to "change" the projection to Mercator (AKA Google Maps): EPSG: 3857
  oc_merc <- st_transform(to_plot, 3857)

  oc_cartogram <- cartogram_cont(oc_merc, "value", itermax = 5)

  # And back to the previous projection
  oc_cartogram <- st_transform(oc_cartogram, st_crs(to_plot))

# This is a new geospatial object, we can visualise it!
plot(st_geometry(oc_cartogram))

ggplot(oc_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds")) +
  theme_void()


# Make pretty:

ggplot(oc_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  # coord_sf(xlim = c(-30, 40), ylim = c(35, 72)) +
  theme_void() +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Reds"),
    name = "Consumption",
    labels = scales::label_dollar(prefix = "$"),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(60, units = "mm"),
      title.position = "top",
      title.hjust = 0,
      label.position = "bottom"
    )
  ) +
  labs(title = "Europe 2005 Population") +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f4", color = NA),
    panel.background = element_rect(fill = "#f5f5f4", color = NA),
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(
      size = 22, hjust = 0.5,
      color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 2,
        unit = "cm"
      )
    ),
    legend.position = c(0.75,0.26)
  )


# A whole world one:

World |> 
  left_join(tt, by = c("name"="country_name")) |>
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)")) -> to_plot

  # Impute NA values with median before creating cartogram
  to_plot <- to_plot |> 
    mutate(value = ifelse(is.na(value), median(value, na.rm = TRUE), value))

  # need first to "change" the projection to Mercator (AKA Google Maps): EPSG: 3857
  oc_merc <- st_transform(to_plot, 3857)

  oc_cartogram <- cartogram_cont(oc_merc, "value", itermax = 5)

  # And back to the previous projection
  oc_cartogram <- st_transform(oc_cartogram, st_crs(to_plot))

# This is a new geospatial object, we can visualise it!
plot(st_geometry(oc_cartogram))

ggplot(oc_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds")) +
  theme_void()


# Make pretty:

ggplot(oc_cartogram) +
  geom_sf(aes(fill = value), linewidth = .5, alpha = 0.9) +
  theme_void() +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Reds"),
    name = "Consumption",
    labels = scales::label_dollar(prefix = "$"),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(60, units = "mm"),
      title.position = "top",
      title.hjust = 0,
      label.position = "bottom"
    )
  ) +
  labs(title = "Europe 2005 Population") +
  annotate(
    "curve",
    x = -130, y = 40,
    xend = -100, yend = 48,
    curvature = -0.3,
    arrow = arrow(length = unit(2, "mm")),
    color = "#4e4d47"
  ) +
  annotate(
    "text",
    x = -130, y = 35,
    label = "This is the\nhighest value",
    size = 3.5,
    color = "#4e4d47"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f4", color = NA),
    panel.background = element_rect(fill = "#f5f5f4", color = NA),
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(
      size = 22, hjust = 0.5,
      color = "#4e4d47",
      margin = margin(
        b = -0.1, t = 0.4, l = 2,
        unit = "cm"
      )
    ),
    legend.position = c(0.15,0.26)
  )

# Implement this:

https://r-graph-gallery.com/web-choropleth-barchart-map.html
# and this 
https://github.com/BjnNowak/bertin?tab=readme-ov-file

https://github.com/eastnile/choroplethr

https://github.com/humaniverse/geographr?tab=readme-ov-file

https://data.london.gov.uk/dataset/statistical-gis-boundary-files-for-london-20od9/ 

pacman::p_load(patchwork)
p1 + p3 + p4 + p2 + plot_layout(ncol = 2)

