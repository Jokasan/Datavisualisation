library(tidyverse)
library(lubridate)
library(ggthemes)
# Using R
# Option 1: tidytuesdayR R package 
##install.packages("tidytuesdayR")
# install.packages("ggthemes")

# some useful tiops to remember:

# cmd+shift+m is the pipe
# alt+- is the assignment operator <-

vesuvius <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')

vesuvius  |> 
  select(time, duration_magnitude_md) |>
  mutate(date = lubridate::floor_date(lubridate::as_date(time),unit = "month")) |>
  summarize(duration_magnitude_md = mean(duration_magnitude_md, na.rm = TRUE), .by = date) |>
  arrange(date) |> 
  ggplot(aes(x = date, y = duration_magnitude_md)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Duration of Mount Vesuvius Over Time",
    x = "Year",
    y = "Duration Magnitude (MD)"
  ) +
  ggthemes::theme_fivethirtyeight()


library(plotly)
# volcano is a numeric matrix that ships with R
fig <- plot_ly(z = ~volcano)
fig <- fig %>% add_surface()

fig

# For a 3D scatter plot (most appropriate for your data)
vesuvius |> 
  na.omit() -> tt

fig <- plot_ly(tt, x = ~longitude, y = ~latitude, z = ~-depth_km, 
         type = 'scatter3d', mode = 'markers',
         marker = list(size = 3, color = ~duration_magnitude_md, 
               colorscale = 'Viridis', showscale = TRUE)) |>
  layout(scene = list(
  xaxis = list(title = 'Longitude'),
  yaxis = list(title = 'Latitude'),
  zaxis = list(title = 'Depth (km)')
  ))

fig

install.packages("akima")
library(akima)  # For interpolation

vesuvius |> 
  na.omit() -> tt

# Create interpolated surface from scattered data
interp_data <- with(tt, interp(longitude, latitude, -depth_km, 
                                nx = 50, ny = 50, 
                                duplicate = "mean",
                                linear = FALSE))

# Alternative 1: Just scatter with better visualization
fig1 <- plot_ly(tt, 
                x = ~longitude, 
                y = ~latitude, 
                z = ~-depth_km,
                type = 'scatter3d', 
                mode = 'markers',
                marker = list(
                  size = 4, 
                  color = ~duration_magnitude_md,
                  colorscale = 'Viridis',
                  showscale = TRUE,
                  colorbar = list(title = "Magnitude"),
                  line = list(width = 0.5, color = 'rgba(0,0,0,0.3)')
                )) |>
  layout(
    title = "Mount Vesuvius Earthquake Activity",
    scene = list(
      xaxis = list(title = 'Longitude'),
      yaxis = list(title = 'Latitude'),
      zaxis = list(title = 'Depth (km)')
    )
  )

fig1

# Alternative 2: Add a ground-level density contour (showing horizontal concentration)
# Create 2D density at z=0 for reference
density_2d <- with(tt, interp(longitude, latitude, rep(0, nrow(tt)),
                               nx = 30, ny = 30,
                               duplicate = "mean"))

fig2 <- plot_ly() |>
  # Ground-level contour showing earthquake density
  add_surface(
    x = density_2d$x,
    y = density_2d$y, 
    z = matrix(0, nrow = length(density_2d$x), ncol = length(density_2d$y)),
    surfacecolor = density_2d$z,
    colorscale = list(c(0, "lightgray"), c(1, "darkred")),
    opacity = 0.3,
    showscale = FALSE,
    contours = list(
      z = list(show = TRUE, color = "white")
    )
  ) |>
  # Earthquake points
  add_trace(
    data = tt,
    x = ~longitude,
    y = ~latitude,
    z = ~-depth_km,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(
      size = 3,
      color = ~duration_magnitude_md,
      colorscale = 'Viridis',
      showscale = TRUE,
      colorbar = list(title = "Magnitude")
    )
  ) |>
  layout(
    title = "Vesuvius Earthquakes with Ground-Level Density",
    scene = list(
      xaxis = list(title = 'Longitude'),
      yaxis = list(title = 'Latitude'),
      zaxis = list(title = 'Depth (km)')
    )
  )

fig2

# Install and load required packages
# install.packages("pacman")
pacman::p_load(elevatr, sf, raster, terra)

# Define Vesuvius location and area (5km radius)
vesuvius_center <- st_point(c(14.4261, 40.8210)) |> 
  st_sfc(crs = 4326)

# Create buffer and convert to sf object (not just sfc)
vesuvius_bbox <- st_buffer(st_transform(vesuvius_center, 32633), 5000) |> 
  st_transform(4326) |>
  st_sf()  # Convert to sf object

# Get elevation data (higher z = more detail, but slower)
elevation <- get_elev_raster(vesuvius_bbox, z = 12)

# Convert raster to matrix for plotly
elev_matrix <- raster::as.matrix(elevation)
elev_matrix <- elev_matrix[nrow(elev_matrix):1, ]  # Flip vertically

# Get coordinate vectors
elev_x <- seq(st_bbox(vesuvius_bbox)["xmin"], 
              st_bbox(vesuvius_bbox)["xmax"], 
              length.out = ncol(elev_matrix))
elev_y <- seq(st_bbox(vesuvius_bbox)["ymin"], 
              st_bbox(vesuvius_bbox)["ymax"], 
              length.out = nrow(elev_matrix))

# Convert elevation from meters to km for consistency with depth
elev_matrix_km <- elev_matrix

# Create 3D plot with real Vesuvius topography
fig3 <- plot_ly() |>
  # Add the actual Vesuvius terrain surface
  add_surface(
    x = elev_x,
    y = elev_y,
    z = elev_matrix_km,
    colorscale = list(
      c(0, "darkgreen"),
      c(0.3, "tan"),
      c(0.6, "brown"),
      c(1, "white")
    ),
    opacity = 0.6,
    showscale = FALSE
  ) |>
  # Add earthquake points below the surface
  add_trace(
    data = tt,
    x = ~longitude,
    y = ~latitude,
    z = ~-depth_km,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(
      size = 3,
      color = ~duration_magnitude_md,
      colorscale = 'Plasma',
      showscale = TRUE,
      colorbar = list(
        title = "Magnitude",
        len = 0.5,
        y = 0.5
      ),
      line = list(width = 0.3, color = 'black')
    ),
    name = 'Earthquakes'
  ) |>
  layout(
    title = "Mount Vesuvius: Terrain and Subsurface Earthquakes",
    scene = list(
      xaxis = list(title = 'Longitude'),
      yaxis = list(title = 'Latitude'),
      zaxis = list(title = 'Elevation/Depth (km)'),
      camera = list(
        eye = list(x = 1.5, y = 1.5, z = 1.3)
      )
    )
  )

fig3

# Interactive Leaflet Map (similar to NZ earthquakes example)
library(leaflet)

# Create color palette for magnitude
pal <- colorNumeric(
  palette = "Plasma",
  domain = tt$duration_magnitude_md
)

# Create leaflet map

tt |> filter(time >= '2024-01-01') -> tt

# Create binned color palette for magnitude (like NZ example)
mag_pal <- colorBin("inferno", domain = 0:5, bins = c(0:4, 5))

# Create leaflet map
leaflet_map <- leaflet(tt) |>
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = ~mag_pal(duration_magnitude_md),
    stroke = FALSE,
    fillOpacity = 0.5,
    radius = ~scales::rescale(sqrt(duration_magnitude_md), c(1, 10)),
    label = ~paste0(
      format(time, "%Y-%m-%d %H:%M"), "<br>",
      "Magnitude: ", round(duration_magnitude_md, 1), "<br>",
      "Depth: ", round(depth_km, 1), " km"
    ) |> purrr::map(htmltools::HTML),
    labelOptions = labelOptions(textsize = "15px")
  ) |>
  addLegend(
    title = "Magnitude",
    colors = mag_pal(0:4),
    labels = c("<1", "1", "2", "3", ">4")
  ) |>
  addTiles(
    "http://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
    options = tileOptions(minZoom = 5, maxZoom = 15)
  ) |>
  setView(
    lng = 14.4261,
    lat = 40.8210,
    zoom = 11
  )

leaflet_map

# Some inspo:

https://r-graph-gallery.com/best-r-chart-examples.html


