# install.packages("devtools")
devtools::install_github("humaniverse/geographr")

geographr::boundaries_utla21-> tt
# geographr::boundaries_utla21-> tt
# geographr::lookup_ltla20_utla20

tm_shape(tt) +
  tm_borders()

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


readxl::read_xls(path = 'earnings-workplace-borough.xls',sheet = 'FT workers annual Median') -> earnings
readxl::read_xls(path = 'land-registry-house-prices-borough.xls',sheet = 'Mean') -> house_prices


house_prices |> 
select(Area, `Year ending Dec 2017`) |> 
  na.omit() |> 
  janitor::clean_names() -> house_prices_clean

https://trustforlondon.org.uk/data/boroughs/overview-of-london-boroughs/


earnings |> 
  select(Area, `2025`) |>
  mutate(`2025`=replace_na(`2025`,mean(earnings$`2025`))) |>
  na.omit()-> earnings_clean

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
left_join(house_prices_clean,by = c("utla21_name"="area"))  -> to_plot



ggplot(to_plot)+
  geom_sf()+  
  geom_sf(aes(fill = `2025`), linewidth = .5, alpha = 0.9) +
  theme_void() +
  theme(legend.position = "none") ->p
p

pacman::p_load(biscale)

data <- bi_class(to_plot, x = `2025`, y = `year_ending_dec_2017`, style = "quantile", dim = 3)

# Load custom fonts
library(showtext)
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

