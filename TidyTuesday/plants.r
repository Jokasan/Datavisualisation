######################################
## Author: Nils Indreiten           ##
## Date: 2026-02-03                 ##
## Description: This script         ##
## explores companies in Braxil.    ##
## TidyTuesday submission,          ##
## 2026-02-03 dataset.              ##
######################################

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2026-02-03')
plants <- tuesdata$edible_plants

# Manipulate data:
plants_ph_summary <- plants %>%
  # Filter to plants with pH data
  filter(!is.na(preferred_ph_lower), !is.na(preferred_ph_upper)) %>%
  filter(!is.na(cultivation)) %>%
  # Group by cultivation type and find the full range
  group_by(cultivation) %>%
  summarise(
    min_ph = min(preferred_ph_lower, na.rm = TRUE),
    max_ph = max(preferred_ph_upper, na.rm = TRUE),
    n_plants = n(),
    .groups = "drop"
  ) %>%
  # Calculate range metrics
  mutate(
    ph_range = max_ph - min_ph,
    ph_midpoint = (min_ph + max_ph) / 2
  ) %>%
  # Sort by pH range (least flexible to most flexible for bottom-to-top ordering)
  arrange(ph_range) %>%
  # Add position index for gradient mapping (1 = bottom, n = top)
  mutate(
    position = row_number(),
    position_normalized = (position - 1) / (n() - 1))  # 0 to 1 scale)


# Define the color scheme:
# Bottom (warm): burgundy → Top (cool): teal
jewel_gradient <- colorRampPalette(c(
  "#c1666b",  # Dusty rose
  "#d4763c",  # Burnt orange
  "#b8956a",  # Tan
  "#7a8f7e",  # Grey-green
  "#4a8a8b"   # Teal
))


# Create plot:

p_jewel_direct <- plants_ph_summary %>%
  ggplot(aes(y = fct_reorder(cultivation, ph_range))) +
  # Top axis labels aligned with neutral line
  annotate("text", x = 5.25, y = Inf,
           label = "← More Acidic", size = 3.5, color = "grey30", 
           fontface = "bold", vjust = -0.5, hjust = 1) +
  annotate("text", x = 7, y = Inf,
           label = "Neutral", size = 3.5, color = "#2d5016", 
           fontface = "bold", vjust = -0.5, hjust = 0.5) +
  annotate("text", x = 8.75, y = Inf,
           label = "More Alkaline →", size = 3.5, color = "grey30", 
           fontface = "bold", vjust = -0.5, hjust = 0) +
  
  # Dumbbells
  geom_segment(aes(x = min_ph, xend = max_ph, 
                   yend = fct_reorder(cultivation, ph_range),
                   color = position_normalized),
               linewidth = 6, alpha = 0.85, lineend = "round") +
  geom_point(aes(x = min_ph), size = 5.5, color = "white") +
  geom_point(aes(x = max_ph), size = 5.5, color = "white") +
  geom_point(aes(x = min_ph, color = position_normalized), size = 3.8) +
  geom_point(aes(x = max_ph, color = position_normalized), size = 3.8) +
  
  # Reference line
  geom_vline(xintercept = 7, linetype = "dashed", 
             color = "#2d5016", linewidth = 0.8, alpha = 0.6) +
  
  # Labels
  geom_text(aes(x = max_ph + 0.15, 
                label = paste0("n=", n_plants, " | ", round(ph_range, 1), " pH")),
            size = 3, hjust = 0, color = "grey50", fontface = "italic") +
  
  scale_color_gradientn(colors = jewel_gradient(nrow(plants_ph_summary)), guide = "none") +
  scale_x_continuous(breaks = seq(4, 9, 0.5), limits = c(4, 9.8),
                    expand = expansion(mult = c(0.01, 0.02))) +
  coord_cartesian(clip = "off") +
  
  labs(
    title = "Soil pH Tolerance by Plant Family",
    subtitle = "Dumbbells show the complete pH range tolerated across all plants in each cultivation type.\nFamilies are ordered from most specific (bottom) to most flexible (top). Labels show plant count and pH range width.",
    x = "Soil pH",
    y = NULL,
    caption = "Data: TidyTuesday 2026-02-03"
  ) +
  
  theme_minimal(base_size = 12, base_family = "Rethink Sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 3), hjust = 0),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 10.5, color = "grey40", 
                                 margin = margin(b = 15), lineheight = 1.3, hjust = 0,family = "Domine"),
    plot.caption = element_text(size = 8, color = "grey50", hjust = 1,
                               margin = margin(t = 10)),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#fdfdf9", color = NA),
    panel.background = element_rect(fill = "#fdfdf9", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    axis.text.y = element_text(size = 11, face = "bold", color = "grey20"),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 12)),
    plot.margin = margin(40, 25, 20, 25)
  )


ggsave("TidyTuesday/soil_ph_tolerance.png", width = 7, height = 6, dpi = 300)