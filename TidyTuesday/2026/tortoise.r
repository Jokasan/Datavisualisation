######################################
## Author: Nils Indreiten           ##
## Date: 2026-03-04                 ##
## Description: This script         ##
## explores Golem Grad Tortoise     ##
## Data.                            ##
## TidyTuesday submission,          ##
## 2026-03-03 dataset.              ##
######################################

library(tidyverse)
library(ggtext)
library(ggrepel)
library(patchwork)
library(ggdist)     

tuesdata <- tidytuesdayR::tt_load('2026-03-03')
clutch <- tuesdata$clutch_size_cleaned
body <- tuesdata$tortoise_body_condition_cleaned

body <- body |>
  filter(locality != "Beach") |>  
  mutate(
    population = if_else(locality == "Konjsko",
                         "Mainland (Konjsko)",
                         "Plateau"),
    sex_label = case_when(
      sex == "f" ~ "Female",
      sex == "m" ~ "Male",
      TRUE       ~ "Unknown"
    ),
    sex_label = factor(sex_label, levels = c("Female", "Male"))
  ) |>
  filter(!is.na(sex_label))   

col_female   <- "#c44536"   
col_male     <- "#457b9d"   
col_bg       <- "#faf6f1"   
col_grid     <- "#e0d8cf"
col_text     <- "#3d3229"
col_annot    <- "#6b5b4f"

sex_cols <- c("Female" = col_female, "Male" = col_male)

# Summary Statistics:
bci_summary <- body |>
  group_by(population, sex_label) |>
  summarise(
    med_bci = median(straight_carapace_length_mm, na.rm = TRUE),
    mean_bci = mean(straight_carapace_length_mm, na.rm = TRUE),
    max_val = max(straight_carapace_length_mm, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(label_y = max_val )


p1 <- ggplot(
  body,
  aes(x = sex_label, y = straight_carapace_length_mm,
      fill = sex_label, colour = sex_label)
) +
  stat_halfeye(
    adjust = 0.8,
    width = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA,
    alpha = 0.45
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.5,
    linewidth = 0.6
  ) +

  facet_wrap(~ population) +

  scale_fill_manual(values = sex_cols) +
  scale_colour_manual(values = sex_cols) +


  labs(
    title = "Plateau Females Are Smaller Than Mainland Tortoises",
    subtitle = paste0(
      "Straight Carapace length of Hermann's tortoises across all recapture events.\n",
      "Plateau females show notably smaller body size and are severely outnumbered by males."
    ),
    x = NULL,
    y = "Straight Carapace Length (mm)",
    caption = "Data: Bonnet et al. (2026) Ecology Letters · TidyTuesday: 2026-03-03"
  ) +
  # Theme
  theme_minimal(base_size = 12, "Rethink Sans") +
  theme(
    plot.background     = element_rect(fill = col_bg, colour = NA),
    panel.background    = element_rect(fill = col_bg, colour = NA),
    strip.background    = element_rect(fill = "#ede7df", colour = NA, size = 0.5),
    strip.text          = element_text(face = "bold", size = 12, colour = col_text),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y  = element_line(colour = col_grid, linewidth = 0.4),
    panel.grid.minor    = element_blank(),
    plot.title          = element_text(face = "bold", size = 14, colour = col_text,
                                       margin = margin(b = 4), hjust = 0),
    plot.title.position = "plot",
    plot.subtitle       = element_text(size = 10, colour = col_annot,
                                        margin = margin(b = 12), lineheight = 1.3,
                                        family = "Domine"),
    plot.caption        = element_text(size = 8, colour = col_annot, margin = margin(t = 12)),
    axis.title.y        = element_text(face = "bold", colour = col_text),
    axis.text           = element_text(colour = col_text, size = 11),
    legend.position     = "none"
  )
p1

ggsave("TidyTuesday/2026/tortoise_plot.png", plot = p1, width = 7, height = 6, dpi = 300) # Best practice dpi