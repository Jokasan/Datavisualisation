######################################
## Author: Nils Indreiten           ##
## Date: 2026-01-25                 ##
## Description: This script         ##
## explores languages spoken in     ##
## Africa. TidyTuesday submission,  ##
## 2026-01-13 dataset.              ##
######################################

# Load packages & the data:
library(tidyverse)
library(showtext)
library(ggtext)
library(glue)
library(rnaturalearth)

# Some cool fonts:
font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
title_font <- "Oswald"
body_font <- "Nunito"

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2026-01-13')

africa <- tuesdata$africa

# Prepare the data:
top_languages <- africa |> 
  select(language,native_speakers) |>
  arrange(desc(native_speakers)) |> 
  distinct() |> 
  head(10) |>
  mutate(
    language = fct_reorder(language, native_speakers),
    is_arabic = language == "Arabic",
    native_speakers_millions = native_speakers / 1e6)

# Create visualisation 1:
ggplot(top_languages, aes(x = native_speakers_millions, y = language, fill = is_arabic)) +
  geom_col(alpha = 0.9) +
  geom_text(
    aes(label = paste0(round(native_speakers_millions), "M")),
    hjust = -0.1,
    size = 6,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("FALSE" = "lightgrey", "TRUE" = "orange"),
    guide = "none"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15)),
    breaks = c(0, 50, 100, 150, 200),
    labels = c("0", "50M", "100M", "150M", "200M")
  ) +
  labs(
    title = "Arabic Dominates Language Speakers in Africa",
    subtitle = "150 Million Arabic speakers, more than 3x the second most spoken language",
    x = NULL,
    y = NULL,
    caption = "Source: TidyTuesday 2026-01-13"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "#666666", size = 16, margin = margin(b = 15)),
    plot.caption = element_text(size = 12, hjust = 1, margin = margin(t = 10)),
    plot.margin = margin(15, 15, 15, 35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(size = 14, hjust = 1),
    axis.text.x = element_blank(),
    axis.title.x = element_text(size = 14, margin = margin(t = 5))
  ) ->p1

# Prepare visualisation 2:
# Reused the function created by nrenie @https://github.com/nrennie/tidytuesday/tree/main/2026/2026-01-13


bg_col <- "white"
text_col <- "#151C28"
highlight_col <- "orange"


top_10 <- africa |> 
  select(language, native_speakers) |> 
  distinct() |> 
  slice_max(native_speakers, n = 10) |> 
  mutate(label = glue("**{language}**<br>{round(native_speakers / 1000000, 1)} million native speakers"))

africa_map <- ne_countries(
  continent = "Africa",
  scale = "medium"
) |> 
  select(name_en, geometry) |> 
  mutate(
    name_en = case_when(
      name_en == "The Gambia" ~ "Gambia",
      name_en == "Democratic Republic of the Congo" ~ "Congo",
      TRUE ~ name_en
    )
  )

africa_data <- africa |> 
  filter(language %in% top_10$language) |> 
  select(language, native_speakers, country) 

# check match on country name:
africa_map$name_en |> unique() |> sort()
africa_data$country |> unique() |> sort()

prep_lang_data <- function(lang) {
  af_data <- africa_data |> 
    filter(language == lang) 
  output_data <- africa_map |> 
    left_join(
      af_data, by = c("name_en" = "country")
    ) |> 
    mutate(language = lang,
           fill = if_else(is.na(native_speakers), "grey70", highlight_col)) |> 
    select(language, fill, geometry) |> 
    left_join(
      top_10, by = c("language" = "language")
    ) 
  return(output_data)
}

plot_data <- map(
  .x = top_10$language,
  .f = ~prep_lang_data(.x)
) |> 
  bind_rows() |> 
  mutate(
    language = factor(
      language, levels = top_10$language, labels = top_10$label
    )
  )

title <- glue("<span style='font-family:{title_font}; font-size: 20pt;'>**Arabic is the most widely spoken language in Africa**</span>")
st <- "Top 10 most widely spoken languages in Africa by number of native speakers"
cap <- paste0(
  title, "<br>", st, "<br><br>",
  "**Data**: Wikipedia (Languages of Africa) "
)

ggplot() +
  geom_sf(
    data = plot_data |> 
      filter(language == "**Arabic**<br>150 million native speakers"),
    mapping = aes(fill = fill),
    colour = bg_col
  ) +
  scale_fill_identity() +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
   plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font,
      maxwidth = 0.5
    ),
    plot.tag.position = c(0.55, 0.15),
    strip.text = element_textbox_simple(
      margin = margin(t = 5, b = 5),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank()
  ) ->p2

library(patchwork)

# Combine plots with p2 in the bottom right & save:
p1 + inset_element(p2, left = 0.30, bottom = 0, right = 1, top = 0.80)->p3
p3
ggsave("languages_in_africa.png", plot = p3, width = 14, height = 10, dpi = 300)