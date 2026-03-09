######################################
## Author: Nils Indreiten           ##
## Date: 2026-03-10                 ##
## Description: This script         ##
## explores likely word.            ##
## probabilities.                   ##
## TidyTuesday submission,          ##
## 2026-03-10 dataset.              ##
######################################

# Loads data and libraries:
library(tidyverse)
library(ggtext)
library(showtext)
library(glue)

font_add_google("DM Serif Display", "dm_serif")
font_add_google("Rethink Sans",     "rethink")
font_add_google("Domine",           "domine")
showtext_auto()
showtext_opts(dpi = 300)

bg_col   <- "#0D1821"
bg_alt   <- "#152030"       
txt_col  <- "#DCE8F0"
dim_col  <- "#6A8A9E"
grid_col <- "#1E3040"

absolute_judgements <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/absolute_judgements.csv",
  show_col_types = FALSE
)

# Wrangle data and prepare plots:

phrase_stats <- absolute_judgements |>
  group_by(term) |>
  summarise(
    med = median(probability),
    sd  = sd(probability),
    p10 = quantile(probability, 0.10),
    p25 = quantile(probability, 0.25),
    p75 = quantile(probability, 0.75),
    p90 = quantile(probability, 0.90),
    n   = n(),
    .groups = "drop"
  ) |>

  arrange(desc(sd)) |>
  mutate(
    rank = row_number(),

    term = fct_reorder(term, sd, .desc = FALSE)   
  )

# Top 3:
n_phrases <- nrow(phrase_stats)

phrase_stats <- phrase_stats |>
  mutate(
    annotate_sd = rank <= 3 | rank >= (n_phrases - 2),
    sd_label    = if_else(annotate_sd, glue("SD = {round(sd, 1)}"), NA_character_),
    label_x     = p90 - .5
  )

# Colors:
prob_colours <- c(
  "#3A7EBF",   
  "#72B0D4",   
  "#E8D5A3",   
  "#E8903A",   
  "#C0392B"    
)

p <- ggplot(phrase_stats) +

  geom_rect(
    aes(
      xmin = -3,
      xmax = 107,
      ymin = as.numeric(term) - 0.5,
      ymax = as.numeric(term) + 0.5
    ),
    fill  = ifelse(as.numeric(fct_reorder(phrase_stats$term, phrase_stats$sd)) %% 2 == 0,
                   bg_alt, bg_col),
    colour = NA
  ) +

  geom_vline(xintercept = c(25, 50, 75),
             colour = grid_col, linewidth = 0.5, linetype = "solid") +

  geom_segment(
    aes(x = p10, xend = p90, y = term, yend = term, colour = med),
    linewidth = 0.65,
    alpha     = 0.45,
    lineend   = "round"
  ) +

  geom_segment(
    aes(x = p25, xend = p75, y = term, yend = term, colour = med),
    linewidth = 3.5,
    alpha     = 0.90,
    lineend   = "round"
  ) +

  geom_point(
    aes(x = med, y = term, colour = med),
    shape  = 21,
    size   = 3.6,
    fill   = bg_col,
    stroke = 1.6
  ) +

  geom_text(
    data    = filter(phrase_stats, annotate_sd),
    aes(x   = 113, y = term, label = sd_label, colour = med),
    hjust   = 1,
    family  = "rethink",
    size    = 3.1,
    fontface = "bold"
  ) +

  geom_text(
    data     = filter(phrase_stats, !annotate_sd),
    aes(x    = 113, y = term, colour = med,
        label = glue("±{round(sd)}")),
    hjust    = 1,
    family   = "rethink",
    size     = 2.5,
    alpha    = 0.6
  ) +

  annotate("text", x = 25, y = n_phrases + 0.9,
           label = "25%", colour = dim_col, family = "rethink",
           size = 3.0, hjust = 0.5) +
  annotate("text", x = 50, y = n_phrases + 0.9,
           label = "50%", colour = dim_col, family = "rethink",
           size = 3.0, hjust = 0.5) +
  annotate("text", x = 75, y = n_phrases + 0.9,
           label = "75%", colour = dim_col, family = "rethink",
           size = 3.0, hjust = 0.5) +

  annotate("text", x = -2, y = n_phrases - 0.01,
           label = "most contested word", colour = "#FF7B6B",
           family = "rethink", size = 2.7, hjust = 0,
           fontface = "italic") +
  annotate("text", x = -2, y = 1 + 0.01,
           label = "most agreed word", colour = "#69C3A2",
           family = "rethink", size = 2.7, hjust = 0,
           fontface = "italic") +

  scale_colour_gradientn(
    colours = prob_colours,
    values  = c(0, 0.25, 0.5, 0.75, 1),
    limits  = c(0, 100),
    name    = "Median probability",
    breaks  = c(0, 25, 50, 75, 100),
    labels  = c("0%", "25%", "50%", "75%", "100%"),
    guide   = guide_colourbar(
      direction      = "horizontal",
      barwidth       = 10,
      barheight      = 0.35,
      title.position = "top",
      title.hjust    = 0.5,
      title.theme    = element_text(
        family = "rethink", size = 8, colour = txt_col,
        margin = margin(b = 4)
      ),
      label.theme    = element_text(
        family = "rethink", size = 7.5, colour = dim_col
      )
    )
  ) +

  scale_x_continuous(
    limits = c(-3, 115),
    breaks = c(0, 25, 50, 75, 100),
    labels = c("0%", "25%", "50%", "75%", "100%"),
    expand = c(0, 0)
  ) +

  scale_y_discrete(
    expand = expansion(add = c(0.8, 1.6))
  ) +

  labs(
    title    = "Some words are precise. Others are a gamble.",
    subtitle = "In an online quiz, 5,000+ participants assigned a 0–100% probability to each of 19 common phrases (e.g. 'Likely', 'Probable'), to determine\nwhich conveys the most probability. Using the standard deviation of the assigned probability, Realistic Possibility is the most contested\nphrase, whilst About Even is the most agreed upon phrase.",

    caption  = "Data: Adam Kucharski / kucharski.io  •  #TidyTuesday: 2026-03-10",
    x        = "Probability assigned by respondents  (0% → 100%)",
    y        = NULL
  ) +

  theme_minimal(base_family = "rethink", base_size = 11) +
  theme(
    plot.background    = element_rect(fill = bg_col, colour = NA),
    panel.background   = element_rect(fill = bg_col, colour = NA),
    panel.grid         = element_blank(),

    plot.title         = element_text(
      family     = "rethink",
      size       = 18,
      colour     = txt_col,
      lineheight = 1.15,
      hjust      = 0,
      margin     = margin(b = 6)
    ),
    plot.title.position = "plot",
    plot.subtitle      = element_text(
      family     = "domine",
      size       = 6.5,
      colour     = dim_col,
      lineheight = 1.4,
      margin     = margin(b = 12)
    ),
    plot.caption       = element_text(
      size   = 6.5,
      colour = "#3A5060",
      hjust  = 1,
      margin = margin(t = 8)
    ),

    plot.margin        = margin(t = 18, r = 15, b = 12, l = 25),

    axis.text.y        = element_text(
      size   = 8,
      colour = txt_col,
      hjust  = 1,
      margin = margin(r = 4)
    ),
    axis.text.x        = element_text(size = 8.5, colour = dim_col),
    axis.title.x       = element_text(
      size   = 8.5,
      colour = dim_col,
      margin = margin(t = 10)
    ),

    legend.position    = "bottom",
    legend.background  = element_rect(fill = bg_col, colour = NA),
    legend.box.margin  = margin(t = 0)
  )

p
# Save:

ggsave(
  "likely.png",
  plot   = p,
  width  = 7,
  height = 6,
  dpi    = 300,
  bg     = bg_col
)