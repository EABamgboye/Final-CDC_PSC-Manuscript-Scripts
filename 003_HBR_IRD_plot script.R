library(tidyverse)
library(patchwork)

##Summaries of hbr_df extracted from Model based scripts for count data while
##ird_df from Ento descriptive script


#=========================================================
# HUMAN BITING RATE DATA
#=========================================================

hbr_df <- tribble(
  ~season, ~model, ~location, ~site, ~estimate, ~lower, ~upper,
  
  # DRY SEASON
  "Dry", "Crude", "Indoor", "Agugu",      0.05, -0.01, 0.38,
  "Dry", "Crude", "Indoor", "Challenge",  0.56,  0.23, 1.11,
  "Dry", "Crude", "Indoor", "Olopomewa",  0.20,  0.03, 0.61,
  
  "Dry", "Crude", "Outdoor", "Agugu",    -0.02, -0.08, 0.22,
  "Dry", "Crude", "Outdoor", "Challenge", 0.32,  0.09, 0.78,
  "Dry", "Crude", "Outdoor", "Olopomewa",-0.02, -0.08, 0.22,
  
  "Dry", "GEE-adjusted", "Indoor", "Agugu",      0.02, -0.01, 0.09,
  "Dry", "GEE-adjusted", "Indoor", "Challenge",  0.46,  0.22, 0.93,
  "Dry", "GEE-adjusted", "Indoor", "Olopomewa",  0.09,  0.02, 0.33,
  
  "Dry", "GEE-adjusted", "Outdoor", "Agugu",    -0.01, -0.03, 0.07,
  "Dry", "GEE-adjusted", "Outdoor", "Challenge", 0.17,  0.01, 1.40,
  "Dry", "GEE-adjusted", "Outdoor", "Olopomewa", 0.02, -0.01, 0.22,
  
  # WET SEASON
  "Wet", "Crude", "Indoor", "Agugu",      0.58, 0.45, 0.76,
  "Wet", "Crude", "Indoor", "Challenge",  0.36, 0.24, 0.51,
  
  "Wet", "Crude", "Outdoor", "Agugu",     0.06, 0.02, 0.15,
  "Wet", "Crude", "Outdoor", "Challenge", 0.60, 0.47, 0.79,
  
  "Wet", "GEE-adjusted", "Indoor", "Agugu",      0.55, 0.39, 0.81,
  "Wet", "GEE-adjusted", "Indoor", "Challenge",  0.34, 0.15, 0.75,
  
  "Wet", "GEE-adjusted", "Outdoor", "Agugu",     0.06, 0.03, 0.13,
  "Wet", "GEE-adjusted", "Outdoor", "Challenge", 0.57, 0.18, 1.81
)

#=========================================================
# INDOOR RESTING DENSITY DATA
#=========================================================

ird_df <- tribble(
  ~season, ~site, ~estimate, ~lower, ~upper,
  
  "Dry", "Olopomewa", 0.025, 0.000, 0.139,
  "Dry", "Challenge", 0.125, 0.035, 0.290,
  "Dry", "Agugu",     0.000, 0.000, 0.090,
  
  "Wet", "Challenge", 0.083, 0.039, 0.150,
  "Wet", "Agugu",     0.692, 0.550, 0.850
)

#=========================================================
# COMMON THEME
#=========================================================

theme_pub <- theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank()
  )

#=========================================================
# PANEL A - DRY SEASON HBR
#=========================================================

p_dry <- hbr_df %>%
  filter(season == "Dry") %>%
  ggplot(
    aes(
      x = site,
      y = estimate,
      colour = model,
      shape = model
    )
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.4),
    width = 0.15,
    linewidth = 0.7
  ) +
  geom_point(
    position = position_dodge(width = 0.4),
    size = 3
  ) +
  facet_grid(. ~ location) +
  coord_flip() +
  scale_colour_manual(
    values = c(
      "Crude" = "#E76F51",
      "GEE-adjusted" = "#2A9D8F"
    )
  ) +
  labs(
    title = "A) Dry Season",
    y = "Human biting rate (mosquitoes/person-night)"
  ) +
  coord_flip(ylim = c(-0.1, 1.5)) +
  theme_pub

#=========================================================
# PANEL B - WET SEASON HBR
#=========================================================

p_wet <- hbr_df %>%
  filter(season == "Wet") %>%
  ggplot(
    aes(
      x = site,
      y = estimate,
      colour = model,
      shape = model
    )
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.4),
    width = 0.15,
    linewidth = 0.7
  ) +
  geom_point(
    position = position_dodge(width = 0.4),
    size = 3
  ) +
  facet_grid(. ~ location) +
  coord_flip() +
  scale_colour_manual(
    values = c(
      "Crude" = "#E76F51",
      "GEE-adjusted" = "#2A9D8F"
    )
  ) +
  labs(
    title = "B) Wet Season",
    y = "Human biting rate (mosquitoes/person-night)"
  ) +
  coord_flip(ylim = c(0, 1.9)) +
  theme_pub

#=========================================================
# PANEL C - INDOOR RESTING DENSITY
#=========================================================

p_ird <- ggplot(
  ird_df,
  aes(site, estimate)
) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15,
    linewidth = 0.7
  ) +
  geom_point(
    size = 3.5,
    colour = "#0072B2"
  ) +
  geom_text(
    aes(label = sprintf("%.3f", estimate)),
    hjust = -0.3,
    size = 4
  ) +
  facet_grid(. ~ season) +
  coord_flip() +
  labs(
    title = "C) Indoor Resting Density",
    y = "Mosquitoes/house"
  ) +
  coord_flip(ylim = c(0, 0.9)) +
  theme_pub +
  theme(
    legend.position = "none"
  )

#=========================================================
# COMBINE FIGURE
#=========================================================

final_plot <- (
  p_dry +
    p_wet +
    p_ird
) +
  plot_layout(
    ncol = 3,
    widths = c(1.3, 1.3, 1)
  ) &
  theme(
    legend.position = "bottom"
  )

final_plot

# save
ggsave(
  "Figure_HBR_IRD.pdf",
  final_plot,
  device = cairo_pdf,
  width = 14,
  height = 5.5,
  units = "in"
)
