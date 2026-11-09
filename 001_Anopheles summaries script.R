##Load extra packages
library(readxl)
library(scatterpie)
library(tidyverse)
library(scales)
library(ggforce)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(forcats)

## =========================================================================================================================================
### Data Prep
## =========================================================================================================================================
EntoDat <- ("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Osun-excel")
shapepath <- ("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_shape_files")
## -----------------------------------------------------------------------------------------------------------------------------------------

#Results for Manuscript
##Read in relevant data prepared by @Grace updated by EAB
all_ento_data <- read.csv(file.path(EntoDat, "all_ento_dry_wet_data_updated.csv"))

all_ento_data$Other[all_ento_data$Other == 0] <- NA

#Objective One: Specie composition and relative abundance

#Figure 1: Number of mosquitoes caught 
#a) By Season
##Some data wrangling
species_inventory_by_season <- all_ento_data %>%
  group_by(season, method) %>% 
  summarise(
    total_An.gambiae = sum(An.gambiae, na.rm = TRUE),
    total_An.funestus = sum(An.funestus, na.rm = TRUE),
    total_Culicine = sum(Culicine, na.rm = TRUE),
    total_Other = sum(Other, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("total_"), # include all species columns starting with "total_"
    names_to = "species",
    values_to = "count"
  ) %>%
  # calculate total count and proportions for each settlement type within each season
  group_by(season, method) %>%
  mutate(
    total_count = sum(count, na.rm = TRUE), # total count for the settlement type and season
    proportion = (count / total_count) * 100 # proportion for each species
  ) %>%
  ungroup()

# summarize counts by species and season
species_summary <- species_inventory_by_season %>%
  group_by(species, season, method) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  group_by(season, method) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  ungroup()

# rename species for presentation
species_summary <- species_summary %>%
  mutate(
    species = case_when(
      species == "total_An.gambiae" ~ "An. gambiae",
      species == "total_An.funestus" ~ "An. funestus",
      species == "total_Culicine" ~ "Culicine",
      species == "total_Other" ~ "Other Anopheles",
      TRUE ~ species
    )
  )

# create abundance column with count and percentage
species_summary <- species_summary %>%
  mutate(
    abundance = paste0(count, " (", round(proportion, 1), "%)")
  )

###Updated data and plot for anopheles alone
species_inventory_by_season <- all_ento_data %>%
  group_by(season, method) %>% 
  summarise(
    total_An.gambiae = sum(An.gambiae, na.rm = TRUE),
    total_An.funestus = sum(An.funestus, na.rm = TRUE),
    total_Culicine = sum(Culicine, na.rm = TRUE),
    total_Other = sum(Other, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("total_"), # include all species columns starting with "total_"
    names_to = "species",
    values_to = "count"
  ) %>%
  # calculate total count and proportions for each settlement type within each season
  group_by(season, method) %>%
  mutate(
    total_count = sum(count, na.rm = TRUE), # total count for the settlement type and season
    proportion = (count / total_count) * 100 # proportion for each species
  ) %>%
  ungroup()


# Filter out Culicine and clean up species labels
plot_df_anoph <- species_inventory_by_season_method %>%
  filter(species != "total_Culicine") %>%
  mutate(
    species = case_when(
      species == "total_An.gambiae"      ~ "An. gambiae",
      species == "total_An.funestus"     ~ "An. funestus",
      species == "total_Other_Anopheles" ~ "Other Anopheles",
      TRUE ~ species
    ),
    season = str_to_title(season),
    method = factor(method, levels = c("CDC", "PSC"))
  )

# Recompute proportion among Anopheles only (excluding Culicine)
plot_df_anoph <- plot_df_anoph %>%
  group_by(season, settlement_type, method) %>%
  mutate(anopheles_total = sum(count),
         an_proportion = if_else(anopheles_total > 0, 100 * count / anopheles_total, 0)) %>%
  ungroup()

# --- Plot 1: raw counts n % ---
plot_df_anoph <- plot_df_anoph %>%
  mutate(bar_label = if_else(count > 0, paste0(count, "\n(", round(an_proportion, 1), "%)"), ""))

# Fig1ai <- ggplot(plot_df_anoph, aes(x = settlement_type, y = count, fill = species)) +
#   geom_col(position = position_dodge(width = 0.75), width = 0.7) +
#   geom_text(
#     aes(label = bar_label),
#     position = position_dodge(width = 0.75),
#     vjust = -0.3,
#     size = 2.8,
#     lineheight = 0.85
#   ) +
#   facet_grid(method ~ season, labeller = label_both) +
#   scale_fill_manual(values = c(
#     "An. gambiae"     = "#1b7837",
#     "An. funestus"    = "#762a83",
#     "Other Anopheles" = "#b8860b"
#   )) +
#   labs(
#     x = "Settlement type",
#     y = "Anopheles count",
#     fill = "Species",
#     title = "Anopheles species collected by season, settlement type and method"
#   ) +
#   theme_bw(base_size = 12) +
#   theme(
#     strip.background = element_rect(fill = "grey90"),
#     panel.grid.minor = element_blank(),
#     legend.position = "bottom"
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.15))) # headroom for labels
# 
# ggsave("Seasonal distribution of Anophles by settlement and method.pdf", Fig1ai, width = 10, height = 8)


##make another plot for Fig A to capture method of data collection
#-----------------------------
# Clean species_inventory_by_season into plot_df
#-----------------------------
# Aggregate species_inventory_by_season with location split for CDC
#-----------------------------
species_inventory_by_season_ml <- all_ento_data %>%
  group_by(season, method, location) %>%
  summarise(
    total_An.gambiae  = sum(An.gambiae, na.rm = TRUE),
    total_An.funestus = sum(An.funestus, na.rm = TRUE),
    total_Culicine    = sum(Culicine, na.rm = TRUE),
    total_Other       = sum(Other, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("total_"),
    names_to = "species",
    values_to = "count"
  ) %>%
  # combine method + location: CDC splits by Indoor/Outdoor, PSC stays as "PSC"
  mutate(
    method_location = case_when(
      method == "CDC" ~ paste("CDC", location),
      TRUE ~ method
    )
  ) %>%
  group_by(season, method_location) %>%
  mutate(
    total_count = sum(count, na.rm = TRUE),
    proportion = (count / total_count) * 100
  ) %>%
  ungroup()


#-----------------------------
# Clean into plot_df
#-----------------------------
plot_df_ml <- species_inventory_by_season_ml %>%
  mutate(
    species = case_when(
      species == "total_An.gambiae"  ~ "An. gambiae",
      species == "total_An.funestus" ~ "An. funestus",
      species == "total_Culicine"    ~ "Culicine",
      species == "total_Other"       ~ "Other Anopheles",
      TRUE ~ species
    )
  ) %>%
  dplyr::select(species, season, method = method_location, count)
#-----------------------------

# plot_df <- species_inventory_by_season %>%
#   mutate(
#     species = case_when(
#       species == "total_An.gambiae"  ~ "An. gambiae",
#       species == "total_An.funestus" ~ "An. funestus",
#       species == "total_Culicine"    ~ "Culicine",
#       species == "total_Other"       ~ "Other Anopheles",
#       TRUE ~ species
#     )
#   ) %>%
#   dplyr::select(species, season, method, count)

#-----------------------------
# Function
#-----------------------------
bar_of_pie <- function(seas){
  
  #-----------------------------
  # PIE DATA — aggregate across methods, Culicine vs Anopheles
  #-----------------------------
  pie_df <- plot_df_ml %>%
    filter(season == seas) %>%
    mutate(group = ifelse(species == "Culicine", "Culicine", "Anopheles")) %>%
    group_by(group) %>%
    summarise(count = sum(count), .groups = "drop") %>%
    mutate(prop = count / sum(count)) %>%
    arrange(group) %>%
    mutate(
      ymax = cumsum(prop),
      ymin = lag(ymax, default = 0)
    )
  
  anoph <- pie_df %>% filter(group == "Anopheles")
  
  theta_top <- 2 * pi * anoph$ymin
  theta_bottom <- 2 * pi * anoph$ymax
  
  x_top <- cos(theta_top);    y_top <- sin(theta_top)
  x_bottom <- cos(theta_bottom); y_bottom <- sin(theta_bottom)
  
  pie_x <- function(x) 0.25 + x * 0.18
  pie_y <- function(y) 0.50 + y * 0.40
  
  x1 <- pie_x(x_top);    y1 <- pie_y(y_top)
  x2 <- pie_x(x_bottom); y2 <- pie_y(y_bottom)
  
  p_pie <- ggplot(pie_df, aes(x = 1, y = count, fill = group)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    xlim(0, 1.6) +
    geom_text(
      aes(label = paste0(group, "\n", count, " (", round(prop*100,1), "%)")),
      position = position_stack(vjust = 0.5),
      size = 4
    ) +
    scale_fill_manual(values = c(
      "Culicine" = "#7FC97F",
      "Anopheles" = "#BEAED4"
    )) +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = paste0(toupper(seas), " season"))
  
  #-----------------------------
  # BAR DATA — species (excl. Culicine) split by method
  #-----------------------------
  bar_df <- plot_df_ml %>%
    filter(season == seas, species != "Culicine") %>%
    group_by(species) %>%
    mutate(species_total = sum(count)) %>%
    ungroup()
  
  species_order <- bar_df %>%
    distinct(species, species_total) %>%
    arrange(species_total) %>%
    pull(species)
  
  bar_df <- bar_df %>%
    mutate(species = factor(species, levels = species_order))
  
  label_df <- bar_df %>%
    distinct(species, species_total)
  
  p_bar <- ggplot(bar_df, aes(x = species, y = count, fill = method)) +
    geom_col(position = "stack") +
    geom_text(
      aes(label = ifelse(count > 0, count, "")),
      position = position_stack(vjust = 0.5),
      size = 3.2,
      color = "white"
    ) +
    geom_text(
      data = label_df,
      aes(x = species, y = species_total, label = species_total),
      inherit.aes = FALSE,
      hjust = -0.2,
      size = 3.8
    ) +
    coord_flip() +
    scale_y_continuous(
      limits = c(0, ifelse(seas == "dry", 30, 250)),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(values = c(
      "CDC Indoor"  = "#1B9E77",
      "CDC Outdoor" = "#66F2EB",
      "PSC"         = "#D95F02"
    )) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(title = "Anopheles breakdown", x = NULL, y = "Count", fill = "Method")
  
  #-----------------------------
  # FINAL COMPOSITION + CONNECTORS
  #-----------------------------
  ggdraw() +
    draw_plot(p_pie, x = 0, y = 0, width = 0.5, height = 1) +
    draw_plot(p_bar, x = 0.55, y = 0.15, width = 0.42, height = 0.7) +
    draw_line(x = c(x1, 0.55), y = c(y1, 0.75), size = 0.8) +
    draw_line(x = c(x2, 0.55), y = c(y2, 0.25), size = 0.8)
}

#-----------------------------
# Run
#-----------------------------
wet_plot <- bar_of_pie("wet")
dry_plot <- bar_of_pie("dry")

Fig1aii <- plot_grid(wet_plot, dry_plot, ncol = 1)

ggsave("Seasonal distribution of mosquito relative abundance2.pdf", Fig1aii, width = 10, height = 8)





##Fig 1E
## =========================================================================================================================================
### INDOOR RESIDUAL DENSITY (IRD) - Using PSC Data
# Indoor Resting Density (IRD) = Number of Anopheles mosquitoes collected divided by the total number of rooms sampled
## =========================================================================================================================================

# filter data to include only PSC wet and dry (no CDC data)
psc_wet_dry = subset(all_ento_data, (method %in% c("PSC")))

ano_caught_psc <- psc_wet_dry %>% 
  group_by(settlement_type, season) %>%
  summarise(anopheles_caught = sum(An.gambiae + An.funestus + Other, na.rm = TRUE)) %>%  # sum Anopheles, handling NA
  ungroup()

# set number of rooms sampled to 40 for dry season and 120 for wet season
ano_caught_psc <- ano_caught_psc %>%
  mutate(no_rooms_sampled = ifelse(season == "dry", 40, 120))

# calculate IRD (# of mosquitoes collected / number of rooms sampled)
ano_caught_psc <- ano_caught_psc %>%
  mutate(IRD = anopheles_caught / no_rooms_sampled)

ano_caught_psc <- ano_caught_psc |> 
  mutate(
    ward = case_when(
      settlement_type == "Formal"   & season == "dry" ~ "Olopomewa",
      settlement_type == "Informal" & season == "dry" ~ "Challenge",
      settlement_type == "Slum"     & season == "dry" ~ "Agugu",
      settlement_type == "Formal"   & season == "wet" ~ "Challenge",
      settlement_type == "Slum"     & season == "wet" ~ "Agugu",
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(ward, .before = settlement_type)


##Compute CIs
ano_caught_psc <- ano_caught_psc %>%
  mutate(
    ci_exact = map2(
      anopheles_caught, no_rooms_sampled,
      ~ poisson.test(.x, T = .y)$conf.int
    ),
    ci_low_exact  = map_dbl(ci_exact, 1),
    ci_high_exact = map_dbl(ci_exact, 2)
  ) %>%
  dplyr::select(-ci_exact)


ano_caught_psc
