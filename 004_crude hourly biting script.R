library(readxl)
library(tidyverse)
## =========================================================================================================================================
### Data Prep
## =========================================================================================================================================
EntoDat <- ("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Osun-excel")
shapepath <- ("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_shape_files")
WetData <- ("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Wet Season Data_Ibadan")
## -----------------------------------------------------------------------------------------------------------------------------------------


##---CDC hourly biting profile analysis
cdc <- read_excel(file.path(EntoDat, "cdc_ibadan_dry.xlsx"))

cdc_wet <- read_excel(file.path(WetData,"WET_SEASON_ENTO_COLLECTION_CDC_-_all_versions_-_labels_-_2024-08-12-21-20-44.xlsx"))

cdc_wet <- cdc_wet |> 
  slice(4:399)

cdc_wet <- cdc_wet %>%
  mutate(`Ward Name` = case_when(
    `Ward Name` == "Zango" ~ "Challenge",
    TRUE ~ `Ward Name`
  ))

cdc_wet <- cdc_wet %>%
  mutate(`Settlement Type` = case_when(
    `Settlement Type` == "Informal" ~ "Slum",
    TRUE ~ `Settlement Type`
  ))

## =========================================================================================================================================
### Indoor Transmission: Ibadan
## =========================================================================================================================================
##Dry
# filter the dataset for indoor CDC collections and summarize by group
indoor_cdc <- cdc %>%
  dplyr::filter(Location == "Indoor") %>% 
  group_by(`Ward Name`,`Settlement Classification`, `Time of Collection`) %>%
  summarise(total_mosquitoes = sum(`Total Anopheles`, na.rm = TRUE)) %>% # calculate total mosquitoes
  ungroup()

indoor_cdc$`Location of CDC` <- "Indoor"

# # create a plot for hourly indoor biting of anopheles mosquitoes (Ibadan only)
# indoor_anopheles_plot <- ggplot(data = indoor_cdc, aes(
#   x = `Time of Collection`, 
#   y = total_mosquitoes, 
#   group = `Settlement Classification`, 
#   colour = `Settlement Classification`)) +
#   scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "4-5am", "5-6am")) + 
#   geom_point() + 
#   labs(
#     y = "Total Number of Anopheles \nMosquitos Caught per Hour",
#     x = "Time of Collection"
#   ) +
#   geom_line() + 
#   ggtitle("Hourly Indoor Biting") + 
#   geom_point(size = 3.0) + 
#   theme(plot.title = element_text(size = 12)) + 
#   theme_manuscript() + 
#   theme(
#     legend.position = c(.27, .95),
#     legend.justification = c("right", "top"),
#     legend.box.just = "right",
#     legend.margin = margin(6, 6, 6, 6),
#     axis.text.x = element_text(size = 10),
#     legend.key.size = unit(0.8, "lines"),
#     legend.title = element_text(size = 10),
#     legend.text = element_text(size = 9),
#     legend.background = element_rect(color = "black", size = 0.5)
#   )
# indoor_anopheles_plot
# 
# # save the indoor plot as a PDF file
# ggsave(file.path(NewFigDir, '_indoor_cdc_ibadan.pdf'), plot = indoor_anopheles_plot, width = 8, height = 6)

## =========================================================================================================================================
### Outdoor Transmission: Ibadan
## =========================================================================================================================================

# filter the dataset for outdoor CDC collections and summarize by group
outdoor_cdc <- cdc %>%
  dplyr::filter(Location == "Outdoor") %>%
  group_by(`Ward Name`,`Settlement Classification`, `Time of Collection`) %>%
  summarise(total_mosquitoes = sum(`Total Anopheles`)) %>% # calculate total mosquitoes
  ungroup()

outdoor_cdc$`Location of CDC` <- "Outdoor"


# # create a plot for hourly outdoor biting of anopheles mosquitoes
# outdoor_anopheles_plot <- ggplot(data = outdoor_cdc, aes(
#   x = `Time of Collection`, 
#   y = total_mosquitoes, 
#   group = `Settlement Classification`, 
#   colour = `Settlement Classification`
# )) + 
#   scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "4-5am", "5-6am")) + 
#   geom_point() + 
#   labs(
#     y = "Total Number of Anopheles \nMosquitos Caught per Hour",
#     x = "Time of Collection"
#   ) + 
#   geom_line() + 
#   ggtitle("Hourly Outdoor Biting") + 
#   geom_point(size = 3.0) + 
#   theme(plot.title = element_text(size = 12)) + 
#   theme_manuscript() + 
#   theme(
#     legend.position = c(.27, .95),
#     legend.justification = c("right", "top"),
#     legend.box.just = "right",
#     legend.margin = margin(6, 6, 6, 6),
#     axis.text.x = element_text(size = 10),
#     legend.key.size = unit(0.8, "lines"),
#     legend.title = element_text(size = 10),
#     legend.text = element_text(size = 9),
#     legend.background = element_rect(color = "black", size = 0.5)
#   )
# outdoor_anopheles_plot
# 
# # save the outdoor plot as a PDF file
# ggsave(file.path(NewFigDir, '_outdoor_cdc_ibadan.pdf'), plot = outdoor_anopheles_plot, width = 8, height = 6)

# # remove x axis label from indoor biting plot
# indoor_anopheles_plot <- indoor_anopheles_plot + theme(axis.title.x = element_blank())
# 
# # arrange indoor biting and outdoor biting plots into a grid
# hourly_biting_plots <- grid.arrange(indoor_anopheles_plot, outdoor_anopheles_plot, nrow = 2, ncol = 1)
# 
# # save combined plots as .pdf
# ggsave(file.path(NewFigDir, '_bitingrate_cdc_ibadan.pdf'), plot = hourly_biting_plots, width = 10, height = 6)


##Wet Season CDC hourly biting
##Disregard this "#Refer to script "cdc_wet_anopheles_hourly_summary"
##Plot hourly biting time combined wet and dry season 
#Refer to script "mosquito_hourly_biting_wet_dry".....

##Set hour factors
hour_labels <- c("6-7pm","7-8pm","8-9pm","9-10pm","10-11pm","11-12am",
                 "12-1am","1-2am","2-3am","3-4am","4-5am","5-6am")

block_start <- seq(11, by = 9, length.out = 12)

anoph_long <- map2_dfr(block_start, hour_labels, function(start_col, hr) {
  cdc_wet %>%
    transmute(
      `Ward Name`         = `Ward Name`,
      `Settlement Type`   = `Settlement Type`,
      `Location of CDC`   = `Location of CDC`,
      hour                = hr,
      gambiae             = as.numeric(.data[[names(cdc_wet)[start_col + 6]]]),
      funestus            = as.numeric(.data[[names(cdc_wet)[start_col + 7]]])
    )
})

anoph_long <- anoph_long %>%
  mutate(
    anopheles_total = replace_na(gambiae, 0) + replace_na(funestus, 0),
    hour = factor(hour, levels = hour_labels)
  )

##Make another data frame with separate species

hour_labels <- c("6-7pm","7-8pm","8-9pm","9-10pm","10-11pm","11-12am",
                 "12-1am","1-2am","2-3am","3-4am","4-5am","5-6am")
block_start <- seq(11, by = 9, length.out = 12)

# 1. Reshape wide CDC blocks into long format, keeping gambiae/funestus separate
anoph_long_species <- map2_dfr(block_start, hour_labels, function(start_col, hr) {
  cdc_wet %>%
    transmute(
      `Ward Name`         = `Ward Name`,
      `Settlement Type`   = `Settlement Type`,
      `Location of CDC`   = `Location of CDC`,
      hour                = hr,
      gambiae             = as.numeric(.data[[names(cdc_wet)[start_col + 6]]]),
      funestus            = as.numeric(.data[[names(cdc_wet)[start_col + 7]]])
    )
})

# 2. Pivot species into rows instead of columns
anoph_long_species <- anoph_long_species %>%
  pivot_longer(cols = c(gambiae, funestus), names_to = "species", values_to = "count") %>%
  mutate(
    count = replace_na(count, 0),
    hour = factor(hour, levels = hour_labels),
    species = recode(species, "gambiae" = "An. gambiae", "funestus" = "An. funestus")
  )

# 3. Apply the same Challenge Settlement Type fix as before (re-check this applies here too)
anoph_long_species <- anoph_long_species %>%
  mutate(`Settlement Type` = if_else(`Ward Name` == "Challenge", "Formal", `Settlement Type`))

# 4. Summarise by hour, species, ward (add season here via your existing merge step if needed)
hourly_species_summary <- anoph_long_species %>%
  group_by(`Ward Name`, `Settlement Type`, `Location of CDC`, hour, species) %>%
  summarise(total = sum(count, na.rm = TRUE), .groups = "drop")

# Check what values Location of CDC actually contains before proceeding
table(anoph_long$`Location of CDC`, useNA = "always")

# Fix at the anoph_long stage, before grouping
anoph_long <- anoph_long %>%
  mutate(`Settlement Type` = if_else(
    `Ward Name` == "Challenge",
    "Formal",
    `Settlement Type`
  ))

# Now regroup — Challenge/Outdoor rows will correctly collapse into one row per hour
hourly_summary <- anoph_long %>%
  group_by(`Ward Name`, `Settlement Type`, `Location of CDC`, hour) %>%
  summarise(total_anopheles = sum(anopheles_total, na.rm = TRUE), .groups = "drop")

##Combine Wet and Dry Season Data
# Standardize indoor/outdoor_cdc column names to match hourly_summary
outdoor_cdc <- outdoor_cdc %>%
  rename(
    `Settlement Type`  = `Settlement Classification`,
    hour               = `Time of Collection`,
    total_anopheles    = total_mosquitoes
  )

indoor_cdc <- indoor_cdc %>%
  rename(
    `Settlement Type`  = `Settlement Classification`,
    hour               = `Time of Collection`,
    total_anopheles    = total_mosquitoes
  )


# Confirm all three have identical column names/order before merging
identical(sort(names(hourly_summary)), sort(names(indoor_cdc)))
identical(sort(names(hourly_summary)), sort(names(outdoor_cdc)))

##Add season column
indoor_cdc$season <- "Dry"
outdoor_cdc$season <- "Dry"
hourly_summary$season <- "Wet"

combined_cdc <- bind_rows(
  hourly_summary,
  indoor_cdc,
  outdoor_cdc
)

write.csv(combined_cdc, "combined_cdc_hrly_data.csv")

# Re-apply chronological hour ordering (bind_rows may have coerced factor to character)
hour_labels <- c("6-7pm","7-8pm","8-9pm","9-10pm","10-11pm","11-12am",
                 "12-1am","1-2am","2-3am","3-4am","4-5am","5-6am")
combined_cdc <- combined_cdc %>%
  mutate(hour = factor(hour, levels = hour_labels))



max_y <- max(combined_cdc$total_anopheles, na.rm = TRUE)


# Colorblind-safe palette 
ward_colors <- c("Agugu" = "#D55E00", "Challenge" = "#0072B2", "Olopomewa" = "#009E73")

max_y <- max(combined_cdc$total_anopheles, na.rm = TRUE)


##More data wrangling
# Define exactly which Ward × Settlement Type × Season combinations were actually sampled
valid_combos <- tribble(
  ~`Ward Name`, ~`Settlement Type`, ~season,
  "Olopomewa", "Formal",   "Dry",
  "Challenge", "Informal", "Dry",
  "Agugu",     "Slum",     "Dry",
  "Challenge", "Formal",   "Wet",
  "Agugu",     "Slum",     "Wet"
)

# Keep only rows matching a real sampled combination
combined_cdc_filtered <- combined_cdc %>%
  semi_join(valid_combos, by = c("Ward Name", "Settlement Type", "season"))

# Confirm this dropped exactly what we expect (no Informal-Wet, no cross-ward placeholders)
combined_cdc_filtered %>%
  distinct(`Ward Name`, `Settlement Type`, season) %>%
  arrange(`Settlement Type`, season)


##Summarize across Ward and Season
ward_season_summary <- combined_cdc_filtered %>%
  group_by(`Ward Name`, `Location of CDC`, hour, season) %>%
  summarise(total_anopheles = sum(total_anopheles, na.rm = TRUE), .groups = "drop")

ward_colors <- c("Agugu" = "#D55E00", "Challenge" = "#0072B2", "Olopomewa" = "#009E73")
max_y <- max(ward_season_summary$total_anopheles, na.rm = TRUE)

# fig2 <- ggplot(ward_season_summary, aes(x = hour, y = total_anopheles,
#                                         color = `Ward Name`, linetype = season,
#                                         group = interaction(`Ward Name`, season))) +
#   geom_line(linewidth = 0.7) +
#   geom_point(size = 1.6) +
#   facet_wrap(~ `Location of CDC`, nrow = 2) +
#   scale_color_manual(values = ward_colors, name = "Ward") +
#   scale_linetype_manual(values = c("Wet" = "solid", "Dry" = "22"), name = "Season") +
#   scale_y_continuous(breaks = seq(0, max_y, by = 2), expand = expansion(mult = c(0, 0.08))) +
#   labs(
#     x = "Time interval",
#     y = "Number of Anopheles collected (An. gambiae + An. funestus)"
#   ) +
#   theme_bw(base_size = 10) +
#   theme(
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_line(linewidth = 0.25, color = "grey88"),
#     strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.3),
#     strip.text = element_text(face = "bold", size = 9),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title = element_text(face = "bold", size = 10),
#     legend.position = "bottom",
#     panel.border = element_rect(color = "black", linewidth = 0.4, fill = NA)
#   )
# 
# fig2
##Final Figure 2A
fig2A <- ggplot(ward_season_summary, aes(x = hour, y = total_anopheles,
                                        color = `Ward Name`,
                                        group = `Ward Name`)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_grid( season~ `Location of CDC`) +
  scale_color_manual(values = ward_colors, name = "Ward") +
  scale_y_continuous(breaks = seq(0, max_y, by = 2), expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Time interval", y = "Number of Anopheles collected (An. gambiae + An. funestus)") +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, color = "grey88"),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.3),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", linewidth = 0.4, fill = NA)
  )

print(fig2A)

ggsave("Fig2A_ward_season_indoor_outdoor1.pdf", fig2A, width = 240, height = 130, units = "mm")
