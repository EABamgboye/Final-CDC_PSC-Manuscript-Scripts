## =========================================================================================================================================
### MOLECULAR ID ANALYSIS
## =========================================================================================================================================
## =========================================================================================================================================
### Data Prep
## =========================================================================================================================================
EntoDat <- ("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Osun-excel")
shapepath <- ("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_shape_files")
WetData <- ("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Wet Season Data_Ibadan")
## -----------------------------------------------------------------------------------------------------------------------------------------



molecular_df <- readxl::read_xlsx(file.path(WetData, "Copy of Molecular analysis of ibadan samples.xlsx"))


library(dplyr)

sporozoite_rate_df <- molecular_df |> 
  filter(!is.na(`Sporozoite Result`)) |>  # only mosquitoes actually tested
  group_by(Area, Method) |> 
  summarise(
    n_tested = n(),
    n_positive = sum(`Sporozoite Result` == "Positive"),
    sporozoite_rate = round((n_positive / n_tested) * 100, 2),
    .groups = "drop"
  )

sporozoite_rate_df


sporozoite_rate_by_species <- molecular_df |> 
  filter(!is.na(`Sporozoite Result`)) |> 
  group_by(Area, Method, Species) |> 
  summarise(
    n_tested = n(),
    n_positive = sum(`Sporozoite Result` == "Positive"),
    sporozoite_rate = round((n_positive / n_tested) * 100, 2),
    .groups = "drop"
  )


molecular_df |> 
  filter(!is.na(`Sporozoite Result`)) |> 
  summarise(
    n_tested = n(),
    n_positive = sum(`Sporozoite Result` == "Positive"),
    sporozoite_rate = round((n_positive / n_tested) * 100, 2)
  )

sporozoite_rate_df <- molecular_df |> 
  filter(!is.na(`Sporozoite Result`)) |> 
  mutate(
    Status_clean = str_to_title(Status),  # standardizes "fed"/"Fed" -> "Fed", "gravid" -> "Gravid"
    grouping_status = if_else(Method == "PSC", "All", Status_clean)
  ) |> 
  group_by(Area, Species, Method, grouping_status) |> 
  summarise(
    n_tested = n(),
    n_positive = sum(`Sporozoite Result` == "Positive"),
    sporozoite_rate = round((n_positive / n_tested) * 100, 2),
    .groups = "drop"
  )

sporozoite_rate_df


library(ggplot2)
library(dplyr)
library(binom)  # for Wilson CIs

# Compute Wilson score confidence intervals
sporozoite_rate_df <- sporozoite_rate_df |> 
  rowwise() |> 
  mutate(
    ci = list(binom.confint(n_positive, n_tested, methods = "wilson")),
    lower_ci = ci$lower * 100,
    upper_ci = ci$upper * 100
  ) |> 
  ungroup() |> 
  dplyr::select(-ci) |> 
  mutate(
    group_label = paste0(Area, " — ", Species, " (", Method, 
                         if_else(grouping_status != "All", paste0(", ", grouping_status), ""), 
                         ")"),
    group_label = forcats::fct_reorder(group_label, sporozoite_rate)
  )

ggplot(sporozoite_rate_df, aes(x = sporozoite_rate, y = group_label)) +
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, colour = "grey40") +
  geom_point(aes(size = n_tested), colour = "#8B0000") +
  geom_text(aes(label = paste0(sporozoite_rate, "% (", n_positive, "/", n_tested, ")")), 
            hjust = -0.15, vjust = -0.8, size = 3) +
  scale_size_continuous(name = "N tested", range = c(2, 8)) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0.02, 0.25))) +
  labs(
    x = "Sporozoite Rate (%, Wilson 95% CI)",
    y = NULL,
    title = "Plasmodium Sporozoite Rate by Area, Species, and Collection Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 10)
  )


library(ggplot2)
library(dplyr)
library(forcats)

sporozoite_rate_df <- sporozoite_rate_df |> 
  mutate(
    group_label = if_else(grouping_status != "All", 
                          paste0(Species, " (", grouping_status, ")"), 
                          Species)
  ) |> 
  arrange(Area, Method, grouping_status, desc(sporozoite_rate)) |> 
  mutate(group_label = fct_inorder(group_label))

ggplot(sporozoite_rate_df, aes(x = sporozoite_rate, y = group_label)) +
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, colour = "grey40") +
  geom_point(aes(size = n_tested), colour = "#8B0000") +
  geom_text(aes(label = paste0(sporozoite_rate, "% (", n_positive, "/", n_tested, ")")), 
            hjust = -0.15, vjust = -0.8, size = 3) +
  facet_grid(Area ~ Method, scales = "free_y", space = "free_y") +
  scale_size_continuous(name = "N tested", range = c(2, 8)) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0.02, 0.3))) +
  labs(
    x = "Sporozoite Rate (%, Wilson 95% CI)",
    y = NULL,
    title = "Plasmodium Sporozoite Rate by Area, Species, and Collection Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey90", color = NA)
  )


library(ggplot2)
library(dplyr)
library(forcats)

sporozoite_rate_df <- sporozoite_rate_df |> 
  mutate(
    group_label = Species,  # no longer need status in the label itself
    facet_row = if_else(Method == "PSC", "PSC", grouping_status)  # "Indoor"/"Outdoor" for CDC, "PSC" for PSC
  ) |> 
  arrange(Area, Method, grouping_status, desc(sporozoite_rate)) |> 
  mutate(group_label = fct_inorder(group_label))

sprop <- ggplot(sporozoite_rate_df, aes(x = sporozoite_rate, y = group_label)) +
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.2, colour = "grey40") +
  geom_point(aes(size = n_tested), colour = "#8B0000") +
  geom_text(aes(label = paste0(sporozoite_rate, "% (", n_positive, "/", n_tested, ")")), 
            hjust = -0.15, vjust = -0.8, size = 3) +
  facet_grid(Area + facet_row ~ Method, scales = "free_y", space = "free_y") +
  scale_size_continuous(name = "N tested", range = c(2, 8)) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0.02, 0.3))) +
  labs(
    x = "Sporozoite Rate (%, Wilson 95% CI)",
    y = NULL,
    title = "Plasmodium Sporozoite Rate by Ward, Species, and Collection Method"
  ) +
  theme_manuscript() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text.y = element_text(angle = 0)
  )

##Corrected Dataframe and Plot
library(dplyr)

psc_species <- c("An. coustani", "An. hervyi", "An. parensi")

molecular_df_corrected <- molecular_df %>%
  mutate(
    Method = as.character(Method),
    Method = if_else(
      Species %in% psc_species,
      "PSC",
      Method
    )
  )
library(dplyr)
library(purrr)
library(ggplot2)

sporozoite_rate_df <- molecular_df_corrected %>%
  filter(!is.na(`Sporozoite Result`)) %>%
  group_by(Area, Method, Species) %>%
  summarise(
    n_tested = n(),
    n_positive = sum(`Sporozoite Result` == "Positive"),
    sporozoite_rate = n_positive / n_tested * 100,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    wilson_ci = list(
      prop.test(n_positive, n_tested, correct = FALSE)$conf.int * 100
    )
  ) %>%
  ungroup() %>%
  mutate(
    lower_ci = map_dbl(wilson_ci, 1),
    upper_ci = map_dbl(wilson_ci, 2)
  )


##CDC specific
sporozoite_rate_df <- molecular_df_corrected %>%
  filter(!is.na(`Sporozoite Result`)) %>%
  group_by(Area, Method, Status, Species) %>%
  summarise(
    n_tested = n(),
    n_positive = sum(`Sporozoite Result` == "Positive"),
    sporozoite_rate = n_positive / n_tested * 100,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    wilson_ci = list(
      prop.test(n_positive, n_tested, correct = FALSE)$conf.int * 100
    )
  ) %>%
  ungroup() %>%
  mutate(
    lower_ci = purrr::map_dbl(wilson_ci, 1),
    upper_ci = purrr::map_dbl(wilson_ci, 2)
  )

p_cdc <- sporozoite_rate_df %>%
  filter(Method == "CDC") %>%
  ggplot(aes(x = sporozoite_rate, y = Species)) +
  geom_errorbarh(
    aes(xmin = lower_ci, xmax = upper_ci),
    height = 0.2,
    colour = "grey40"
  ) +
  geom_point(aes(size = n_tested), colour = "#8B0000") +
  geom_text(
    aes(label = paste0(
      round(sporozoite_rate, 1), "% (",
      n_positive, "/", n_tested, ")"
    )),
    hjust = -0.15,
    vjust = -0.8,
    size = 3
  ) +
  facet_grid(
    Area ~ Status,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_size_continuous(
    name = "N tested",
    range = c(2, 8),
    limits = range(sporozoite_rate_df$n_tested, na.rm = TRUE)
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0.02, 0.3))
  ) +
  labs(
    x = "Sporozoite rate (%, Wilson 95% CI)",
    y = NULL,
    title = "CDC-derived Plasmodium sporozoite rate by ward, location, and species"
  ) +
  theme_manuscript()

p_cdc



##Others
plot_sporozoite <- function(data, method_name) {
  
  data %>%
    filter(Method == method_name) %>%
    ggplot(aes(x = sporozoite_rate, y = Species)) +
    geom_errorbarh(
      aes(xmin = lower_ci, xmax = upper_ci),
      height = 0.2,
      colour = "grey40"
    ) +
    geom_point(aes(size = n_tested), colour = "#8B0000") +
    geom_text(
      aes(label = paste0(
        round(sporozoite_rate, 1), "% (",
        n_positive, "/", n_tested, ")"
      )),
      hjust = -0.15,
      vjust = -0.8,
      size = 3
    ) +
    facet_grid(
      Area ~ .,
      scales = "free_y",
      space = "free_y"
    ) +
    scale_size_continuous(
      name = "N tested",
      range = c(2, 8),
      limits = range(sporozoite_rate_df$n_tested, na.rm = TRUE)
    ) +
    scale_x_continuous(
      limits = c(0, 100),
      expand = expansion(mult = c(0.02, 0.3))
    ) +
    labs(
      x = "Sporozoite rate (%, Wilson 95% CI)",
      y = NULL,
      title = paste(
        "Plasmodium sporozoite rate by ward and species —",
        method_name
      )
    ) +
    theme_manuscript() 
}

#p_cdc <- plot_sporozoite(sporozoite_rate_df, "CDC")
p_psc <- plot_sporozoite(sporozoite_rate_df, "PSC")

p_cdc
p_psc

ggsave('CDC_sporozoite_method_plot.pdf', plot = p_cdc, width = 8, height = 6)

ggsave('PSC_sporozoite_method_plot.pdf', plot = p_psc, width = 8, height = 6)

##Recompute EIR for wet season alone
hbr_mapped <- ano_caught_cdc |>
  filter(season == "wet") |>
  mutate(
    Area = case_when(
      settlement_type == "Slum"    ~ "Agugu",
      settlement_type == "Formal"  ~ "Challenge",
      TRUE ~ NA_character_   # Informal has no Area match yet
    )
  ) |>
  filter(!is.na(Area))

sporozoite_rate_df |>
  distinct(Method, Species, grouping_status) |>
  mutate(
    Method_q = paste0("'", Method, "'"),
    Species_q = paste0("'", Species, "'"),
    grouping_q = paste0("'", grouping_status, "'")
  )

sr_wet_cdc <- sporozoite_rate_df |>
  filter(
    trimws(Method) == "CDC",
    grepl("gambiae", Species, ignore.case = TRUE),
    trimws(grouping_status) %in% c("Indoor", "Outdoor")
  ) |>
  transmute(
    Area,
    location = trimws(grouping_status),
    SR = n_positive / n_tested
  )

sr_wet_cdc


eir_wet <- hbr_mapped |>
  inner_join(sr_wet_cdc, by = c("Area", "location")) |>
  mutate(
    EIR_nightly = HBR * SR,
    EIR_annual  = EIR_nightly * 365   # optional; wet-only, so prefer nightly
  ) |>
  dplyr::select(Area, settlement_type, location, season, HBR, SR, EIR_nightly, EIR_annual)

eir_wet

##Final script for EIR plot
# ============================================================
# Wet-season EIR figure (Lancet-style 3-panel)
# Requires in environment:
#   - hbr_mapped
#   - sporozoite_rate_df
# ============================================================

library(tidyverse)
library(patchwork)

# ---- 1. Sporozoite rate (CDC, An. gambiae s.l.) + 95% CI ----
sr_ci <- sporozoite_rate_df |>
  filter(
    trimws(Method) == "CDC",
    grepl("gambiae", Species, ignore.case = TRUE),
    trimws(grouping_status) %in% c("Indoor", "Outdoor")
  ) |>
  rowwise() |>
  mutate(
    location = trimws(grouping_status),
    SR = n_positive / n_tested,
    SR_lo = binom.test(n_positive, n_tested)$conf.int[1],
    SR_hi = binom.test(n_positive, n_tested)$conf.int[2]
  ) |>
  ungroup() |>
  dplyr::select(Area, location, n_tested, n_positive, SR, SR_lo, SR_hi)

# ---- 2. Join HBR + SR and compute EIR ----
eir_wet <- hbr_mapped |>
  inner_join(
    sr_ci |> dplyr::select(Area, location, SR, SR_lo, SR_hi, n_tested, n_positive),
    by = c("Area", "location")
  ) |>
  mutate(
    EIR_nightly = HBR * SR,
    EIR_annual  = EIR_nightly * 365
  )

print(eir_wet)

# ---- 3. Shared theme / colours ----
pal <- c(Indoor = "#1B4F72", Outdoor = "#B9770E")

theme_lancet <- theme_classic(base_size = 10) +
  theme(
    plot.tag = element_text(face = "bold", size = 11),
    axis.title = element_text(size = 9),
    axis.text = element_text(colour = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(4, 6, 4, 4)
  )

# ---- 4A. HBR ----
p_hbr <- ggplot(eir_wet, aes(x = Area, y = HBR, fill = location)) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.65,
    colour = "black",
    linewidth = 0.25
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    tag = "A",
    x = NULL,
    y = "HBR\n(bites/person/night)",
    fill = NULL
  ) +
  theme_lancet +
  theme(legend.position = "none")

# ---- 4B. SR with 95% CI ----
p_sr <- ggplot(eir_wet, aes(x = Area, y = SR, colour = location)) +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_pointrange(
    aes(ymin = SR_lo, ymax = SR_hi),
    position = position_dodge(width = 0.45),
    size = 0.35,
    linewidth = 0.5
  ) +
  scale_colour_manual(values = pal) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    tag = "B",
    x = NULL,
    y = "Sporozoite rate\n(95% CI)",
    colour = NULL
  ) +
  theme_lancet +
  theme(legend.position = "none")

# ---- 4C. EIR dumbbell (Indoor vs Outdoor) ----
eir_wide <- eir_wet |>
  dplyr::select(Area, location, EIR_nightly) |>
  tidyr::pivot_wider(names_from = location, values_from = EIR_nightly)

p_eir <- ggplot(eir_wide, aes(y = Area)) +
  geom_segment(
    aes(x = Indoor, xend = Outdoor, yend = Area),
    colour = "grey75",
    linewidth = 0.9
  ) +
  geom_point(aes(x = Indoor, colour = "Indoor"), size = 3.2) +
  geom_point(aes(x = Outdoor, colour = "Outdoor"), size = 3.2) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  labs(
    tag = "C",
    x = "EIR (infectious bites/person/night)",
    y = NULL,
    colour = NULL
  ) +
  theme_lancet

# ---- 5. Combine ----
fig_eir <- (p_hbr | p_sr | p_eir) +
  plot_layout(guides = "collect", widths = c(1, 1, 1.15)) &
  theme(legend.position = "bottom")

fig_eir <- fig_eir +
  plot_annotation(
    title = "Wet-season entomological inoculation rate",
    subtitle = "CDC light traps; An. gambiae s.l. · Informal settlement not sampled in the wet season",
    theme = theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8.5, colour = "grey30")
    )
  )

print(fig_eir)

# ---- 6. Save (Lancet ~2-column width) ----
ggsave(
  filename = "fig_EIR_wet_cdc.pdf",
  plot = fig_eir,
  width = 7.2,
  height = 3.3,
  units = "in",
  device = cairo_pdf
)

ggsave(
  filename = "fig_EIR_wet_cdc.pdf",
  plot = fig_eir,
  width = 8.2,
  height = 4.3,
  units = "in",
  dpi = 300
)

