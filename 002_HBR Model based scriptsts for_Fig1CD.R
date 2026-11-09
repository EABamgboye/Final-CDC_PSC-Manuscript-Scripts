# ============================================================
# 1. PACKAGES
# ============================================================

library(dplyr)
library(tidyr)
library(glmmTMB)
library(emmeans)
library(geepack)
library(MASS)
library(ggplot2)


# ============================================================
# 2. RESHAPE JANUARY-MARCH DATA
# ============================================================
cdc_dat <- read.csv("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/Osun-excel/cdc_dry_summary_night_wide.csv")


dry_long <- cdc_dat %>%
  
  dplyr::select(
    Household.Code,
    Ward.Name,
    Month,
    starts_with("anopheles_Indoor_N"),
    starts_with("anopheles_Outdoor_N")
  ) %>%
  
  pivot_longer(
    cols = starts_with("anopheles_"),
    names_to = c("Setting", "Night"),
    names_pattern = "anopheles_(Indoor|Outdoor)_(N[0-9]+)",
    values_to = "Anopheles"
  ) %>%
  
  # March N2 and N3 are not sampling occasions
  filter(!is.na(Anopheles)) %>%
  
  mutate(
    PersonNight = 1,
    
    Ward.Name = factor(
      Ward.Name,
      levels = c(
        "Agugu",
        "Challenge",
        "Olopomewa"
      )
    ),
    
    Month = factor(
      Month,
      levels = c(
        "January",
        "February",
        "March"
      )
    ),
    
    Setting = factor(
      Setting,
      levels = c(
        "Indoor",
        "Outdoor"
      )
    )
  )


# ============================================================
# 3. CHECK SAMPLING
# ============================================================

dry_long %>%
  group_by(Ward.Name, Setting) %>%
  summarise(
    PersonNights = n(),
    Anopheles = sum(Anopheles),
    .groups = "drop"
  )


# ============================================================
# 4. CRUDE HBR
# ============================================================

dcrude_HBR <- dry_long %>%
  group_by(Ward.Name, Setting) %>%
  summarise(
    Anopheles = sum(Anopheles),
    PersonNights = sum(PersonNight),
    HBR = Anopheles / PersonNights,
    .groups = "drop"
  )

dcrude_HBR


# ============================================================
# 5. POISSON
# ============================================================

dm_pois <- glmmTMB(
  Anopheles ~
    Ward.Name * Setting +
    Month +
    offset(log(PersonNight)),
  family = poisson,
  data = dry_long
)


# ============================================================
# 6. NEGATIVE BINOMIAL 1
# ============================================================

dm_nb1 <- glmmTMB(
  Anopheles ~
    Ward.Name * Setting +
    Month +
    offset(log(PersonNight)),
  family = nbinom1,
  data = dry_long
)


# ============================================================
# 7. NEGATIVE BINOMIAL 2
# ============================================================

dm_nb2 <- glmmTMB(
  Anopheles ~
    Ward.Name * Setting +
    Month +
    offset(log(PersonNight)),
  family = nbinom2,
  data = dry_long
)


# ============================================================
# 8. ZERO-INFLATED NEGATIVE BINOMIAL
# ============================================================

dm_zinb <- glmmTMB(
  Anopheles ~
    Ward.Name * Setting +
    Month +
    offset(log(PersonNight)),
  
  ziformula = ~1,
  
  family = nbinom2,
  
  data = dry_long
)


# ============================================================
# 9. HURDLE NEGATIVE BINOMIAL
# ============================================================

dm_hurdle <- glmmTMB(
  Anopheles ~
    Ward.Name * Setting +
    Month +
    offset(log(PersonNight)),
  
  ziformula = ~1,
  
  family = truncated_nbinom2,
  
  data = dry_long
)


# ============================================================
# 10. COMPARE MODELS
# ============================================================

AIC(
  dm_pois,
  dm_nb1,
  dm_nb2,
  dm_zinb,
  dm_hurdle
)


# ============================================================
# 11. NEGATIVE-BINOMIAL MIXED MODEL
#     Accounts for repeated observations within household
# ============================================================

m_nb2_mixed <- glmmTMB(
  Anopheles ~
    Ward.Name * Setting +
    Month +
    offset(log(PersonNight)) +
    (1 | Household.Code),
  
  family = nbinom2,
  
  data = dry_long
)

summary(m_nb2_mixed)


# ============================================================
# 12. MODEL-BASED HBR
# ============================================================

HBR_nb2 <- emmeans(
  m_nb2_mixed,
  ~ Ward.Name * Setting,
  type = "response"
)

HBR_nb2


# ============================================================
# 13. CONFIDENCE INTERVALS
# ============================================================

HBR_nb2_CI <- as.data.frame(
  confint(
    HBR_nb2,
    type = "response"
  )
)

HBR_nb2_CI


# ============================================================
# 14. ZINB HBR
# ============================================================

dHBR_zinb <- emmeans(
  dm_zinb,
  ~ Ward.Name * Setting,
  component = "response"
)

dHBR_zinb


# ============================================================
# 15. EXPORT RESULTS
# ============================================================

write.csv(
  HBR_nb2_CI,
  "HBR_NB2_mixed_estimates.csv",
  row.names = FALSE
)

write.csv(
  crude_HBR,
  "HBR_crude_estimates.csv",
  row.names = FALSE
)


#####-------------------FINAL CRUDE RATE-------------------------------------------#####
##Dry season
dcrude_HBR <- dry_long %>%
  group_by(Ward.Name, Setting) %>%
  summarise(
    Anopheles = sum(Anopheles),
    PersonNights = sum(PersonNight),
    HBR = Anopheles / PersonNights,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    CI_lower = poisson.test(Anopheles, PersonNights)$conf.int[1],
    CI_upper = poisson.test(Anopheles, PersonNights)$conf.int[2]
  ) %>%
  ungroup()

write.csv(crude_HBR, "HBR_crude_estimates.csv", row.names = FALSE)


# Estimate NB dispersion (theta) first — GEE-NB needs a fixed theta as input
dm_glmnb <- glm.nb(
  Anopheles ~ Ward.Name * Setting + Month + offset(log(PersonNight)),
  data = dry_long
)
theta_est <- dm_glmnb$theta

# Sort by cluster — geeglm requires clustered rows to be contiguous
dry_long <- dry_long %>% arrange(Household.Code)


dry_long <- dry_long %>%
  mutate(Household.Code = as.integer(factor(Household.Code))) %>%
  arrange(Household.Code)


# Check the NB dispersion estimate that's feeding into geeglm
summary(dm_glmnb)
theta_est
dm_glmnb$SE.theta

dm_gee_qp <- geeglm(
  Anopheles ~ Ward.Name + Setting + Month + offset(log(PersonNight)),
  id = Household.Code,
  family = poisson,
  corstr = "independence",
  data = dry_long
)

summary(dm_gee_qp)

dHBR_gee_qp <- emmeans(dm_gee_qp, ~ Ward.Name * Setting, type = "response")
dHBR_gee_qp_CI <- as.data.frame(confint(dHBR_gee_qp))

dHBR_gee_qp_CI


# dm_gee_qp_int <- geeglm(
#   Anopheles ~ Ward.Name * Setting + Month + offset(log(PersonNight)),
#   id = Household.Code,
#   family = poisson,
#   corstr = "independence",
#   data = dry_long
# )
# 
# summary(dm_gee_qp_int)
# 
# dHBR_gee_qp_int <- as.data.frame(confint(emmeans(dm_gee_qp_int, ~ Ward.Name * Setting, type = "response")))
# dHBR_gee_qp_int
# 

# dm_gee_nb <- geeglm(
#   Anopheles ~ Ward.Name + Setting + Month + offset(log(PersonNight)),
#   id = Household.Code,
#   family = MASS::negative.binomial(theta_est),
#   corstr = "exchangeable",
#   data = dry_long
# )
# 
# summary(dm_gee_nb)
# 
# HBR_gee <- emmeans(m_gee_nb, ~ Ward.Name * Setting, type = "response")
# HBR_gee_CI <- as.data.frame(confint(HBR_gee))

#write.csv(HBR_gee_CI, "HBR_GEE_NB_estimates.csv", row.names = FALSE)

##Visualizations

##Crude HBR 
pd1 <- ggplot(
  dcrude_HBR,
  aes(
    x = `Ward.Name`,
    y = HBR,
    ymin = CI_lower,
    ymax = CI_upper,
    color = Setting
  )
) +
  geom_pointrange(
    position = position_dodge(width = 0.35),
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = sprintf("%.2f", HBR)),
    position = position_dodge(width = 0.35),
    vjust = -1.2,
    hjust = 1.2,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Indoor" = "#2C7FB8", "Outdoor" = "#D95F02")) +
  #facet_wrap(~ Month) +
  labs(
    x = "Ward",
    y = "Anopheles Crude HBR\n(per person-night)",
    shape = "Collection setting"
  ) +
  theme_manuscript() +
  theme(
    legend.position = "top"
  )

pd1

ggsave("GEE_HBR_dry.pdf", pd1, width = 7, height = 9, units = "in")

# ###--2----Negative Binomial 1---------------------------------------------##
# emm_wet <- emmeans(
#   m_nb1_wet,
#   ~ `Ward.Name` * Setting,
#   data = wet_long,
#   type = "response"
# )
# 
# emm_wet
# 
# emm_df <- as.data.frame(emm_wet)
# 
# emm_df
# 
# p2 <- ggplot(
#   emm_df,
#   aes(
#     x = Ward.Name,
#     y = response,
#     ymin = asymp.LCL,
#     ymax = asymp.UCL,
#     shape = Setting,
#     color = Setting
#   )
# ) +
#   geom_pointrange(
#     position = position_dodge(width = 0.35),
#     linewidth = 0.5
#   ) +
#   geom_text(
#     aes(label = sprintf("%.2f", response)),
#     position = position_dodge(width = 0.35),
#     vjust = -1.2,
#     hjust = 1.2,
#     size = 3,
#     show.legend = FALSE
#   ) +
#   scale_color_manual(values = c("Indoor" = "#2C7FB8", "Outdoor" = "#D95F02")) +
#   #facet_wrap(~ Month) +
#   labs(
#     x = "Ward",
#     y = "NB 1 Adjusted Anopheles HBR\n(per person-night)",
#     shape = "Collection setting",
#     color = "Collection setting"
#   ) +
#   theme_manuscript() +
#   theme(
#     legend.position = "top",
#     strip.background = element_blank(),
#     strip.text = element_text(face = "bold")
#   )
# 
# p2
# 
# ggsave("NB1_HBR_wet.pdf", p2, width = 7, height = 9, units = "in")

##3-------------------------------------------------------------###

# GEE-adjusted HBR plot
# Convert emmeans results to a data frame
dhbrmm_gee_df <- as.data.frame(dHBR_gee_qp_CI)

# Set ordering
emm_gee_df <- emm_gee_df %>%
  mutate(
    Ward = factor(Ward.Name, levels = c("Agugu", "Challenge")),
    Setting = factor(Setting, levels = c("Indoor", "Outdoor"))
    # Month = factor(Month, levels = c("July", "August"))
  )

p3 <- ggplot(
  dhbrmm_gee_df,
  aes(
    x = Ward.Name,
    y = rate,
    ymin = lower.CL,
    ymax = upper.CL,
    shape = Setting,
    color = Setting
  )
) +
  geom_pointrange(
    position = position_dodge(width = 0.35),
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = sprintf("%.2f", rate)),
    position = position_dodge(width = 0.35),
    vjust = -1.2,
    hjust = 1.2,
    size = 3,
    show.legend = FALSE
  ) +
  #facet_wrap(~ Month) +
  scale_color_manual(values = c("Indoor" = "#2C7FB8", "Outdoor" = "#D95F02")) +
  labs(
    x = "Ward",
    y = "GEE Adjusted Anopheles HBR\n(per person-night)dry",
    shape = "Collection setting",
    color = "Collection setting"
  ) +
  theme_manuscript() +
  theme(
    legend.position = "top",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

p3

ggsave("GEE_HBR_dry.pdf", p3, width = 7, height = 9, units = "in")


##Update plots

dcrude_plot <- dcrude_HBR %>%
  transmute(
    Ward.Name,
    Setting,
    HBR = HBR,
    lower = CI_lower,
    upper = CI_upper,
    Type = "Crude"
  )

dgee_plot <- dHBR_gee_qp_CI %>%
  transmute(
    Ward.Name,
    Setting,
    HBR = rate,
    lower = lower.CL,
    upper = upper.CL,
    Type = "GEE-adjusted"
  )

dplot_dat <- bind_rows(dcrude_plot, dgee_plot)


pd4 <- ggplot(
  dplot_dat,
  aes(x = Ward.Name,
      y = HBR,
      colour = Type)
) +
  geom_point(
    position = position_dodge(width = 0.4),
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15,
    position = position_dodge(width = 0.4)
  ) +
  facet_wrap(~Setting)+
  scale_y_continuous(
    limits = c(0, 1.5),
    breaks = seq(0, 1.5, 0.5),
    expand = expansion(mult = c(0, 0.03))
  )+
  labs(
    x = "",
    y = "Human biting rate (mosquitoes/person-night)",
    colour = ""
  ) +
  theme_bw()

print(pd4)

ggsave("Figure1C_HBR_comparison_dry.pdf", pd4, width = 7, height = 12, units = "in")



