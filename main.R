library(tidyverse)
library(fixest)
library(ggtext)
library(binsreg)
library(gridExtra)
library(grid)

rm(list = ls())
set.seed(123)
options(max.print = 250)

# This code returns the main estimates.
#
# NOTE:
# mop: modern system operational
# pmq: prescriber must query

# Set the path
file_location <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_location)) # set path to location

# Auxiliary functions
source(paste(getwd(), "code/data_analysis/utilities.R", sep = "/"))
source(paste(getwd(), "code/data_analysis/plots.R", sep = "/"))

# Percentiles of the wage distribution
percentiles <- c("0.10", "0.25", "0.50", "0.75", "0.90")

# Load data's location
data_location_pmq <- paste(
  getwd(),
  "data/processed/joined_data_pmq.csv",
  sep = "/"
  )

# Load the data for Must Query PDMPs
df_pmq <- read.csv(
  data_location_pmq,
  header = TRUE,
  sep = ","
  )

# Include relative-to-treatement time markers and presc per capita

df_pmq <- df_pmq |>
  dplyr::mutate(
    relative_to_treat_pmq = time_marker - first_treatment_pmq,
    dosage_unit_pc = round(dosage_unit / population, 3)
  )

# Select the covariate columns
names_covar_pmq <- names(
  df_pmq[, c(grep("_ratio$", colnames(df_pmq)),
        grep("model_values", colnames(df_pmq)))]
  )

# Prepare the covariates (as formula)
covariates_pmq <- paste(
  names_covar_pmq,
  collapse = " + "
  )

# Compute the coefficient values
unem_rate_pmq_effects  <- df_pmq |>
  IndEffects(yname = "unem_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq,
             only_full_horizon = FALSE)

emp_rate_pmq_effects  <- df_pmq |>
  IndEffects(yname = "emp_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq,
             only_full_horizon = FALSE)

lab_force_rate_pmq_effects  <- df_pmq |>
  IndEffects(yname = "lab_force_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq,
             only_full_horizon = FALSE)

# Set plot locations
pmq_unrate_10_loc <- paste(getwd(), "slides/pmq10_unemp_rate.png", sep = "/")
pmq_emrate_10_loc <- paste(getwd(), "slides/pmq10_emp_rate.png", sep = "/")
pmq_lfrate_10_loc <- paste(getwd(), "slides/pmq10_lab_for_rate.png", sep = "/")

# Set plot datasets
pmq_unrate_plot_df <- unem_rate_pmq_effects$df_indcp
pmq_emrate_plot_df <- emp_rate_pmq_effects$df_indcp
pmq_lfrate_plot_df <- lab_force_rate_pmq_effects$df_indcp

# Display and save the results for participation rates
png(filename = pmq_lfrate_10_loc, width = 600, height = 539)
IndividualEffectsPlot(
  pmq_lfrate_plot_df,
  c(1, 6, 12, 24),
  percentiles[1]
  )
dev.off()

# Display and save the results for unemployment rates
png(filename = pmq_unrate_10_loc, width = 600, height = 539)
IndividualEffectsPlot(
  pmq_unrate_plot_df,
  c(1, 6, 12, 24),
  percentiles[1]
  )
dev.off()

# Display and save the results for employment rates
png(filename = pmq_emrate_10_loc, width = 600, height = 539)
IndividualEffectsPlot(
  pmq_emrate_plot_df,
  c(1, 6, 12, 24),
  percentiles[1]
  )
dev.off()

# Time averages

# Set plot locations
pmq_unrate_10_ta_loc <- paste(getwd(), "slides/pmq10_unemp_rate_ta.png", sep = "/")
pmq_emrate_10_ta_loc <- paste(getwd(), "slides/pmq10_emp_rate_ta.png", sep = "/")
pmq_lfrate_10_ta_loc <- paste(getwd(), "slides/pmq10_lab_for_rate_ta.png", sep = "/")

# Display and save the results for participation rates
png(filename = pmq_lfrate_10_ta_loc, width = 600, height = 539)
TimeAveragesPlot(
  pmq_lfrate_plot_df,
  percentiles[1],
  24
  )
dev.off()

# Display and save the results for unemployment rates
png(filename = pmq_unrate_10_ta_loc, width = 600, height = 539)
TimeAveragesPlot(
  pmq_unrate_plot_df,
  percentiles[1],
  24
  )
dev.off()

# Display and save the results for employment rates
png(filename = pmq_emrate_10_ta_loc, width = 600, height = 539)
TimeAveragesPlot(
  pmq_emrate_plot_df,
  percentiles[1],
  24
  )
dev.off()

# Compare percentiles
pmq_lfrate_10_comp6_loc <- paste(getwd(), "slides/pmq10_lab_for_rate_comp6.png", sep = "/")
png(filename = pmq_lfrate_10_comp6_loc, width = 600, height = 539)
IndividualEffectsCompPlot(pmq_lfrate_plot_df, 6)
dev.off()

pmq_lfrate_10_comp24_loc <- paste(getwd(), "slides/pmq10_lab_for_rate_comp24.png", sep = "/")
png(filename = pmq_lfrate_10_comp24_loc, width = 600, height = 539)
IndividualEffectsCompPlot(pmq_lfrate_plot_df, 24)
dev.off()

pmq_unrate_10_comp6_loc <- paste(getwd(), "slides/pmq10_unemp_rate_comp6.png", sep = "/")
png(filename = pmq_unrate_10_comp6_loc, width = 600, height = 539)
IndividualEffectsCompPlot(pmq_unrate_plot_df, 6)
dev.off()

pmq_unrate_10_comp24_loc <- paste(getwd(), "slides/pmq10_unemp_rate_comp24.png", sep = "/")
png(filename = pmq_unrate_10_comp24_loc, width = 600, height = 539)
IndividualEffectsCompPlot(pmq_unrate_plot_df, 24)
dev.off()

pmq_emrate_10_comp6_loc <- paste(getwd(), "slides/pmq10_emp_rate_comp6.png", sep = "/")
png(filename = pmq_emrate_10_comp6_loc, width = 600, height = 539)
IndividualEffectsCompPlot(pmq_emrate_plot_df, 6)
dev.off()

pmq_emrate_10_comp24_loc <- paste(getwd(), "slides/pmq10_emp_rate_comp24.png", sep = "/")
png(filename = pmq_emrate_10_comp24_loc, width = 600, height = 539)
IndividualEffectsCompPlot(pmq_emrate_plot_df, 24)
dev.off()

# Controlling for prescriptions

# Select the covariate columns
names_covar_pmq_p <- names(
  df_pmq[, c(grep("_ratio$", colnames(df_pmq)),
        grep("model_values", colnames(df_pmq)),
        grep("dosage_unit_pc", colnames(df_pmq)))]
  )
# Prepare the covariates (as formula)
covariates_pmq_p <- paste(
  names_covar_pmq_p,
  collapse = " + "
  )

# Compute the coefficient values
unem_rate_pmq_effects_p  <- df_pmq |>
  IndEffects(yname = "unem_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq_p,
             only_full_horizon = FALSE)

emp_rate_pmq_effects_p  <- df_pmq |>
  IndEffects(yname = "emp_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq_p,
             only_full_horizon = FALSE)

lab_force_rate_pmq_effects_p  <- df_pmq |>
  IndEffects(yname = "lab_force_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq_p,
             only_full_horizon = FALSE)

# Set plot locations
pmq_unrate_10_loc_p <- paste(getwd(), "slides/pmq10_unemp_rate_p.png", sep = "/")
pmq_emrate_10_loc_p <- paste(getwd(), "slides/pmq10_emp_rate_p.png", sep = "/")
pmq_lfrate_10_loc_p <- paste(getwd(), "slides/pmq10_lab_for_rate_p.png", sep = "/")

# Set plot datasets
pmq_unrate_plot_df_p <- unem_rate_pmq_effects_p$df_indcp
pmq_emrate_plot_df_p <- emp_rate_pmq_effects_p$df_indcp
pmq_lfrate_plot_df_p <- lab_force_rate_pmq_effects_p$df_indcp

# Display and save the results for unemployment rates
png(filename = pmq_unrate_10_loc_p, width = 600, height = 539)
IndividualEffectsPlot(
  pmq_unrate_plot_df_p,
  c(1, 12),
  percentiles[1]
  )
dev.off()

# Display and save the results for employment rates
png(filename = pmq_emrate_10_loc_p, width = 600, height = 539)
IndividualEffectsPlot(
  pmq_emrate_plot_df_p,
  c(1, 12),
  percentiles[1]
  )
dev.off()

# Display and save the results for participation rates
png(filename = pmq_lfrate_10_loc_p, width = 600, height = 539)
IndividualEffectsPlot(
  pmq_lfrate_plot_df_p,
  c(1, 12),
  percentiles[1]
  )
dev.off()

# maybe the decrease in prescriptions in low
# kaitz counties is driven by mortality numbers

# But again, is not prescription per se, but misuse

# Select the covariate columns
names_covar_pmq_presc <- names(
  df_pmq[, c(grep("_ratio$", colnames(df_pmq)),
        grep("model_values", colnames(df_pmq)))]
  )

# Prepare the covariates (as formula)
covariates_pmq_presc <- paste(
  names_covar_pmq_presc,
  collapse = " + "
  )

# Compute the coefficient values
prescriptions_pmq_effects  <- df_pmq |>
  IndEffects(yname = "dosage_unit_pc",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq_presc,
             only_full_horizon = FALSE)

pmq_presc_plot_df <- prescriptions_pmq_effects$df_indcp

# Display and save the results for unemployment rates
#png(filename = pmq_unrate_10_loc, width = 600, height = 539)
IndividualEffectsPlot(
  pmq_presc_plot_df,
  c(1, 6, 12, 24),
  percentiles[1]
  )
dev.off()


# Load data's location
data_location_opioid_deaths <- paste(
  getwd(),
  "data/processed/opioid_deaths_df.csv",
  sep = "/"
  )

# Load the data for Must Query PDMPs
df_opioid_deaths <- read.csv(
  data_location_opioid_deaths,
  header = TRUE,
  sep = ","
  )

# Include relative-to-treatement time markers and presc per capita

df_opioid_deaths <- df_opioid_deaths |>
  dplyr::mutate(
    relative_to_treat_pmq = year - pmq_year,
    deaths_pc = round((deaths / population) * 100000, 3)#,
    #dosage_unit_pc = round(dosage_unit / population, 3)
  )

# Select the covariate columns
opioid_deaths_covar_pmq <- names(
  df_opioid_deaths[, grep("_ratio$", colnames(df_opioid_deaths))]
  )

#,grep("dosage_unit_pc", colnames(df_pmq))

# Prepare the covariates (as formula)
opioid_deaths_covariates_pmq <- paste(
  opioid_deaths_covar_pmq,
  collapse = " + "
  )

opioids_pmq_effects  <- df_opioid_deaths |>
  IndEffects(yname = "deaths_pc",
             iname = "state_code",
             tname = "year",
             kname = "relative_to_treat_pmq",
             aname = "pmq_year",
             covariates = opioid_deaths_covariates_pmq,
             only_full_horizon = FALSE)

opioids_pmq_effects <- opioids_pmq_effects$df_indcp

binsreg(y = deaths_pc_tilde,
           x = kaitz_pct10,
           data = opioids_pmq_effects[!is.infinite(opioids_pmq_effects[['kaitz_pct10']]),], 
           ci= T) # mean over periods


df_heroin_deaths <- df_opioid_deaths[df_opioid_deaths$cause_of_death == "Heroin",]

heroin_pmq_effects  <- df_heroin_deaths |>
  IndEffects(yname = "deaths_pc",
             iname = "state_code",
             tname = "year",
             kname = "relative_to_treat_pmq",
             aname = "pmq_year",
             covariates = opioid_deaths_covariates_pmq,
             only_full_horizon = FALSE)

heroin_pmq_effects <- heroin_pmq_effects$df_indcp
heroin_time_avg <- heroin_pmq_effects[heroin_pmq_effects$relative_to_treat_pmq >= 1, ]

heroin_time_avg <- heroin_time_avg |>
  group_by(state_code) |>
  summarize(
    deaths_pc_tilde_mean = mean(deaths_pc_tilde, na.rm = TRUE),
    kaitz_pct10_mean = mean(kaitz_pct10, na.rm = TRUE))

summary(lm(deaths_pc_tilde_mean ~ kaitz_pct10_mean, heroin_time_avg))
heroin_time_avg_plot <- ggplot(data = heroin_time_avg) +
  geom_point(aes(x = kaitz_pct10_mean, y = deaths_pc_tilde_mean), size = 4) +
  geom_smooth(method = "lm", aes(x = kaitz_pct10_mean, y = deaths_pc_tilde_mean), se = FALSE, color = "#0098e9") +
  ylab(
  ""
  ) +
  xlab(
  ""
  ) +
  labs(
    title = "Heroin"
  ) +
  geom_hline(
    yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
  )

heroin_time_avg_plot <- heroin_time_avg_plot +
    theme_classic() +
    theme(
      text = element_text(size = 12, family = "serif"),
      plot.title = element_text(hjust = 0.5)
    )

heroin_time_avg_plot

df_methadone_deaths <- df_opioid_deaths[df_opioid_deaths$cause_of_death == "Methadone",]

methadone_pmq_effects  <- df_methadone_deaths |>
  IndEffects(yname = "deaths_pc",
             iname = "state_code",
             tname = "year",
             kname = "relative_to_treat_pmq",
             aname = "pmq_year",
             covariates = opioid_deaths_covariates_pmq,
             only_full_horizon = FALSE)

methadone_pmq_effects <- methadone_pmq_effects$df_indcp
methadone_time_avg <- methadone_pmq_effects[methadone_pmq_effects$relative_to_treat_pmq >= 1, ]

methadone_time_avg <- methadone_time_avg |>
  group_by(state_code) |>
  summarize(
    deaths_pc_tilde_mean = mean(deaths_pc_tilde, na.rm = TRUE),
    kaitz_pct10_mean = mean(kaitz_pct10, na.rm = TRUE))

summary(lm(deaths_pc_tilde_mean ~ kaitz_pct10_mean, methadone_time_avg))
methadone_time_avg_plot <- ggplot(data = methadone_time_avg) +
  geom_point(aes(x = kaitz_pct10_mean, y = deaths_pc_tilde_mean), size = 4) +
  geom_smooth(method = "lm", aes(x = kaitz_pct10_mean, y = deaths_pc_tilde_mean), se = FALSE, color = "#0098e9") +
  ylab(
  ""
  ) +
  xlab(
  ""
  ) +
  labs(
    title = "Methadone"
  ) +
  geom_hline(
    yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
  )

methadone_time_avg_plot <- methadone_time_avg_plot +
    theme_classic() +
    theme(
      text = element_text(size = 12, family = "serif"),
      plot.title = element_text(hjust = 0.5)
    )

methadone_time_avg_plot

# No big effect

df_otheropioids_deaths <- df_opioid_deaths[df_opioid_deaths$cause_of_death == "Other opioids",]

otheropioids_pmq_effects  <- df_otheropioids_deaths |>
  IndEffects(yname = "deaths_pc",
             iname = "state_code",
             tname = "year",
             kname = "relative_to_treat_pmq",
             aname = "pmq_year",
             covariates = opioid_deaths_covariates_pmq,
             only_full_horizon = FALSE)

otheropioids_pmq_effects <- otheropioids_pmq_effects$df_indcp
otheropioids_time_avg <- otheropioids_pmq_effects[otheropioids_pmq_effects$relative_to_treat_pmq >= 1, ]

otheropioids_time_avg <- otheropioids_time_avg |>
  group_by(state_code) |>
  summarize(
    deaths_pc_tilde_mean = mean(deaths_pc_tilde, na.rm = TRUE),
    kaitz_pct10_mean = mean(kaitz_pct10, na.rm = TRUE))

summary(lm(deaths_pc_tilde_mean ~ kaitz_pct10_mean, otheropioids_time_avg))
otheropioids_time_avg_plot <- ggplot(data = otheropioids_time_avg) +
  geom_point(aes(x = kaitz_pct10_mean, y = deaths_pc_tilde_mean), size = 4) +
  geom_smooth(method = "lm", aes(x = kaitz_pct10_mean, y = deaths_pc_tilde_mean), se = FALSE, color = "#0098e9") +
  ylab(
  ""
  ) +
  xlab(
  ""
  ) +
  labs(
    title = "Other opioids"
  ) +
  geom_hline(
    yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
  )

otheropioids_time_avg_plot <- otheropioids_time_avg_plot +
    theme_classic() +
    theme(
      text = element_text(size = 12, family = "serif"),
      plot.title = element_text(hjust = 0.5)
    )

otheropioids_time_avg_plot

df_synopioids_deaths <- df_opioid_deaths[df_opioid_deaths$cause_of_death == "Other synthetic narcotics",]

synopioids_pmq_effects  <- df_synopioids_deaths |>
  IndEffects(yname = "deaths_pc",
             iname = "state_code",
             tname = "year",
             kname = "relative_to_treat_pmq",
             aname = "pmq_year",
             covariates = opioid_deaths_covariates_pmq,
             only_full_horizon = FALSE)

synopioids_pmq_effects <- synopioids_pmq_effects$df_indcp
synopioids_time_avg <- synopioids_pmq_effects[synopioids_pmq_effects$relative_to_treat_pmq >= 1, ]

synopioids_time_avg <- synopioids_time_avg |>
  group_by(state_code) |>
  summarize(
    deaths_pc_tilde_mean = mean(deaths_pc_tilde, na.rm = TRUE),
    kaitz_pct10_mean = mean(kaitz_pct10, na.rm = TRUE))

summary(lm(deaths_pc_tilde_mean ~ kaitz_pct10_mean, synopioids_time_avg))
synopioids_time_avg_plot <- ggplot(data = synopioids_time_avg) +
  geom_point(aes(x = kaitz_pct10_mean, y = deaths_pc_tilde_mean), size = 4) +
  geom_smooth(method = "lm", aes(x = kaitz_pct10_mean, y = deaths_pc_tilde_mean), se = FALSE, color = "#0098e9") +
  ylab(
  ""
  ) +
  xlab(
  ""
  ) +
  labs(
    title = "Other synthetic narcotics"
  ) +
  geom_hline(
    yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
  )

synopioids_time_avg_plot <- synopioids_time_avg_plot +
      theme_classic() +
      theme(
        text = element_text(size = 12, family = "serif"),
        plot.title = element_text(hjust = 0.5)
      )

synopioids_time_avg_plot

setting_labs_opioids <- gpar(fontsize = 17, fontfamily = "serif")
setting_title_opioids <- gpar(fontsize = 20, fontfamily = "serif")
lab_left_opioids <- textGrob("Individual effects", rot = 90, gp = setting_labs_opioids)
lab_bottom_opioids <- textGrob("Kaitz-p values", gp = setting_labs_opioids)
lab_top_opioids <- textGrob(paste0("Effects of Must Query PDMPs on overdose deaths\n by minimum wage bindingness (Kaitz-0.10 index),\n time averages"), gp = setting_title_opioids)

plot_grid_opioids <- grid.arrange(grobs = list(heroin_time_avg_plot, methadone_time_avg_plot, otheropioids_time_avg_plot, synopioids_time_avg_plot),
nrow = 2, left = lab_left_opioids, bottom = lab_bottom_opioids, top = lab_top_opioids)

pmq_heroin_10_loc <- paste(getwd(), "slides/pmq10_heroin.png", sep = "/")
png(filename = pmq_heroin_10_loc, width = 600, height = 539)
grid.arrange(grobs = list(heroin_time_avg_plot, methadone_time_avg_plot, otheropioids_time_avg_plot, synopioids_time_avg_plot),
nrow = 2, left = lab_left_opioids, bottom = lab_bottom_opioids, top = lab_top_opioids)
dev.off()







# Load data's location
data_location_opioid_deaths_m <- paste(
  getwd(),
  "data/processed/opioid_deaths_df_m.csv",
  sep = "/"
  )

# Load the data for Must Query PDMPs
df_opioid_deaths_m <- read.csv(
  data_location_opioid_deaths_m,
  header = TRUE,
  sep = ","
  )

# Include relative-to-treatement time markers and presc per capita

df_opioid_deaths_m <- df_opioid_deaths_m |>
  dplyr::mutate(
    relative_to_treat_pmq = time_marker - first_treatment_pmq,
    deaths_pc = round((deaths / population) * 100000, 3)#,
    #dosage_unit_pc = round(dosage_unit / population, 3)
  )

# Select the covariate columns
opioid_deaths_covar_pmq_m <- names(
  df_opioid_deaths_m[, grep("_ratio$", colnames(df_opioid_deaths_m))]
  )

#,grep("dosage_unit_pc", colnames(df_pmq))

# Prepare the covariates (as formula)
opioid_deaths_covariates_pmq_m <- paste(
  opioid_deaths_covar_pmq_m,
  collapse = " + "
  )


df_heroin_deaths_m <- df_opioid_deaths_m[df_opioid_deaths_m$cause_of_death == "Heroin",]

heroin_pmq_effects_m  <- df_heroin_deaths_m |>
  IndEffects(yname = "deaths_pc",
             iname = "state_code",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = opioid_deaths_covariates_pmq_m,
             only_full_horizon = FALSE)

heroin_pmq_effects_m <- heroin_pmq_effects_m$df_indcp
heroin_time_avg_m <- heroin_pmq_effects_m[heroin_pmq_effects_m$relative_to_treat_pmq == 1, ]

heroin_time_avg_m <- heroin_time_avg_m |>
  group_by(state_code) |>
  summarize(
    deaths_pc_tilde_mean = mean(deaths_pc_tilde, na.rm = TRUE),
    kaitz_pct10_mean = mean(kaitz_pct10, na.rm = TRUE))

heroin_time_avg_m

summary(lm(deaths_pc_tilde_mean ~ kaitz_pct10_mean, heroin_time_avg_m))

ggplot(data = heroin_time_avg_m) +
  geom_point(aes(x = kaitz_pct10_mean, y = deaths_pc_tilde_mean), size = 4) +
  geom_smooth(method = "lm", aes(x = kaitz_pct10_mean, y = deaths_pc_tilde_mean), se = FALSE) +
  ylab(
  ""
  ) +
  xlab(
  ""
  ) +
  theme_minimal() +
  geom_text(x = -0.3, y = 5, label = lm_eqn(heroin_time_avg), parse = TRUE)











# Select the covariate columns
names_covar_pmq_mort <- names(
  df_pmq[, grep("_ratio$", colnames(df_pmq))]
  )

# Prepare the covariates (as formula)
covariates_pmq_mort <- paste(
  names_covar_pmq_mort,
  collapse = " + "
  )

df_pmq_jan <- df_pmq[df_pmq$month == 1,]

# Compute the coefficient values
mortality_pmq_effects  <- df_pmq_jan |>
  IndEffects(yname = "model_values",
             iname = "fips",
             tname = "year",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq_presc,
             only_full_horizon = FALSE)

pmq_mort_plot_df <- mortality_pmq_effects$df_indcp

# Display and save the results for unemployment rates
png(filename = pmq_unrate_10_loc, width = 600, height = 539)
IndividualEffectsPlot(
  pmq_mort_plot_df,
  c(1, 6, 12, 24),
  percentiles[1]
  )
dev.off()

















names_covar_pmq_mort <- names(
  df_pmq[, c(grep("_ratio$", colnames(df_pmq)),
        grep("dosage_unit_pc", colnames(df_pmq)))]
  )

covariates_pmq_mort <- paste(
  names_covar_pmq_mort,
  collapse = " + "
  )

df_pmq[['deaths_pc']] <- round(df_pmq[['deaths']]/df_pmq[['population']], 3)

mortality_pmq_effects  <- df_pmq |>
  IndEffects(yname = "deaths_pc",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq,
             only_full_horizon = FALSE)

#png(filename = paste(getwd(),"slides/pmq10_lab_force_rate.png",sep = "/"),width = 600, height = 539)
IndividualEffectsPlot(mortality_pmq_effects$df_indcp,c(1,6,12,24),percentiles[1])
#dev.off()















# Load the data
data_location <- paste(
  getwd(),
  "data/processed/joined_data_mop.csv",
  sep = "/"
  )

df <- read.csv(
  data_location,
  header=TRUE,
  sep=","
  )

# Prepare the covariates (as formula)
names_covar <- names(
  df[,c(grep("_ratio$", colnames(df)),
    grep("scaled_model_values", colnames(df)))]
  )

covariates <- paste(
  names_covar,
  collapse = " + "
  )

df[['relative_to_treat_mop']] = df[['time_marker']] - df[['first_treatment_mop']]
names(df)[names(df) == 'kaitz_median'] <- 'kaitz_pct50'

#df[['relative_to_treat_pmq']] = df[['time_marker']] - df[['first_treatment_pmq']]

unem_rate_mop_effects  <- df |>
  IndEffects(yname = "unem_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_mop",
             aname = "first_treatment_mop",
             covariates = covariates,
             only_full_horizon = FALSE)

emp_rate_mop_effects  <- df |>
  IndEffects(yname = "emp_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_mop",
             aname = "first_treatment_mop",
             covariates = covariates,
             only_full_horizon = FALSE)

lab_force_rate_mop_effects  <- df |>
  IndEffects(yname = "lab_force_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_mop",
             aname = "first_treatment_mop",
             covariates = covariates,
             only_full_horizon = FALSE)

png(filename = paste(getwd(),"slides/mop10_unemp_rate.png",sep = "/"),width = 600, height = 539)
IndividualEffectsPlot(unem_rate_mop_effects$df_indcp,c(1,6,12,24),percentiles[1])
dev.off() 

png(filename = paste(getwd(),"slides/mop10_emp_rate.png",sep = "/"),width = 600, height = 539)
IndividualEffectsPlot(emp_rate_mop_effects$df_indcp,c(1,6,12,24),percentiles[1])
dev.off() 

png(filename = paste(getwd(),"slides/mop10_lab_force_rate.png",sep = "/"),width = 600, height = 539)
IndividualEffectsPlot(lab_force_rate_mop_effects$df_indcp,c(1,6,12,24),percentiles[1])
dev.off() 

png(filename = paste(getwd(),"slides/mop10_unemp_rate_comp_t1.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(unem_rate_mop_effects$df_indcp,1)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_emp_rate_comp_t1.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(emp_rate_mop_effects$df_indcp,1)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_lab_force_rate_comp_t1.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(lab_force_rate_mop_effects$df_indcp,1)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_unemp_rate_comp_t24.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(unem_rate_mop_effects$df_indcp,24)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_emp_rate_comp_t24.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(emp_rate_mop_effects$df_indcp,24)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_lab_force_rate_comp_t24.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(lab_force_rate_mop_effects$df_indcp,24)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_unemp_rate_average.png",sep = "/"),width = 600, height = 539)
TimeAveragesPlot(unem_rate_mop_effects$df_indcp,percentiles[1],24)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_emp_rate_average.png",sep = "/"),width = 600, height = 539)
TimeAveragesPlot(emp_rate_mop_effects$df_indcp,percentiles[1],24)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_lab_force_rate_average.png",sep = "/"),width = 600, height = 539)
TimeAveragesPlot(lab_force_rate_mop_effects$df_indcp,percentiles[1],24)
dev.off() 

png(filename = paste(getwd(),"slides/kaitz_percentiles.png",sep = "/"),width = 600, height = 539)
KaitzDensitiesPlot(unem_rate_mop_effects$df_indcp)
dev.off()




# Reformulation -> not gonna work, no control group

df[['reformulation_date']] = (2010 - 1960) * 12 + 8 # August 4, 2010
df[['relative_to_refor']] = df[['time_marker']] - df[['reformulation_date']]

unem_rate_refor_effects  <- df |>
  IndEffects(yname = "unem_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_refor",
             aname = "reformulation_date",
             covariates = covariates,
             only_full_horizon = FALSE)

emp_rate_refor_effects  <- df |>
  IndEffects(yname = "emp_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_refor",
             aname = "reformulation_date",
             covariates = covariates,
             only_full_horizon = FALSE)

lab_force_rate_refor_effects  <- df |>
  IndEffects(yname = "lab_force_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_refor",
             aname = "reformulation_date",
             covariates = covariates,
             only_full_horizon = FALSE)

IndividualEffectsPlot_Oxy(unem_rate_refor_effects$df_indcp,c(1,6,12,24),percentiles[1])
IndividualEffectsPlot(emp_rate_refor_effects$df_indcp,c(1,6,12,24),percentiles[1])
IndividualEffectsPlot(lab_force_rate_refor_effects$df_indcp,c(1,6,12,24),percentiles[1])

prueba <- unem_rate_refor_effects$df_indcp
prueba[prueba[["relative_to_refor"]] == 1,]

prueba2 <- unem_rate_mop_effects$df_indcp
prueba2[prueba2[["relative_to_treat_mop"]] == 1,]

# Load the data
data_location_mortality <- paste(
  getwd(),
  "data/source/opioid_deaths_yearly.csv",
  sep = "/"
  )

df_mortality <- read.csv(
  data_location_mortality,
  header= TRUE,
  sep= ","
  )

names_covar_mortality <- names(
  df_mortality[,grep("_ratio$", colnames(df_mortality))]
  )

covariates_mortality <- paste(
  names_covar_mortality,
  collapse = " + "
  )

df_mortality[['relative_to_treat_mop']] = df_mortality[['year']] - df_mortality[['mop_year']]

mortality_mop_effects  <- df_mortality |>
  IndEffects(yname = "deaths",
             iname = "state_code",
             tname = "year",
             kname = "relative_to_treat_mop",
             aname = "mop_year",
             covariates = covariates_mortality,
             only_full_horizon = FALSE)

df_year <- df[df[['month']] == 1, ]
df_year_kaitz <- df_year[,c('year','state','fips','kaitz_pct10')]
df_year_kaitz <- df_year_kaitz |>
  group_by(year,state) |>
  summarize(kaitz_pct10 = mean(kaitz_pct10, na.rm = TRUE))

df_mort_merged <- merge(mortality_mop_effects$df_indcp, df_year_kaitz, by = c('year','state'))
df_mort_merged <- df_mort_merged[(df_mort_merged[['year']] >= 2003) & (df_mort_merged[['year']] <= 2016), ]
binsreg(y = deaths_tilde,
        x = kaitz_pct10,
        data = df_mort_merged[!is.infinite(df_mort_merged[['kaitz_pct10']]),], 
        ci= T)



# Load the data
file_location <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_location)) # set path to location

data_location_prescriptions_mop <- paste(
  getwd(),
  "data/prescription_monthly_mop.csv",
  sep = "/"
  )

df_prescriptions_mop <- read.csv(
  data_location_prescriptions_mop,
  header=TRUE,
  sep=","
  )

names_covar_prescriptions_mop <- names(
  df_prescriptions_mop[,grep("_ratio$", colnames(df_prescriptions_mop))]
  )

covariates_prescriptions_mop <- paste(
  names_covar_prescriptions_mop,
  collapse = " + "
  )

df_prescriptions_mop[['relative_to_treat_mop']] = df_prescriptions_mop[['year']] - df_prescriptions_mop[['mop_year']]

df_prescriptions_mop |> head()

prescriptions_mop_effects  <- df_prescriptions_mop |>
  IndEffects(yname = "dosage_unit_pc",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_mop",
             aname = "first_treatment_mop",
             covariates = covariates_prescriptions_mop,
             only_full_horizon = FALSE)

binsreg(y = dosage_unit_pc_tilde,
        x = kaitz_pct10,
        data = prescriptions_mop_effects$df_indcp, 
        ci= T)


data_location_prescriptions_pmq <- paste(
  getwd(),
  "data/processed/prescription_monthly_pmq.csv",
  sep = "/"
  )

df_prescriptions_pmq <- read.csv(
  data_location_prescriptions_pmq,
  header = TRUE,
  sep = ","
  )

names_covar_prescriptions_pmq <- names(
  df_prescriptions_pmq[, c(grep("_ratio$", colnames(df_prescriptions_pmq)),
  grep("scaled_model_values", colnames(df_prescriptions_pmq)))]
  )

covariates_prescriptions_pmq <- paste(
  names_covar_prescriptions_pmq,
  collapse = " + "
  )

df_prescriptions_pmq[['relative_to_treat_pmq']] = df_prescriptions_pmq[['year']] - df_prescriptions_pmq[['pmq_year']]

prescriptions_pmq_effects  <- df_prescriptions_pmq |>
  IndEffects(yname = "dosage_unit_pc",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_prescriptions_pmq,
             only_full_horizon = FALSE)


IndividualEffectsPlotPrescriptions(prescriptions_pmq_effects$df_indcp,percentiles[1])

mop_main_plot <- IndividualEffectsPlot(unem_rate_mop_effects$df_indcp,c(1,6,12,24),percentiles[1],T)
mop_prescription_plot <- IndividualEffectsPlotPrescriptions(prescriptions_mop_effects$df_indcp,percentiles[1])
pmq_main_plot <- IndividualEffectsPlot(unem_rate_pmq_effects$df_indcp,c(1,6,12,24),percentiles[1],T)
pmq_prescription_plot <- IndividualEffectsPlotPrescriptions(prescriptions_pmq_effects$df_indcp,percentiles[1])

grid.arrange(grobs = list(mop_main_plot,mop_prescription_plot),nrow = 2)
grid.arrange(grobs = list(pmq_main_plot,pmq_prescription_plot),nrow = 2)

# prescriptions fall -> heroin?

covariates_prescriptions_pmq
