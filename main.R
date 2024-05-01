
# This code returns the main estimates.
# 
# NOTE: 
# mop: modern system operational
# pmq: prescriber must query

rm(list=ls()) 
set.seed(123)

library(tidyverse)
library(fixest)
library(ggtext)
library(binsreg)
library(gridExtra)
library(grid)

# Set the path
file_location <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_location)) # set path to location

# Auxiliary functions
source(paste(getwd(), "data/utilities.R", sep = "/"))
source(paste(getwd(), "data/plots.R", sep = "/"))

# Handy strings
outcomes <- c("unem_rate", "emp_rate","lab_force_rate")
policies <- c("mop","pmq")
percentiles <- c("0.10", "0.25", "0.50", "0.75", "0.90")

# One-side length of effects considered
window = 24 # two years

# Load the data
data_location <- paste(
  getwd(),
  "data/joined_data.csv",
  sep = "/"
  )

df <- read.csv(
  data_location,
  header=TRUE,
  sep=","
  )
  
#df <- df[df$year >= 2003 & df$year <= 2016,]

# Prepare the covariates (as formula)
names_covar <- names(
  df[,grep("_ratio$", colnames(df))]
  )

names_covar <- c(names_covar,"scaled_model_values")

covariates <- paste(
  names_covar,
  collapse = " + "
  )

# Select not yet treated counties
not_yet_treated_mop <- df[df[["time_marker"]] < df$first_treatment_mop,] # mop
not_yet_treated_pmq <- df[df[["time_marker"]] < df$first_treatment_pmq,] # pmq

# First stage regressions, TWFE model:
# For each policy
for (policy in policies){
  # retrieve the not yet treated sample
  not_yet_treated_df = get(paste0("not_yet_treated_",policy)) 
  # and for each outcome 
  for (outcome in outcomes){
    # regress outcome on fixed effects and covariates,
    first_stage <- fixest::feols(
      stats::as.formula(
        paste0(outcome," ~ ", covariates, " |", "fips", " + ", "time_marker")
        ),
      data = not_yet_treated_df,
      combine.quick = FALSE,
      warn = FALSE,
      notes = FALSE
      )
    # then create a column with the predicted values (\hat{y})
    column_explained <- paste0(outcome, "_hat_", policy)
    df[[column_explained]] <- stats::predict(
      first_stage,
      newdata = df
      )
    # and a column with the unexplained part (y - \hat{y})
    column_unexplained <- paste0(outcome, "_tilde_", policy)
    df[[column_unexplained]] <- df[[outcome]] - df[[column_explained]] 
    }
  }

# # Drop never-takers for predictions
# df_mop <- df[!is.na(df$first_treatment_mop),]
# df_pmq <- df[!is.na(df$first_treatment_pmq),]

# Obtain the effects by county (see data/utilities.R)
effects_mop_county = CountyEffects(df = df, policy = policies[1], window = window)
effects_pmq_county = CountyEffects(df = df, policy = policies[2], window = window)

# Obtain the values of the Kaitz-p index at treatment (see data/utilities.R)
kaitz_at_treatment_mop = KaitzAtTreatment(df = df, policy = policies[1])
kaitz_at_treatment_pmq = KaitzAtTreatment(df = df, policy = policies[2])

# Plot the distributions of the index at treatment (see data/plots.R)
densities_plot_mop = KaitzDensitiesPlot(kaitz_at_treatment_mop)
densities_plot_pmq = KaitzDensitiesPlot(kaitz_at_treatment_pmq)

# Initialize the list of data frames
joint_dfs <- list()
for (outcome in outcomes){
  for (policy in policies){
    ind = which(outcomes == outcome)
    # Turn the matrices of results to data frames
    effects_county_df <- get(paste0("effects_",policy,"_county"))[,,ind] |>
      as.data.frame() |>
      setNames(
        c("fips",paste0("V",-24:24))
        )
    # Merge them with the Kaitz-p values by using identifiers
    joint_dfs[[paste0("effects_",policy,"_county_",outcome)]] <- merge(
      effects_county_df,
      get(paste0("kaitz_at_treatment_",policy)),
      by="fips"
      )
  }
}


IndividualEffectsPlot(joint_dfs,policies[1],outcomes[1],percentiles[1])
IndividualEffectsPlot(joint_dfs,policies[1],outcomes[2],percentiles[1])
IndividualEffectsPlot(joint_dfs,policies[1],outcomes[3],percentiles[1])


