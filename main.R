
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

# One-side length of effects considered
window = 24 # two years

# Set the path
file_location <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_location)) # set path to location

data_location <- paste(
  getwd(),
  "data/joined_data.csv",
  sep = "/"
  )

# Load the data
df <- read.csv(
  data_location,
  header=TRUE,
  sep=","
  )
  
# Prepare the covariates (as formula)
names_covar <- names(
  df[,grep("_ratio$", colnames(df))]
  )

covariates <- paste(
  names_covar,
  collapse = " + "
  )

# Select not yet treated counties
not_yet_treated_mop <- df[df[["time_marker"]] < df$first_treatment_mop,] # mop
not_yet_treated_pmq <- df[df[["time_marker"]] < df$first_treatment_pmq,] # pmq

# First stage regressions
first_stage_mop_unem_rate <- fixest::feols(
  stats::as.formula(
    paste0("unem_rate ~ ", covariates, " |", "fips", " + ", "time_marker")
    ),
  data = not_yet_treated_mop,
  combine.quick = FALSE,
  warn = FALSE,
  notes = FALSE
  )

first_stage_mop_unem <- fixest::feols(
  stats::as.formula(
    paste0("unem ~ ", covariates, " |", "fips", " + ", "time_marker")
  ),
  data = not_yet_treated_mop,
  combine.quick = FALSE,
  warn = FALSE,
  notes = FALSE
  )

first_stage_mop_emp <- fixest::feols(
  stats::as.formula(
    paste0("emp ~ ", covariates, " |", "fips", " + ", "time_marker")
  ),
  data = not_yet_treated_mop,
  combine.quick = FALSE,
  warn = FALSE,
  notes = FALSE
  )

first_stage_mop_lab_force <- fixest::feols(
  stats::as.formula(
    paste0("lab_force ~ ", covariates, " |", "fips", " + ", "time_marker")
  ),
  data = not_yet_treated_mop,
  combine.quick = FALSE,
  warn = FALSE,
  notes = FALSE
  )

first_stage_pmq_unem_rate <- fixest::feols(
  stats::as.formula(
    paste0("unem_rate ~ ", covariates, " |", "fips", " + ", "time_marker")
    ),
  data = not_yet_treated_pmq,
  combine.quick = FALSE,
  warn = FALSE,
  notes = FALSE
  ) 

first_stage_pmq_unem <- fixest::feols(
  stats::as.formula(
    paste0("unem ~ ", covariates, " |", "fips", " + ", "time_marker")
  ),
  data = not_yet_treated_pmq,
  combine.quick = FALSE,
  warn = FALSE,
  notes = FALSE
  ) 

first_stage_pmq_emp <- fixest::feols(
  stats::as.formula(
    paste0("emp ~ ", covariates, " |", "fips", " + ", "time_marker")
  ),
  data = not_yet_treated_pmq,
  combine.quick = FALSE,
  warn = FALSE,
  notes = FALSE
  ) 

first_stage_pmq_lab_force <- fixest::feols(
  stats::as.formula(
    paste0("lab_force ~ ", covariates, " |", "fips", " + ", "time_marker")
  ),
  data = not_yet_treated_pmq,
  combine.quick = FALSE,
  warn = FALSE,
  notes = FALSE
  ) 

# Include fitted values
df[["unem_rate_hat_mop"]] <- stats::predict(
  first_stage_mop_unem_rate,
  newdata = df
  )

df[["unem_hat_mop"]] <- stats::predict(
  first_stage_mop_unem,
  newdata = df
  )

df[["emp_hat_mop"]] <- stats::predict(
  first_stage_mop_emp,
  newdata = df
  )

df[["lab_force_hat_mop"]] <- stats::predict(
  first_stage_mop_lab_force,
  newdata = df
  )

df[["unem_rate_hat_pmq"]] <- stats::predict(
  first_stage_pmq_unem_rate,
  newdata = df
  )

df[["unem_hat_pmq"]] <- stats::predict(
  first_stage_pmq_unem,
  newdata = df
  )

df[["emp_hat_pmq"]] <- stats::predict(
  first_stage_pmq_emp,
  newdata = df
  )

df[["lab_force_hat_pmq"]] <- stats::predict(
  first_stage_pmq_lab_force,
  newdata = df
  )

# Include unexplained part
df[["unem_rate_tilde_mop"]] <- df[["unem_rate"]] - df[["unem_rate_hat_mop"]] 
df[["unem_tilde_mop"]] <- df[["unem"]] - df[["unem_hat_mop"]] 
df[["emp_tilde_mop"]] <- df[["emp"]] - df[["emp_hat_mop"]] 
df[["lab_force_tilde_mop"]] <- df[["lab_force"]] - df[["lab_force_hat_mop"]] 

df[["unem_rate_tilde_pmq"]] <- df[["unem_rate"]] - df[["unem_rate_hat_pmq"]]
df[["unem_tilde_pmq"]] <- df[["unem"]] - df[["unem_hat_pmq"]] 
df[["emp_tilde_pmq"]] <- df[["emp"]] - df[["emp_hat_pmq"]] 
df[["lab_force_tilde_pmq"]] <- df[["lab_force"]] - df[["lab_force_hat_pmq"]] 

# Drop never-takers for predictions
df_mop <- df[!is.na(df$first_treatment_mop),]
df_pmq <- df[!is.na(df$first_treatment_pmq),]

# Initialize matrices
empty_array <- function(rows, columns, slices) {
  array(NA, c(rows, columns, slices))
}

# Average effects on each state
effects_mop_state <- empty_array(length(unique(df$state)), 2*window+1, 4) 
effects_pmq_state <- empty_array(length(unique(df$state)), 2*window+1, 4)

# Effects on each county
effects_mop_cty <- empty_array(length(unique(df$fips)), 2*window+1, 4)
effects_pmq_cty <- empty_array(length(unique(df$fips)), 2*window+1, 4)

# States considered
state_names <- unique(df$state)
state_names_mop <- unique(df_mop$state)
state_names_pmq <- unique(df_pmq$state)

# FIPS considered
fips_names <- unique(df$fips)
fips_names_mop <- unique(df_mop$fips)
fips_names_pmq <- unique(df_pmq$fips)

# For minimum wage comparisons (to arrays too?)
above_below_minw_mop = matrix(0,length(state_names_mop),4)
above_below_minw_pmq = matrix(0,length(state_names_pmq),4)

# Initialize lists
kaitz_matrices_mop <- vector("list", length = 5)
kaitz_matrices_pmq <- vector("list", length = 5)

names(kaitz_matrices_mop) <- c("kaitz_pct10_mop_matrix", 
                               "kaitz_pct25_mop_matrix", 
                               "kaitz_median_mop_matrix", 
                               "kaitz_pct75_mop_matrix", 
                               "kaitz_pct90_mop_matrix")

names(kaitz_matrices_pmq) <- c("kaitz_pct10_pmq_matrix", 
                               "kaitz_pct25_pmq_matrix", 
                               "kaitz_median_pmq_matrix", 
                               "kaitz_pct75_pmq_matrix", 
                               "kaitz_pct90_pmq_matrix")


source(paste(getwd(), "data/utilities.R", sep = "/"))

pol_mop = "mop"
pol_pmq = "pmq"

effects_mop_state = f_state_avg_effects(df = df,policy = pol_mop,window = window)
effects_pmq_state = f_state_avg_effects(df = df,policy = pol_pmq,window = window)

effects_mop_cty = f_county_effects(df = df,policy = pol_mop,window = window)
effects_pmq_cty = f_county_effects(df = df,policy = pol_pmq,window = window)

f_states_above_below(df=df,pol=pol_mop)


# As dataframes
above_below_minw_mop <- as.data.frame(above_below_minw_mop)
above_below_minw_pmq <- as.data.frame(above_below_minw_pmq)

# Get indicators
above_below_minw_mop <- above_below_minw_mop |> mutate(above = 1*(V3 > V4))
above_below_minw_pmq <- above_below_minw_pmq |> mutate(above = 1*(V3 > V4))

# Select the states
below_med_states_mop <- state_names_mop[which(above_below_minw_mop$above == 0)]
above_med_states_mop <- state_names_mop[which(above_below_minw_mop$above == 1)]

below_med_states_pmq <- state_names_pmq[which(above_below_minw_pmq$above == 0)]
above_med_states_pmq <- state_names_pmq[which(above_below_minw_pmq$above == 1)]

# Split the sample accordingly
effects_mop_below_mw <- effects_mop_state[state_names_mop %in% below_med_states_mop,,]
effects_mop_above_mw <- effects_mop_state[state_names_mop %in% above_med_states_mop,,]

effects_pmq_below_mw <- effects_pmq_state[state_names_pmq %in% below_med_states_pmq,,]
effects_pmq_above_mw <- effects_pmq_state[state_names_pmq %in% above_med_states_pmq,,]

# Split counties by distribution of Kaitz-p indices
for (kma in names(kaitz_matrices_mop)){
  
  # fill the list with initialized matrices  
  kaitz_values <- matrix(0,length(fips_names_mop),4)
  
  for (i in fips_names_mop){
    
    # get index for element in vector
    j = which(1*(fips_names_mop == i) == 1)
    
    # get treatment date for each state
    kaitz_values[j,1] <- unique(
      df_mop[df_mop$fips == i,]$first_treatment_mop
    )
    
    # retrieve the year of treatment (this can be done way easier (?))
    # year_treatment <- (
    #   kaitz_values[j,1] - unique(
    #     df_mop[(df_mop$fips == i)&(df_mop$time_marker == kaitz_values[j,1]),]$mop_month
    #   )
    # )/12 + 1960
    
    at_treatment <- df_mop[(df_mop$fips == i)&(df_mop$time_marker == kaitz_values[j,1]),]
    
    year_treatment <- unique(
      at_treatment$year
      )
    
    if (length(year_treatment) > 0) {
      kaitz_values[j,2] <- year_treatment
    } else {
      kaitz_values[j,2] = NaN
      print("Replacement vector is empty. NA assignment performed.")
    } #CHECK
    
    # get kaitz percentile at treatment
    kaitz_pct_treat <- at_treatment |>
      select(
        sub("_mop_matrix$", "", kma)
      )
    
    if (length(unique(kaitz_pct_treat[[1]])) > 0) {
      kaitz_values[j,3] <- unique(kaitz_pct_treat[[1]])
    } else {
      kaitz_values[j,3] = NaN
      print("Replacement vector is empty. NA assignment performed.")
    }
    
    # get national median of kaitz percentile at treatment
    kaitz_pct_treat_nac <- df_mop[df_mop$time_marker == kaitz_values[j,1],] |> 
      select(
        sub("_mop_matrix$", "", kma)
      )
    
    kaitz_values[j,4] <- kaitz_pct_treat_nac[[1]] |> 
      median(na.rm = T)
    
  }
  
  kaitz_matrices_mop[[kma]] <- kaitz_values
  
}

# pruedf <- df_mop[df_mop$time_marker == 556,] |> select(sub("_mop_matrix$", "", "kaitz_pct10_mop_matrix")) 
# pruedf[[1]] |> median(na.rm = T)

for (kma in names(kaitz_matrices_pmq)){
  
  # fill the list with initialized matrices  
  kaitz_values <- matrix(0,length(fips_names_pmq),4)
  
  for (i in fips_names_pmq){
    
    # get index for element in vector
    j = which(1*(fips_names_pmq == i) == 1)
    
    # get treatment date for each state
    kaitz_values[j,1] <- unique(
      df_pmq[df_pmq$fips == i,]$first_treatment_pmq
    )
    
    # retrieve the year of treatment (this can be done way easier (?))
    # year_treatment <- (
    #   kaitz_values[j,1] - unique(
    #     df_pmq[(df_pmq$fips == i)&(df_pmq$time_marker == kaitz_values[j,1]),]$pmq_month
    #   )
    # )/12 + 1960
    
    at_treatment <- df_pmq[(df_pmq$fips == i)&(df_pmq$time_marker == kaitz_values[j,1]),]
    
    year_treatment <- unique(
      at_treatment$year
      )
    
    if (length(year_treatment) > 0) {
      kaitz_values[j,2] <- year_treatment
    } else {
      kaitz_values[j,2] = NaN
      print("Replacement vector is empty. NA assignment performed.")
    } #CHECK
    
    # get kaitz percentile at treatment
    kaitz_pct_treat <- at_treatment |>
      select(
        sub("_pmq_matrix$", "", kma)
      )
    
    if (length(unique(kaitz_pct_treat[[1]])) > 0) {
      kaitz_values[j,3] <- unique(kaitz_pct_treat[[1]])
    } else {
      kaitz_values[j,3] = NaN
      print("Replacement vector is empty. NA assignment performed.")
    }
    
    # get national median of kaitz percentile at treatment
    kaitz_pct_treat_nac <- df_pmq[df_pmq$time_marker == kaitz_values[j,1],] |> 
      select(
        sub("_pmq_matrix$", "", kma)
      )
    
    kaitz_values[j,4] <- kaitz_pct_treat_nac[[1]] |> 
      median(na.rm = T)
    
  }
  
  kaitz_matrices_pmq[[kma]] <- kaitz_values
  
}


# Combine matrices and corresponding data frames into lists
kaitz_matrices_list <- list(kaitz_matrices_mop, kaitz_matrices_pmq)
kaitz_df_names <- c("kaitz_pct10_mop_df", 
                    "kaitz_pct25_mop_df", 
                    "kaitz_median_mop_df", 
                    "kaitz_pct75_mop_df", 
                    "kaitz_pct90_mop_df")

# Iterate over each list of matrices
for (kaitz_matrices in kaitz_matrices_list) {
  # Iterate over each matrix in the list
  for (kma in names(kaitz_matrices)) {
    # Assign the matrix to its corresponding data frame
    assign(sub("_matrix$", "_df", kma), as.data.frame(kaitz_matrices[[kma]]))
  }
}

# Iterate over each data frame and apply mutation
for (kaitz_df_name in kaitz_df_names) {
  # Extract the data frame object
  kaitz_df <- get(kaitz_df_name)
  
  # Apply mutation
  kaitz_df <- kaitz_df |>
    as.data.frame() |> 
    mutate(above = 1 * (V3 > V4))
  
  # Assign the modified data frame back to the global environment
  assign(kaitz_df_name, kaitz_df)
}

below_med_cts_mop_pct10 <- fips_names_mop[which(kaitz_pct10_mop_df$above == 0)] 
above_med_cts_mop_pct10 <- fips_names_mop[which(kaitz_pct10_mop_df$above == 1)] #using set() maybe simplifies this?

below_med_cts_mop_pct25 <- fips_names_mop[which(kaitz_pct25_mop_df$above == 0)]
above_med_cts_mop_pct25 <- fips_names_mop[which(kaitz_pct25_mop_df$above == 1)] # nor !%in%

below_med_cts_mop_median <- fips_names_mop[which(kaitz_median_mop_df$above == 0)]
above_med_cts_mop_median <- fips_names_mop[which(kaitz_median_mop_df$above == 1)]

below_med_cts_mop_pct75 <- fips_names_mop[which(kaitz_pct75_mop_df$above == 0)]
above_med_cts_mop_pct75 <- fips_names_mop[which(kaitz_pct75_mop_df$above == 1)]

below_med_cts_mop_pct90 <- fips_names_mop[which(kaitz_pct90_mop_df$above == 0)]
above_med_cts_mop_pct90 <- fips_names_mop[which(kaitz_pct90_mop_df$above == 1)]

# Split
effects_mop_below_mw_pct10 <- effects_mop_cty[fips_names_mop %in% below_med_cts_mop_pct10,,]
effects_mop_above_mw_pct10 <- effects_mop_cty[fips_names_mop %in% above_med_cts_mop_pct10,,]

effects_mop_below_mw_pct25 <- effects_mop_cty[fips_names_mop %in% below_med_cts_mop_pct25,,]
effects_mop_above_mw_pct25 <- effects_mop_cty[fips_names_mop %in% above_med_cts_mop_pct25,,]

effects_mop_below_mw_median <- effects_mop_cty[fips_names_mop %in% below_med_cts_mop_median,,]
effects_mop_above_mw_median <- effects_mop_cty[fips_names_mop %in% above_med_cts_mop_median,,]

effects_mop_below_mw_pct75 <- effects_mop_cty[fips_names_mop %in% below_med_cts_mop_pct75,,]
effects_mop_above_mw_pct75 <- effects_mop_cty[fips_names_mop %in% above_med_cts_mop_pct75,,]

effects_mop_below_mw_pct90 <- effects_mop_cty[fips_names_mop %in% below_med_cts_mop_pct90,,]
effects_mop_above_mw_pct90 <- effects_mop_cty[fips_names_mop %in% above_med_cts_mop_pct90,,]

# Same for pmq
below_med_cts_pmq_pct10 <- fips_names_pmq[which(kaitz_pct10_pmq_df$above == 0)] 
above_med_cts_pmq_pct10 <- fips_names_pmq[which(kaitz_pct10_pmq_df$above == 1)] 

below_med_cts_pmq_pct25 <- fips_names_pmq[which(kaitz_pct25_pmq_df$above == 0)]
above_med_cts_pmq_pct25 <- fips_names_pmq[which(kaitz_pct25_pmq_df$above == 1)] 

below_med_cts_pmq_median <- fips_names_pmq[which(kaitz_median_pmq_df$above == 0)]
above_med_cts_pmq_median <- fips_names_pmq[which(kaitz_median_pmq_df$above == 1)]

below_med_cts_pmq_pct75 <- fips_names_pmq[which(kaitz_pct75_pmq_df$above == 0)]
above_med_cts_pmq_pct75 <- fips_names_pmq[which(kaitz_pct75_pmq_df$above == 1)]

below_med_cts_pmq_pct90 <- fips_names_pmq[which(kaitz_pct90_pmq_df$above == 0)]
above_med_cts_pmq_pct90 <- fips_names_pmq[which(kaitz_pct90_pmq_df$above == 1)]

# Split
effects_pmq_below_mw_pct10 <- effects_pmq_cty[fips_names_pmq %in% below_med_cts_pmq_pct10,,]
effects_pmq_above_mw_pct10 <- effects_pmq_cty[fips_names_pmq %in% above_med_cts_pmq_pct10,,]

effects_pmq_below_mw_pct25 <- effects_pmq_cty[fips_names_pmq %in% below_med_cts_pmq_pct25,,]
effects_pmq_above_mw_pct25 <- effects_pmq_cty[fips_names_pmq %in% above_med_cts_pmq_pct25,,]

effects_pmq_below_mw_median <- effects_pmq_cty[fips_names_pmq %in% below_med_cts_pmq_median,,]
effects_pmq_above_mw_median <- effects_pmq_cty[fips_names_pmq %in% above_med_cts_pmq_median,,]

effects_pmq_below_mw_pct75 <- effects_pmq_cty[fips_names_pmq %in% below_med_cts_pmq_pct75,,]
effects_pmq_above_mw_pct75 <- effects_pmq_cty[fips_names_pmq %in% above_med_cts_pmq_pct75,,]

effects_pmq_below_mw_pct90 <- effects_pmq_cty[fips_names_pmq %in% below_med_cts_pmq_pct90,,]
effects_pmq_above_mw_pct90 <- effects_pmq_cty[fips_names_pmq %in% above_med_cts_pmq_pct90,,]

# Plots

source(paste(getwd(), "data/plots.R", sep = "/"))
state_plot(effects_mop_below_mw,effects_mop_above_mw,window,"mop","unemployment rate")
state_plot(effects_pmq_below_mw,effects_pmq_above_mw,window,"pmq","unemployment rate")

state_plot(effects_mop_below_mw,effects_mop_above_mw,window,"mop","unemployment")
state_plot(effects_pmq_below_mw,effects_pmq_above_mw,window,"pmq","unemployment")

state_plot(effects_mop_below_mw,effects_mop_above_mw,window,"mop","employment")
state_plot(effects_pmq_below_mw,effects_pmq_above_mw,window,"pmq","employment")

state_plot(effects_mop_below_mw,effects_mop_above_mw,window,"mop","labor force")
state_plot(effects_pmq_below_mw,effects_pmq_above_mw,window,"pmq","labor force")

county_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","unemployment rate")
county_plot(effects_pmq_below_mw_pct10,effects_pmq_above_mw_pct10,window,"pmq","0.10","unemployment rate")

county_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","unemployment")
county_plot(effects_pmq_below_mw_pct10,effects_pmq_above_mw_pct10,window,"pmq","0.10","unemployment")

county_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","employment")
county_plot(effects_pmq_below_mw_pct10,effects_pmq_above_mw_pct10,window,"pmq","0.10","employment")

county_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","labor force")
county_plot(effects_pmq_below_mw_pct10,effects_pmq_above_mw_pct10,window,"pmq","0.10","labor force")

county_plot(effects_mop_below_mw_pct25,effects_mop_above_mw_pct25,window,"mop","0.25")
county_plot(effects_pmq_below_mw_pct25,effects_pmq_above_mw_pct25,window,"pmq","0.25")

county_plot(effects_mop_below_mw_median,effects_mop_above_mw_median,window,"mop","0.50")
county_plot(effects_pmq_below_mw_median,effects_pmq_above_mw_median,window,"pmq","0.50")

county_plot(effects_mop_below_mw_pct75,effects_mop_above_mw_pct75,window,"mop","0.75")
county_plot(effects_pmq_below_mw_pct75,effects_pmq_above_mw_pct75,window,"pmq","0.75")

county_plot(effects_mop_below_mw_pct90,effects_mop_above_mw_pct90,window,"mop","0.90")
county_plot(effects_pmq_below_mw_pct90,effects_pmq_above_mw_pct90,window,"pmq","0.90")


# county[(policies = 2)*(outcomes = 4)*(percentiles = 5)] + state[(policies = 2)*(outcomes = 4)] = 48 potential results
