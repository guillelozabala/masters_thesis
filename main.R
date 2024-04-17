
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
first_stage_mop <- fixest::feols(
  stats::as.formula(
    paste0("unem_rate ~ ", covariates, " |", "fips", " + ", "time_marker")
    ),
  data = not_yet_treated_mop,
  combine.quick = FALSE,
  warn = FALSE,
  notes = FALSE
  ) # mop

first_stage_pmq <- fixest::feols(
  stats::as.formula(
    paste0("unem_rate ~ ", covariates, " |", "fips", " + ", "time_marker")
    ),
  data = not_yet_treated_pmq,
  combine.quick = FALSE,
  warn = FALSE,
  notes = FALSE
  ) # pmq

# Include fitted values
df[["unem_rate_hat_mop"]] <- stats::predict(
  first_stage_mop,
  newdata = df
  )

df[["unem_rate_hat_pmq"]] <- stats::predict(
  first_stage_pmq,
  newdata = df
  )

# Include unexplained part
df[["unem_rate_tilde_mop"]] <- df[["unem_rate"]] - df[["unem_rate_hat_mop"]] 
df[["unem_rate_tilde_pmq"]] <- df[["unem_rate"]] - df[["unem_rate_hat_pmq"]]

# Drop never-takers for predictions
df_mop <- df[!is.na(df$first_treatment_mop),]
df_pmq <- df[!is.na(df$first_treatment_pmq),]

# Initialize matrices
empty_matrix <- function(rows, columns) {
  matrix(0, nrow = rows, ncol = columns)
}

# Average effects on each state
effects_mop_state <- empty_matrix(length(unique(df$state)),2*window+1) 
effects_pmq_state <- empty_matrix(length(unique(df$state)),2*window+1)

# Effects on each county
effects_mop_cty <- empty_matrix(length(unique(df$fips)),2*window+1)
effects_pmq_cty <- empty_matrix(length(unique(df$fips)),2*window+1)

# States considered
state_names_mop <- empty_matrix(length(unique(df_mop$state)),1)
state_names_pmq <- empty_matrix(length(unique(df_pmq$state)),1)

state_names_mop <- unique(df_mop$state)
state_names_pmq <- unique(df_pmq$state)

# FIPS considered
fips_names_mop <- empty_matrix(length(unique(df_mop$state)),1)
fips_names_pmq <- empty_matrix(length(unique(df_pmq$state)),1)

fips_names_mop <- unique(df_mop$fips)
fips_names_pmq <- unique(df_pmq$fips)

# For minimum wage comparisons
above_below_minw_mop = empty_matrix(length(state_names_mop),4)
above_below_minw_pmq = empty_matrix(length(state_names_pmq),4)

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

# Obtain average effects by state
for (i in state_names_mop){
  
  # get index for element in vector
  j = which(1*(state_names_mop == i) == 1)
  
  # get treatment date for each state
  treatment_mop_state = unique(
    df_mop[df_mop$state == i,]$first_treatment_mop
  )
  
  # create a range around the date of treatment
  range_mop = (treatment_mop_state - window):(treatment_mop_state + window)
  
  # average the effects of the counties in each period
  avg_effects_mop_state <- df_mop[(df_mop$state == i)&(df_mop$time_marker %in% range_mop),] |>
    dplyr::group_by(time_marker) |>
    summarise(avg = mean(unem_rate_tilde_mop, na.rm = T))
  
  # Kentucky was an early adopter 
  # (mop passed in 07/1999).
  # to include the available effects we have to 
  # plug in missing values in the initial periods
  # (otherwise vector to short to fit the row of
  # 'effects_mop_state')
  
  # get the lengths
  len_range = length(range_mop)
  len_effects = length(avg_effects_mop_state$avg)

  if (len_effects == len_range){
    effects_mop_state[j,] <- matrix(
      avg_effects_mop_state$avg,
      1,
      len_range) 
  } else { #Kentucky
    early_adopters <- rep(NaN,len_range)
    early_adopters[(len_range - len_effects + 1):len_range] <- avg_effects_mop_state$avg
    effects_mop_state[j,] <- matrix(
      early_adopters,
      1,
      len_range)
  }
  
} 

for (i in state_names_pmq){
  
  # get index for element in vector
  j = which(1*(state_names_pmq == i) == 1)
  
  # get treatment date for each state
  treatments_pmq_state = unique(
    df_pmq[df_pmq$state == i,]$first_treatment_pmq
    )
  
  # create a range around the date of treatment
  range_pmq = (treatments_pmq_state - window):(treatments_pmq_state + window)
  
  # average the effects of the counties in each period
  avg_effects_pmq_state <- df_pmq[(df_pmq$state == i)&(df_pmq$time_marker %in% range_pmq),] |>
    dplyr::group_by(time_marker) |>
    summarise(avg = mean(unem_rate_tilde_pmq, na.rm = T))
  
  # in this case we have a lot of late
  # adopters, so the adjust is in the other direction
  
  # get the lengths
  len_range = length(range_pmq)
  len_effects = length(avg_effects_pmq_state$avg)
  
  if (len_effects == len_range){
    effects_pmq_state[j,] <- matrix(
      avg_effects_pmq_state$avg,
      1,
      len_range) 
  } else {
    late_adopters <- rep(NaN,len_range)
    late_adopters[1:len_effects] <- avg_effects_pmq_state$avg
    effects_pmq_state[j,] <- matrix(
      late_adopters,
      1,
      len_range)
  }
  
} 

# Obtain effects by county
for (i in fips_names_mop){
  
  # get index for element in vector
  j = which(1*(fips_names_mop == i) == 1)
  
  # get treatment date for each state
  treatments_mop_cty = unique(
    df_mop[df_mop$fips == i,]$first_treatment_mop
    )
  
  # create a range around the date of treatment
  range_mop = (treatments_mop_cty - window):(treatments_mop_cty + window)
  
  # obtain the effects of the counties in each period
  avg_effects_mop_cty <- df_mop[(df_mop$fips == i)&(df_mop$time_marker %in% range_mop),] 
  avg_effects_mop_cty <- avg_effects_mop_cty[order(avg_effects_mop_cty$time_marker),]
  
  # same provision for Kentucky
  len_range = length(range_mop)
  len_effects = length(avg_effects_mop_cty$unem_rate_tilde_mop)
  
  # print(i)
  # print(len_range)
  # print(len_effects)
  
  if (len_effects == len_range){
    effects_mop_cty[j,] <- matrix(
      avg_effects_mop_cty$unem_rate_tilde_mop,
      1,
      len_range)
  } else { #Kentucky
    early_adopters <- rep(NaN,len_range)
    early_adopters[(len_range - len_effects + 1):len_range] <- avg_effects_mop_cty$unem_rate_tilde_mop
    effects_mop_cty[j,] <- matrix(
      early_adopters,
      1,
      len_range)
  }

} 

for (i in fips_names_pmq){
  
  # get index for element in vector
  j = which(1*(fips_names_pmq == i) == 1)
  
  # get treatment date for each state
  treatments_pmq_cty = unique(
    df_pmq[df_pmq$fips == i,]$first_treatment_pmq
    )
  
  # create a range around the date of treatment
  range_pmq = (treatments_pmq_cty - window):(treatments_pmq_cty + window)
  
  # obtain the effects of the counties in each period
  avg_effects_pmq_cty <- df_pmq[(df_pmq$fips == i)&(df_pmq$time_marker %in% range_pmq),] 
  avg_effects_pmq_cty <- avg_effects_pmq_cty[order(avg_effects_pmq_cty$time_marker),]
  
  # same provision as before
  len_range = length(range_pmq)
  len_effects = length(avg_effects_pmq_cty$unem_rate_tilde_pmq)
  
  if (len_effects == len_range){
    effects_pmq_cty[j,] <- matrix(
      avg_effects_pmq_cty$unem_rate_tilde_pmq,
      1,
      len_range) 
  } else {
    late_adopters <- rep(NaN,len_range)
    late_adopters[1:len_effects] <- avg_effects_pmq_cty$unem_rate_tilde_pmq
    effects_pmq_cty[j,] <- matrix(
      late_adopters,
      1,
      len_range)
  }
  
} 

# Find which states were above the median min wage when PDMP passed
for (i in state_names_mop){
  
  # get index for element in vector
  j = which(1*(state_names_mop == i) == 1)
  
  # get treatment date for each state
  above_below_minw_mop[j,1] <- unique(
    df_mop[df_mop$state == i,]$first_treatment_mop
    )
  
  # retrieve the year of treatment (this can be done way easier (?))
  above_below_minw_mop[j,2] <- (
    above_below_minw_mop[j,1] - unique(
      df_mop[(df_mop$state == i)&(df_mop$time_marker == above_below_minw_mop[j,1]),]$mop_month
      )
    )/12 + 1960
  
  # get minimum wage at treatment
  above_below_minw_mop[j,3] <- unique(
    df_mop[(df_mop$state == i)&(df_mop$time_marker == above_below_minw_mop[j,1]),]$minw
    )
  
  # get the national medians at treatment
  above_below_minw_mop[j,4] <- median(
    df_mop[df_mop$time_marker == above_below_minw_mop[j,1],]$minw
    )

}

for (i in state_names_pmq){
  
  # get index for element in vector
  j = which(1*(state_names_pmq == i) == 1)
  
  # get treatment date for each state
  above_below_minw_pmq[j,1] <- unique(
    df_pmq[df_pmq$state == i,]$first_treatment_pmq
  )
  
  # retrieve the year of treatment (this can be done way easier (?))
  above_below_minw_pmq[j,2] <- (
    above_below_minw_pmq[j,1] - unique(
      df_pmq[(df_pmq$state == i)&(df_pmq$time_marker == above_below_minw_pmq[j,1]),]$pmq_month
    )
  )/12 + 1960
  
  # get minimum wage at treatment
  above_below_minw_pmq[j,3] <- unique(
    df_pmq[(df_pmq$state == i)&(df_pmq$time_marker == above_below_minw_pmq[j,1]),]$minw
  )
  
  # get the national medians at treatment
  above_below_minw_pmq[j,4] <- median(
    df_pmq[df_pmq$time_marker == above_below_minw_pmq[j,1],]$minw
  )
  
}

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
effects_mop_below_mw <- effects_mop_state[state_names_mop %in% below_med_states_mop,]
effects_mop_above_mw <- effects_mop_state[state_names_mop %in% above_med_states_mop,]

effects_pmq_below_mw <- effects_pmq_state[state_names_pmq %in% below_med_states_pmq,]
effects_pmq_above_mw <- effects_pmq_state[state_names_pmq %in% above_med_states_pmq,]

# Split counties by distribution of Kaitz-p indices
for (kma in names(kaitz_matrices_mop)){
  
  # fill the list with initialized matrices  
  kaitz_values <- empty_matrix(length(fips_names_mop),4)
  
  for (i in fips_names_mop){
    
    # get index for element in vector
    j = which(1*(fips_names_mop == i) == 1)
    
    # get treatment date for each state
    kaitz_values[j,1] <- unique(
      df_mop[df_mop$fips == i,]$first_treatment_mop
    )
    
    # retrieve the year of treatment (this can be done way easier (?))
    year_treatment <- (
      kaitz_values[j,1] - unique(
        df_mop[(df_mop$fips == i)&(df_mop$time_marker == kaitz_values[j,1]),]$mop_month
      )
    )/12 + 1960
    
    if (length(year_treatment) > 0) {
      kaitz_values[j,2] <- year_treatment
    } else {
      kaitz_values[j,2] = NaN
      print("Replacement vector is empty. NA assignment performed.")
    } #CHECK
    
    # get kaitz percentile at treatment
    kaitz_pct_treat <- df_mop[(df_mop$fips == i)&(df_mop$time_marker == kaitz_values[j,1]),] |>
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
  kaitz_values <- empty_matrix(length(fips_names_pmq),4)
  
  for (i in fips_names_pmq){
    
    # get index for element in vector
    j = which(1*(fips_names_pmq == i) == 1)
    
    # get treatment date for each state
    kaitz_values[j,1] <- unique(
      df_pmq[df_pmq$fips == i,]$first_treatment_pmq
    )
    
    # retrieve the year of treatment (this can be done way easier (?))
    year_treatment <- (
      kaitz_values[j,1] - unique(
        df_pmq[(df_pmq$fips == i)&(df_pmq$time_marker == kaitz_values[j,1]),]$pmq_month
      )
    )/12 + 1960
    
    if (length(year_treatment) > 0) {
      kaitz_values[j,2] <- year_treatment
    } else {
      kaitz_values[j,2] = NaN
      print("Replacement vector is empty. NA assignment performed.")
    } #CHECK
    
    # get kaitz percentile at treatment
    kaitz_pct_treat <- df_pmq[(df_pmq$fips == i)&(df_pmq$time_marker == kaitz_values[j,1]),] |>
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
effects_mop_below_mw_pct10 <- effects_mop_cty[fips_names_mop %in% below_med_cts_mop_pct10,]
effects_mop_above_mw_pct10 <- effects_mop_cty[fips_names_mop %in% above_med_cts_mop_pct10,]

effects_mop_below_mw_pct25 <- effects_mop_cty[fips_names_mop %in% below_med_cts_mop_pct25,]
effects_mop_above_mw_pct25 <- effects_mop_cty[fips_names_mop %in% above_med_cts_mop_pct25,]

effects_mop_below_mw_median <- effects_mop_cty[fips_names_mop %in% below_med_cts_mop_median,]
effects_mop_above_mw_median <- effects_mop_cty[fips_names_mop %in% above_med_cts_mop_median,]

effects_mop_below_mw_pct75 <- effects_mop_cty[fips_names_mop %in% below_med_cts_mop_pct75,]
effects_mop_above_mw_pct75 <- effects_mop_cty[fips_names_mop %in% above_med_cts_mop_pct75,]

effects_mop_below_mw_pct90 <- effects_mop_cty[fips_names_mop %in% below_med_cts_mop_pct90,]
effects_mop_above_mw_pct90 <- effects_mop_cty[fips_names_mop %in% above_med_cts_mop_pct90,]

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
effects_pmq_below_mw_pct10 <- effects_pmq_cty[fips_names_pmq %in% below_med_cts_pmq_pct10,]
effects_pmq_above_mw_pct10 <- effects_pmq_cty[fips_names_pmq %in% above_med_cts_pmq_pct10,]

effects_pmq_below_mw_pct25 <- effects_pmq_cty[fips_names_pmq %in% below_med_cts_pmq_pct25,]
effects_pmq_above_mw_pct25 <- effects_pmq_cty[fips_names_pmq %in% above_med_cts_pmq_pct25,]

effects_pmq_below_mw_median <- effects_pmq_cty[fips_names_pmq %in% below_med_cts_pmq_median,]
effects_pmq_above_mw_median <- effects_pmq_cty[fips_names_pmq %in% above_med_cts_pmq_median,]

effects_pmq_below_mw_pct75 <- effects_pmq_cty[fips_names_pmq %in% below_med_cts_pmq_pct75,]
effects_pmq_above_mw_pct75 <- effects_pmq_cty[fips_names_pmq %in% above_med_cts_pmq_pct75,]

effects_pmq_below_mw_pct90 <- effects_pmq_cty[fips_names_pmq %in% below_med_cts_pmq_pct90,]
effects_pmq_above_mw_pct90 <- effects_pmq_cty[fips_names_pmq %in% above_med_cts_pmq_pct90,]

# Plots

source(paste(getwd(), "data/plots.R", sep = "/"))
state_plot(effects_mop_below_mw,effects_mop_above_mw,window,"mop")
state_plot(effects_pmq_below_mw,effects_pmq_above_mw,window,"pmq")

county_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10")
county_plot(effects_pmq_below_mw_pct10,effects_pmq_above_mw_pct10,window,"pmq","0.10")

county_plot(effects_mop_below_mw_pct25,effects_mop_above_mw_pct25,window,"mop","0.25")
county_plot(effects_pmq_below_mw_pct25,effects_pmq_above_mw_pct25,window,"pmq","0.25")

county_plot(effects_mop_below_mw_median,effects_mop_above_mw_median,window,"mop","0.50")
county_plot(effects_pmq_below_mw_median,effects_pmq_above_mw_median,window,"pmq","0.50")

county_plot(effects_mop_below_mw_pct75,effects_mop_above_mw_pct75,window,"mop","0.75")
county_plot(effects_pmq_below_mw_pct75,effects_pmq_above_mw_pct75,window,"pmq","0.75")

county_plot(effects_mop_below_mw_pct90,effects_mop_above_mw_pct90,window,"mop","0.90")
county_plot(effects_pmq_below_mw_pct90,effects_pmq_above_mw_pct90,window,"pmq","0.90")
