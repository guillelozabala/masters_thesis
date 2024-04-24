
# This code returns the main estimates.
# 
# NOTE: 
# mop: modern system operational
# pmq: prescriber must query

rm(list=ls()) 
set.seed(123)

library(tidyverse)
library(reshape2)

library(fixest)

library(ggtext)
library(ggridges)

# Set the path
file_location <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_location)) # set path to location

# Auxiliary functions
source(paste(getwd(), "data/utilities.R", sep = "/"))
source(paste(getwd(), "data/plots.R", sep = "/"))

# One-side length of effects considered
window = 24 # two years

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
samples_fs <- c("not_yet_treated_mop","not_yet_treated_pmq")
outcomes_fs <- c("unem_rate ~ ","unem ~ ", "emp ~ ","lab_force ~ ")

# TWFE (linear)
for (i in samples_fs){
  sample_i = sub("not_yet_treated_", "", i)
  for (j in outcomes_fs){
    outcome_j = sub(" ~ ", "", j)
    assign(
      paste0("first_stage_",sample_i,"_",outcome_j),
      fixest::feols(
        stats::as.formula(
          paste0(j, covariates, " |", "fips", " + ", "time_marker")
        ),
        data = get(i),
        combine.quick = FALSE,
        warn = FALSE,
        notes = FALSE
      )
    )
  }
}

# # Error: cannot allocate vector of size 672.8 Gb
# library(randomForest)
# ind <- sample(2, nrow(not_yet_treated_mop), replace = TRUE, prob = c(0.7, 0.3))
# not_yet_treated_mop_train <- not_yet_treated_mop[ind==1,]
# not_yet_treated_mop_test <- not_yet_treated_mop[ind==2,]
# randomForest(
#   stats::as.formula(
#   #paste0("unem_rate ~ ", covariates)
#   unem_rate ~ as.factor(fips)
#   ),
#   data = not_yet_treated_mop_train[1:1000,],
#   proximity = TRUE,
#   na.action = na.omit
#   ) 


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

# # Initialize matrices
# empty_array <- function(rows, columns, slices) {
#   array(NA, c(rows, columns, slices))
# }

# # Average effects on each state
# effects_mop_state <- empty_array(length(unique(df$state)), 2*window+1, 4) 
# effects_pmq_state <- empty_array(length(unique(df$state)), 2*window+1, 4)
# 
# # Effects on each county
# effects_mop_cty <- empty_array(length(unique(df$fips)), 2*window+1, 4)
# effects_pmq_cty <- empty_array(length(unique(df$fips)), 2*window+1, 4)

# States considered
state_names <- unique(df$state)
state_names_mop <- unique(df_mop$state)
state_names_pmq <- unique(df_pmq$state)

# FIPS considered
fips_names <- unique(df$fips)
fips_names_mop <- unique(df_mop$fips)
fips_names_pmq <- unique(df_pmq$fips)

# # For minimum wage comparisons (to arrays too?)
# above_below_minw_mop = matrix(0,length(state_names_mop),4)
# above_below_minw_pmq = matrix(0,length(state_names_pmq),4)

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


pol_mop = "mop"
pol_pmq = "pmq"

effects_mop_state = f_state_avg_effects(df = df,policy = pol_mop,window = window)
effects_pmq_state = f_state_avg_effects(df = df,policy = pol_pmq,window = window)

effects_mop_cty = f_county_effects(df = df,policy = pol_mop,window = window)
effects_pmq_cty = f_county_effects(df = df,policy = pol_pmq,window = window)

above_below_minw_mop = f_states_above_below(df=df,pol=pol_mop)
above_below_minw_pmq = f_states_above_below(df=df,pol=pol_pmq)

kaitz_matrices_mop = f_county_kaitz(df=df,pol=pol_mop)

# write.table(x = kaitz_matrices_mop, file = "C:/Users/guill/Desktop/kaitz_matrices_mop.csv", sep = ',', row.names = FALSE, col.names = FALSE)
# kaitz_matrices_mop_loaded <- read.table(file = "C:/Users/guill/Desktop/kaitz_matrices_mop.csv", header = FALSE, sep = ',')
# m1<- as.matrix(kaitz_matrices_mop_loaded)[,1:4]
# as.array(as.matrix(kaitz_matrices_mop_loaded),c(nrow(kaitz_matrices_mop_loaded),4,4))

kaitz_matrices_pmq = f_county_kaitz(df=df,pol=pol_pmq)

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

state_plot(effects_mop_below_mw,effects_mop_above_mw,window,"mop","unemployment rate")
state_plot(effects_pmq_below_mw,effects_pmq_above_mw,window,"pmq","unemployment rate")

state_plot(effects_mop_below_mw,effects_mop_above_mw,window,"mop","unemployment")
state_plot(effects_pmq_below_mw,effects_pmq_above_mw,window,"pmq","unemployment")

state_plot(effects_mop_below_mw,effects_mop_above_mw,window,"mop","employment")
state_plot(effects_pmq_below_mw,effects_pmq_above_mw,window,"pmq","employment")

state_plot(effects_mop_below_mw,effects_mop_above_mw,window,"mop","labor force")
state_plot(effects_pmq_below_mw,effects_pmq_above_mw,window,"pmq","labor force")

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/mop010unemprate.png",width = 600, height = 539)
county_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","unemployment rate")
dev.off() 

county_plot(effects_pmq_below_mw_pct10,effects_pmq_above_mw_pct10,window,"pmq","0.10","unemployment rate")

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/mop010unemp.png",width = 600, height = 539)
county_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","unemployment")
dev.off() 

county_plot(effects_pmq_below_mw_pct10,effects_pmq_above_mw_pct10,window,"pmq","0.10","unemployment")

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/mop010emp.png",width = 600, height = 539)
county_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","employment")
dev.off() 

county_plot(effects_pmq_below_mw_pct10,effects_pmq_above_mw_pct10,window,"pmq","0.10","employment")

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/mop010labforce.png",width = 600, height = 539)
county_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","labor force")
dev.off() 

county_plot(effects_pmq_below_mw_pct10,effects_pmq_above_mw_pct10,window,"pmq","0.10","labor force")

county_plot(effects_mop_below_mw_pct25,effects_mop_above_mw_pct25,window,"mop","0.25","unemployment rate")
county_plot(effects_pmq_below_mw_pct25,effects_pmq_above_mw_pct25,window,"pmq","0.25","unemployment rate")

county_plot(effects_mop_below_mw_pct25,effects_mop_above_mw_pct25,window,"mop","0.25","unemployment")
county_plot(effects_pmq_below_mw_pct25,effects_pmq_above_mw_pct25,window,"pmq","0.25","unemployment")

county_plot(effects_mop_below_mw_pct25,effects_mop_above_mw_pct25,window,"mop","0.25","employment")
county_plot(effects_pmq_below_mw_pct25,effects_pmq_above_mw_pct25,window,"pmq","0.25","employment")

county_plot(effects_mop_below_mw_pct25,effects_mop_above_mw_pct25,window,"mop","0.25","labor force")
county_plot(effects_pmq_below_mw_pct25,effects_pmq_above_mw_pct25,window,"pmq","0.25","labor force")

county_plot(effects_mop_below_mw_median,effects_mop_above_mw_median,window,"mop","0.50","employment")
county_plot(effects_pmq_below_mw_median,effects_pmq_above_mw_median,window,"pmq","0.50")

county_plot(effects_mop_below_mw_pct75,effects_mop_above_mw_pct75,window,"mop","0.75","employment")
county_plot(effects_pmq_below_mw_pct75,effects_pmq_above_mw_pct75,window,"pmq","0.75")

county_plot(effects_mop_below_mw_pct90,effects_mop_above_mw_pct90,window,"mop","0.90","employment")
county_plot(effects_pmq_below_mw_pct90,effects_pmq_above_mw_pct90,window,"pmq","0.90")

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/comparison_unemprates.png",width = 600, height = 539)
comparison_plot(effects_mop_above_mw_pct10,effects_mop_above_mw_pct25,effects_mop_above_mw_median,
                effects_mop_above_mw_pct75,effects_mop_above_mw_pct90,window,"mop","unemployment rate")
dev.off() 

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/comparison_unemp.png",width = 600, height = 539)
comparison_plot(effects_mop_above_mw_pct10,effects_mop_above_mw_pct25,effects_mop_above_mw_median,
                effects_mop_above_mw_pct75,effects_mop_above_mw_pct90,window,"mop","unemployment")
dev.off() 

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/comparison_emp.png",width = 600, height = 539)
comparison_plot(effects_mop_above_mw_pct10,effects_mop_above_mw_pct25,effects_mop_above_mw_median,
                effects_mop_above_mw_pct75,effects_mop_above_mw_pct90,window,"mop","employment")
dev.off() 

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/comparison_labforce.png",width = 600, height = 539)
comparison_plot(effects_mop_above_mw_pct10,effects_mop_above_mw_pct25,effects_mop_above_mw_median,
                effects_mop_above_mw_pct75,effects_mop_above_mw_pct90,window,"mop","labor force")
dev.off() 

# county[(policies = 2)*(outcomes = 4)*(percentiles = 5)] + state[(policies = 2)*(outcomes = 4)] = 48 potential results

df_densities <- cbind(kaitz_pct10_mop_df$V3,kaitz_pct25_mop_df$V3,kaitz_median_mop_df$V3,kaitz_pct75_mop_df$V3,kaitz_pct90_mop_df$V3)
df_densities <- as.data.frame(df_densities)
kaitz_densities_plot(df_densities)

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/mop010_dens_unemp_rate.png",width = 600, height = 539)
joy_division_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","unemployment rate")
dev.off() 

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/mop010_dens_unemp.png",width = 600, height = 539)
joy_division_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","unemployment")
dev.off() 

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/mop010_dens_emp.png",width = 600, height = 539)
joy_division_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","employment")
dev.off() 

png(filename = "C:/Users/guill/Documents/GitHub/masters_thesis/slides/mop010_dens_labforce.png",width = 600, height = 539)
joy_division_plot(effects_mop_below_mw_pct10,effects_mop_above_mw_pct10,window,"mop","0.10","labor force")
dev.off() 

