
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
  
  #effects_mop_cty[j,] <- matrix(avg_effects_mop_cty$unem_rate_tilde_mop,1,length(range_mop))
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
  len_effects = length(avg_effects_pmq_state$avg)
  
  if (len_effects == len_range){
    effects_pmq_cty[j,] <- matrix(
      avg_effects_pmq_cty$unem_rate_tilde_mop,
      1,
      len_range) 
  } else {
    late_adopters <- rep(NaN,len_range)
    late_adopters[1:len_effects] <- avg_effects_pmq_cty$unem_rate_tilde_mop
    effects_pmq_cty[j,] <- matrix(
      late_adopters,
      1,
      len_range)
  }
  
  #effects_pmq_cty[j,] <- matrix(avg_effects_pmq_cty$unem_rate_tilde_pmq,1,length(range_pmq))
} 

# Find which states were above the median min wage when PDMP passed
first_ts = matrix(0,length(unique(df_mop$state)),4)

for (i in unique(df_mop$state)){
  j = which(1*(state_names_mop == i) == 1)
  
  first_ts[j,1] <- unique(
    df_mop[df_mop$state == i,]$first_treatment_mop
    )
  
  first_ts[j,2] <- (
    first_ts[j,1] - unique(
      df_mop[(df_mop$state == i)&(df_mop$time_marker == first_ts[j,1]),]$mop_month
      )
    )/12 + 1960
  
  first_ts[j,3] <- unique(
    df_mop[(df_mop$state == i)&(df_mop$time_marker == first_ts[j,1]),]$minw
    )
  
  first_ts[j,4] <- median(
    df_mop[df_mop$time_marker == first_ts[j,1],]$minw
    )

}

first_ts <- as.data.frame(first_ts)
first_ts <- first_ts |> mutate(above = 1*(V3 > V4))

below_med_states <- state_names_mop[which(first_ts$above == 0)]
above_med_states <- state_names_mop[which(first_ts$above == 1)]

# Split the sample accordingly
effects_mop_below_mw <- effects_mop_state[state_names_mop %in% below_med_states,]
effects_mop_above_mw <- effects_mop_state[state_names_mop %in% above_med_states,]

effects_pmq_below_mw <- effects_pmq[state_names_pmq %in% below_med_states,]
effects_pmq_above_mw <- effects_pmq[state_names_pmq %in% above_med_states,]

# Plot
gplot_values <- as_tibble(cbind(-window:window,
                                colMeans(effects_mop_below_mw,na.rm=T),
                                sapply(1:(2*window+1),function(x){sd(effects_mop_below_mw[,x],na.rm = T)/sqrt(length(effects_mop_below_mw[,x]))}),
                                colMeans(effects_mop_above_mw,na.rm=T),
                                sapply(1:(2*window+1),function(x){sd(effects_mop_above_mw[,x],na.rm = T)/sqrt(length(effects_mop_above_mw[,x]))})))

gplot_values <- cbind(gplot_values,
                      gplot_values[["V2"]] - 1.96*gplot_values[["V3"]],
                      gplot_values[["V2"]] + 1.96*gplot_values[["V3"]],
                      gplot_values[["V4"]] - 1.96*gplot_values[["V5"]],
                      gplot_values[["V4"]] + 1.96*gplot_values[["V5"]])

gplot_values <- gplot_values |>
  rename(ci1 = 'gplot_values[["V2"]] - 1.96 * gplot_values[["V3"]]') |>
  rename(ci2 = 'gplot_values[["V2"]] + 1.96 * gplot_values[["V3"]]') |>
  rename(ci3 = 'gplot_values[["V4"]] - 1.96 * gplot_values[["V5"]]') |>
  rename(ci4 = 'gplot_values[["V4"]] + 1.96 * gplot_values[["V5"]]') 

g_plot <- ggplot(gplot_values) + 
  geom_line(aes(x=V1,y=V2),color = "#0098e9",linewidth=1) +
  geom_line(aes(x=V1,y=ci1),color = "#0098e9",linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=ci2),color = "#0098e9", linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=V4),color = "#ff5ca8",linewidth=1) +
  geom_line(aes(x=V1,y=ci3),color = "#ff5ca8", linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=ci4),color = "#ff5ca8",linetype=2,linewidth=1) +
  ylab("Effect on unemployment rate") +
  xlab("Period relative to treatment") + 
  labs(title="Effect of Modern System PDMPs on unemployment rates,<br>for counties with minimum wage <span style='color:#0098e9;'>below</span> and <span style='color:#ff5ca8;'>above</span> the median<br>at t=0") +
  geom_hline(yintercept = 0, linetype=5, color= "Black", linewidth=0.5) +
  geom_vline(xintercept = 0, linetype=5, color= "Black", linewidth=0.5)
g_plot <- g_plot + theme_classic() + theme(text = element_text(size=16, family="serif"),plot.title = element_markdown(lineheight = 1.1)) 
g_plot



gplot_values1 <- as_tibble(cbind(-window:window,
                                 colMeans(effects_pmq_below_mw,na.rm=T),
                                 sapply(1:(2*window+1),function(x){sd(effects_pmq_below_mw[,x],na.rm = T)/sqrt(length(effects_pmq_below_mw[,x]))}),
                                 colMeans(effects_pmq_above_mw,na.rm=T),
                                 sapply(1:(2*window+1),function(x){sd(effects_pmq_above_mw[,x],na.rm = T)/sqrt(length(effects_pmq_above_mw[,x]))})))

gplot_values1 <- cbind(gplot_values1,
                       gplot_values1[["V2"]] - 1.96*gplot_values1[["V3"]],
                       gplot_values1[["V2"]] + 1.96*gplot_values1[["V3"]],
                       gplot_values1[["V4"]] - 1.96*gplot_values1[["V5"]],
                       gplot_values1[["V4"]] + 1.96*gplot_values1[["V5"]])

gplot_values1 <- gplot_values1 |>
  rename(ci1 = 'gplot_values1[["V2"]] - 1.96 * gplot_values1[["V3"]]') |>
  rename(ci2 = 'gplot_values1[["V2"]] + 1.96 * gplot_values1[["V3"]]') |>
  rename(ci3 = 'gplot_values1[["V4"]] - 1.96 * gplot_values1[["V5"]]') |>
  rename(ci4 = 'gplot_values1[["V4"]] + 1.96 * gplot_values1[["V5"]]') 

g_plot1 <- ggplot(gplot_values1) + 
  geom_line(aes(x=V1,y=V2),color = "#0098e9",linewidth=1) +
  geom_line(aes(x=V1,y=ci1),color = "#0098e9",linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=ci2),color = "#0098e9", linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=V4),color = "#ff5ca8",linewidth=1) +
  geom_line(aes(x=V1,y=ci3),color = "#ff5ca8", linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=ci4),color = "#ff5ca8",linetype=2,linewidth=1) +
  ylab("Effect on unemployment rate") +
  xlab("Period relative to treatment") +  
  labs(title="Effect of Must Query PDMPs on unemployment rates,<br>for counties with minimum wage <span style='color:#0098e9;'>below</span> and <span style='color:#ff5ca8;'>above</span> the median<br>at t=0") +
  geom_hline(yintercept = 0, linetype=5, color= "Black", linewidth=0.5) +
  geom_vline(xintercept = 0, linetype=5, color= "Black", linewidth=0.5)
g_plot1 <- g_plot1 + theme_classic() + theme(text = element_text(size=16, family="serif"),plot.title = element_markdown(lineheight = 1.1)) 

g_plot1






# Split states by distribution of Kaitz p indices
kaitz_matrices <- vector("list", length = 5)


names(kaitz_matrices) <- c("kaitz_pct10_matrix", 
                           "kaitz_pct25_matrix", 
                           "kaitz_median_matrix", 
                           "kaitz_pct75_matrix", 
                           "kaitz_pct90_matrix")

for (kaimatrix in names(kaitz_matrices)){
  
  kaitz_matrices[[kaimatrix]] <- matrix(0, nrow = length(unique(df_mop$fips)), ncol = 4)
  
  kaimat <- kaitz_matrices[[kaimatrix]]
  
  for (j in unique(df_mop$fips)){
    
    k = which(
      1*(fips_names_mop == j) == 1
      )
    
    kaimat[k,1] <- unique(
      df_mop[df_mop$fips == j,]$first_treatment_mop
      )
    
    replacement <- (
      kaimat[k,1] - unique(
        df_mop[(df_mop$fips == j)&(df_mop$time_marker == kaimat[k,1]),]$mop_month
        )
      )/12 + 1960
    
    if (length(replacement) > 0) {
      kaimat[k,2] <- replacement
    } else {
      kaimat[k,2] = NA
      print("Replacement vector is empty. NA assignment performed.")
    }
    
    kaitz_values_0 <- df_mop[(df_mop$fips == j)&(df_mop$time_marker == kaimat[k,1]),] |>
      select(
        sub("_matrix$", "", kaimatrix)
      )
    
    if (length(unique(kaitz_values_0[[1]])) > 0) {
      kaimat[k,3] <- unique(kaitz_values_0[[1]])
    } else {
      kaimat[k,3] = NA
      print("Replacement vector is empty. NA assignment performed.")
    }
    
    kaitz_values_1 <- df_mop[df_mop$time_marker == kaimat[k,1],] |> 
      select(
        sub("_matrix$", "", kaimatrix)
        )
    
    kaimat[k,4] <- kaitz_values_1[[1]] |> 
      median(na.rm = T)
    
  }
  
  kaitz_matrices[[kaimatrix]] <- kaimat
  
}


for (kaimatrix in names(kaitz_matrices)){
  assign(sub("_matrix$", "_df", kaimatrix),kaitz_matrices[[kaimatrix]])
}


kaitz_pct10_df <- as.data.frame(kaitz_pct10_df)

kaitz_pct10_df <- kaitz_pct10_df |> 
  mutate(
    above = 1*(V3 > V4)
  )

kaitz_pct25_df <- as.data.frame(kaitz_pct25_df)

kaitz_pct25_df <- kaitz_pct25_df |> 
  mutate(
    above = 1*(V3 > V4)
  )

kaitz_median_df <- as.data.frame(kaitz_median_df)

kaitz_median_df <- kaitz_median_df |> 
  mutate(
    above = 1*(V3 > V4)
  )

kaitz_pct75_df <- as.data.frame(kaitz_pct75_df)

kaitz_pct75_df <- kaitz_pct75_df |> 
  mutate(
    above = 1*(V3 > V4)
  )

kaitz_pct90_df <- as.data.frame(kaitz_pct90_df)

kaitz_pct90_df <- kaitz_pct90_df |> 
  mutate(
    above = 1*(V3 > V4)
  )


below_med_cts_pct10 <- fips_names_mop[which(kaitz_pct10_df$above == 0)]
above_med_cts_pct10 <- fips_names_mop[which(kaitz_pct10_df$above == 1)]

below_med_cts_pct25 <- fips_names_mop[which(kaitz_pct25_df$above == 0)]
above_med_cts_pct25 <- fips_names_mop[which(kaitz_pct25_df$above == 1)]

below_med_cts_median <- fips_names_mop[which(kaitz_median_df$above == 0)]
above_med_cts_median <- fips_names_mop[which(kaitz_median_df$above == 1)]

below_med_cts_pct75 <- fips_names_mop[which(kaitz_pct75_df$above == 0)]
above_med_cts_pct75 <- fips_names_mop[which(kaitz_pct75_df$above == 1)]

below_med_cts_pct90 <- fips_names_mop[which(kaitz_pct90_df$above == 0)]
above_med_cts_pct90 <- fips_names_mop[which(kaitz_pct90_df$above == 1)]







# Split
effects_mop_below_mw_pct10 <- effects_mop_cty[fips_names_mop %in% below_med_cts_pct10,]
effects_mop_above_mw_pct10 <- effects_mop_cty[fips_names_mop %in% above_med_cts_pct10,]

effects_mop_below_mw_pct25 <- effects_mop_cty[fips_names_mop %in% below_med_cts_pct25,]
effects_mop_above_mw_pct25 <- effects_mop_cty[fips_names_mop %in% above_med_cts_pct25,]

effects_mop_below_mw_median <- effects_mop_cty[fips_names_mop %in% below_med_cts_median,]
effects_mop_above_mw_median <- effects_mop_cty[fips_names_mop %in% above_med_cts_median,]

effects_mop_below_mw_pct75 <- effects_mop_cty[fips_names_mop %in% below_med_cts_pct75,]
effects_mop_above_mw_pct75 <- effects_mop_cty[fips_names_mop %in% above_med_cts_pct75,]

effects_mop_below_mw_pct90 <- effects_mop_cty[fips_names_mop %in% below_med_cts_pct90,]
effects_mop_above_mw_pct90 <- effects_mop_cty[fips_names_mop %in% above_med_cts_pct90,]

# Plot
gplot_values_pct10 <- as_tibble(cbind(-window:window,
                                colMeans(effects_mop_below_mw_pct10,na.rm=T),
                                sapply(1:(2*window+1),function(x){sd(effects_mop_below_mw_pct10[,x],na.rm = T)/sqrt(length(effects_mop_below_mw_pct10[,x]))}),
                                colMeans(effects_mop_above_mw_pct10,na.rm=T),
                                sapply(1:(2*window+1),function(x){sd(effects_mop_above_mw_pct10[,x],na.rm = T)/sqrt(length(effects_mop_above_mw_pct10[,x]))})))

gplot_values_pct10 <- cbind(gplot_values_pct10,
                      gplot_values_pct10[["V2"]] - 1.96*gplot_values_pct10[["V3"]],
                      gplot_values_pct10[["V2"]] + 1.96*gplot_values_pct10[["V3"]],
                      gplot_values_pct10[["V4"]] - 1.96*gplot_values_pct10[["V5"]],
                      gplot_values_pct10[["V4"]] + 1.96*gplot_values_pct10[["V5"]])

gplot_values_pct10 <- gplot_values_pct10 |>
  rename(ci1 = 'gplot_values_pct10[["V2"]] - 1.96 * gplot_values_pct10[["V3"]]') |>
  rename(ci2 = 'gplot_values_pct10[["V2"]] + 1.96 * gplot_values_pct10[["V3"]]') |>
  rename(ci3 = 'gplot_values_pct10[["V4"]] - 1.96 * gplot_values_pct10[["V5"]]') |>
  rename(ci4 = 'gplot_values_pct10[["V4"]] + 1.96 * gplot_values_pct10[["V5"]]') 

g_plot_pct10 <- ggplot(gplot_values_pct10) + 
  geom_line(aes(x=V1,y=V2),color = "#0098e9",linewidth=1) +
  geom_line(aes(x=V1,y=ci1),color = "#0098e9",linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=ci2),color = "#0098e9", linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=V4),color = "#ff5ca8",linewidth=1) +
  geom_line(aes(x=V1,y=ci3),color = "#ff5ca8", linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=ci4),color = "#ff5ca8",linetype=2,linewidth=1) +
  ylab("Effect on unemployment rate") +
  xlab("Period relative to treatment") + 
  labs(title="Effect of Modern System PDMPs on unemployment rates,<br>for counties with minimum wage <span style='color:#0098e9;'>below</span> and <span style='color:#ff5ca8;'>above</span> the median<br>at t=0") +
  geom_hline(yintercept = 0, linetype=5, color= "Black", linewidth=0.5) +
  geom_vline(xintercept = 0, linetype=5, color= "Black", linewidth=0.5)
g_plot_pct10 <- g_plot_pct10 + theme_classic() + theme(text = element_text(size=16, family="serif"),plot.title = element_markdown(lineheight = 1.1)) 
g_plot_pct10





gplot_values_pct25 <- as_tibble(cbind(-window:window,
                                      colMeans(effects_mop_below_mw_pct25,na.rm=T),
                                      sapply(1:(2*window+1),function(x){sd(effects_mop_below_mw_pct25[,x],na.rm = T)/sqrt(length(effects_mop_below_mw_pct25[,x]))}),
                                      colMeans(effects_mop_above_mw_pct25,na.rm=T),
                                      sapply(1:(2*window+1),function(x){sd(effects_mop_above_mw_pct25[,x],na.rm = T)/sqrt(length(effects_mop_above_mw_pct25[,x]))})))

gplot_values_pct25 <- cbind(gplot_values_pct25,
                            gplot_values_pct25[["V2"]] - 1.96*gplot_values_pct25[["V3"]],
                            gplot_values_pct25[["V2"]] + 1.96*gplot_values_pct25[["V3"]],
                            gplot_values_pct25[["V4"]] - 1.96*gplot_values_pct25[["V5"]],
                            gplot_values_pct25[["V4"]] + 1.96*gplot_values_pct25[["V5"]])

gplot_values_pct25 <- gplot_values_pct25 |>
  rename(ci1 = 'gplot_values_pct25[["V2"]] - 1.96 * gplot_values_pct25[["V3"]]') |>
  rename(ci2 = 'gplot_values_pct25[["V2"]] + 1.96 * gplot_values_pct25[["V3"]]') |>
  rename(ci3 = 'gplot_values_pct25[["V4"]] - 1.96 * gplot_values_pct25[["V5"]]') |>
  rename(ci4 = 'gplot_values_pct25[["V4"]] + 1.96 * gplot_values_pct25[["V5"]]') 

g_plot_pct25 <- ggplot(gplot_values_pct25) + 
  geom_line(aes(x=V1,y=V2),color = "#0098e9",linewidth=1) +
  geom_line(aes(x=V1,y=ci1),color = "#0098e9",linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=ci2),color = "#0098e9", linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=V4),color = "#ff5ca8",linewidth=1) +
  geom_line(aes(x=V1,y=ci3),color = "#ff5ca8", linetype=2,linewidth=1) +
  geom_line(aes(x=V1,y=ci4),color = "#ff5ca8",linetype=2,linewidth=1) +
  ylab("Effect on unemployment rate") +
  xlab("Period relative to treatment") + 
  labs(title="Effect of Modern System PDMPs on unemployment rates,<br>for counties with minimum wage <span style='color:#0098e9;'>below</span> and <span style='color:#ff5ca8;'>above</span> the median<br>at t=0") +
  geom_hline(yintercept = 0, linetype=5, color= "Black", linewidth=0.5) +
  geom_vline(xintercept = 0, linetype=5, color= "Black", linewidth=0.5)
g_plot_pct25 <- g_plot_pct25 + theme_classic() + theme(text = element_text(size=16, family="serif"),plot.title = element_markdown(lineheight = 1.1)) 
g_plot_pct25
