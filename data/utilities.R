
f_state_avg_effects <- function(df, policy, window){
  
  # df = all states
  
  if (policy == "mop"){
    labor_outcomes <- c("unem_rate_tilde_mop", "unem_tilde_mop",
                        "emp_tilde_mop", "lab_force_tilde_mop")
    df0 <- df[!is.na(df$first_treatment_mop),]
  } else if (policy == "pmq"){
    labor_outcomes <- c("unem_rate_tilde_pmq", "unem_tilde_pmq",
                        "emp_tilde_pmq", "lab_force_tilde_pmq")
    df0 <- df[!is.na(df$first_treatment_pmq),]
  } else {
    print("Policy must be either mop or pmq")
  }

  state_names <- unique(df$state)
  state_names0 <- unique(df0$state)
  
  effects_state <- array(NA, c(length(state_names), 2*window+1, length(labor_outcomes))) 
  
  # Obtain average effects by state and for each outcome
  for (i in state_names0){
    
    # get index for element in vector
    j = which(1*(state_names == i) == 1)
    
    # get treatment date for each state
    if (policy == "mop"){
      treatment_state = unique(
        df0[df0$state == i,]$first_treatment_mop
        )
    } else if (policy == "pmq"){
      treatment_state = unique(
        df0[df0$state == i,]$first_treatment_pmq
        )
    }
    
    # create a range around the date of treatment
    range = (treatment_state - window):(treatment_state + window)
    len_range = length(range)
    
    # aggregation
    h = 0
    for (outcome in labor_outcomes){
      
      # average the effects of the counties in each period
      avg_effects_state <- df0[(df0$state == i)&(df0$time_marker %in% range),] |>
        dplyr::group_by(time_marker) |>
        summarise_at(
          vars(outcome),
          list(avg = mean)
        )
      
      len_effects = length(avg_effects_state$avg)
      
      h = h + 1
      
      if (len_effects == len_range){
        effects_state[j,,h] <- matrix(
          avg_effects_state$avg,
          1,
          len_range
          )
      } else if (policy == "mop"){
        # Kentucky was an early adopter 
        # (mop passed in 07/1999).
        # to include the available effects we have to 
        # plug in missing values in the initial periods
        # (otherwise vector to short to fit the row of
        # 'effects_mop_state')
        early_adopters <- rep(NaN,len_range)
        early_adopters[(len_range - len_effects + 1):len_range] <- avg_effects_state$avg
        effects_state[j,,h] <- matrix(
          early_adopters,
          1,
          len_range
          )
      } else if (policy == "pmq"){
        # in this case we have a lot of late adopters,
        # so the adjustment is in the other direction
        late_adopters <- rep(NaN,len_range)
        late_adopters[1:len_effects] <- avg_effects_state$avg
        effects_state[j,,h] <- matrix(
          late_adopters,
          1,
          len_range
          )
        }
        
      }
      }
  
  return(effects_state)
  
}

f_county_effects <- function(df,policy,window){
  
  # df = all states
  
  if (policy == "mop"){
    labor_outcomes <- c("unem_rate_tilde_mop", "unem_tilde_mop",
                        "emp_tilde_mop", "lab_force_tilde_mop")
    df0 <- df[!is.na(df$first_treatment_mop),]
  } else if (policy == "pmq"){
    labor_outcomes <- c("unem_rate_tilde_pmq", "unem_tilde_pmq",
                        "emp_tilde_pmq", "lab_force_tilde_pmq")
    df0 <- df[!is.na(df$first_treatment_pmq),]
  } else {
    print("Policy must be either mop or pmq")
  }
  
  fips_names <- unique(df$fips)
  fips_names0 <- unique(df0$fips)

  effects_cty <- array(NA, c(length(fips_names), 2*window+1, length(labor_outcomes)))
  
  # Obtain effects by county
  for (i in fips_names0){
    
    # get index for element in vector
    j = which(1*(fips_names == i) == 1)
    
    # get treatment date for each county
    if (policy == "mop"){
      treatment_cty = unique(
        df0[df0$fips == i,]$first_treatment_mop
      )
    } else if (policy == "pmq"){
      treatment_cty = unique(
        df0[df0$fips == i,]$first_treatment_pmq
      )
    }
    
    # create a range around the date of treatment
    range = (treatment_cty - window):(treatment_cty + window)
    len_range = length(range)
    
    # obtain the effects of the counties in each period
    avg_effects_cty <- df0[(df0$fips == i)&(df0$time_marker %in% range),] 
    avg_effects_cty <- avg_effects_cty[order(avg_effects_cty$time_marker),]
    
    # Aggregation
    h = 0
    for (outcome in labor_outcomes){
      h = h + 1
      len_effects = length(avg_effects_cty[[outcome]])
      if (len_effects == len_range){
        effects_cty[j,,h] <- matrix(
          avg_effects_cty[[outcome]],
          1,
          len_range)
      } else if (policy == "mop"){ 
        # Kentucky
        early_adopters <- rep(NaN,len_range)
        early_adopters[(len_range - len_effects + 1):len_range] <- avg_effects_cty[[outcome]]
        effects_cty[j,,h] <- matrix(
          early_adopters,
          1,
          len_range)
      } else if (policy == "pmq"){ 
        # Same provision as before
        late_adopters <- rep(NaN,len_range)
        late_adopters[1:len_effects] <- avg_effects_cty[[outcome]]
        effects_cty[j,,h] <- matrix(
          late_adopters,
          1,
          len_range)
      }
    } 
    }
  
  return(effects_cty)
  
}

f_states_above_below <- function(df,policy){
  
  if (policy == "mop"){
    df0 <- df[!is.na(df$first_treatment_mop),]
  } else if (policy == "pmq"){
    df0 <- df[!is.na(df$first_treatment_pmq),]
  } else {
    print("Policy must be either mop or pmq")
  }

  state_names0 <- unique(df0$state)
  
  above_below_minw = matrix(NA,length(state_names0),4)
  
  for (i in state_names0){
    # get index for element in vector
    j = which(1*(state_names0 == i) == 1)
    
    # get treatment date for each state
    if (policy == "mop"){
      above_below_minw[j,1] = unique(
        df0[df0$state == i,]$first_treatment_mop
      )
    } else if (policy == "pmq"){
      above_below_minw[j,1] = unique(
        df0[df0$state == i,]$first_treatment_pmq
      )
    }
    
    at_treatment <- df0[(df0$state == i) & (df0$time_marker == above_below_minw[j,1]),]
    
    # retrieve the year of treatment
    above_below_minw[j,2] <- unique(
      at_treatment$year
    )
    
    # get minimum wage at treatment
    above_below_minw[j,3] <- unique(
      at_treatment$minw
    )
    
    # get the national medians at treatment
    above_below_minw[j,4] <- median(
      df0[df0$time_marker == above_below_minw[j,1],]$minw
    )
    }

  return(above_below_minw)
  
}


f_county_kaitz <- function(df,policy){
  
  kaitz_matrices <- vector("list", length = 5)
  
  if (policy == "mop"){
    names(kaitz_matrices) <- c("kaitz_pct10_mop_matrix", "kaitz_pct25_mop_matrix", 
                               "kaitz_median_mop_matrix", "kaitz_pct75_mop_matrix", 
                               "kaitz_pct90_mop_matrix")
    df0 <- df[!is.na(df$first_treatment_mop),]
  } else if (policy == "pmq"){
    names(kaitz_matrices) <- c("kaitz_pct10_pmq_matrix", "kaitz_pct25_pmq_matrix", 
                               "kaitz_median_pmq_matrix", "kaitz_pct75_pmq_matrix", 
                               "kaitz_pct90_pmq_matrix")
    df0 <- df[!is.na(df$first_treatment_pmq),]
  } else {
    print("Policy must be either mop or pmq")
  }

  fips_names0 <- unique(df0$fips)
  
  # Split counties by distribution of Kaitz-p indices
  for (kma in names(kaitz_matrices)){
    
    # fill the list with initialized matrices  
    kaitz_values <- matrix(0,length(fips_names0),4)
    
    for (i in fips_names0){
      
      # get index for element in vector
      j = which(1*(fips_names0 == i) == 1)
      
      # get treatment date for each county
      if (policy == "mop"){
        kaitz_values[j,1] = unique(
          df0[df0$fips == i,]$first_treatment_mop
        )
      } else if (policy == "pmq"){
        kaitz_values[j,1] = unique(
          df0[df0$fips == i,]$first_treatment_pmq
        )
      }
      
      at_treatment <- df0[(df0$fips == i)&(df0$time_marker == kaitz_values[j,1]),]
      
      # get treatment year for each county
      year_treatment <- unique(
        at_treatment$year
      )
      
      if (length(year_treatment) > 0) {
        kaitz_values[j,2] <- year_treatment
      } else {
        kaitz_values[j,2] = NaN
      }
      
      # get kaitz percentile at treatment
      if (policy == "mop"){
        kaitz_pct_treat <- at_treatment |>
          select(
            sub("_mop_matrix$", "", kma)
          )
      } else if (policy == "pmq"){
        kaitz_pct_treat <- at_treatment |>
          select(
            sub("_pmq_matrix$", "", kma)
          )
      }
      
      kaitz_pct_treat <- unique(kaitz_pct_treat[[1]])
      
      if (length(kaitz_pct_treat) > 0) {
        kaitz_values[j,3] <- kaitz_pct_treat
      } else {
        kaitz_values[j,3] = NaN
      }
      
      # get national median of kaitz percentile at treatment
      if (policy == "mop"){
        kaitz_pct_treat_nac <- df0[df0$time_marker == kaitz_values[j,1],] |> 
          select(
            sub("_mop_matrix$", "", kma)
          )
      } else if (policy == "pmq"){
        kaitz_pct_treat_nac <- df0[df0$time_marker == kaitz_values[j,1],] |> 
          select(
            sub("_pmq_matrix$", "", kma)
          )
      }
      
      kaitz_values[j,4] <- kaitz_pct_treat_nac[[1]] |> 
        median(na.rm = T)
      
    }
    
    kaitz_matrices[[kma]] <- kaitz_values
    
  }
  
  return(kaitz_matrices)
  
}


