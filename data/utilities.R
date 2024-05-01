
CountyEffects <- function(df,policy,window){
  
  # df => all counties, not only those for which
  # policy was implemented

  if (policy %in% c("mop", "pmq")) {
    # Define unexplained parts of interest
    labor_outcomes <- c(paste0("unem_rate_tilde_", policy), 
                        paste0("emp_rate_tilde_", policy), 
                        paste0("lab_force_rate_tilde_", policy)
                        )
    # As well as the subsample of treated counties
    df_treated <- df[!is.na(df[[paste0("first_treatment_", policy)]]),]
  } else {
    print("Policy must be either mop or pmq")
  }
  
  # Get the unique identifiers for both the sample and subsample
  fips_names <- unique(df$fips)
  fips_names_treated <- unique(df_treated$fips)
  
  # Initialize array
  effects_cty <- array(NA, c(length(fips_names), 2*window+2, length(labor_outcomes)))
  
  # Obtain effects by county
  for (fip in fips_names_treated){
    
    # Get index for element in vector (all counties)
    ind = which(1*(fips_names == fip) == 1)
    
    # Assign county identifier
    effects_cty[ind,1,] <- fip
    
    # Get treatment date for each county
    treatment_cty <- unique(
      df_treated[df_treated$fips == fip, paste0("first_treatment_", policy)]
      )
    
    # Create a range around the date of treatment
    range = (treatment_cty - window):(treatment_cty + window)
    len_range = length(range)
    
    # Obtain the values for the counties in each period within the range
    avg_effects_cty <- df_treated[(df_treated$fips == i)&(df_treated$time_marker %in% range),] 
    avg_effects_cty <- avg_effects_cty[order(avg_effects_cty$time_marker),]
    
    # Set indices
    out_ind = 0
    effs_cols = 2:(2*window+2) 
    
    # Iterate over the outcomes of interest
    for (outcome in labor_outcomes){
      out_ind = out_ind + 1
      len_effects = length(avg_effects_cty[[outcome]])
      if (len_effects == len_range){
        effects_cty[ind,effs_cols,out_ind] <- matrix(
          avg_effects_cty[[outcome]],
          1,
          len_range)
      } else if (policy == "mop"){ 
        # Kentucky was an early adopter 
        # (mop passed in 07/1999).
        # to include the available effects we have to 
        # plug in missing values in the initial periods
        # (otherwise vector to short to fit the row of
        # 'effects_mop_state')
        early_adopters <- rep(NaN,len_range)
        early_adopters[(len_range - len_effects + 1):len_range] <- avg_effects_cty[[outcome]]
        effects_cty[ind,effs_cols,out_ind] <- matrix(
          early_adopters,
          1,
          len_range)
      } else if (policy == "pmq"){ 
        # in this case we have a lot of late adopters,
        # so the adjustment is in the other direction
        late_adopters <- rep(NaN,len_range)
        late_adopters[1:len_effects] <- avg_effects_cty[[outcome]]
        effects_cty[ind,effs_cols,out_ind] <- matrix(
          late_adopters,
          1,
          len_range)
      }
    } 
  }

  return(effects_cty)
  
  }


KaitzAtTreatment <- function(df,policy){
  
  # df => all counties, not only those for which
  # policy was implemented
  
  if (policy %in% c("mop", "pmq")) {
    # Define the subsample of treated counties
    df_treated <- df[!is.na(df[[paste0("first_treatment_", policy)]]),]
  } else {
    print("Policy must be either mop or pmq")
  }
  
  # Get the unique identifiers
  fips_names_treated <- unique(df_treated$fips)
  
  # Initizalize dataframe (five percentiles + fips column)
  kaitz_cty <- data.frame(matrix(nrow = 0, ncol = 6))
  
  # Set the names
  column_indices <- c(grep("fips", colnames(df)), grep("kaitz_", colnames(df)))
  colnames(kaitz_cty) <- colnames(df[,column_indices])
  
  # Obtain indices by county
  for (fip in fips_names_treated){
    
    # Get treatment date for each county
    treatment_cty <- unique(
      df_treated[df_treated$fips == fip, paste0("first_treatment_", policy)]
    )
    
    # Select individual observations at treatment
    df_at_treatment <- df_treated[(df_treated$fips == fip)&(df_treated$time_marker == treatment_cty),]
    
    # Assign Kaitz-p index values and identifiers
    kaitz_cty <- rbind(kaitz_cty,df_at_treatment[,column_indices])
  }
   
  return(kaitz_cty) 

  }
