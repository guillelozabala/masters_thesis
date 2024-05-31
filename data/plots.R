
KaitzDensitiesPlot <- function(df){
  
  df_colnames <- df |> colnames()
  
  rel_to_treat_name <- df_colnames[grep("relative_to_treat_", df_colnames)]

  df <- df[df[[rel_to_treat_name]] == 0,]
  
  density_colors <- c("0.10" = "#0098e9",
                      "0.25" = "#ff5ca8",
                      "0.50" = "#5aa800",
                      "0.75" = "#f29318",
                      "0.90" = "#B79F00")
  
  densities_plot <- ggplot(df) +
    geom_density(
      aes(x = kaitz_pct10, ..count../sum(..count..), color = "0.10"), linewidth = 1
      ) +
    geom_density(
      aes(x = kaitz_pct25, ..count../sum(..count..), color = "0.25"), linewidth = 1
      ) + 
    geom_density(
      aes(x = kaitz_pct50, ..count../sum(..count..), color = "0.50"), linewidth = 1
      ) + 
    geom_density(
      aes(x = kaitz_pct75, ..count../sum(..count..), color = "0.75"), linewidth = 1
      ) + 
    geom_density(
      aes(x = kaitz_pct90, ..count../sum(..count..), color = "0.90"), linewidth = 1
      ) +
    scale_color_manual(
      name = element_blank(), values = density_colors
      )  +
    ylab(
      ""
      ) + 
    xlab(
      "Kaitz-p index"
      ) + 
    labs(
      title = "Distribution of Kaitz-p indices across counties, for different percentiles<br>at treatment"
      )
  
  densities_plot <- densities_plot + 
    theme_classic() + 
    theme(
      text = element_text(size = 16, family="serif"), 
      plot.title = element_markdown(lineheight = 1.1),
      legend.position = c(0.15, 0.80)
      ) 
  
  return(densities_plot)
  
}

TitleAndColors <- function(policy, outcome, percentile){
  
  percentiles <- c("0.10", "0.25", "0.50", "0.75", "0.90")
  
  if (percentile %in% percentiles) {
    # Define the Kaitz-p index percentile
    percentile_name <- paste0("kaitz_pct",gsub("0.","",percentile))
  } else {
    print("Percentile must be 0.10, 0.25, 0.50, 0.75 or 0.90")
  }
  
  if (policy == "mop"){
    policy_title <- "Modern System PDMPs"
  } else if (policy == "pmq") {
    policy_title <- "Must Query PDMPs"
  }
  
  if (outcome == "unem_rate"){
    outcome_title <- "unemployment rate"
    plot_color <- "#0098e9"
    plot_yrange <- ylim(-1.0,3.0)
  } else if (outcome == "emp_rate"){
    outcome_title <- "employment rate"
    plot_color <- "#ff5ca8"
    plot_yrange <- ylim(-1.5,1.0)
  } else if (outcome == "lab_force_rate"){
    outcome_title <- "labor force rate"
    plot_color <- "#5aa800"
    plot_yrange <- ylim(-3.5,3.5)
  }
  
  return(c(percentile_name, plot_color, plot_yrange, policy_title, outcome_title))
  
}


IndividualEffectsPlot <- function(df,
                                  periods,
                                  percentile,
                                  single_period=FALSE){
  
  df_colnames <- df |> colnames()
  
  # Obtain the policy from treatment name
  policy_name <- df_colnames[grep("first_treatment_", colnames(df))]
  policy <- sub("first_treatment_", "", policy_name)

  # Obtain the outcome from dependend name
  outcome_name <- df_colnames[grep("_tilde", colnames(df))]
  outcome <- sub("_tilde", "", outcome_name)
  
  naming <- TitleAndColors(policy, outcome, percentile)
  
  percentile_name = naming[[1]]
  plot_color = naming[[2]]
  plot_yrange = naming[[3]]
  policy_title  = naming[[4]]
  outcome_title = naming[[5]]
  
  plot_collection = list()
  
  for (period in periods){
    
    plot_data <- df[df[[paste0("relative_to_treat_",policy)]] == period,]
    
    binscatter <- binsreg(y = plot_data[[outcome_name]],
                          x = plot_data[[percentile_name]],
                          data = plot_data,
                          ci = T)
    
    binscatter_data <- binscatter$data.plot$`Group Full Sample`$data.bin
    
    allocation <- matrix(0, nrow(binscatter_data), 4)
    
    for (i in 1:nrow(binscatter_data)){
      c_interv <- plot_data[(plot_data[[percentile_name]] >= binscatter_data[['left.endpoint']][i])&(plot_data[[percentile_name]] < binscatter_data[['right.endpoint']][i]),]
      allocation[i,1] <- mean(c_interv[[outcome_name]], na.rm = T)
      allocation[i,2] <- allocation[i,1] - qnorm(0.025)*sqrt(var(c_interv[[outcome_name]], na.rm = T)/nrow(c_interv))
      allocation[i,3] <- allocation[i,1] + qnorm(0.025)*sqrt(var(c_interv[[outcome_name]], na.rm = T)/nrow(c_interv))
    }
    
    allocation[,4] <- binscatter$data.plot$`Group Full Sample`$data.dots$x
    
    binscatter_plot_df <- as_tibble(allocation)
    names(binscatter_plot_df) <- c('fit','ci.l','ci.r','x')
    
    plot <- ggplot(binscatter_plot_df, aes(x = x, y = fit)) +
      geom_point(
        color = plot_color
      ) +
      geom_line(
        color = plot_color
      ) +
      geom_errorbar(
        aes(ymin = ci.l, ymax = ci.r),
        color = plot_color
      ) +
      plot_yrange +
      xlim(-0.8,-0.2) +
      ylab(
        ""
      ) + 
      xlab(
        ""
      ) +
      labs(
        title = paste0("t = ", gsub("V", "", period))
      ) + 
      geom_hline(
        yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
      )
    
    plot <- plot + 
      theme_classic() +
      theme(
        text = element_text(size = 12, family="serif"), 
        plot.title = element_text(hjust = 0.5)
      )
    
    plot_collection[[paste0("plot_",period)]] <- plot
    
  }
  
  setting_labs <- gpar(fontsize=15, fontfamily="serif")
  setting_title <- gpar(fontsize=20, fontfamily="serif")
  
  lab_left = textGrob("Individual effects", rot=90, gp = setting_labs)
  
  lab_bottom = textGrob("Kaitz-p values", gp = setting_labs)
  
  lab_top = textGrob(paste0("Effects of ", policy_title," on ", outcome_title, "\n by minimum wage bindingness (Kaitz-", percentile, " index)"), gp = setting_title)
  
  plot_grid <- grid.arrange(grobs = plot_collection,
                            nrow = 2,
                            left = lab_left,
                            bottom = lab_bottom,
                            top = lab_top)
  
  if (single_period == T){
    single_plot <- plot_collection[[paste0("plot_",period[1])]]
    single_plot <- single_plot +
      ylab(
        "Individual effects"
      ) + 
      xlab(
        "Kaitz-p values"
      ) +
      labs(
        title = paste0("Effects of ", policy_title," on ", outcome_title, "\n by minimum wage bindingness (Kaitz-", percentile, " index)")
      )
    return(single_plot)
  }
  
  return(plot_grid)
  
}


IndividualEffectsCompPlot <- function(df,period){
  
  df_colnames <- df |> colnames()
  
  # Obtain the policy from treatment name
  policy_name <- df_colnames[grep("first_treatment_", colnames(df))]
  policy <- sub("first_treatment_", "", policy_name)
  
  # Obtain the outcome from dependend name
  outcome_name <- df_colnames[grep("_tilde", colnames(df))]
  outcome <- sub("_tilde", "", outcome_name)
  
  plot_colors <- c("0.10" = "#0098e9",
                   "0.25" = "#ff5ca8",
                   "0.50" = "#5aa800",
                   "0.75" = "#f29318",
                   "0.90" = "#B79F00")
  
  plot_collection = list()
  
  plot_data <- df[df[[paste0("relative_to_treat_",policy)]] == period,]
  
  binscatter_collection = list()
    
  for (percentile in percentiles){
    
    naming <- TitleAndColors(policy, outcome, percentile)
    
    percentile_name = naming[[1]]

    fit_name <- paste0(percentile_name,"_fit")
    
    binscatter_collection[[percentile_name]] <- binsreg(y = plot_data[[outcome_name]],
                                                        x = plot_data[[percentile_name]],
                                                        data = plot_data
                                                        )$data.plot$`Group Full Sample`$data.dots

    binscatter_collection[[percentile_name]] <- binscatter_collection[[percentile_name]] |>
      select(x, fit) |>
      setNames(c(percentile_name,fit_name)) 
    
    }

  binscatter_plot_df <- list_c(binscatter_collection) # different lengths, so no list_cbind()

  plot_yrange = naming[[3]]
  policy_title  = naming[[4]]
  outcome_title = naming[[5]]
  
  plot <- ggplot(binscatter_plot_df) +
    geom_point(
      aes(x = kaitz_pct10,
          y = kaitz_pct10_fit,
          color = "0.10")
      ) +
    geom_line(
      aes(x = kaitz_pct10,
          y = kaitz_pct10_fit,
          color = "0.10")
      ) +
    geom_point(
      aes(x = kaitz_pct25,
          y = kaitz_pct25_fit,
          color = "0.25")
      ) +
    geom_line(
      aes(x = kaitz_pct25,
          y = kaitz_pct25_fit,
          color = "0.25")
      ) +
    geom_point(
      aes(x = kaitz_pct50,
          y = kaitz_pct50_fit,
          color = "0.50")
      ) +
    geom_line(
      aes(x = kaitz_pct50,
          y = kaitz_pct50_fit,
          color = "0.50")
      ) +
    geom_point(
      aes(x = kaitz_pct75,
          y = kaitz_pct75_fit,
          color = "0.75")
      ) +
    geom_line(
      aes(x = kaitz_pct75,
          y = kaitz_pct75_fit,
          color = "0.75")
      ) +
    geom_point(
      aes(x = kaitz_pct90,
          y = kaitz_pct90_fit,
          color = "0.90")
      ) +
    geom_line(
      aes(x = kaitz_pct90,
          y = kaitz_pct90_fit,
          color = "0.90")
      ) +
    plot_yrange +
    ylab(
      "Individual effects"
      ) + 
    xlab(
      "Kaitz-p values"
      ) +
    labs(
      title = paste0("Effects of ",policy_title," on ", outcome_title, "\n for different Kaitz-p indices, t = ", gsub("V", "", period))
      ) +
    scale_color_manual(
      name = element_blank(), values = plot_colors
      ) + 
    geom_hline(
      yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
      )
  
  plot <- plot + 
    theme_classic() +
    theme(
      text = element_text(size = 16, family="serif"), 
      plot.title = element_text(hjust = 0.5)
    )
  
  return(plot)
  
}


TimeAveragesPlot <- function(df,percentile,up_to){
  
  df_colnames <- df |> colnames()
  
  # Obtain the policy from treatment name
  policy_name <- df_colnames[grep("first_treatment_", colnames(df))]
  policy <- sub("first_treatment_", "", policy_name)
  
  # Obtain the outcome from dependend name
  outcome_name <- df_colnames[grep("_tilde", colnames(df))]
  outcome <- sub("_tilde", "", outcome_name)
  
  rel_treat_name <- df_colnames[grep("relative_to_treat_", colnames(df))]
  
  naming <- TitleAndColors(policy, outcome, percentile)
  
  percentile_name = naming[[1]]
  plot_color = naming[[2]]
  plot_yrange = naming[[3]]
  policy_title  = naming[[4]]
  outcome_title = naming[[5]]

  df_plot <- df[,c("fips", rel_treat_name, outcome_name)]
  
  allocation <- data.frame(unique(df_plot$fips))
  names(allocation) <- 'fips'
  
  for (period in 1:up_to){
    allocation_inperiod <- df_plot[df_plot[[rel_treat_name]] == period, ]
    names(allocation_inperiod) <- c("fips", rel_treat_name, paste0(outcome_name,"_",period))
    allocation <- merge(allocation,
                        allocation_inperiod[,c("fips",paste0(outcome_name,"_",period))],
                        by = 'fips'
                        )
    
  }
  
  df_plot_1 <- data.frame(cbind(allocation[,1], rowMeans(allocation[,2:(up_to+1)])))
  names(df_plot_1) <- c('fips','average')
  
  df_period_1 <- df[df[[rel_treat_name]] == 1, ]
  df_plot_1 <- merge(df_plot_1,df_period_1[,c('fips',percentile_name)],by = 'fips')
  
  binscatter <- binsreg(y = df_plot_1[['average']],
                        x = df_plot_1[[percentile_name]],
                        data = df_plot_1,
                        ci = T)
  
  binscatter_data <- binscatter$data.plot$`Group Full Sample`$data.bin
  
  allocation_1 <- matrix(0, nrow(binscatter_data), 4)
  
  for (i in 1:nrow(binscatter_data)){
    c_interv <- df_plot_1[(df_plot_1[[percentile_name]] >= binscatter_data[['left.endpoint']][i])&(df_plot_1[[percentile_name]] < binscatter_data[['right.endpoint']][i]),]
    allocation_1[i,1] <- mean(c_interv[['average']], na.rm = T)
    allocation_1[i,2] <- allocation_1[i,1] - qnorm(0.025)*sqrt(var(c_interv[['average']], na.rm = T)/nrow(c_interv))
    allocation_1[i,3] <- allocation_1[i,1] + qnorm(0.025)*sqrt(var(c_interv[['average']], na.rm = T)/nrow(c_interv))
  }
  
  allocation_1[,4] <- binscatter$data.plot$`Group Full Sample`$data.dots$x
  
  binscatter_plot_df <- as_tibble(allocation_1)
  names(binscatter_plot_df) <- c('fit','ci.l','ci.r','x')

  plot <- ggplot(binscatter_plot_df, aes(x = x, y = fit)) +
    geom_point(
      color = plot_color
    ) +
    geom_line(
      color = plot_color
    ) +
    geom_errorbar(
      aes(ymin = ci.l, ymax = ci.r),
      color = plot_color
    ) +
    plot_yrange +
    ylab(
      "Time average of the effects"
    ) + 
    xlab(
      "Kaitz-p index"
    ) + 
    geom_hline(
      yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
    ) + 
    labs(
      title = paste0("Effects of ",policy_title," on ", outcome_title, "\n by minimum wage bindingness (Kaitz-", percentile, " index),\n time average")
    )
  
  plot <- plot + 
    theme_classic() +
    theme(
      text = element_text(size = 16, family="serif"), 
      plot.title = element_text(hjust = 0.5)
    )
  
  return(plot)
  
}



IndividualEffectsPlotPrescriptions <- function(df,percentile){
  
  df_colnames <- df |> colnames()
  
  # Obtain the policy from treatment name
  policy_name <- df_colnames[grep("first_treatment_", colnames(df))]
  policy <- sub("first_treatment_", "", policy_name)
  
  # Obtain the outcome from dependend name
  outcome_name <- "dosage_unit_pc_tilde"
  outcome <- "dosage_unit_pc"
  
  naming <- TitleAndColors(policy, "unem_rate", percentile)
  
  percentile_name = naming[[1]]
  plot_color = naming[[2]]
  plot_yrange <- ylim(-0.15,0.15)
  policy_title  = naming[[4]]
  outcome_title = naming[[5]]
  
  plot_data <- df
  
  binscatter <- binsreg(y = plot_data[[outcome_name]],
                        x = plot_data[[percentile_name]],
                        data = plot_data,
                        ci = T)
  
  binscatter_data <- binscatter$data.plot$`Group Full Sample`$data.bin
  
  allocation <- matrix(0, nrow(binscatter_data), 4)
  
  for (i in 1:nrow(binscatter_data)){
    c_interv <- plot_data[(plot_data[[percentile_name]] >= binscatter_data[['left.endpoint']][i])&(plot_data[[percentile_name]] < binscatter_data[['right.endpoint']][i]),]
    allocation[i,1] <- mean(c_interv[[outcome_name ]], na.rm = T)
    allocation[i,2] <- allocation[i,1] - qnorm(0.025)*sqrt(var(c_interv[[outcome_name]], na.rm = T)/nrow(c_interv))
    allocation[i,3] <- allocation[i,1] + qnorm(0.025)*sqrt(var(c_interv[[outcome_name]], na.rm = T)/nrow(c_interv))
  }
  
  allocation[,4] <- binscatter$data.plot$`Group Full Sample`$data.dots$x
  
  binscatter_plot_df <- as_tibble(allocation)
  
  names(binscatter_plot_df) <- c('fit','ci.l','ci.r','x')
  
  plot <- ggplot(binscatter_plot_df, aes(x = x, y = fit)) +
    geom_point(
      color = plot_color
    ) +
    geom_line(
      color = plot_color
    ) +
    geom_errorbar(
      aes(ymin = ci.l, ymax = ci.r),
      color = plot_color
    ) +
    plot_yrange +
    ylab(
      "Individual effects"
    ) + 
    xlab(
      "Kaitz-p values"
    ) +
    geom_hline(
      yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
    ) +
    labs(
      title = paste0("Effects of ", policy_title," on prescriptions", "\n by minimum wage bindingness (Kaitz-", percentile, " index)")
    )
  
  plot <- plot + 
    theme_classic() +
    theme(
      text = element_text(size = 12, family="serif"), 
      plot.title = element_text(hjust = 0.5)
    )
  
  return(plot)
  
}



IndividualEffectsPlot_Oxy <- function(df,
                                      periods,
                                      percentile,
                                      single_period=FALSE){
  
  df_colnames <- df |> colnames()
  
  # Obtain the policy from treatment name
  # policy_name <- df_colnames[grep("first_treatment_", colnames(df))]
  # policy <- sub("first_treatment_", "", policy_name)
  policy <- "mop"
  
  # Obtain the outcome from dependend name
  outcome_name <- df_colnames[grep("_tilde", colnames(df))]
  outcome <- sub("_tilde", "", outcome_name)
  
  naming <- TitleAndColors(policy, outcome, percentile)
  
  percentile_name = naming[[1]]
  plot_color = naming[[2]]
  plot_yrange = naming[[3]]
  policy_title  = naming[[4]]
  outcome_title = naming[[5]]
  
  plot_collection = list()
  
  for (period in periods){
    
    plot_data <- df[df[["relative_to_refor"]] == period,]
    
    binscatter <- binsreg(y = plot_data[[outcome_name]],
                          x = plot_data[[percentile_name]],
                          data = plot_data,
                          ci = T)
    
    binscatter_data <- binscatter$data.plot$`Group Full Sample`$data.bin
    
    allocation <- matrix(0, nrow(binscatter_data), 4)
    
    for (i in 1:nrow(binscatter_data)){
      c_interv <- plot_data[(plot_data[[percentile_name]] >= binscatter_data[['left.endpoint']][i])&(plot_data[[percentile_name]] < binscatter_data[['right.endpoint']][i]),]
      allocation[i,1] <- mean(c_interv[[outcome_name]], na.rm = T)
      allocation[i,2] <- allocation[i,1] - qnorm(0.025)*sqrt(var(c_interv[[outcome_name]], na.rm = T)/nrow(c_interv))
      allocation[i,3] <- allocation[i,1] + qnorm(0.025)*sqrt(var(c_interv[[outcome_name]], na.rm = T)/nrow(c_interv))
      }
    
    allocation[,4] <- binscatter$data.plot$`Group Full Sample`$data.dots$x
    
    binscatter_plot_df <- as_tibble(allocation)
    names(binscatter_plot_df) <- c('fit','ci.l','ci.r','x')
    
    plot <- ggplot(binscatter_plot_df, aes(x = x, y = fit)) +
      geom_point(
        color = plot_color
      ) +
      geom_line(
        color = plot_color
      ) +
      geom_errorbar(
        aes(ymin = ci.l, ymax = ci.r),
        color = plot_color
      ) +
      plot_yrange +
      xlim(-0.8,-0.2) +
      ylab(
        ""
      ) + 
      xlab(
        ""
      ) +
      labs(
        title = paste0("t = ", gsub("V", "", period))
      ) + 
      geom_hline(
        yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
      )
    
    plot <- plot + 
      theme_classic() +
      theme(
        text = element_text(size = 12, family="serif"), 
        plot.title = element_text(hjust = 0.5)
      )
    
    plot_collection[[paste0("plot_",period)]] <- plot
    
  }
  
  setting_labs <- gpar(fontsize=15, fontfamily="serif")
  setting_title <- gpar(fontsize=20, fontfamily="serif")
  
  lab_left = textGrob("Individual effects", rot=90, gp = setting_labs)
  
  lab_bottom = textGrob("Kaitz-p values", gp = setting_labs)
  
  lab_top = textGrob(paste0("Effects of ", policy_title," on ", outcome_title, "\n by minimum wage bindingness (Kaitz-", percentile, " index)"), gp = setting_title)
  
  plot_grid <- grid.arrange(grobs = plot_collection,
                            nrow = 2,
                            left = lab_left,
                            bottom = lab_bottom,
                            top = lab_top)
  
  if (single_period == T){
    single_plot <- plot_collection[[paste0("plot_",period[1])]]
    single_plot <- single_plot +
      ylab(
        "Individual effects"
      ) + 
      xlab(
        "Kaitz-p values"
      ) +
      labs(
        title = paste0("Effects of ", policy_title," on ", outcome_title, "\n by minimum wage bindingness (Kaitz-", percentile, " index)")
      )
    return(single_plot)
  }
  
  return(plot_grid)
  
}
