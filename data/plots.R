
KaitzDensitiesPlot <- function(df){
  
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

IndividualEffectsPlot <- function(df,policy,outcome,percentile){
  
  outcomes <- c("unem_rate", "emp_rate","lab_force_rate")
  policies <- c("mop", "pmq")
  percentiles <- c("0.10", "0.25", "0.50", "0.75", "0.90")
  periods <- c("V1","V6","V12","V24")
  plot_colors <- c("#0098e9","#ff5ca8","#5aa800")
  plot_yranges <- c(ylim(-1.0,1.5),ylim(-1.5,1.0),ylim(-3.5,1.5))
  
  if (percentile %in% percentiles) {
    # Define the Kaitz-p index percentile
    percentile_name <- paste0("kaitz_pct",gsub("0.","",percentile))
  } else {
    print("Percentile must be 0.10, 0.25, 0.50, 0.75 or 0.90")
  }
  
  if ((policy %in% policies) & (outcome %in% outcomes)) {
    # Select data frame and color
    plot_data <- df[[paste0('effects_',policy,'_county_',outcome)]]
    plot_color <- plot_colors[outcomes == outcome]
    plot_yrange <- plot_yranges[outcomes == outcome] 
  } else {
    print("Policy must be either mop or pmq, and outcome must be unem_rate, emp_rate or lab_force_rate")
  }
  
  plot_collection = list()
  
  for (period in periods){
    
    binscatter <- binsreg(y = plot_data[[period]],
                          x = plot_data[[percentile_name]],
                          data = plot_data,
                          ci=T)
    
    binscatter_data <- binscatter$data.plot$`Group Full Sample`
    
    binscatter_plot_df <- binscatter_data$data.dots |>
      mutate(
        ci.l = binscatter_data$data.ci$ci.l,
        ci.r = binscatter_data$data.ci$ci.r
      )
    
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
  
  if (policy == "mop"){
    policy <- "Modern System PDMPs"
  } else if (policy == "pmq") {
    policy <- "Must Query PDMPs"
  }
  
  if (outcome == "unem_rate"){
    outcome <- "unemployment rate"
  } else if (outcome == "emp_rate"){
    outcome <- "employment rate"
  } else if (outcome == "lab_force_rate"){
    outcome <- "labor force rate"
  }
  
  lab_top = textGrob(paste0("Effects of ",policy," on ", outcome, "\n by minimum wage bindingness (Kaitz-", percentile, " index)"), gp = setting_title)
  
  plot_grid <- grid.arrange(grobs = plot_collection,
                            nrow = 2,
                            left = lab_left,
                            bottom = lab_bottom,
                            top = lab_top)
  
  return(plot_grid)
  
}


IndividualEffectsCompPlot <- function(df,policy,outcome,period){
  
  #periods <- c("V1","V6","V12","V24")
  
  outcomes <- c("unem_rate", "emp_rate","lab_force_rate")
  policies <- c("mop", "pmq")
  percentiles <- c("0.10", "0.25", "0.50", "0.75", "0.90")
  plot_yranges <- c(ylim(-1.0,1.5),ylim(-1.5,1.0),ylim(-3.5,1.5))
  
  plot_colors <- c("0.10" = "#0098e9",
                   "0.25" = "#ff5ca8",
                   "0.50" = "#5aa800",
                   "0.75" = "#f29318",
                   "0.90" = "#B79F00")
  
  if ((policy %in% policies) & (outcome %in% outcomes)) {
    # Select data frame and range
    plot_data <- df[[paste0('effects_',policy,'_county_',outcome)]]
    plot_yrange <- plot_yranges[outcomes == outcome] 
  } else {
    print("Policy must be either mop or pmq, and outcome must be unem_rate, emp_rate or lab_force_rate")
  }
  
  binscatter_collection = list()
    
  for (percentile in percentiles){
    
    percentile_name <- paste0("kaitz_pct",gsub("0.","",percentile))
    fit_name <- paste0(percentile_name,"_fit")
    
    binscatter_collection[[percentile_name]] <- binsreg(y = plot_data[[period]],
                                                        x = plot_data[[percentile_name]],
                                                        data = plot_data
                                                        )$data.plot$`Group Full Sample`$data.dots 
    
    binscatter_collection[[percentile_name]] <- binscatter_collection[[percentile_name]] |>
      select(x, fit) |>
      setNames(c(percentile_name,fit_name)) 
    
    }

  binscatter_plot_df <- list_c(binscatter_collection) # different lengths, so no list_cbind()
  
  if (policy == "mop"){
    policy <- "Modern System PDMPs"
  } else if (policy == "pmq") {
    policy <- "Must Query PDMPs"
  }
  
  if (outcome == "unem_rate"){
    outcome <- "unemployment rate"
  } else if (outcome == "emp_rate"){
    outcome <- "employment rate"
  } else if (outcome == "lab_force_rate"){
    outcome <- "labor force rate"
  }
  
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
      title = paste0("Effects of ",policy," on ", outcome, "\n for different Kaitz-p indices, t = ", gsub("V", "", period))
      ) +
    scale_color_manual(
      name = element_blank(), values = density_colors
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

TimeAveragesPlot <- function(df,df_fips,policy,outcome,percentile){
  
  df_averages <- df_fips
  
  if (percentile %in% percentiles) {
    # Define the Kaitz-p index percentile
    percentile_name <- paste0("kaitz_pct",gsub("0.","",percentile))
  } else {
    print("Percentile must be 0.10, 0.25, 0.50, 0.75 or 0.90")
  }
  
  outcomes <- c("unem_rate", "emp_rate","lab_force_rate")
  policies <- c("mop", "pmq")
  plot_colors <- c("#0098e9","#ff5ca8","#5aa800")
  plot_yranges <- c(ylim(-1.0,1.5),ylim(-1.5,1.0),ylim(-3.5,1.5))
  
  if ((policy %in% policies) & (outcome %in% outcomes)) {
    # Select color and range
    plot_color <- plot_colors[outcomes == outcome]
    plot_yrange <- plot_yranges[outcomes == outcome] 
  } else {
    print("Policy must be either mop or pmq, and outcome must be unem_rate, emp_rate or lab_force_rate")
  }
  
  for (output_name in names(df)){
    output_df <- df[[output_name]][,c("fips",paste0("V",1:24))]
    time_averages_df <- data.frame(cbind(output_df[,1],rowMeans(output_df[,-1])))
    time_averages_df <- time_averages_df |> 
      setNames(c("fips",output_name)) 
    df_averages <- merge(df_averages,
                         time_averages_df,
                         by = "fips",
                         all = T)
    }

  kaitzs_df <- df[[paste0('effects_',policy,'_county_',outcome)]]
  kaitzs_df <- kaitzs_df[,c("fips",percentile_name)]
  
  df_averages <- merge(df_averages,
                       kaitzs_df,
                       by = "fips",
                       all = T)
  

  binscatter <- binsreg(y = df_averages[[paste0('effects_',policy,'_county_',outcome)]],
                        x = df_averages[[percentile_name]],
                        data = df_averages,
                        ci = T)
  
  binscatter_data <- binscatter$data.plot$`Group Full Sample`
  
  binscatter_plot_df <- binscatter_data$data.dots |>
    mutate(
      ci.l = binscatter_data$data.ci$ci.l,
      ci.r = binscatter_data$data.ci$ci.r
      )
  
  if (policy == "mop"){
    policy <- "Modern System PDMPs"
  } else if (policy == "pmq") {
    policy <- "Must Query PDMPs"
  }
  
  if (outcome == "unem_rate"){
    outcome <- "unemployment rate"
  } else if (outcome == "emp_rate"){
    outcome <- "employment rate"
  } else if (outcome == "lab_force_rate"){
    outcome <- "labor force rate"
  }
  
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
      title = paste0("Effects of ",policy," on ", outcome, "\n by minimum wage bindingness (Kaitz-", percentile, " index),\n time average")
    )
  
  plot <- plot + 
    theme_classic() +
    theme(
      text = element_text(size = 16, family="serif"), 
      plot.title = element_text(hjust = 0.5)
    )
  
  return(plot)
  
}

