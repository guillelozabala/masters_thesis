
KaitzDensitiesPlot <- function(df){
  
  density_colors <- c("0.10" = "#0098e9",
                      "0.25" = "#ff5ca8",
                      "0.50" = "#5aa800",
                      "0.75" = "#f29318",
                      "0.90" = "#B79F00")
  
  densities_plot <- ggplot(kaitz_at_treatment) +
    geom_density(
      aes(x = kaitz_pct10, ..count../sum(..count..), color = "0.10"), linewidth = 1
      ) +
    geom_density(
      aes(x = kaitz_pct25, ..count../sum(..count..), color = "0.25"), linewidth = 1
      ) + 
    geom_density(
      aes(x = kaitz_median, ..count../sum(..count..), color = "0.50"), linewidth = 1
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
        )
    
    plot <- plot + 
      theme_classic() +
      theme(
        text = element_text(size = 10, family="serif"), 
        plot.title = element_text(hjust = 0.5)
      )
   
    plot_collection[[paste0("plot_",period)]] <- plot
    
  }
  
  lab_left = textGrob("Individual effects",
                      rot=90,
                      gp = gpar(fontsize=15, fontfamily="serif")
                      )
  
  lab_bottom = textGrob("Kaitz-p values",
                        gp = gpar(fontsize=15, fontfamily="serif")
                        )
  
  lab_top = textGrob(paste0(policy," ", outcomes, " ", percentile),
                     gp = gpar(fontsize=20, fontfamily="serif")
                     )
  
  plot_grid <- grid.arrange(grobs = plot_collection,
                            nrow = 2,
                            left = lab_left,
                            bottom = lab_bottom,
                            top = lab_top)
  
  return(plot_grid)
  
}

