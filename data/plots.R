
state_plot <- function(df1,df2,window,policy,outcome){
  
  if (policy == "mop"){
    policy <- "Modern System PDMPs"
  } else if (policy == "pmq") {
    policy <- "Must Query PDMPs"
  } else {
    print("Policy must be either mop or pmq")
  }
  
  if (outcome == "unemployment rate"){
    df1 <- df1[,,1]
    df2 <- df2[,,1]
  } else if (outcome == "unemployment"){
    df1 <- df1[,,2]
    df2 <- df2[,,2]
  } else if (outcome == "employment"){
    df1 <- df1[,,3]
    df2 <- df2[,,3]
  } else if (outcome == "labor force"){
    df1 <- df1[,,4]
    df2 <- df2[,,4]
  } else {
    print("Outcome must be either unemployment rate, unemployment, employment or labor force")
  }

  plot_range <- -window:window
  
  effect_below_means <- colMeans(df1,na.rm=T)
  
  effect_below_sd <- sapply(
    1:(2*window+1),
    function(x){
      sd(df1[,x], na.rm = T)/sqrt(length(df1[,x]))
    }
  )
  
  effect_above_means <- colMeans(df2,na.rm=T)
  
  effect_above_sd <- sapply(
    1:(2*window+1),
    function(x){
      sd(df2[,x],na.rm = T)/sqrt(length(df2[,x]))
    }
  )
  
  state_plot_df <- as_tibble(
    cbind(plot_range,effect_below_means,effect_below_sd,effect_above_means,effect_above_sd)
  )
  
  names(state_plot_df) <- c("plot_range",
                            "effect_below_means",
                            "effect_below_sd",
                            "effect_above_means",
                            "effect_above_sd")
  
  state_plot_df <- state_plot_df |>
    mutate(
      ci_left_below = state_plot_df[["effect_below_means"]] - 1.96*state_plot_df[["effect_below_sd"]]
    ) |>
    mutate(
      ci_right_below = state_plot_df[["effect_below_means"]] + 1.96*state_plot_df[["effect_below_sd"]]
    ) |>
    mutate(
      ci_left_above = state_plot_df[["effect_above_means"]] - 1.96*state_plot_df[["effect_above_sd"]]
    ) |>
    mutate(
      ci_right_above = state_plot_df[["effect_above_means"]] + 1.96*state_plot_df[["effect_above_sd"]]
    )
  
  state_plot <- ggplot(state_plot_df) + 
    geom_line(
      aes(x = plot_range, y = effect_below_means), color = "#0098e9", linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_left_below), color = "#0098e9", linetype = 2, linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_right_below), color = "#0098e9", linetype = 2, linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = effect_above_means), color = "#ff5ca8", linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_left_above), color = "#ff5ca8", linetype = 2,linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_right_above), color = "#ff5ca8", linetype = 2,linewidth = 1
    ) +
    ylab(
      paste0("Effect on ", outcome)
    ) +
    xlab(
      "Period relative to treatment"
    ) + 
    labs(
      title = paste0("Effect of ", policy, " on ", outcome, "<br>for counties with minimum wage <span style='color:#0098e9;'>below</span> and <span style='color:#ff5ca8;'>above</span> the median<br>at t=0")
    ) +
    geom_hline(
      yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
    ) +
    geom_vline(
      xintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
    )
  
  state_plot <- state_plot + 
    theme_classic() + 
    theme(
      text = element_text(size = 16, family="serif"), plot.title = element_markdown(lineheight = 1.1)
    ) 
  
  return(state_plot)
  
}

county_plot <- function(df1,df2,window,policy,percentile,outcome){
  
  if (policy == "mop"){
    policy <- "Modern System PDMPs"
  } else if (policy == "pmq") {
    policy <- "Must Query PDMPs"
  } else {
    print("Policy must be either mop or pmq")
  }
  
  if (outcome == "unemployment rate"){
    df1 <- df1[,,1]
    df2 <- df2[,,1]
  } else if (outcome == "unemployment"){
    df1 <- df1[,,2]
    df2 <- df2[,,2]
  } else if (outcome == "employment"){
    df1 <- df1[,,3]
    df2 <- df2[,,3]
  } else if (outcome == "labor force"){
    df1 <- df1[,,4]
    df2 <- df2[,,4]
  } else {
    print("Outcome must be either unemployment rate, unemployment, employment or labor force")
  }
  
  plot_range <- -window:window
  
  effect_below_means <- colMeans(df1,na.rm=T) #makes sense? check
  
  effect_below_sd <- sapply(
    1:(2*window+1),
    function(x){
      sd(df1[,x], na.rm = T)/sqrt(length(df1[,x]))
    }
  )
  
  effect_above_means <- colMeans(df2,na.rm=T)
  
  effect_above_sd <- sapply(
    1:(2*window+1),
    function(x){
      sd(df2[,x],na.rm = T)/sqrt(length(df2[,x]))
    }
  )
  
  cty_plot_df <- as_tibble(
    cbind(plot_range,effect_below_means,effect_below_sd,effect_above_means,effect_above_sd)
  )
  
  names(cty_plot_df) <- c("plot_range",
                          "effect_below_means",
                          "effect_below_sd",
                          "effect_above_means",
                          "effect_above_sd")
  
  cty_plot_df <- cty_plot_df |>
    mutate(
      ci_left_below = cty_plot_df[["effect_below_means"]] - 1.96*cty_plot_df[["effect_below_sd"]]
    ) |>
    mutate(
      ci_right_below = cty_plot_df[["effect_below_means"]] + 1.96*cty_plot_df[["effect_below_sd"]]
    ) |>
    mutate(
      ci_left_above = cty_plot_df[["effect_above_means"]] - 1.96*cty_plot_df[["effect_above_sd"]]
    ) |>
    mutate(
      ci_right_above = cty_plot_df[["effect_above_means"]] + 1.96*cty_plot_df[["effect_above_sd"]]
    )
  
  cty_plot <- ggplot(cty_plot_df) + 
    geom_line(
      aes(x = plot_range, y = effect_below_means), color = "#0098e9", linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_left_below), color = "#0098e9", linetype = 2, linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_right_below), color = "#0098e9", linetype = 2, linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = effect_above_means), color = "#ff5ca8", linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_left_above), color = "#ff5ca8", linetype = 2,linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_right_above), color = "#ff5ca8", linetype = 2,linewidth = 1
    ) +
    ylab(
      paste0("Effect on ", outcome)
    ) +
    xlab(
      "Period relative to treatment"
    ) + 
    labs(
      title = paste0("Effect of ", policy, " on ", outcome, ",<br>for counties with Kaitz-", percentile ," index <span style='color:#0098e9;'>below</span> and <span style='color:#ff5ca8;'>above</span> the median<br>at t=0")
    ) +
    geom_hline(
      yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
    ) +
    geom_vline(
      xintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
    )
  
  cty_plot <- cty_plot + 
    theme_classic() + 
    theme(
      text = element_text(size = 16, family="serif"), plot.title = element_markdown(lineheight = 1.1)
    ) 
  
  return(cty_plot)
  
}



comparison_plot <- function(df1,df2,df3,df4,df5,window,policy,outcome){
  
  if (policy == "mop"){
    policy <- "Modern System PDMPs"
  } else if (policy == "pmq") {
    policy <- "Must Query PDMPs"
  } else {
    print("Policy must be either mop or pmq")
  }
  
  if (outcome == "unemployment rate"){
    df1 <- df1[,,1]
    df2 <- df2[,,1]
    df3 <- df3[,,1]
    df4 <- df4[,,1]
    df5 <- df5[,,1]
  } else if (outcome == "unemployment"){
    df1 <- df1[,,2]
    df2 <- df2[,,2]
    df3 <- df3[,,2]
    df4 <- df4[,,2]
    df5 <- df5[,,2]
  } else if (outcome == "employment"){
    df1 <- df1[,,3]
    df2 <- df2[,,3]
    df3 <- df3[,,3]
    df4 <- df4[,,3]
    df5 <- df5[,,3]
  } else if (outcome == "labor force"){
    df1 <- df1[,,4]
    df2 <- df2[,,4]
    df3 <- df3[,,4]
    df4 <- df4[,,4]
    df5 <- df5[,,4]
  } else {
    print("Outcome must be either unemployment rate, unemployment, employment or labor force")
  }
  
  plot_range <- -window:window
  
  effect_means_1 <- colMeans(df1,na.rm=T) 
  
  effect_sd_1 <- sapply(
    1:(2*window+1),
    function(x){
      sd(df1[,x], na.rm = T)/sqrt(length(df1[,x]))
    }
  )
  
  effect_means_2 <- colMeans(df2,na.rm=T) 
  
  effect_sd_2 <- sapply(
    1:(2*window+1),
    function(x){
      sd(df2[,x], na.rm = T)/sqrt(length(df2[,x]))
    }
  )
  
  effect_means_3 <- colMeans(df3,na.rm=T) 
  
  effect_sd_3 <- sapply(
    1:(2*window+1),
    function(x){
      sd(df3[,x], na.rm = T)/sqrt(length(df3[,x]))
    }
  )
  
  effect_means_4 <- colMeans(df4,na.rm=T) 
  
  effect_sd_4 <- sapply(
    1:(2*window+1),
    function(x){
      sd(df4[,x], na.rm = T)/sqrt(length(df4[,x]))
    }
  )
  
  effect_means_5 <- colMeans(df5,na.rm=T) 
  
  effect_sd_5 <- sapply(
    1:(2*window+1),
    function(x){
      sd(df5[,x], na.rm = T)/sqrt(length(df5[,x]))
    }
  )
  
  cty_plot_df <- as_tibble(
    cbind(plot_range,effect_means_1,effect_sd_1,effect_means_2,effect_sd_2,
          effect_means_3,effect_sd_3,effect_means_4,effect_sd_4,
          effect_means_5,effect_sd_5)
  )
  
  names(cty_plot_df) <- c("plot_range",
                          "effect_means_1",
                          "effect_sd_1",
                          "effect_means_2",
                          "effect_sd_2",
                          "effect_means_3",
                          "effect_sd_3",
                          "effect_means_4",
                          "effect_sd_4",
                          "effect_means_5",
                          "effect_sd_5")
  
  cty_plot_df <- cty_plot_df |>
    mutate(
      ci_left_1 = cty_plot_df[["effect_means_1"]] - 1.96*cty_plot_df[["effect_sd_1"]]
    ) |>
    mutate(
      ci_right_1 = cty_plot_df[["effect_means_1"]] + 1.96*cty_plot_df[["effect_sd_1"]]
    ) |>
    mutate(
      ci_left_2 = cty_plot_df[["effect_means_2"]] - 1.96*cty_plot_df[["effect_sd_2"]]
    ) |>
    mutate(
      ci_right_2 = cty_plot_df[["effect_means_2"]] + 1.96*cty_plot_df[["effect_sd_2"]]
    ) |>
    mutate(
      ci_left_3 = cty_plot_df[["effect_means_3"]] - 1.96*cty_plot_df[["effect_sd_3"]]
    ) |>
    mutate(
      ci_right_3 = cty_plot_df[["effect_means_3"]] + 1.96*cty_plot_df[["effect_sd_3"]]
    ) |>
    mutate(
      ci_left_4 = cty_plot_df[["effect_means_4"]] - 1.96*cty_plot_df[["effect_sd_4"]]
    ) |>
    mutate(
      ci_right_4 = cty_plot_df[["effect_means_4"]] + 1.96*cty_plot_df[["effect_sd_4"]]
    ) |> 
    mutate(
      ci_left_5 = cty_plot_df[["effect_means_5"]] - 1.96*cty_plot_df[["effect_sd_5"]]
    ) |>
    mutate(
      ci_right_5 = cty_plot_df[["effect_means_5"]] + 1.96*cty_plot_df[["effect_sd_5"]]
    )
  
  cty_plot <- ggplot(cty_plot_df) + 
    geom_line(
      aes(x = plot_range, y = effect_means_1), color = "#0098e9", linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_left_1), color = "#0098e9", linetype = 2, linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_right_1), color = "#0098e9", linetype = 2, linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = effect_means_2), color = "#ff5ca8", linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_left_2), color = "#ff5ca8", linetype = 2,linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_right_2), color = "#ff5ca8", linetype = 2,linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = effect_means_3), color = "#5aa800", linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_left_3), color = "#5aa800", linetype = 2,linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_right_3), color = "#5aa800", linetype = 2,linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = effect_means_4), color = "#f29318", linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_left_4), color = "#f29318", linetype = 2,linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_right_4), color = "#f29318", linetype = 2,linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = effect_means_5), color = "#B79F00", linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_left_5), color = "#B79F00", linetype = 2,linewidth = 1
    ) +
    geom_line(
      aes(x = plot_range, y = ci_right_5), color = "#B79F00", linetype = 2,linewidth = 1
    ) +
    ylab(
      paste0("Effect on ", outcome)
    ) +
    xlab(
      "Period relative to treatment"
    ) + 
    labs(
      title = paste0("Effect of ", policy, " on ", outcome, ",for different values<br>of the Kaitz index")
    ) +
    geom_hline(
      yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
    ) +
    geom_vline(
      xintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
    )
  
  cty_plot <- cty_plot + 
    theme_classic() + 
    theme(
      text = element_text(size = 16, family="serif"), plot.title = element_markdown(lineheight = 1.1)
    ) 
  
  return(cty_plot)
  
}

kaitz_densities_plot <- function(df){
  
  density_colors <- c("0.10" = "#0098e9",
                      "0.25" = "#ff5ca8",
                      "0.50" = "#5aa800",
                      "0.75" = "#f29318",
                      "0.90" = "#B79F00")
  
  densities_plot <- ggplot(df) +
    geom_density(
      aes(x = V1, ..count../sum(..count..), color = "0.10"), linewidth = 1
    ) +
    geom_density(
      aes(x = V2, ..count../sum(..count..), color = "0.25"), linewidth = 1
    ) + 
    geom_density(
      aes(x = V3, ..count../sum(..count..), color = "0.50"), linewidth = 1
    ) + 
    geom_density(
      aes(x = V4, ..count../sum(..count..), color = "0.75"), linewidth = 1
    ) + 
    geom_density(
      aes(x = V5, ..count../sum(..count..), color = "0.90"), linewidth = 1
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
      title = "Distribution of Kaitz-p indices across counties, for different percentiles"
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


joy_division_plot <- function(df1,df2,window,policy,percentile,outcome){
  
  if (policy == "mop"){
    policy <- "Modern System PDMPs"
  } else if (policy == "pmq") {
    policy <- "Must Query PDMPs"
  } else {
    print("Policy must be either mop or pmq")
  }
  
  if (outcome == "unemployment rate"){
    df1 <- df1[,,1]
    df2 <- df2[,,1]
    xlim_plot = xlim(-3,3)
  } else if (outcome == "unemployment"){
    df1 <- df1[,,2]
    df2 <- df2[,,2]
    xlim_plot = xlim(-500,1500)
  } else if (outcome == "employment"){
    df1 <- df1[,,3]
    df2 <- df2[,,3]
    xlim_plot = xlim(-1500,1000)
  } else if (outcome == "labor force"){
    df1 <- df1[,,4]
    df2 <- df2[,,4]
    xlim_plot = xlim(-1000,1000)
  } else {
    print("Outcome must be either unemployment rate, unemployment, employment or labor force")
  }
  
  plot_range <- -window:window
  
  joy_div_df_1 <- as_tibble(cbind(melt(df1),0))
  joy_div_df_2 <- as_tibble(cbind(melt(df2),1))
  
  names(joy_div_df_1) <- c("fips",
                           "range",
                           "value",
                           "group")
  
  names(joy_div_df_2) <- c("fips",
                           "range",
                           "value",
                           "group")
  
  joy_div_df <- rbind(joy_div_df_1,joy_div_df_2)
  
  joy_div_df[["range"]] <- joy_div_df[["range"]] - window - 1
  
  joy_div_plot <- ggplot(joy_div_df) +
    geom_density_ridges(data=joy_div_df[(joy_div_df$group==0)&(joy_div_df$range %in% seq(-window,window,4)),],aes(x = value, y = range, group = as.numeric(range)),color="#0098e9",fill=NA) + 
    geom_density_ridges(data=joy_div_df[(joy_div_df$group==1)&(joy_div_df$range %in% seq(-window,window,4)),],aes(x = value, y = range, group = as.numeric(range)),color="#ff5ca8",fill=NA) + 
    theme_ridges() +
    xlim_plot +
    coord_flip() +
    xlab(
      paste0("Effect on ", outcome)
    ) +
    ylab(
      "Period relative to treatment"
    ) +
    labs(
      title = paste0("Effect of ", policy, " on ", outcome, ",<br>for counties with Kaitz-", percentile ," index <span style='color:#0098e9;'>below</span> and <span style='color:#ff5ca8;'>above</span> the median<br>at t=0")
    ) +
    geom_hline(
      yintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
    ) +
    geom_vline(
      xintercept = 0, linetype = 5, color = "#000000", linewidth = 0.5
    )
  
  joy_div_plot <- joy_div_plot +
    theme_classic() +
    theme(
      text = element_text(size = 16, family="serif"), plot.title = element_markdown(lineheight = 1.1)
    )
  
  return(joy_div_plot)
  
}



