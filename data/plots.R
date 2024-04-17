
state_plot <- function(df1,df2,window,policy){
  
  if (policy == "mop"){
    policy <- "Modern System PDMPs"
  } else if (policy == "pmq") {
    policy <- "Must Query PDMPs"
  } else {
    print("Policy must be either mop or pmq")
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
      "Effect on unemployment rate"
    ) +
    xlab(
      "Period relative to treatment"
    ) + 
    labs(
      title = paste0("Effect of ", policy, " on unemployment rates,<br>for counties with minimum wage <span style='color:#0098e9;'>below</span> and <span style='color:#ff5ca8;'>above</span> the median<br>at t=0")
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

county_plot <- function(df1,df2,window,policy,percentile){
  
  if (policy == "mop"){
    policy <- "Modern System PDMPs"
  } else if (policy == "pmq") {
    policy <- "Must Query PDMPs"
  } else {
    print("Policy must be either mop or pmq")
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
      "Effect on unemployment rate"
    ) +
    xlab(
      "Period relative to treatment"
    ) + 
    labs(
      title = paste0("Effect of ", policy, " on unemployment rates,<br>for counties with Kaitz-", percentile ," index <span style='color:#0098e9;'>below</span> and <span style='color:#ff5ca8;'>above</span> the median<br>at t=0")
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

