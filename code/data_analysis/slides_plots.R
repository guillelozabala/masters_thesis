
rm(list=ls()) 

library(tidyverse)
library(extrafont)
library(gridExtra)

#Install and initialize the fonts
font_install('fontcm')
extrafont::loadfonts(device="win")

# Set the path
file_location <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_location)) #set path to location

# Load the data
minwg <- read.csv(
  paste(getwd(),"minimum_wage/minwage_clean_fed_ub.csv",sep = "/"),
  header=TRUE,
  sep=","
  )

ODD <- read.csv(
  paste(getwd(),"drugs/OD_deaths_educ.csv",sep = "/"),
  header=TRUE,
  sep=","
  )

# Transform the data
minwg <- as_tibble(minwg)

minwg <- minwg |> 
  rename(year = Year) |>
  rename(state = State.or.otherjurisdiction) |>
  rename(minw = Value)

minwg <- minwg[minwg$state != 'Federal (FLSA)',]

minwg <- minwg[minwg$year != 2023,]

missing_minw <- minwg[minwg$year == 1998,] |> mutate(year = 1999)

minwg <- rbind(minwg,missing_minw) 

ODD$perc_coll_educ <- gsub(',', '.', ODD$perc_coll_educ)

ODD$perc_coll_educ <- as.numeric(ODD$perc_coll_educ)

ODD <- ODD |>
  mutate(
    total_col_pop = total_pop*(perc_coll_educ/100)
    ) |>
  mutate(
    total_non_col_pop = total_pop*(1 - perc_coll_educ/100)
    ) |>
  mutate(
    Any_Non_Coll_pc = Any_Non_Coll/total_non_col_pop*100000
    ) |>
  mutate(
    Heroin_Non_Coll_pc = Heroin_Non_Coll/total_non_col_pop*100000
    ) |>
  mutate(
    Prescription_Non_Coll_pc = Prescription_Non_Coll/total_non_col_pop*100000
    ) |>
  mutate(
    Synthetic_Non_Coll_pc = Synthetic_Non_Coll/total_non_col_pop*100000
    ) |>
  mutate(
    Any_Coll_pc = Any_Coll/total_col_pop*100000
    ) |>
  mutate(
    Heroin_Coll_pc = Heroin_Coll/total_col_pop*100000
    ) |>
  mutate(
    Prescription_Coll_pc = Prescription_Coll/total_col_pop*100000
    ) |>
  mutate(
    Synthetic_Coll_pc = Synthetic_Coll/total_col_pop*100000
    )

# Plot 1: overdose deaths over time
ODD_colors <- c("All" = "#0098e9",
                "Heroin" = "#ff5ca8",
                "Prescription" = "#5aa800",
                "Synthetic" = "#f29318")

non_coll_plot <- ggplot(ODD) + 
  geom_line(
    aes(x = year, y = Any_Non_Coll_pc, color = "All"), linewidth = 1
    ) +
  geom_line(
    aes(x = year, y = Heroin_Non_Coll_pc, color = "Heroin"), linewidth = 1, linetype = 1
    ) +
  geom_line(
    aes(x = year, y = Prescription_Non_Coll_pc, color = "Prescription"), linewidth = 1, linetype = 2
    ) +
  geom_line(
    aes(x = year, y = Synthetic_Non_Coll_pc, color = "Synthetic"), linewidth = 1, linetype = 4
    ) +
  scale_color_manual(
    name = element_blank(), values = ODD_colors
    ) +
  ylab(
    "Opioid deaths"
    ) + 
  xlab(
    ""
    ) + 
  labs(
    title = "Non-college"
    )

non_coll_plot <- non_coll_plot + 
  theme_classic() +
  theme(text = element_text(size = 16, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.3, 0.8)
        )

coll_plot <- ggplot(ODD) + 
  geom_line(
    aes(x = year, y = Any_Coll_pc, color = "All"), linewidth = 1
    ) +
  geom_line(
    aes(x = year, y = Heroin_Coll_pc, color="Heroin"), linewidth = 1, linetype = 1
    ) +
  geom_line(
    aes(x = year, y = Prescription_Coll_pc, color = "Prescription"), linewidth = 1, linetype = 2
    ) +
  geom_line(
    aes(x = year, y = Synthetic_Coll_pc, color = "Synthetic"), linewidth = 1,linetype = 4
    ) +
  scale_color_manual(
    name = element_blank(), values = ODD_colors
    ) + 
  ylim(0,20) +
  ylab(
    "Opioid deaths"
    ) +
  xlab(
    ""
    )  +
  labs(
    title="College"
    )

coll_plot <- coll_plot + 
  theme_classic() +
  theme(text = element_text(size = 16, family = "serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.3, 0.8)
        )

ODDplot <- grid.arrange(non_coll_plot, coll_plot, ncol=2)
ODDplot

# Plot 2: minimum wage rates over time
minw_plot <- ggplot(minwg) +
  geom_line(
    aes(year, minw, group = state, colour = state)
    ) + 
  geom_point(
    aes(year, minw, colour = state)
    ) +
  geom_text(
    label = "DC", x = 2018, y = 15, color = "#B79F00", size = 6, family = "serif"
    ) + 
  geom_text(
    label = "Federal rate", x = 2016, y = 6.5, color = "#F8766D", size = 6, family = "serif"
    ) +
  xlab(
    ""
    ) +
  ylab(
    "Effective Minimum Wage ($/hr.)"
    )

minw_plot <- minw_plot + 
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 16, family = "serif")
        )

minw_plot

# Plot 3: minimum wage rates over time (simplified)
minw_simple <- minwg[(minwg$state == "Alabama") | 
                       (minwg$state == "District of Columbia") | 
                       (minwg$state == "Minnesota"),]

minw_plot_simple <- ggplot(minw_simple) +
  geom_line(
    aes(year, minw, group = state, colour = state), linewidth = 1
    ) + 
  geom_point(
    aes(year, minw, colour = state, shape = state), size = 2.5
    ) +
  geom_text(
    label = "DC", x = 2018, y = 15, color = "#00BA38", size = 5, family = "serif"
    ) + 
  geom_text(
    label = "Federal", x = 2016, y = 6.5, color = "#F8766D", size = 5, family = "serif"
    ) +
  geom_text(
    label = "Minnesota", x = 2019, y = 11, color = "#619CFF", size = 5, family= "serif"
    ) +
  xlab(
    ""
    ) +
  ylab(
    "Effective Minimum Wage ($/hr.)"
    )

minw_plot_simple <- minw_plot_simple + 
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 16, family = "serif")
        )

minw_plot_simple


