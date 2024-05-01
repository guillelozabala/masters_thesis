
# This code returns the main estimates.
# 
# NOTE: 
# mop: modern system operational
# pmq: prescriber must query

rm(list=ls()) 
set.seed(123)

library(tidyverse)
#library(reshape2)
library(fixest)
library(ggtext)
#library(ggridges)
library(binsreg)

# Set the path
file_location <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_location)) # set path to location

# Auxiliary functions
source(paste(getwd(), "data/utilities.R", sep = "/"))
source(paste(getwd(), "data/plots.R", sep = "/"))

# Handy strings
outcomes <- c("unem_rate", "emp_rate","lab_force_rate")
policies <- c("mop","pmq")

# One-side length of effects considered
window = 24 # two years

# Load the data
data_location <- paste(
  getwd(),
  "data/joined_data.csv",
  sep = "/"
  )

df <- read.csv(
  data_location,
  header=TRUE,
  sep=","
  )
  
#df <- df[df$year >= 2003 & df$year <= 2016,]

# Prepare the covariates (as formula)
names_covar <- names(
  df[,grep("_ratio$", colnames(df))]
  )

names_covar <- c(names_covar,"scaled_model_values")

covariates <- paste(
  names_covar,
  collapse = " + "
  )

# Select not yet treated counties
not_yet_treated_mop <- df[df[["time_marker"]] < df$first_treatment_mop,] # mop
not_yet_treated_pmq <- df[df[["time_marker"]] < df$first_treatment_pmq,] # pmq

# First stage regressions, TWFE model:
# For each policy
for (policy in policies){
  # retrieve the not yet treated sample
  not_yet_treated_df = get(paste0("not_yet_treated_",policy)) 
  # and for each outcome 
  for (outcome in outcomes){
    # regress outcome on fixed effects and covariates,
    first_stage <- fixest::feols(
      stats::as.formula(
        paste0(outcome," ~ ", covariates, " |", "fips", " + ", "time_marker")
        ),
      data = not_yet_treated_df,
      combine.quick = FALSE,
      warn = FALSE,
      notes = FALSE
      )
    # then create a column with the predicted values (\hat{y})
    column_explained <- paste0(outcome, "_hat_", policy)
    df[[column_explained]] <- stats::predict(
      first_stage,
      newdata = df
      )
    # and a column with the unexplained part (y - \hat{y})
    column_unexplained <- paste0(outcome, "_tilde_", policy)
    df[[column_unexplained]] <- df[[outcome]] - df[[column_explained]] 
    }
  }

# # Drop never-takers for predictions
# df_mop <- df[!is.na(df$first_treatment_mop),]
# df_pmq <- df[!is.na(df$first_treatment_pmq),]

# Obtain the effects by county (see data/utilities.R)
effects_mop_county = CountyEffects(df = df, policy = policies[1], window = window)
effects_pmq_county = CountyEffects(df = df, policy = policies[2], window = window)

# Obtain the values of the Kaitz-p index at treatment (see data/utilities.R)
kaitz_at_treatment_mop = KaitzAtTreatment(df = df, policy = policies[1])
kaitz_at_treatment_pmq = KaitzAtTreatment(df = df, policy = policies[2])

# Plot the distributions of the index at treatment (see data/plots.R)
densities_plot_mop = KaitzDensitiesPlot(kaitz_at_treatment_mop)
densities_plot_pmq = KaitzDensitiesPlot(kaitz_at_treatment_pmq)

# Initialize the list of data frames
joint_dfs <- list()
for (outcome in outcomes){
  for (policy in policies){
    ind = which(outcomes == outcome)
    # Turn the matrices of results to data frames
    effects_county_df <- get(paste0("effects_",policy,"_county"))[,,ind] |>
      as.data.frame() |>
      setNames(
        c("fips",paste0("V",-24:24))
        )
    # Merge them with the Kaitz-p values by using identifiers
    joint_dfs[[paste0("effects_",policy,"_county_",outcome)]] <- merge(
      effects_county_df,
      get(paste0("kaitz_at_treatment_",policy)),
      by="fips"
      )
  }
}

# effects_mop_county[,,1] %>% as.data.frame() %>% setNames(c("fips",paste0("V",-24:24)))
# effects_pmq_county[,,1] %>% as.data.frame() %>% setNames(c("fips",paste0("V",-24:24)))





names(kaitz_wfips) <- c("kaitz_pct10_mop_matrix", 
                        "kaitz_pct25_mop_matrix", 
                        "kaitz_median_mop_matrix", 
                        "kaitz_pct75_mop_matrix", 
                        "kaitz_pct90_mop_matrix")

kaitz_df_names <- c("kaitz_pct10_mop_df_wfips", 
                    "kaitz_pct25_mop_df_wfips", 
                    "kaitz_median_mop_df_wfips", 
                    "kaitz_pct75_mop_df_wfips", 
                    "kaitz_pct90_mop_df_wfips")

for (kma in names(kaitz_wfips)){
  # Assign the matrix to its corresponding data frame
  assign(sub("_matrix$", "_df_wfips", kma), as.data.frame(kaitz_wfips[[kma]]))
  }

for (i in ((window+2):(2*window+1) - 26)){
  assign(paste0("tau_",i,"_unemp_rate"),cbind(effects_mop_state_wfips[,1,1], effects_mop_state_wfips[,window+2+i,1]))
  assign(paste0("tau_",i,"_emp_rate"),cbind(effects_mop_state_wfips[,1,1], effects_mop_state_wfips[,window+2+i,2]))
  assign(paste0("tau_",i,"_lab_force_rate"),cbind(effects_mop_state_wfips[,1,1], effects_mop_state_wfips[,window+2+i,3]))
}

tau_0_unemp_rate <- as.data.frame(tau_0_unemp_rate)
tau_5_unemp_rate <- as.data.frame(tau_1_unemp_rate)
tau_11_unemp_rate <- as.data.frame(tau_2_unemp_rate)
tau_23_unemp_rate <- as.data.frame(tau_3_unemp_rate)

tau_0_emp_rate <- as.data.frame(tau_0_emp_rate)
tau_5_emp_rate <- as.data.frame(tau_1_emp_rate)
tau_11_emp_rate <- as.data.frame(tau_2_emp_rate)
tau_23_emp_rate <- as.data.frame(tau_3_emp_rate)

tau_0_lab_force_rate <- as.data.frame(tau_0_lab_force_rate)
tau_5_lab_force_rate <- as.data.frame(tau_1_lab_force_rate)
tau_11_lab_force_rate <- as.data.frame(tau_2_lab_force_rate)
tau_23_lab_force_rate <- as.data.frame(tau_3_lab_force_rate)

names(tau_0_unemp_rate) <- c('fips','value')
names(tau_5_unemp_rate) <- c('fips','value')
names(tau_11_unemp_rate) <- c('fips','value')
names(tau_23_unemp_rate) <- c('fips','value')

names(tau_0_emp_rate) <- c('fips','value')
names(tau_5_emp_rate) <- c('fips','value')
names(tau_11_emp_rate) <- c('fips','value')
names(tau_23_emp_rate) <- c('fips','value')

names(tau_0_lab_force_rate) <- c('fips','value')
names(tau_5_lab_force_rate) <- c('fips','value')
names(tau_11_lab_force_rate) <- c('fips','value')
names(tau_23_lab_force_rate) <- c('fips','value')

kindex_0 <- cbind(kaitz_pct10_mop_df$V3,kaitz_pct10_mop_df$V5) #static kaitz (at treatment)
kindex_0 <- as.data.frame(kindex_0)
names(kindex_0) <- c('kindex','fips')

plot_taus_df_0_unemp_rate <- merge(tau_0_unemp_rate,kindex_0,by=c('fips'))
plot_taus_df_5_unemp_rate <- merge(tau_5_unemp_rate,kindex_0,by=c('fips'))
plot_taus_df_11_unemp_rate <- merge(tau_11_unemp_rate,kindex_0,by=c('fips'))
plot_taus_df_23_unemp_rate <- merge(tau_23_unemp_rate,kindex_0,by=c('fips'))

plot_taus_df_0_emp_rate <- merge(tau_0_emp_rate,kindex_0,by=c('fips'))
plot_taus_df_5_emp_rate <- merge(tau_5_emp_rate,kindex_0,by=c('fips'))
plot_taus_df_11_emp_rate <- merge(tau_11_emp_rate,kindex_0,by=c('fips'))
plot_taus_df_23_emp_rate <- merge(tau_23_emp_rate,kindex_0,by=c('fips'))

plot_taus_df_0_lab_force_rate <- merge(tau_0_lab_force_rate,kindex_0,by=c('fips'))
plot_taus_df_5_lab_force_rate <- merge(tau_5_lab_force_rate,kindex_0,by=c('fips'))
plot_taus_df_11_lab_force_rate <- merge(tau_11_lab_force_rate,kindex_0,by=c('fips'))
plot_taus_df_23_lab_force_rate <- merge(tau_23_lab_force_rate,kindex_0,by=c('fips'))

# plot_taus_0 <- ggplot(plot_taus_df_0) +
#   geom_point(aes(x=kindex,y=value),color = "deeppink")
# plot_taus_0 <- plot_taus_0 + theme_classic()
# plot_taus_0



binsreg(y=value,x=kindex,data=plot_taus_df_0_unemp_rate,ci=T)$data.plot$`Group Full Sample`

binscatter_df_0_unemp_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_0_unemp_rate,ci=T)$data.plot$`Group Full Sample`
plot_taus_0_unemp_rate_df <- binscatter_df_0_unemp_rate$data.dots
plot_taus_0_unemp_rate_df <- plot_taus_0_unemp_rate_df |>
  mutate(
    ci.l = binscatter_df_0_unemp_rate$data.ci$ci.l,
    ci.r = binscatter_df_0_unemp_rate$data.ci$ci.r
  )

plot_taus_0_unemp_rate <- ggplot(plot_taus_0_unemp_rate_df,aes(x=x,y=fit)) +
  geom_point(color = "deeppink") +
  geom_line(color = "deeppink") +
  geom_errorbar(aes(ymin = ci.l, ymax = ci.r),color = "deeppink") +
  ylim(-0.5,1.5)
plot_taus_0_unemp_rate <- plot_taus_0_unemp_rate + theme_classic()
plot_taus_0_unemp_rate

binscatter_df_5_unemp_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_5_unemp_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_5_unemp_rate <- ggplot(binscatter_df_5_unemp_rate) +
  geom_point(aes(x=x,y=fit),color = "deeppink") +
  geom_line(aes(x=x,y=fit),color = "deeppink") +
  ylim(-0.5,1.5)
plot_taus_5_unemp_rate <- plot_taus_5_unemp_rate + theme_classic()
plot_taus_5_unemp_rate

binscatter_df_11_unemp_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_11_unemp_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_11_unemp_rate <- ggplot(binscatter_df_11_unemp_rate) +
  geom_point(aes(x=x,y=fit),color = "deeppink") +
  geom_line(aes(x=x,y=fit),color = "deeppink") +
  ylim(-0.5,1.5)
plot_taus_11_unemp_rate <- plot_taus_11_unemp_rate + theme_classic()
plot_taus_11_unemp_rate

binscatter_df_23_unemp_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_23_unemp_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_23_unemp_rate <- ggplot(binscatter_df_23_unemp_rate) +
  geom_point(aes(x=x,y=fit),color = "deeppink") +
  geom_line(aes(x=x,y=fit),color = "deeppink") +
  ylim(-0.5,1.5)
plot_taus_23_unemp_rate <- plot_taus_23_unemp_rate + theme_classic()
plot_taus_23_unemp_rate


binscatter_df_0_emp_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_0_emp_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_0_emp_rate <- ggplot(binscatter_df_0_emp_rate) +
  geom_point(aes(x=x,y=fit),color = "skyblue") +
  geom_line(aes(x=x,y=fit),color = "skyblue") +
  ylim(-1.5,1.0)
plot_taus_0_emp_rate <- plot_taus_0_emp_rate + theme_classic()
plot_taus_0_emp_rate

binscatter_df_5_emp_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_5_emp_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_5_emp_rate <- ggplot(binscatter_df_5_emp_rate) +
  geom_point(aes(x=x,y=fit),color = "skyblue") +
  geom_line(aes(x=x,y=fit),color = "skyblue") +
  ylim(-1.5,1.0)
plot_taus_5_emp_rate <- plot_taus_5_emp_rate + theme_classic()
plot_taus_5_emp_rate

binscatter_df_11_emp_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_11_emp_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_11_emp_rate <- ggplot(binscatter_df_11_emp_rate) +
  geom_point(aes(x=x,y=fit),color = "skyblue") +
  geom_line(aes(x=x,y=fit),color = "skyblue") +
  ylim(-1.5,1.0)
plot_taus_11_emp_rate <- plot_taus_11_emp_rate + theme_classic()
plot_taus_11_emp_rate

binscatter_df_23_emp_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_23_emp_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_23_emp_rate <- ggplot(binscatter_df_23_emp_rate) +
  geom_point(aes(x=x,y=fit),color = "skyblue") +
  geom_line(aes(x=x,y=fit),color = "skyblue") +
  ylim(-1.5,1.0)
plot_taus_23_emp_rate <- plot_taus_23_emp_rate + theme_classic()
plot_taus_23_emp_rate


binscatter_df_0_lab_force_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_0_lab_force_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_0_lab_force_rate <- ggplot(binscatter_df_0_lab_force_rate) +
  geom_point(aes(x=x,y=fit),color = "purple") +
  geom_line(aes(x=x,y=fit),color = "purple") +
  ylim(-2.2,1.1)
plot_taus_0_lab_force_rate <- plot_taus_0_lab_force_rate + theme_classic()
plot_taus_0_lab_force_rate

binscatter_df_5_lab_force_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_5_lab_force_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_5_lab_force_rate <- ggplot(binscatter_df_5_lab_force_rate) +
  geom_point(aes(x=x,y=fit),color = "purple") +
  geom_line(aes(x=x,y=fit),color = "purple") +
  ylim(-2.2,1.1)
plot_taus_5_lab_force_rate <- plot_taus_5_lab_force_rate + theme_classic()
plot_taus_5_lab_force_rate

binscatter_df_11_lab_force_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_11_lab_force_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_11_lab_force_rate <- ggplot(binscatter_df_11_lab_force_rate) +
  geom_point(aes(x=x,y=fit),color = "purple") +
  geom_line(aes(x=x,y=fit),color = "purple") +
  ylim(-2.2,1.1)
plot_taus_11_lab_force_rate <- plot_taus_11_lab_force_rate + theme_classic()
plot_taus_11_lab_force_rate

binscatter_df_23_lab_force_rate <- binsreg(y=value,x=kindex,data=plot_taus_df_23_lab_force_rate)$data.plot$`Group Full Sample`$data.dots
plot_taus_23_lab_force_rate <- ggplot(binscatter_df_23_lab_force_rate) +
  geom_point(aes(x=x,y=fit),color = "purple") +
  geom_line(aes(x=x,y=fit),color = "purple") +
  ylim(-2.2,1.1)
plot_taus_23_lab_force_rate <- plot_taus_23_lab_force_rate + theme_classic()
plot_taus_23_lab_force_rate


library(gridExtra)
grid.arrange(grobs = list(plot_taus_0_unemp_rate,plot_taus_5_unemp_rate,plot_taus_11_unemp_rate,plot_taus_23_unemp_rate), nrow = 1)
grid.arrange(grobs = list(plot_taus_0_emp_rate,plot_taus_5_emp_rate,plot_taus_11_emp_rate,plot_taus_23_emp_rate), nrow = 1)
grid.arrange(grobs = list(plot_taus_0_lab_force_rate,plot_taus_5_lab_force_rate,plot_taus_11_lab_force_rate,plot_taus_23_lab_force_rate), nrow = 1)

grid.arrange(grobs = list(plot_taus_0_unemp_rate,plot_taus_5_unemp_rate,plot_taus_11_unemp_rate,plot_taus_23_unemp_rate,
                          plot_taus_0_emp_rate,plot_taus_5_emp_rate,plot_taus_11_emp_rate,plot_taus_23_emp_rate,
                          plot_taus_0_lab_force_rate,plot_taus_5_lab_force_rate,plot_taus_11_lab_force_rate,plot_taus_23_lab_force_rate), nrow = 3)


# binsreg(y=value,x=kindex,data=plot_taus_df_1)
# binsreg(y=value,x=kindex,data=plot_taus_df_2)
# binsreg(y=value,x=kindex,data=plot_taus_df_3)
# binsreg(y=value,x=kindex,data=plot_taus_df_4)
binsreg(y=value,x=kindex,data=plot_taus_df_5)

tau_11_unemp_rate <- as.data.frame(tau_11_unemp_rate)
names(tau_11_unemp_rate) <- c('fips','value')
plot_taus_df_11 <- merge(tau_11_unemp_rate,kindex_0,by=c('fips')) 
binsreg(y=value,x=kindex,data=plot_taus_df_11)

tau_23_unemp_rate <- as.data.frame(tau_23_unemp_rate)
names(tau_23_unemp_rate) <- c('fips','value')
plot_taus_df_23 <- merge(tau_23_unemp_rate,kindex_0,by=c('fips')) 
binsreg(y=value,x=kindex,data=plot_taus_df_23)
