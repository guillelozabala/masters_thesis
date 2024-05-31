
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
library(binsreg)
library(gridExtra)
library(grid)

# Set the path
file_location <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_location)) # set path to location

# Auxiliary functions
source(paste(getwd(), "data/utilities.R", sep = "/"))
source(paste(getwd(), "data/plots.R", sep = "/"))

percentiles <- c("0.10", "0.25", "0.50", "0.75", "0.90")

# Load the data
data_location <- paste(
  getwd(),
  "data/joined_data_mop.csv",
  sep = "/"
  )

df <- read.csv(
  data_location,
  header=TRUE,
  sep=","
  )

# Prepare the covariates (as formula)
names_covar <- names(
  df[,c(grep("_ratio$", colnames(df)),
    grep("scaled_model_values", colnames(df)))]
  )

covariates <- paste(
  names_covar,
  collapse = " + "
  )

df[['relative_to_treat_mop']] = df[['time_marker']] - df[['first_treatment_mop']]
names(df)[names(df) == 'kaitz_median'] <- 'kaitz_pct50'

#df[['relative_to_treat_pmq']] = df[['time_marker']] - df[['first_treatment_pmq']]

unem_rate_mop_effects  <- df |>
  IndEffects(yname = "unem_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_mop",
             aname = "first_treatment_mop",
             covariates = covariates,
             only_full_horizon = FALSE)

emp_rate_mop_effects  <- df |>
  IndEffects(yname = "emp_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_mop",
             aname = "first_treatment_mop",
             covariates = covariates,
             only_full_horizon = FALSE)

lab_force_rate_mop_effects  <- df |>
  IndEffects(yname = "lab_force_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_mop",
             aname = "first_treatment_mop",
             covariates = covariates,
             only_full_horizon = FALSE)

png(filename = paste(getwd(),"slides/mop10_unemp_rate.png",sep = "/"),width = 600, height = 539)
IndividualEffectsPlot(unem_rate_mop_effects$df_indcp,c(1,6,12,24),percentiles[1])
dev.off() 

png(filename = paste(getwd(),"slides/mop10_emp_rate.png",sep = "/"),width = 600, height = 539)
IndividualEffectsPlot(emp_rate_mop_effects$df_indcp,c(1,6,12,24),percentiles[1])
dev.off() 

png(filename = paste(getwd(),"slides/mop10_lab_force_rate.png",sep = "/"),width = 600, height = 539)
IndividualEffectsPlot(lab_force_rate_mop_effects$df_indcp,c(1,6,12,24),percentiles[1])
dev.off() 

png(filename = paste(getwd(),"slides/mop10_unemp_rate_comp_t1.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(unem_rate_mop_effects$df_indcp,1)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_emp_rate_comp_t1.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(emp_rate_mop_effects$df_indcp,1)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_lab_force_rate_comp_t1.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(lab_force_rate_mop_effects$df_indcp,1)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_unemp_rate_comp_t24.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(unem_rate_mop_effects$df_indcp,24)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_emp_rate_comp_t24.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(emp_rate_mop_effects$df_indcp,24)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_lab_force_rate_comp_t24.png",sep = "/"),width = 600, height = 539)
IndividualEffectsCompPlot(lab_force_rate_mop_effects$df_indcp,24)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_unemp_rate_average.png",sep = "/"),width = 600, height = 539)
TimeAveragesPlot(unem_rate_mop_effects$df_indcp,percentiles[1],24)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_emp_rate_average.png",sep = "/"),width = 600, height = 539)
TimeAveragesPlot(emp_rate_mop_effects$df_indcp,percentiles[1],24)
dev.off() 

png(filename = paste(getwd(),"slides/mop10_lab_force_rate_average.png",sep = "/"),width = 600, height = 539)
TimeAveragesPlot(lab_force_rate_mop_effects$df_indcp,percentiles[1],24)
dev.off() 

png(filename = paste(getwd(),"slides/kaitz_percentiles.png",sep = "/"),width = 600, height = 539)
KaitzDensitiesPlot(unem_rate_mop_effects$df_indcp)
dev.off()


# Load the data
data_location_pmq <- paste(
  getwd(),
  "data/joined_data_pmq.csv",
  sep = "/"
  )

df_pmq <- read.csv(
  data_location_pmq,
  header=TRUE,
  sep=","
  )

# Prepare the covariates (as formula)
names_covar_pmq <- names(
  df_pmq[,c(grep("_ratio$", colnames(df_pmq)),
        grep("scaled_model_values", colnames(df_pmq)))]
  )

covariates_pmq <- paste(
  names_covar_pmq,
  collapse = " + "
  )

df_pmq[['relative_to_treat_pmq']] = df_pmq[['time_marker']] - df_pmq[['first_treatment_pmq']]
names(df_pmq)[names(df_pmq) == 'kaitz_median'] <- 'kaitz_pct50'

unem_rate_pmq_effects  <- df_pmq |>
  IndEffects(yname = "unem_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq,
             only_full_horizon = FALSE)

IndividualEffectsPlot(unem_rate_pmq_effects$df_indcp,c(1,6,12,24),percentiles[1])

emp_rate_pmq_effects  <- df_pmq |>
  IndEffects(yname = "emp_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq,
             only_full_horizon = FALSE)

IndividualEffectsPlot(emp_rate_pmq_effects$df_indcp,c(1,6,12,24),percentiles[1])

lab_force_rate_pmq_effects  <- df_pmq |>
  IndEffects(yname = "lab_force_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_pmq,
             only_full_horizon = FALSE)

IndividualEffectsPlot(lab_force_rate_pmq_effects$df_indcp,c(1,6,12,24),percentiles[1])

# Reformulation

df[['reformulation_date']] = (2010 - 1960)*12 + 4 # April 4, 2010
df[['relative_to_refor']] = df[['time_marker']] - df[['reformulation_date']]

unem_rate_refor_effects  <- df |>
  IndEffects(yname = "unem_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_refor",
             aname = "reformulation_date",
             covariates = covariates,
             only_full_horizon = FALSE)

emp_rate_refor_effects  <- df |>
  IndEffects(yname = "emp_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_refor",
             aname = "reformulation_date",
             covariates = covariates,
             only_full_horizon = FALSE)

lab_force_rate_refor_effects  <- df |>
  IndEffects(yname = "lab_force_rate",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_refor",
             aname = "reformulation_date",
             covariates = covariates,
             only_full_horizon = FALSE)

IndividualEffectsPlot_Oxy(unem_rate_refor_effects$df_indcp,c(1,6,12,24),percentiles[1])
IndividualEffectsPlot(emp_rate_refor_effects$df_indcp,c(1,6,12,24),percentiles[1])
IndividualEffectsPlot(lab_force_rate_refor_effects$df_indcp,c(1,6,12,24),percentiles[1])

prueba <- unem_rate_refor_effects$df_indcp
prueba[prueba[["relative_to_refor"]] == 1,]

prueba2 <- unem_rate_mop_effects$df_indcp
prueba2[prueba2[["relative_to_treat_mop"]] == 1,]

# Load the data
data_location_mortality <- paste(
  getwd(),
  "data/opioid_deaths_yearly.csv",
  sep = "/"
  )

df_mortality <- read.csv(
  data_location_mortality,
  header=TRUE,
  sep=","
  )

names_covar_mortality <- names(
  df_mortality[,grep("_ratio$", colnames(df_mortality))]
  )

covariates_mortality <- paste(
  names_covar_mortality,
  collapse = " + "
  )

df_mortality[['relative_to_treat_mop']] = df_mortality[['year']] - df_mortality[['mop_year']]

mortality_mop_effects  <- df_mortality |>
  IndEffects(yname = "deaths",
             iname = "state_code",
             tname = "year",
             kname = "relative_to_treat_mop",
             aname = "mop_year",
             covariates = covariates_mortality,
             only_full_horizon = FALSE)

df_year <- df[df[['month']] == 1, ]
df_year_kaitz <- df_year[,c('year','state','fips','kaitz_pct10')]
df_year_kaitz <- df_year_kaitz |>
  group_by(year,state) |>
  summarize(kaitz_pct10 = mean(kaitz_pct10, na.rm = TRUE))

df_mort_merged <- merge(mortality_mop_effects$df_indcp, df_year_kaitz, by = c('year','state'))
df_mort_merged <- df_mort_merged[(df_mort_merged[['year']] >= 2003) & (df_mort_merged[['year']] <= 2016), ]
binsreg(y = deaths_tilde,
        x = kaitz_pct10,
        data = df_mort_merged[!is.infinite(df_mort_merged[['kaitz_pct10']]),], 
        ci= T)



# Load the data
file_location <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_location)) # set path to location

data_location_prescriptions_mop <- paste(
  getwd(),
  "data/prescription_monthly_mop.csv",
  sep = "/"
  )

df_prescriptions_mop <- read.csv(
  data_location_prescriptions_mop,
  header=TRUE,
  sep=","
  )

names_covar_prescriptions_mop <- names(
  df_prescriptions_mop[,grep("_ratio$", colnames(df_prescriptions_mop))]
  )

covariates_prescriptions_mop <- paste(
  names_covar_prescriptions_mop,
  collapse = " + "
  )

df_prescriptions_mop[['relative_to_treat_mop']] = df_prescriptions_mop[['year']] - df_prescriptions_mop[['mop_year']]

df_prescriptions_mop |> head()

prescriptions_mop_effects  <- df_prescriptions_mop |>
  IndEffects(yname = "dosage_unit_pc",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_mop",
             aname = "first_treatment_mop",
             covariates = covariates_prescriptions_mop,
             only_full_horizon = FALSE)

binsreg(y = dosage_unit_pc_tilde,
        x = kaitz_pct10,
        data = prescriptions_mop_effects$df_indcp, 
        ci= T)


data_location_prescriptions_pmq <- paste(
  getwd(),
  "data/prescription_monthly_pmq.csv",
  sep = "/"
  )

df_prescriptions_pmq <- read.csv(
  data_location_prescriptions_pmq,
  header=TRUE,
  sep=","
  )

names_covar_prescriptions_pmq <- names(
  df_prescriptions_pmq[,grep("_ratio$", colnames(df_prescriptions_pmq))]
  )

covariates_prescriptions_pmq <- paste(
  names_covar_prescriptions_pmq,
  collapse = " + "
  )

df_prescriptions_pmq[['relative_to_treat_pmq']] = df_prescriptions_pmq[['year']] - df_prescriptions_pmq[['pmq_year']]

prescriptions_pmq_effects  <- df_prescriptions_pmq |>
  IndEffects(yname = "dosage_unit_pc",
             iname = "fips",
             tname = "time_marker",
             kname = "relative_to_treat_pmq",
             aname = "first_treatment_pmq",
             covariates = covariates_prescriptions_pmq,
             only_full_horizon = FALSE)

binsreg(y = dosage_unit_pc_tilde,
        x = kaitz_pct10,
        data = prescriptions_pmq_effects$df_indcp, 
        ci= T)

mop_main_plot <- IndividualEffectsPlot(unem_rate_mop_effects$df_indcp,c(1,6,12,24),percentiles[1],T)
mop_prescription_plot <- IndividualEffectsPlotPrescriptions(prescriptions_mop_effects$df_indcp,percentiles[1])
pmq_main_plot <- IndividualEffectsPlot(unem_rate_pmq_effects$df_indcp,c(1,6,12,24),percentiles[1],T)
pmq_prescription_plot <- IndividualEffectsPlotPrescriptions(prescriptions_pmq_effects$df_indcp,percentiles[1])

grid.arrange(grobs = list(mop_main_plot,mop_prescription_plot),nrow = 2)
grid.arrange(grobs = list(pmq_main_plot,pmq_prescription_plot),nrow = 2)

# prescriptions fall -> heroin?