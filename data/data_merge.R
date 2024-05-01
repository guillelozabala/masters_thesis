
# CLEAN, COMMENT

rm(list=ls()) 
set.seed(123)

library(tidyverse)

# Set path
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the data
unemp_rate <- read.csv(
  file.path(getwd(), "labor_market_outcomes/unemployment_rate.csv"), 
  header = TRUE,
  sep = ","
  )

# unemp <- read.csv(
#   file.path(getwd(), "labor_market_outcomes/unemployment.csv"),
#   header = TRUE,
#   sep = ","
#   )

emp_rate <- read.csv(
  file.path(getwd(), "labor_market_outcomes/employment_rate.csv"),
  header = TRUE,
  sep = ","
  )

lab_force <- read.csv(
  file.path(getwd(), "labor_market_outcomes/labor_force.csv"),
  header = TRUE,
  sep = ","
  )

horwitz <- read.csv(
  file.path(getwd(), "pdmps/horwitz2020.csv"),
  header = TRUE,
  sep = ","
  )

minwage <- read.csv(
  file.path(getwd(), "minimum_wage/minwage_clean_fed_ub.csv"),
  header = TRUE,
  sep = ","
  )

minw_bindgness <- read.csv(
  file.path(getwd(), "sector_wages/county_percentiles.csv"),
  header = TRUE,
  sep = ","
)

demographics <- read.csv(
  file.path(getwd(), "demographics/county_demographics.csv"),
  header = TRUE,
  sep = ","
  )

drug_deaths <- read.csv(
  file.path(getwd(), "drugs/NCHS_Drug_Poisoning_Mortality_by_County_United_States.csv"), 
  header = TRUE,
  sep = ","
  )

# PDMPs

## Divide months and years in PDMPs dataset
horwitz <- horwitz |> 
  separate_wider_delim(
    Enactment,
    delim = "-",
    names = c("enact_month", "enact_year")
    ) |> 
  separate_wider_delim(
    Enactment.funding.contingent,
    delim = "-",
    names = c("enact_fc_month", "enact_fc_year"),
    too_few = "align_start"
    ) |>
  separate_wider_delim(
    Enactment.electronic,
    delim = "-",
    names = c("enact_e_month", "enact_e_year"),
    too_few = "align_start"
    ) |> 
  separate_wider_delim(
    Modern.system.operational,
    delim = "-",
    names = c("mop_month", "mop_year"),
    too_few = "align_start"
    ) |> 
  separate_wider_delim(
    Prescriber.must.query,
    delim = "-",
    names = c("pmq_month", "pmq_year"),
    too_few = "align_start"
    ) |> 
  mutate_if(
    is.character,
    list(~na_if(.,""))
    ) 

## Numeric values
horwitz$enact_month <- replace(
  horwitz$enact_month,
  horwitz$enact_month == "Pre",
  "0"
  )

horwitz[,2:length(horwitz)] <- sapply(
  horwitz[,2:length(horwitz)],
  as.numeric
  )

## Select the policies
PDMPs_columns <- c("state", "pmq_year", "pmq_month", "mop_year", "mop_month")
horwitz <- horwitz[PDMPs_columns]

## Add time markers
horwitz <- horwitz |> 
  mutate(
    first_treatment_pmq = (pmq_year-1960)*12 + pmq_month
    ) |>
  mutate(
    first_treatment_mop = (mop_year-1960)*12 + mop_month
    ) 

# labor market data

## Rename, time marker, select and numeric values

labor_market <- list(unemp_rate,emp_rate,lab_force)

clean_lab_market_data <- function(df){
  
  df <- df |> 
    rename(
      state = state_name,
      county = county_name,
      rate = value
    ) |>
    mutate(
      time_marker = (year-1960)*12 + month
    )
  
  df <- df[,colnames(df) != "state_abbr"]
  
  df$rate <- as.numeric(df$rate)
  
  return(df)
}

labor_market_clean <- purrr::map(
  labor_market,
  clean_lab_market_data
  )

names(labor_market_clean) <- c("unemp_rate",
                               "emp_rate",
                               "lab_force")

# Minimum Wage

## Rename, drop federal observations

minwage <- minwage |> 
  rename(
    year = Year,
    state = State.or.otherjurisdiction,
    minw = Value
    )

minwage <- minwage[minwage$state != 'Federal (FLSA)',]

#no changes in 1999
missing_minw <- minwage[minwage$year == 1998,] |> 
  mutate(year = 1999)

minwage <- rbind(
  minwage,
  missing_minw
  ) 

# Demographics

## Select the ratios and population only

demographics <- cbind(
  demographics[,1:3],
  demographics[, grep("_ratio$", colnames(demographics))]
  )

demographics[['working_age_pop_weight']] <- demographics[['age5_population_ratio']] +
  demographics[['age6_population_ratio']] + demographics[['age7_population_ratio']] +
  demographics[['age8_population_ratio']] + demographics[['age9_population_ratio']] +
  demographics[['age10_population_ratio']] + demographics[['age11_population_ratio']] +
  demographics[['age12_population_ratio']] + demographics[['age13_population_ratio']]

demographics[['working_age_pop']] <- demographics[['working_age_pop_weight']]*demographics[['population']]

# Drugs

## Select the scaled predictions only

drug_deaths_scaled <- drug_deaths[F,]
for (i in unique(drug_deaths$Year)){
  drug_deaths_y <- drug_deaths[drug_deaths['Year'] == i,]
  drug_deaths_y <- drug_deaths_y |>
    mutate(
      scaled_model_values = scale(Model.based.Death.Rate)
      )
  drug_deaths_scaled <- rbind(drug_deaths_scaled,drug_deaths_y)
  }

drug_deaths_scaled <- drug_deaths_scaled |>
  rename(
    fips = FIPS,
    year = Year,
    model_values = Model.based.Death.Rate
    )

overdose_columns <- c("fips", "year", "model_values", "scaled_model_values")
drug_deaths_scaled <- drug_deaths_scaled[,overdose_columns]

# Merging

df <- merge(
  rename(
    labor_market_clean$unemp_rate,
    unem_rate = rate
    ),
  rename(
    labor_market_clean$emp_rate,
    emp_rate = rate
    ),
  by = c(
    "year",
    "fips",
    "month",
    "county",
    "state",
    "time_marker"
    )
) 


df <- merge(
  df,
  rename(
    labor_market_clean$lab_force,
    lab_force = rate
    ),
  by = c(
    "year",
    "fips",
    "month",
    "county",
    "state",
    "time_marker"
    )
  ) 

df <- merge(
  df,
  demographics,
  by = c(
    "year",
    "fips"
    )
  )

df <- merge(
  df,
  drug_deaths_scaled,
  by = c(
    "year",
    "fips"
    ),
  all = T
  )

df <- merge(
  df,
  horwitz,
  by = "state"
  )

df <- merge(
  df,
  minwage,
  by = c(
    "state",
    "year"
    )
  )

df <- merge(
  df,
  minw_bindgness,
  by = c(
    "fips",
    "year"
  ),
  all = T
  )

df <- df |>
  mutate(
    lab_force_rate = round((lab_force/working_age_pop)*100, 1)
  )

# take logs
df <- df |>
  mutate(
    log_minw = log(minw)
  ) |>
  mutate(
    log_h_pct10 = log(h_pct10),
    log_h_pct25 = log(h_pct25),
    log_h_median = log(h_median),
    log_h_pct75 = log(h_pct75),
    log_h_pct90 = log(h_pct90)
  )

# kaitz-p indices
df <- df |>
  mutate(
    kaitz_pct10 = log_minw - log(h_pct10),
    kaitz_pct25 = log_minw - log(h_pct25),
    kaitz_median = log_minw - log(h_median),
    kaitz_pct75 = log_minw - log(h_pct75),
    kaitz_pct90 = log_minw - log(h_pct90)
  )

df <- df[df$year >= 1998 & df$year <= 2019,]

# DC (one county) has duplicated values
df_dc <- df[df["fips"] == 11001,]
df_dc <- df_dc[!duplicated(df_dc),]
df <- df[!(df["fips"] == 11001),]
df <- rbind(df,df_dc)

# df <- df |>
#   mutate(
#     indicator_pmq = 1*(time_marker >= first_treatment_pmq)
#     ) |>
#   mutate(
#     indicator_mop = 1*(time_marker >= first_treatment_mop)
#     )

write.csv(
  df, 
  paste(getwd(),"joined_data.csv",sep = "/"),
  row.names = F
  )
