
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

naics <- read.csv(
  file.path(getwd(), "sector_composition/naics_shares_merged.csv"),
  header = TRUE,
  sep = ","
  )

naics <- naics[,!(names(naics) %in% 'emp')]

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

horwitz_mop <- horwitz[c('state','mop_year','mop_month','first_treatment_mop')]
horwitz_pmq <- horwitz[c('state','pmq_year','pmq_month','first_treatment_pmq')]

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

df_mop <- merge(
  df,
  horwitz_mop,
  by = "state"
  )

df_pmq <- merge(
  df,
  horwitz_pmq,
  by = "state"
  )

df <- merge(
  df,
  horwitz,
  by = "state"
  )

df_mop <- merge(
  df_mop,
  naics,
  by = c(
    "year",
    "fips"
  ),
  all = T
  )

df_pmq <- merge(
  df_pmq,
  naics,
  by = c(
    "year",
    "fips"
  ),
  all = T
)

df <- merge(
  df,
  naics,
  by = c(
    "year",
    "fips"
  ),
  all = T
)

write.csv(
  df_mop,
  paste(getwd(),"joined_data_mop.csv",sep = "/"),
  row.names = F
  )

write.csv(
  df_pmq,
  paste(getwd(),"joined_data_pmq.csv",sep = "/"),
  row.names = F
  )

write.csv(
  df,
  paste(getwd(),"joined_data.csv",sep = "/"),
  row.names = F
  )



opioid_deaths_yearly <- read.delim(
  file.path(getwd(), "drugs/Multiple Cause of Death, 1999-2020.txt")
  )

opioid_deaths_yearly <- opioid_deaths_yearly[,c('State','State.Code','Year','Multiple.Cause.of.death.Code','Deaths')] 

opioid_deaths_yearly <- opioid_deaths_yearly |>
  group_by(State, State.Code, Year) |>
  summarize(Deaths = sum(Deaths, na.rm = TRUE))

opioid_deaths_yearly <- opioid_deaths_yearly[2:nrow(opioid_deaths_yearly),]

opioid_deaths_yearly <- opioid_deaths_yearly |>
  rename(
    state = State,
    state_code = State.Code,
    year = Year,
    deaths = Deaths
    )

opioid_deaths_yearly <- merge(
  opioid_deaths_yearly,
  horwitz_mop,
  by = 'state',
  all = T
  )

demographics_states <- read.csv(
  file.path(getwd(), "demographics/county_demographics_states.csv"),
  header = TRUE,
  sep = ","
  )

demographics_states <- cbind(
  demographics_states[,1:3],
  demographics_states[, grep("_ratio$", colnames(demographics_states))]
)

demographics_states[['working_age_pop_weight']] <- demographics_states[['age5_population_ratio']] +
  demographics_states[['age6_population_ratio']] + demographics_states[['age7_population_ratio']] +
  demographics_states[['age8_population_ratio']] + demographics_states[['age9_population_ratio']] +
  demographics_states[['age10_population_ratio']] + demographics_states[['age11_population_ratio']] +
  demographics_states[['age12_population_ratio']] + demographics_states[['age13_population_ratio']]

demographics_states[['working_age_pop']] <- demographics_states[['working_age_pop_weight']]*demographics_states[['population']]

demographics_states <- demographics_states |>
  rename(
    state_code = state_fip
    )

opioid_deaths_yearly <- merge(
  opioid_deaths_yearly,
  demographics_states,
  by = c('state_code','year'),
  all = T
  )

write.csv(
  opioid_deaths_yearly,
  paste(getwd(),"opioid_deaths_yearly.csv",sep = "/"),
  row.names = F
  )

# PRESCRIPTIONS
buyer_monthly <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(buyer_monthly) <- c('buyer_county','buyer_state','year','month','dosage_unit')
for (year in 2006:2014){
  buyer_monthly_year <- read.csv(
    file.path(getwd(), paste0("drugs/buyer_monthly",year,".csv")),
    header = TRUE,
    sep = ","
    )
  buyer_monthly_year <- buyer_monthly_year[,c('BUYER_COUNTY','BUYER_STATE','year','month','DOSAGE_UNIT')]
  buyer_monthly_year <- buyer_monthly_year |>
    rename(
      buyer_county = BUYER_COUNTY,
      buyer_state = BUYER_STATE,
      dosage_unit = DOSAGE_UNIT 
      )
  buyer_monthly_year <- buyer_monthly_year |>
    group_by(buyer_county, buyer_state, year, month) |>
    summarize(dosage_unit = sum(dosage_unit, na.rm = TRUE))
  
  buyer_monthly <- rbind(buyer_monthly,buyer_monthly_year)
  }

county_fips_prescriptions <- read.csv(
  file.path(getwd(), paste0("drugs/county_fips.csv")),
  header = TRUE,
  sep = ","
  )

county_fips_prescriptions <- county_fips_prescriptions |>
  rename(
    buyer_county = BUYER_COUNTY,
    buyer_state = BUYER_STATE,
    fips = countyfips 
    )

buyer_monthly <- merge(
  buyer_monthly,
  county_fips_prescriptions,
  by = c('buyer_state','buyer_county'),
  all = T
  )

# buyer_monthly |> head()

states_prescriptions <- read.csv(
  file.path(getwd(), paste0("drugs/states.csv")),
  header = TRUE,
  sep = ","
  )

states_prescriptions <- states_prescriptions |>
  rename(
    state = State,
    buyer_state = Abbreviation
    )

#states_prescriptions |> head()

buyer_monthly <- merge(
  buyer_monthly,
  states_prescriptions,
  by = 'buyer_state'
  )

buyer_monthly <- buyer_monthly |> 
  drop_na()

buyer_monthly <- buyer_monthly |>
  mutate(
    time_marker = (year-1960)*12 + month
    )

buyer_monthly <- merge(
  buyer_monthly,
  demographics[(demographics[['year']] >= 2006) & (demographics[['year']] <= 2014),],
  by = c('year','fips')
  )

buyer_monthly[['dosage_unit_pc']] <- round(buyer_monthly[['dosage_unit']]/buyer_monthly[['population']], 3)

buyer_monthly |> head()

buyer_monthly_mop <- merge(
  buyer_monthly,
  horwitz_mop,
  by = 'state'
  )

buyer_monthly_pmq <- merge(
  buyer_monthly,
  horwitz_pmq,
  by = 'state'
  )

buyer_monthly_mop <- merge(
  buyer_monthly_mop,
  df_mop[,c('fips','time_marker','kaitz_pct10','kaitz_pct25','kaitz_median','kaitz_pct75','kaitz_pct90')],
  by = c('time_marker','fips')
  )

buyer_monthly_pmq <- merge(
  buyer_monthly_pmq,
  df_pmq[,c('fips','time_marker','kaitz_pct10','kaitz_pct25','kaitz_median','kaitz_pct75','kaitz_pct90')],
  by = c('time_marker','fips')
  )

buyer_monthly_mop |> head()
buyer_monthly_pmq |> head()

write.csv(
  buyer_monthly_mop,
  paste(getwd(),"prescription_monthly_mop.csv",sep = "/"),
  row.names = F
  )

write.csv(
  buyer_monthly_pmq,
  paste(getwd(),"prescription_monthly_pmq.csv",sep = "/"),
  row.names = F
  )

