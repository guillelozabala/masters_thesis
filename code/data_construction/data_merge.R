library(tidyverse)

# Clear the workspace
rm(list = ls())
options(max.print = 250)

# Set the seed for reproducibility
set.seed(123)

# Set the file location
file_location <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(gsub("/code/data_construction", "", file_location))

# Load the data

# Unemployment rate data
unemp_rate_path <- "data/intermediate/labor_market_outcomes/unemployment_rate.csv"

unemp_rate <- read.csv(
  file.path(getwd(), unemp_rate_path),
  header = TRUE,
  sep = ","
  )

# Employment rate data
emp_rate_path <- "data/intermediate/labor_market_outcomes/employment_rate.csv"

emp_rate <- read.csv(
  file.path(getwd(), emp_rate_path),
  header = TRUE,
  sep = ","
  )

# Labor force data
lab_force_path <- "data/intermediate/labor_market_outcomes/labor_force.csv"

lab_force <- read.csv(
  file.path(getwd(), lab_force_path),
  header = TRUE,
  sep = ","
  )

# PDMPs data
horwitz_path <- "data/source/pdmps/horwitz2020.csv"

horwitz <- read.csv(
  file.path(getwd(), horwitz_path),
  header = TRUE,
  sep = ","
  )

# NAICS shares of the counties data
naics_path <- "data/intermediate/sector_shares/naics_shares_merged.csv"

naics <- read.csv(
  file.path(getwd(), naics_path),
  header = TRUE,
  sep = ","
  )

# Minimum wage data
minwage_path <- "data/intermediate/minimum_wage/minwage_clean_fed_ub.csv"

minwage <- read.csv(
  file.path(getwd(), minwage_path),
  header = TRUE,
  sep = ","
  )

# Minimum wage bindingness data
minw_bindgness_path <- "data/intermediate/sector_wages/county_percentiles.csv"

minw_bindgness <- read.csv(
  file.path(getwd(), minw_bindgness_path),
  header = TRUE,
  sep = ","
  )

minw_bindgness <- minw_bindgness |>
  dplyr::select(-fipstate, -fipscty)

# Demographics data
demo_path <- "data/intermediate/demographics/county_demographics.csv"

demographics <- read.csv(
  file.path(getwd(), demo_path),
  header = TRUE,
  sep = ","
  )

# Demographics data by states
demo_states_path <- "data/intermediate/demographics/county_demographics_states.csv"

demographics_states <- read.csv(
  file.path(getwd(), demo_states_path),
  header = TRUE,
  sep = ","
  )

# Overdose deaths data (county-level, all drugs, yearly)
drug_deaths_path <- "data/source/overdose_deaths/NCHS_Drug_Poisoning_Mortality_by_County_United_States.csv"

drug_deaths <- read.csv(
  file.path(getwd(), drug_deaths_path),
  header = TRUE,
  sep = ","
  )

# Overdose deaths data (state-level, opioids, yearly)
drug_deaths_path_state <- "data/source/overdose_deaths/Multiple Cause of Death, 1999-2020.txt"

opioid_deaths_yearly <- read.delim(
  file.path(getwd(), drug_deaths_path_state)
  )

# PDMPs

# Divide months and years in PDMPs dataset

horwitz <- horwitz |>
  tidyr::separate_wider_delim(
    Enactment,
    delim = "-",
    names = c("enact_month", "enact_year")
  ) |>
  tidyr::separate_wider_delim(
    Enactment.funding.contingent,
    delim = "-",
    names = c("enact_fc_month", "enact_fc_year"),
    too_few = "align_start"
  ) |>
  tidyr::separate_wider_delim(
    Enactment.electronic,
    delim = "-",
    names = c("enact_e_month", "enact_e_year"),
    too_few = "align_start"
  ) |>
  tidyr::separate_wider_delim(
    Modern.system.operational,
    delim = "-",
    names = c("mop_month", "mop_year"),
    too_few = "align_start"
  ) |>
  tidyr::separate_wider_delim(
    Prescriber.must.query,
    delim = "-",
    names = c("pmq_month", "pmq_year"),
    too_few = "align_start"
  ) |>
  dplyr::mutate_if(
    is.character,
    list(~na_if(., ""))
  )

# Replace "Pre" with "0" in the 'enact_month' column
horwitz$enact_month <- replace(
  horwitz$enact_month,
  horwitz$enact_month == "Pre",
  "0"
  )

# Convert all columns except the first one to numeric values
horwitz[, 2:length(horwitz)] <- sapply(
  horwitz[, 2:length(horwitz)],
  as.numeric
  )

# Select columns
pdmps_columns <- c("state", "pmq_year", "pmq_month", "mop_year", "mop_month")
horwitz <- horwitz[pdmps_columns]

# Add time markers
horwitz <- horwitz |>
  dplyr::mutate(
    first_treatment_pmq = (pmq_year - 1960) * 12 + pmq_month
  ) |>
  dplyr::mutate(
    first_treatment_mop = (mop_year - 1960) * 12 + mop_month
  )

# Create a new dataset for Modern Operational Systems
mop_columns <- c("state", "mop_year", "mop_month", "first_treatment_mop")
horwitz_mop <- horwitz[mop_columns]

# Create a new dataset for Prescriber Must Query
pmq_columns <- c("state", "pmq_year", "pmq_month", "first_treatment_pmq")
horwitz_pmq <- horwitz[pmq_columns]

# Labor market data

# Restrict data to relevant period
unemp_rate <- unemp_rate[unemp_rate$year >= 1998 & unemp_rate$year <= 2019, ]
emp_rate <- emp_rate[emp_rate$year >= 1998 & emp_rate$year <= 2019, ]
lab_force <- lab_force[lab_force$year >= 1998 & lab_force$year <= 2019, ]

# Define a list of labor market data frames
labor_market <- list(unemp_rate, emp_rate, lab_force)

# Define the names for the labor market data frames
labor_market_names <- c("unemp_rate", "emp_rate", "lab_force")

# Function to clean the labor market data frames
clean_lab_market_data <- function(df) {
  df <- df |>
    dplyr::rename(
      state = state_name,
      county = county_name,
      rate = value
    ) |>
    dplyr::mutate(
      time_marker = (year - 1960) * 12 + month
    )
  df <- df[, colnames(df) != "state_abbr"]
  df$rate <- as.numeric(df$rate)
  return(df)
}

# Apply the clean_lab_market_data function to each labor market data frame
labor_market_clean <- purrr::map(
  labor_market,
  clean_lab_market_data
  )

# Assign names to the cleaned labor market data frames
names(labor_market_clean) <- labor_market_names

# Minimum Wage

# Rename the columns of the minwage data frame
minwage <- minwage |>
  dplyr::rename(
    year = Year,
    state = State.or.otherjurisdiction,
    minw = Value
  )

# Remove observations where the state is "Federal (FLSA)"
minwage <- minwage[minwage$state != "Federal (FLSA)", ]

# Add missing data for the year 1999
missing_minw <- minwage[minwage$year == 1998, ] |>
  dplyr::mutate(
    year = 1999
  )

# Combine the original minwage data frame with the missing data for 1999
minwage <- rbind(
  minwage,
  missing_minw
  )

# Demographics

# Select the relevant columns from the demographics data frame
demographics <- cbind(
  demographics[, 1:3],
  demographics[, grep("_ratio$", colnames(demographics))]
  )

# Create a vector to store the column names of the working age cohorts
working_ages <- c()
for (age_bin in 5:13) {
  working_cohort <- paste("age", age_bin, "_population_ratio", sep = "")
  working_ages <- c(working_ages, working_cohort)
}

# Calculate the weighted working age population
demographics[["working_age_pop_weight"]] <- rowSums(
  demographics[, working_ages],
  na.rm = TRUE
  )

# Calculate the working age population
demographics <- demographics |>
  dplyr::mutate(
    working_age_pop = working_age_pop_weight * population
  )

# Drugs

# Create an empty data frame to store the scaled drug death values
drug_deaths_scaled <- drug_deaths[FALSE, ]

# Iterate over unique years in the drug_deaths data frame
for (year in unique(drug_deaths$Year)) {
  # Subset the drug_deaths data frame for the current year
  drug_deaths_y <- drug_deaths[drug_deaths$Year == year, ]

  # Scale the Model.based.Death.Rate column within each year
  drug_deaths_y <- drug_deaths_y |>
    dplyr::mutate(
      scaled_model_values = scale(Model.based.Death.Rate)
    )

  # Append the scaled values to the drug_deaths_scaled data frame
  drug_deaths_scaled <- rbind(drug_deaths_scaled, drug_deaths_y)
}

# Rename the columns of the drug_deaths_scaled data frame
drug_deaths_scaled <- drug_deaths_scaled |>
  dplyr::rename(
    fips = FIPS,
    year = Year,
    model_values = Model.based.Death.Rate
  )

# Select the relevant columns from the drug_deaths_scaled data frame
overdose_columns <- c("fips", "year", "model_values", "scaled_model_values")
drug_deaths_scaled <- drug_deaths_scaled[, overdose_columns]

# Prescription data

# Create an empty data frame to store the monthly buyer information
buyer_monthly <- data.frame(matrix(ncol = 5, nrow = 0))

# Define the column names for the buyer_monthly data frame
presc_names <- c("BUYER_COUNTY", "BUYER_STATE", "year", "month", "DOSAGE_UNIT")

# Set the column names of the buyer_monthly data frame
colnames(buyer_monthly) <- presc_names

# Loop through each year from 2006 to 2014
for (year in 2006:2014) {
  # Read the buyer monthly data for the current year
  buyer_monthly_year <- read.csv(
    file.path(getwd(), paste0("data/source/prescriptions/buyer_monthly", year, ".csv")),
    header = TRUE,
    sep = ","
  )

  # Select the relevant columns from the buyer monthly data
  buyer_monthly_year <- buyer_monthly_year[, presc_names]

  # Group the buyer monthly data and summarize the dosage unit
  buyer_monthly_year <- buyer_monthly_year |>
    dplyr::group_by(
      BUYER_COUNTY,
      BUYER_STATE,
      year,
      month
    ) |>
    dplyr::summarize(
      dosage_unit = sum(DOSAGE_UNIT, na.rm = TRUE)
    )

  # Append the buyer monthly data to the buyer_monthly data frame
  buyer_monthly <- rbind(buyer_monthly, buyer_monthly_year)
}

county_fips_prescriptions <- read.csv(
  file.path(getwd(), paste0("data/source/fips/county_fips_prescriptions.csv")),
  header = TRUE,
  sep = ","
  )

#county_fips_prescriptions <- county_fips_prescriptions |>
#  rename(
#    buyer_county = BUYER_COUNTY,
#    buyer_state = BUYER_STATE,
#    fips = countyfips
#    )

buyer_monthly <- merge(
  buyer_monthly,
  county_fips_prescriptions,
  by = c(
    "BUYER_STATE",
    "BUYER_COUNTY"
  ),
  all.y = TRUE
)

presc_names <- c("buyer_county", "buyer_state",
"year", "month", "dosage_unit", "fips")
colnames(buyer_monthly) <- presc_names

# Merging the data

# Merge unemployment and employment rates
df <- merge(
  dplyr::rename(
    labor_market_clean$unemp_rate,
    unem_rate = rate
  ),
  dplyr::rename(
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
  ),
  all.x = TRUE
)

df <- df[!duplicated(df), ]

# Merge the merged data frame with the labor force
df <- merge(
  df,
  dplyr::rename(
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
  ),
  all.x = TRUE
)

df <- df[!duplicated(df), ]

# Merge the merged data frame with demographic characteristics
df <- merge(
  df,
  demographics,
  by = c(
    "year",
    "fips"
  ),
  all.x = TRUE
)

# Obtain the labor force participation rate
df <- df |>
  dplyr::mutate(
    lab_force_rate = round((lab_force / working_age_pop) * 100, 1)
  )


# Merge the merged data frame with the scaled number of drug deaths
df <- merge(
  df,
  drug_deaths_scaled,
  by = c(
    "year",
    "fips"
  ),
  all.x = TRUE
)

# Merge the merged data frame with the state-level minwage
df <- merge(
  df,
  minwage,
  by = c(
    "state",
    "year"
  ),
  all.x = TRUE
)

# Merge the merged data frame with the minwage bindingness
df <- merge(
  df,
  minw_bindgness,
  by = c(
    "fips",
    "year"
  ),
  all.x = TRUE
)

# Take the logs of the wage dist moments
df <- df |>
  dplyr::mutate(
    log_minw = log(minw)
  ) |>
  dplyr::rename(
    h_pct50 = h_median
  ) |>
  dplyr::mutate(
    log_h_pct10 = log(h_pct10),
    log_h_pct25 = log(h_pct25),
    log_h_pct50 = log(h_pct50),
    log_h_pct75 = log(h_pct75),
    log_h_pct90 = log(h_pct90)
  )

# Compute the Kaitz-p indices
df <- df |>
  dplyr::mutate(
    kaitz_pct10 = log_minw - log_h_pct10,
    kaitz_pct25 = log_minw - log_h_pct25,
    kaitz_pct50 = log_minw - log_h_pct50,
    kaitz_pct75 = log_minw - log_h_pct75,
    kaitz_pct90 = log_minw - log_h_pct90
  )

# Sanity check (duplicated values)
for (fip in unique(df$fips)) {
  df_fip <- df[df$fips == fip, ]
  if (any(duplicated(df_fip))) {
    print(paste("Duplicated values in", fip))
    # Select the non-duplicated rows
    df_fip <- df_fip[!duplicated(df_fip), ]
    # Remove the rows of the duplicated county from df
    df <- df[!(df$fips == fip), ]
    # Append df_fip to df
    df <- rbind(df, df_fip)
  }
}

# Merge the merged data frame with the PDMPs dates
df <- merge(
  df,
  horwitz,
  by = "state",
  all.x = TRUE
)

# Merge the merged data frame with the NAIC shares
df <- merge(
  df,
  naics,
  by = c(
    "year",
    "fips"
  ),
  all.x = TRUE
)

# Select the columns for Modern Operational Systems treatment
mop_treatment <- colnames(horwitz_mop)
mop_treatment <- mop_treatment[2:length(mop_treatment)]

# Select the columns for Prescriber Must Query treatment
pmq_treatment <- colnames(horwitz_pmq)
pmq_treatment <- pmq_treatment[2:length(pmq_treatment)]

# Exclude Prescriber Must Query columns
df_mop <- df[, !(colnames(df) %in% pmq_treatment)]

# Exclude Modern Operational Systems columns
df_pmq <- df[, !(colnames(df) %in% mop_treatment)]

# Write the data frames to CSV files
write.csv(
  df,
  paste(getwd(), "data/processed/joined_data.csv", sep = "/"),
  row.names = FALSE
)

write.csv(
  df_mop,
  paste(getwd(), "data/processed/joined_data_mop.csv", sep = "/"),
  row.names = FALSE
)

df_pmq <- merge(
  df_pmq,
  buyer_monthly,
  by = c(
    "fips",
    "year",
    "month"
  ),
  all.x = TRUE
)

write.csv(
  df_pmq,
  paste(getwd(), "data/processed/joined_data_pmq.csv", sep = "/"),
  row.names = FALSE
)

# Mortality data

# Overdose deaths data (state-level, opioids, yearly)
drug_deaths_path_state <- "data/source/overdose_deaths/Multiple Cause of Death, 1999-2020.txt"

opioid_deaths_yearly <- read.delim(
  file.path(getwd(), drug_deaths_path_state)
  )
# Select the relevant columns from the opioid deaths data frame
odd_columns <- c("State", "State.Code", "Year", "Multiple.Cause.of.death", "Multiple.Cause.of.death.Code", "Deaths")
opioid_deaths_yearly <- opioid_deaths_yearly[, odd_columns]

# Rename the columns of the opioid deaths data frame
opioid_deaths_yearly <- opioid_deaths_yearly |>
  dplyr::rename(
    state = State,
    state_code = State.Code,
    year = Year,
    cause_of_death = Multiple.Cause.of.death,
    cause_of_death_code = Multiple.Cause.of.death.Code,
    deaths = Deaths
  )

# Merge the opioid deaths data frame with the PDMPs data frame
opioid_deaths_yearly <- merge(
  opioid_deaths_yearly,
  horwitz,
  by = "state"
  )

# Select the relevant columns from the demographics data frame
demographics_states <- cbind(
  demographics_states[, 1:3],
  demographics_states[, grep("_ratio$", colnames(demographics_states))]
  )

# Calculate the weighted working age population
demographics_states[["working_age_pop_weight"]] <- rowSums(
  demographics_states[, working_ages],
  na.rm = TRUE
  )

# Calculate the working age population
demographics_states <- demographics_states |>
  dplyr::mutate(
    working_age_pop = working_age_pop_weight * population
  )

# Rename the states column (NO?)
demographics_states <- demographics_states |>
  dplyr::rename(
    state_code = state_fip
  )

# Merge the opioid deaths data frame with the demographics data frame
opioid_deaths_yearly <- merge(
  opioid_deaths_yearly,
  demographics_states,
  by = c(
    "state_code",
    "year"
    )
)

naics_states <- naics
naics_states <- naics_states |>
  group_by(year, state_code) |>
  summarize(across(everything(), mean, na.rm = TRUE)) |>
  select(-fips, -county_code)

opioid_deaths_yearly <- merge(
  opioid_deaths_yearly,
  naics_states,
  by = c(
    "state_code",
    "year"
    )
)

opioid_deaths_yearly <- merge(
  opioid_deaths_yearly,
  minwage,
  by = c(
    "state",
    "year"
    )
)

minw_bindgness <- read.csv(
  file.path(getwd(), minw_bindgness_path),
  header = TRUE,
  sep = ","
  )

minw_bindgness <- minw_bindgness |>
  dplyr::rename(
    state_code = fipstate,
    county_code = fipscty
    )

minw_bindgness <- minw_bindgness |>
  group_by(year, state_code) |>
  summarize(across(everything(), mean, na.rm = TRUE)) |>
  select(-fips, -county_code)

opioid_deaths_yearly <- merge(
  opioid_deaths_yearly,
  minw_bindgness,
  by = c(
    "state_code",
    "year"
    )
)

opioid_deaths_yearly <- opioid_deaths_yearly |>
  dplyr::mutate(
    log_minw = log(minw)
  ) |>
  dplyr::rename(
    h_pct50 = h_median
  ) |>
  dplyr::mutate(
    log_h_pct10 = log(h_pct10),
    log_h_pct25 = log(h_pct25),
    log_h_pct50 = log(h_pct50),
    log_h_pct75 = log(h_pct75),
    log_h_pct90 = log(h_pct90)
  )

# Compute the Kaitz-p indices
opioid_deaths_yearly <- opioid_deaths_yearly |>
  dplyr::mutate(
    kaitz_pct10 = log_minw - log_h_pct10,
    kaitz_pct25 = log_minw - log_h_pct25,
    kaitz_pct50 = log_minw - log_h_pct50,
    kaitz_pct75 = log_minw - log_h_pct75,
    kaitz_pct90 = log_minw - log_h_pct90
  )

write.csv(
  opioid_deaths_yearly,
  paste(getwd(), "data/processed/opioid_deaths_df.csv", sep = "/"),
  row.names = FALSE
)


# Overdose deaths data (state-level, opioids, monthly, non-complete)
drug_deaths_path_state_m <- "data/source/overdose_deaths/Multiple Cause of Death_state_monthly_incomp.txt"

opioid_deaths_yearly_m <- read.delim(
  file.path(getwd(), drug_deaths_path_state_m)
  )

opioid_deaths_yearly_m
# Select the relevant columns from the opioid deaths data frame

opioid_deaths_yearly_m <- opioid_deaths_yearly_m |>
  separate_wider_delim(Month.Code, delim = "/", names = c("year", "month"),too_few = "debug")

odd_columns_m <- c("State", "State.Code", "year", "month", "Multiple.Cause.of.death", "Multiple.Cause.of.death.Code", "Deaths")
opioid_deaths_yearly_m <- opioid_deaths_yearly_m[, odd_columns_m]

opioid_deaths_yearly_m
# Rename the columns of the opioid deaths data frame
opioid_deaths_yearly_m <- opioid_deaths_yearly_m |>
  dplyr::rename(
    state = State,
    state_code = State.Code,
    cause_of_death = Multiple.Cause.of.death,
    cause_of_death_code = Multiple.Cause.of.death.Code,
    deaths = Deaths
  )

opioid_deaths_yearly_m <- transform(opioid_deaths_yearly_m, year = as.numeric(year))
opioid_deaths_yearly_m <- transform(opioid_deaths_yearly_m, month = as.numeric(month))

opioid_deaths_yearly_m <- opioid_deaths_yearly_m |>
  dplyr::mutate(
    time_marker = (year - 1960) * 12 + month
  )

# Merge the opioid deaths data frame with the PDMPs data frame
opioid_deaths_yearly_m <- merge(
  opioid_deaths_yearly_m,
  horwitz,
  by = "state"
  )


# Merge the opioid deaths data frame with the demographics data frame
opioid_deaths_yearly_m <- merge(
  opioid_deaths_yearly_m,
  demographics_states,
  by = c(
    "state_code",
    "year"
    )
)

naics_states <- naics
naics_states <- naics_states |>
  group_by(year, state_code) |>
  summarize(across(everything(), mean, na.rm = TRUE)) |>
  select(-fips, -county_code)

opioid_deaths_yearly_m <- merge(
  opioid_deaths_yearly_m,
  naics_states,
  by = c(
    "state_code",
    "year"
    )
)

opioid_deaths_yearly_m <- merge(
  opioid_deaths_yearly_m,
  minwage,
  by = c(
    "state",
    "year"
    )
)

minw_bindgness <- read.csv(
  file.path(getwd(), minw_bindgness_path),
  header = TRUE,
  sep = ","
  )

minw_bindgness <- minw_bindgness |>
  dplyr::rename(
    state_code = fipstate,
    county_code = fipscty
    )

minw_bindgness <- minw_bindgness |>
  group_by(year, state_code) |>
  summarize(across(everything(), mean, na.rm = TRUE)) |>
  select(-fips, -county_code)

opioid_deaths_yearly_m <- merge(
  opioid_deaths_yearly_m,
  minw_bindgness,
  by = c(
    "state_code",
    "year"
    )
)

opioid_deaths_yearly_m <- opioid_deaths_yearly_m |>
  dplyr::mutate(
    log_minw = log(minw)
  ) |>
  dplyr::rename(
    h_pct50 = h_median
  ) |>
  dplyr::mutate(
    log_h_pct10 = log(h_pct10),
    log_h_pct25 = log(h_pct25),
    log_h_pct50 = log(h_pct50),
    log_h_pct75 = log(h_pct75),
    log_h_pct90 = log(h_pct90)
  )

# Compute the Kaitz-p indices
opioid_deaths_yearly_m <- opioid_deaths_yearly_m |>
  dplyr::mutate(
    kaitz_pct10 = log_minw - log_h_pct10,
    kaitz_pct25 = log_minw - log_h_pct25,
    kaitz_pct50 = log_minw - log_h_pct50,
    kaitz_pct75 = log_minw - log_h_pct75,
    kaitz_pct90 = log_minw - log_h_pct90
  )

write.csv(
  opioid_deaths_yearly_m,
  paste(getwd(), "data/processed/opioid_deaths_df_m.csv", sep = "/"),
  row.names = FALSE
)

