
# CLEAN, COMMENT

rm(list=ls()) 
set.seed(123)

library(tidyverse)

# Set path
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the data
unemp_rate <- read.csv(
  file.path(getwd(), "unemployment_rate.csv"), 
  header = TRUE,
  sep = ","
  )

unemp <- read.csv(
  file.path(getwd(), "unemployment.csv"),
  header = TRUE,
  sep = ","
  )

emp <- read.csv(
  file.path(getwd(), "employment.csv"),
  header = TRUE,
  sep = ","
  )

lab_force <- read.csv(
  file.path(getwd(), "labor_force.csv"),
  header = TRUE,
  sep = ","
  )

horwitz <- read.csv(
  file.path(getwd(), "horwitz2020.csv"),
  header = TRUE,
  sep = ","
  )

minwage <- read.csv(
  file.path(getwd(), "minwage_clean_fed_ub.csv"),
  header = TRUE,
  sep = ","
  )

minw_bindgness <- read.csv(
  file.path(getwd(), "county_percentiles.csv"),
  header = TRUE,
  sep = ","
)

demographics <- read.csv(
  file.path(getwd(), "county_demographics.csv"),
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

labor_market <- list(unemp_rate,unemp,emp,lab_force)

clean_lab_market_data <- function(df){
  
  df <- df |> 
    rename(
      state = state_name
    ) |>
    rename(
      county = county_name
    ) |>
    rename(
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
                               "unemp",
                               "emp",
                               "lab_force")

# Minimum Wage

## Rename, drop federal observations

minwage <- minwage |> 
  rename(
    year = Year
    ) |>
  rename(
    state = State.or.otherjurisdiction
    ) |>
  rename(
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

## Select the ratios only

demographics <- cbind(
  demographics[,1:2],
  demographics[, grep("_ratio$", colnames(demographics))]
  )

# Merging

df <- merge(
  rename(
    labor_market_clean$unemp_rate,
    unem_rate = rate
    ),
  rename(
    labor_market_clean$unemp,
    unem = rate
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
    labor_market_clean$emp,
    emp = rate
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
  row.names=F
  )
