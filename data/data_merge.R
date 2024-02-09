
rm(list=ls()) 
set.seed(123)

library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
unemp <- read.csv(paste(getwd(),"unemployment.csv",sep = "/"),header=TRUE,sep=",")
horwitz <- read.csv(paste(getwd(),"horwitz2020.csv",sep = "/"),header=TRUE,sep=",")
minwg <- read.csv(paste(getwd(),"minwage_clean_ub.csv",sep = "/"),header=TRUE,sep=",")

horwitz <- horwitz %>% 
  separate_wider_delim(Enactment, delim = "-", names = c("enact_month", "enact_year")) %>% 
  separate_wider_delim(Enactment.funding.contingent, delim = "-", names = c("enact_fc_month", "enact_fc_year"),too_few = "align_start") %>%
  separate_wider_delim(Enactment.electronic, delim = "-", names = c("enact_e_month", "enact_e_year"),too_few = "align_start") %>% 
  separate_wider_delim(Modern.system.operational, delim = "-", names = c("mop_month", "mop_year"),too_few = "align_start") %>% 
  separate_wider_delim(Prescriber.must.query, delim = "-", names = c("pmq_month", "pmq_year"),too_few = "align_start") %>% 
  mutate_if(is.character, list(~na_if(.,""))) 

horwitz$enact_month <- replace(horwitz$enact_month,horwitz$enact_month == "Pre","0")
horwitz[,2:length(horwitz)] <- sapply(horwitz[,2:length(horwitz)], as.numeric)
horwitz <- horwitz[c("state", "pmq_year", "pmq_month", "mop_year", "mop_month")]

horwitz <- horwitz %>% 
  mutate(first_treatment_pmq = (pmq_year-1960)*12 + pmq_month) %>%
  mutate(first_treatment_mop = (mop_year-1960)*12 + mop_month) 

unemp <- unemp %>% 
  rename(state = state_name) %>%
  rename(county = county_name) %>%
  rename(urate = value) %>%
  mutate(time_marker = (year-1960)*12 + month)
unemp <- unemp[,colnames(unemp) != "state_abbr"]
unemp$urate <- as.numeric(unemp$urate)

minwg <- minwg %>% 
  rename(year = Year) %>%
  rename(state = State.or.otherjurisdiction) %>%
  rename(minw = Value)
minwg <- minwg[minwg$state != 'Federal (FLSA)',]

df <- merge(unemp, horwitz, by = "state")
df <- merge(df, minwg, by = c("state","year"))
df <- df[df$year >= 1998 & df$year <= 2019,]

df <- df %>%
  mutate(indicator_pmq = 1*(time_marker >= first_treatment_pmq)) %>%
  mutate(indicator_mop = 1*(time_marker >= first_treatment_mop))

write.csv(df, paste(getwd(),"joined_data.csv",sep = "/"), row.names=F)
