# Initialize functions 
source('00_project_settings.R')



# Read All tile flow and discharge data
nl_ALL <- read_rds('Inter_Data/nl_ALL.rds')



# DAILY N LOAD DATA ----------------------------------------------------

# Standardize N load data
nl_ALL %>%
  # get rid of interpolated concentration data
  filter(!var_NEW %in% c('WAT30', 'WAT31', 'WAT40', 'WAT42', 'WAT43')) %>%
  # add locations and assign plots
  mutate(location = case_when(siteid == 'ACRE' & str_detect(plotid, 'Inlet') ~ NA_character_,
                              TRUE  ~ location),
         plotid = case_when(siteid == 'FAIRM' & plotid == 'Sump1' ~ 'West',
                            siteid == 'FAIRM' & plotid == 'Sump2' ~ 'East',
                            siteid == 'WILKIN2' & plotid == 'WEST' ~ 'West',
                            siteid == 'WILKIN2' & plotid == 'EAST' ~ 'East',
                            TRUE ~ plotid)) %>%
  # remove loads the go to stream (because it can be calculated from Buffer and Field loads)
  mutate(remove = ifelse(location == 'To Stream', 'Yes', NA_character_)) %>% 
  filter(is.na(remove)) %>%
  # standardize timestamp
  mutate(time = NA_character_,
         UTC = NA_character_,
         timestamp_type = 'D') %>%
  # remove loads at ACRE that extend beyond tile flow measurements
  filter(!(siteid == 'ACRE' & date < ymd(20071104))) %>%
  select(siteid, plotid, location, date, time, UTC, timestamp_type, 
         var_NEW, value) -> nl_ALL_daily_standard



# Update load data for SB --------------------------------------------
nl_ALL_daily_standard %>%
  filter(!siteid %in% c('BEAR', 'BEAR2', 'BENTON', 'DIKE', 'HICKORY', 'MAASS', 'SHEARER')) %>%
  bind_rows(read_rds('Input_Data/WATER/load_Iowa_SBs_DAILY.rds')) -> nl_ALL_daily_standard_UPDATED




# Save standardized data --------------------------------------------------
nl_ALL_daily_standard_UPDATED %>%
  write_rds('Standard_Data/nl_ALL_daily.rds', compress = 'xz')

# after saving it was gzip-ed via shell command line  
nl_ALL_daily_standard_UPDATED %>%
  write_csv('Standard_Data/CSV/nitrate_load_ALL_daily.csv')


