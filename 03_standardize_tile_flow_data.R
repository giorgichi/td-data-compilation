# Initialize functions 
source('00_project_settings.R')



# Read All tile flow and discharge data
tf_ALL_daily <- read_rds('Inter_Data/tf_ALL_daily.rds')
irr_ALL_daily <- read_rds('Inter_Data/irr_ALL_daily.rds')



# DAILY TILE FLWO DATA ----------------------------------------------------

# Standardize tile flow and discharge data
tf_ALL_daily %>%
  # remove questionable data at ACRE
  mutate(value = ifelse(siteid == 'ACRE' & plotid == "Outlet" & var_NEW == "WAT05" &
                          between(date, ymd(20080915), ymd(20081013)), NA, value)) %>%
  # convert discharge from m3/hr to m3/day
  mutate(value = ifelse(var_NEW == 'WAT05', value *24, value)) %>%
  # add locations and assign plots
  mutate(location = case_when(siteid %in% c('BEAR', 'BEAR2', 'BENTON', 'DIKE', 
                                            'HICKORY', 'MAASS', 'SHEARER', 'WILKIN3') ~ plotid,
                              siteid == 'ACRE' & plotid == 'Outlet' ~ plotid,
                              siteid == 'DEFI_R' & location == 'L' ~ 'Offsite',
                              siteid == 'DEFI_R' & location == 'O' ~ 'Wetland Out',
                              siteid == 'FULTON' & location == 'A' ~ 'Wetland In',
                              siteid == 'FULTON' & location == 'B' ~ 'Wetland Out',
                              siteid == 'VANWERT' & location == 'WET IN' ~ 'Wetland In',
                              siteid == 'VANWERT' & location == 'WET OUT' ~ 'Wetland Out',
                              siteid == 'VANWERT' & location == 'OFFSITE' ~ 'Offsite',
                              siteid == 'VANWERT' & location == 'RES OUT' ~ 'Reservoir Out',
                              TRUE  ~ location),
         plotid = case_when(siteid == 'ACRE' & plotid == 'Outlet' ~ NA_character_,
                            siteid %in% c('BEAR', 'BEAR2', 'BENTON', 'DIKE', 
                                          'HICKORY', 'MAASS', 'SHEARER', 'WILKIN3') ~ NA_character_,
                            siteid == 'FAIRM' & plotid == 'Sump1' ~ 'West',
                            siteid == 'FAIRM' & plotid == 'Sump2' ~ 'East',
                            siteid == 'WILKIN2' & plotid == 'WEST' ~ 'West',
                            siteid == 'WILKIN2' & plotid == 'EAST' ~ 'East',
                            TRUE ~ plotid)) %>%
  # standardize location names (multi-word names should be separated by space instead of underline)
  mutate(location = str_replace(location, "_", " ")) %>%
  # standardize timestamp
  mutate(time = NA_character_,
         UTC = NA_character_,
         timestamp_type = 'D') %>%
  filter(!is.na(date)) %>%
  select(siteid, plotid, location, date, time, UTC, timestamp_type, 
         var_NEW, value) -> tf_ALL_daily_standard


# Update discharge data for SB --------------------------------------------
tf_ALL_daily_standard %>%
  filter(!siteid %in% c('BEAR', 'BEAR2', 'BENTON', 'DIKE', 'HICKORY', 'MAASS', 'SHEARER')) %>%
  bind_rows(read_rds('Input_Data/WATER/discharge_Iowa_SBs_DAILY.rds')) -> 
  tf_ALL_daily_standard_UPDATED



# DAILY IRRIGATION --------------------------------------------------------

# Standardize daily sub-irrigation data
irr_ALL_daily %>%
  # standardize timestamp
  mutate(time = NA_character_,
         UTC = NA_character_,
         timestamp_type = 'D') %>%
  select(siteid, plotid, date, time, UTC, timestamp_type, 
         var_NEW, value) -> irr_ALL_daily_standard



# Save standardized data --------------------------------------------------
tf_ALL_daily_standard_UPDATED %>%
  write_rds('Standard_Data/tf_ALL_daily.rds', compress = 'xz')

irr_ALL_daily_standard %>%
  write_rds('Standard_Data/irr_ALL_daily.rds', compress = 'xz')

# after saving it was gzip-ed via shell command line  
tf_ALL_daily_standard_UPDATED %>%
  write_csv('Standard_Data/CSV/tile_flow_ALL_daily.csv')

irr_ALL_daily_standard %>%
  write_csv('Standard_Data/CSV/irrigation_ALL_daily.csv')

