# Initialize functions 
source('00_project_settings.R')



# Read All tile flow and discharge data
tf_ALL_daily <- read_rds('Inter_Data/tf_ALL_daily.rds')
irr_ALL_daily <- read_rds('Inter_Data/irr_ALL_daily.rds')



# DAILY TILE FLWO DATA ----------------------------------------------------

# Standardize tile flow and discharge data
tf_ALL_daily %>%
  # add locations and assign plots
  mutate(location = case_when(siteid %in% c('ACRE', 'BEAR', 'BEAR2', 'BENTON', 'DIKE', 
                                            'HICKORY', 'MAASS', 'SHEARER', 'WILKIN3') ~ plotid,
                              siteid == 'DEFI_R' & location == 'L' ~ 'Offsite',
                              siteid == 'DEFI_R' & location == 'O' ~ 'Wetland_Out',
                              siteid == 'FULTON' & location == 'A' ~ 'Wetland_In',
                              siteid == 'FULTON' & location == 'B' ~ 'Wetland_Out',
                              siteid == 'VANWERT' & location == 'WET IN' ~ 'Wetland_In',
                              siteid == 'VANWERT' & location == 'WET OUT' ~ 'Wetland_Out',
                              siteid == 'VANWERT' & location == 'OFFSITE' ~ 'Offsite',
                              siteid == 'VANWERT' & location == 'RES OUT' ~ 'Reservoir_Out',
                              TRUE  ~ location),
         plotid = case_when(siteid == 'ACRE' & plotid == 'Outlet' ~ NA_character_,
                            siteid %in% c('BEAR', 'BEAR2', 'BENTON', 'DIKE', 
                                          'HICKORY', 'MAASS', 'SHEARER', 'WILKIN3') ~ NA_character_,
                            siteid == 'FAIRM' & plotid == 'Sump1' ~ 'West',
                            siteid == 'FAIRM' & plotid == 'Sump2' ~ 'East',
                            siteid == 'WILKIN2' & plotid == 'WEST' ~ 'West',
                            siteid == 'WILKIN2' & plotid == 'EAST' ~ 'East',
                            TRUE ~ plotid)) %>%
  # standardize timestamp
  mutate(time = NA_character_,
         UTC = NA_character_,
         timestamp_type = 'D') %>%
  select(siteid, plotid, location, date, time, UTC, timestamp_type, 
         var_NEW, value) -> tf_ALL_daily_standard



# DAILY IRRIGATION --------------------------------------------------------

# Standardize daily sub-irrigation data
irr_ALL_daily %>%
  # standardize timestamp
  mutate(time = NA_character_,
         UTC = NA_character_,
         timestamp_type = 'D') %>%
  select(siteid, plotid, date, time, UTC, timestamp_type, 
         var_NEW, value) -> irr_ALL_daily_standard


