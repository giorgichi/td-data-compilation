# Initialize functions 
source('00_project_settings.R')


# Read All agr data
wt_ALL_hourly <- read_rds('Inter_Data/wt_ALL_hourly.rds')
wt_ALL_daily <- read_rds('Inter_Data/wt_ALL_daily.rds')



# DAILY WATER TABLE DATA ------------------------------------------------------


# Standardize water table and piezometric data
wt_ALL_daily %>%
  # assign new var codes
  mutate(var_NEW = case_when(var == 'WAT4' ~ 'WAT01',
                             var == 'WATXX' ~ 'WAT03',
                             TRUE ~ NA_character_)) %>%
  # add locations and assign plots
  mutate(location = plotid, 
         plotid = case_when(siteid %in% c('ACRE', 'BEAR', 'MAASS') ~ NA_character_,
                            siteid == 'DEFI_R' & location %in% c('5a', '5b', '6a', '6b') ~ '6',
                            siteid == 'DEFI_R' & location %in% c('7a', '7b', '8a', '8b') ~ '7',
                            siteid == 'DEFI_R' & location %in% c('1a', '1b', '2a', '2b', '16a', '17a') ~ '8',
                            siteid == 'DEFI_R' & location %in% c('3a', '3b', '4a', '4b') ~ '9',
                            siteid == 'DEFI_R' & location %in% c('13a', '13b', '14a', '14b', '15a', '15b') ~ '10',
                            siteid == 'DEFI_R' & location %in% c('9a', '9b', '10a', '10b', 
                                                                 '11a', '11b', '12a', '12b') ~ '11',
                            TRUE ~ NA_character_)) %>%
  # add measurement type 
  mutate(reading_type = case_when(siteid == 'ACRE' ~ 'manual',
                                  siteid == 'DEFI_R'  ~ 'manual',
                                  TRUE ~ 'automated')) %>%
  # correct measurement units (convert to meters)
  mutate(value = case_when(siteid == 'BEAR' ~ value * 0.3048,                        # from ft
                           siteid %in% c('ACRE', 'DEFI_R', 'MAASS') ~ value * 0.01,  # from cm
                           TRUE ~ value)) %>%
  # standardize timestamp
  mutate(time = NA_character_,
         UTC = NA_character_,
         timestamp_type = 'D') %>%
  select(siteid, plotid, location, date, time, UTC, timestamp_type, 
         var_NEW, reading_type, value) -> wt_ALL_hourly_standard



# HOURLY WATER TABLE DATA -------------------------------------------------

# Standardize water table data
wt_ALL_hourly 




  
  


# CHECKIN
wt_ALL_hourly_standard %>%
  filter(siteid == 'DEFI_R') %>%
  ggplot(aes(x=date, y=value, col = location)) +
  geom_point() + geom_line() +
  scale_y_reverse() +
  theme_light()
