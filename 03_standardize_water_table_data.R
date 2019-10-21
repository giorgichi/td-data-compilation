# Initialize functions 
source('00_project_settings.R')


# Read All water level data
wt_ALL_hourly <- read_rds('Inter_Data/wt_ALL_hourly.rds')
wt_ALL_daily <- read_rds('Inter_Data/wt_ALL_daily.rds')
st_ALL_hourly <- read_rds('Inter_Data/st_ALL_hourly.rds')



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
         var_NEW, reading_type, value) -> wt_ALL_daily_standard



# HOURLY WATER TABLE DATA -------------------------------------------------

# Standardize stage and storage data
wt_ALL_hourly %>%  
  # assign new var codes
  mutate(var_NEW = case_when(var == 'WAT4' ~ 'WAT01',
                             var == 'WATXX' ~ 'WAT02',   # BEAR2 and STJOHNS also have wt level
                             TRUE ~ NA_character_)) %>%
  # add locations and assign plots
  mutate(location = ifelse(siteid %in% c('ACRE', 'BEAR2', 'CLAY_C', 'CLAY_R', 'CLAY_U', 'DEFI_R'), 
                           plotid, NA_character_),
         plotid = case_when(siteid == 'ACRE' & location == 'Field63' ~ 'Inlet_A',
                            siteid == 'BEAR2' ~ NA_character_,
                            siteid == 'CLAY_C' ~ 'FD',
                            siteid == 'CLAY_R' ~ str_sub(location, 1, 2),
                            siteid == 'CLAY_U' ~ 'UD',
                            siteid == 'DEFI_R' & location %in% c('5a', '5b', '6a', '6b') ~ '6',
                            siteid == 'DEFI_R' & location %in% c('7a', '7b', '8a', '8b') ~ '7',
                            siteid == 'DEFI_R' & location %in% c('1a', '1b', '2a', '2b', '16a', '17a') ~ '8',
                            siteid == 'DEFI_R' & location %in% c('3a', '3b', '4a', '4b') ~ '9',
                            siteid == 'DEFI_R' & location %in% c('13a', '13b', '14a', '14b', '15a', '15b') ~ '10',
                            siteid == 'DEFI_R' & location %in% c('9a', '9b', '10a', '10b', 
                                                                 '11a', '11b', '12a', '12b') ~ '11',
                            siteid == 'FAIRM' & plotid == 'Sump1' ~ 'West',
                            siteid == 'FAIRM' & plotid == 'Sump2' ~ 'East',
                            siteid == 'HICKS_B' & plotid == 'Bwest' ~ 'BW',
                            siteid == 'HICKS_B' & plotid == 'Beast' ~ 'BE',
                            siteid == 'TIDE' ~ str_sub(plotid, 1, 2),
                            TRUE ~ plotid)) %>%
  # add measurement type 
  mutate(reading_type = 'automated') %>%
  # remove questionable data
  filter(!(siteid == 'CLAY_C' & year(tmsp) == 2018 & month(tmsp) > 4)) %>% 
  mutate(value = ifelse(siteid == 'TIDE' & plotid == 'H4' & between(tmsp, ymd_h(2009090211), ymd_h(2009092810)), 
                        NA_real_, value)) %>%
  mutate(value = ifelse(siteid == 'SERF_IA' & plotid == 'S2' & between(tmsp, ymd_h(2011121919), ymd_h(2012022320)), 
                        NA_real_, value),
         value = ifelse(siteid == 'SERF_IA' & plotid == 'S1' & tmsp > ymd_h(2017040523), NA_real_, value)) %>%
  # correct measurement units (convert to meters)
  mutate(value = case_when(siteid == 'FAIRM' ~ value * 0.3048 * 0.01, # adjusted based on plots published by Rijal, 2012
                           siteid %in% c('ACRE', 'CLAY_C', 'CLAY_R', 'CLAY_U', 'DEFI_R', 'TIDE',
                                         'DPAC', 'HICKS_B', 'SERF_IA', 'SERF_SD') ~ value * 0.01,  # from cm
                           siteid == 'STJOHNS' & var_NEW == 'WAT01' ~ value * 0.01,                # from cm
                           TRUE ~ value)) %>%
  # standardize timestamp
  mutate(date = as_date(tmsp),
         time = format(tmsp, '%H:%M'),
         UTC = case_when(siteid %in% c('ACRE', 'DPAC', 'DEFI_R', 'STJOHNS', 'TIDE') ~ tmsp + hours(5), 
                         siteid %in% c('BEAR2', 'CLAY_C', 'CLAY_R', 'CLAY_U', 'FAIRM', 
                                       'HICKS_B', 'SERF_IA', 'SERF_SD') ~ tmsp + hours(6)),
         UTC = format(UTC, '%Y-%m-%dT%H:%M:%S+00:00'),     # format according to ISO 8601 standard 
         timestamp_type = 'I') %>%
  select(siteid, plotid, location, tmsp, date, time, UTC, timestamp_type, 
         var_NEW, reading_type, value) -> wt_ALL_hourly_standard



# HOURLY STAGE DATA -------------------------------------------------------

# Standardize water table data
st_ALL_hourly %>% 
  # correct measurement units (convert to meters)
  mutate(value = case_when(var_NEW == 'WAT04' ~ value * 0.01,                # from cm
                           TRUE ~ value)) %>%
  # standardize timestamp
  mutate(date = as_date(tmsp),
         time = format(tmsp, '%H:%M'),
         UTC = case_when(siteid %in% c('ACRE', 'DEFI_R', 'FULTON', 'VANWERT') ~ tmsp + hours(5), 
                         siteid %in% c('MUDS3_NEW', 'SWROC') ~ tmsp + hours(6)),
         UTC = format(UTC, '%Y-%m-%dT%H:%M:%S+00:00'),     # format according to ISO 8601 standard 
         timestamp_type = 'I') %>%
  select(siteid, plotid, location, tmsp, date, time, UTC, timestamp_type, 
         var_NEW, reading_type, value) -> st_ALL_hourly_standard




# ---------------

wt_ALL_hourly_standard %>%
  spread(var_NEW, value)


# I CAN SAVE STANDARDIZE WT DATA (DAILY & HOURLY) as separate files for Daryl

# CHECKIN
wt_ALL_hourly_standard %>%
  filter(siteid == 'SERF_IA') %>%
  mutate(value = ifelse(siteid == 'SERF_IA' & plotid == 'S2' & between(date, ymd(20111220), ymd(20120222)), 
                        NA_real_, value),
         value = ifelse(siteid == 'SERF_IA' & plotid == 'S1' & date > ymd(20170405), NA_real_, value)) %>%
  # filter(plotid %in% c('S4', 'S5')) %>%
  # filter(between(date, ymd(20120220), ymd(20120226))) %>%
  # filter(between(date, ymd(20170322), ymd(20170415))) %>%
  filter(year(date) %in% 2016) %>%
  # filter(month(date) %in% 3:4) %>% #filter(day(date) %in% 3:6) %>%
  ggplot(aes(x=tmsp, y=value, group = plotid, col = plotid)) +
  geom_point() + geom_line() +
  scale_y_reverse() +
  # facet_grid(plotid ~ .) +
  theme_light()


