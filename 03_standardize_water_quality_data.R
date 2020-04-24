# Initialize functions 
source('00_project_settings.R')



# Read All water quality data
wq_ALL <- read_rds('Inter_Data/wq_ALL.rds')



# Correct water quality data
wq_ALL %>%
  # add sample type based on TD Sampling Method data
  mutate(sample_type = case_when(siteid == 'ACRE' & date < ymd(20160301) ~ 'Grab',
                                 siteid == 'ACRE' & date > ymd(20160229) ~ 'Time Proportional',     # ISCO
                                 siteid == 'DPAC' ~ 'Time Proportional',                            # ISCO
                                  # some samples at DPAC were grab during the winter but we do not know which
                                 siteid %in% c('AUGLA', 'CRAWF', 'DEFI_M', 'HARDIN') ~ 'Grab',
                                 siteid %in% c('BEAR', 'BEAR2', 'BENTON', 'DIKE', 
                                               'HICKORY', 'MAASS', 'SHEARER') ~ 'Grab',
                                 siteid == 'HICKS_B' & sample_type == 'ISCO' ~ 'Flow Proportional', # ISCO
                                 siteid == 'STORY' ~ 'Flow Proportional',
                                 siteid %in% c('MUDS2', 'MUDS4') ~ 'Time Proportional',             # ISCO
                                 siteid %in% c('MUDS3_OLD', 'MUDS3_NEW') ~ 'Time Proportional',     # Sigma
                                 siteid == 'DEFI_R' & str_detect(location, 'lysimeter') ~ 'Grab',
                                 siteid == 'CLAY_C' ~ 'Grab',
                                 siteid == 'CLAY_R' ~ 'Grab',
                                 siteid == 'FAIRM' & var_NEW != 'WAT15'  ~ 'Grab',
                                 siteid == 'SERF_IA' ~ 'Grab',
                                 siteid == 'SERF_SD' & var_NEW != 'WAT16' ~ 'Grab',
                                 siteid == 'SWROC' ~ 'Grab',
                                 siteid == 'TIDE' ~ 'Flow Proportional',
                                 siteid == 'UBWC' & month(date) > 2 ~ 'Time Proportional',         # ISCO
                                 siteid == 'UBWC' & month(date) < 3 ~ 'Grab',
                                 siteid == 'UBWC' & month(date) == 12 & day(date) > 15 ~ 'Grab',
                                 siteid == 'WILKIN1' ~ 'Flow Proportional',
                                 siteid == 'WILKIN2' ~ 'Flow Proportional',
                                 siteid == 'WILKIN3' ~ 'Grab',
                                 TRUE ~ sample_type)) %>%
  # get rid of plots/locations not being included in TD data
  filter(!(siteid == 'FULTON' & location == 'C')) %>%
  # correct plot IDs
  mutate(plotid = case_when(siteid == 'FAIRM' & plotid == 'Sump1' ~ 'West',
                            siteid == 'FAIRM' & plotid == 'Sump2' ~ 'East',
                            siteid == 'ACRE' & str_detect(location, 'Inlet') ~ location,
                            TRUE ~ plotid),
         location = case_when(siteid == 'ACRE' & str_detect(plotid, 'Inlet') ~ NA_character_,
                              TRUE ~ location)) %>%
  # rename location names
  mutate(location = case_when(siteid == 'DEFI_R' & location == 'L' ~ 'Offsite',
                              siteid == 'DEFI_R' & location == 'O' ~ 'Wetland Out',
                              siteid == 'DEFI_R' & location == 'R' ~ 'Reservoir',
                              siteid == 'DEFI_R' & location == 'RESOUT' ~ 'Reservoir Out',
                              siteid == 'DEFI_R' & location == 'W' ~ 'Wetland',
                              siteid == 'DEFI_R' & location == 'WETIN' ~ 'Wetland In',
                              siteid == 'DEFI_R' & location == 'WETOUT' ~ 'Wetland Out',
                              siteid == 'FULTON' & location %in% c('A', 'WET IN') ~ 'Wetland In',
                              siteid == 'FULTON' & location %in% c('B', 'WET OUT') ~ 'Wetland Out',
                              siteid == 'VANWERT' & location == 'WET IN' ~ 'Wetland In',
                              siteid == 'VANWERT' & location == 'WET OUT' ~ 'Wetland Out',
                              siteid == 'VANWERT' & location == 'OFFSITE' ~ 'Offsite',
                              siteid == 'VANWERT' & location == 'RES OUT' ~ 'Reservoir Out',
                              siteid == 'SERF_SD' & !is.na(location) ~ 'Middle Well',
                              TRUE ~ location)) %>% 
  # correct measurement units
  mutate(value_temp = as.numeric(value),
         # this is to fix P concentration at WRSIS
         value_temp = ifelse(str_detect(var_NEW, '^WAT4'), value_temp * 1000, value_temp),
         value = ifelse(siteid %in% c('DEFI_R', 'FULTON', 'VANWERT') & 
                          str_detect(var_NEW, '^WAT4') & 
                          !is.na(value_temp),
                        as.character(value_temp),
                        value),
         # this is to fix EC  at WRSIS
         value_temp = ifelse(var_NEW == 'WAT15', value_temp * 1000, value_temp),
         value = ifelse(siteid %in% c('DEFI_R', 'FULTON', 'VANWERT') & 
                          var_NEW == 'WAT15' & 
                          !is.na(value_temp),
                        as.character(value_temp),
                        value)) %>%
  # remove erroneous readings
  mutate(value = ifelse(var_NEW == 'WAT50' & value_temp > 50000, NA_character_, value),
         value = ifelse(var_NEW == 'WAT51' & value_temp > 500, as.character(value_temp/10), value),
         value = ifelse(var_NEW == 'WAT52' & value_temp > 200, as.character(value_temp/10), value)) %>%
  select(-value_temp) -> wq_ALL_corrected


# Standardize water quality data
wq_ALL_corrected %>%
  # standartize BDLs
  mutate(comments = ifelse(value == 'no water', value, comments),
         value = ifelse(value == 'no water', NA_character_, value),    # at WILKIN3
         value = ifelse(value == 'NV', NA_character_, value),          # at WRSIS
         comments = str_replace_all(comments, 'changed to .* from', 'changed to NA from'),
         value = str_replace(value, '< ', '<')) %>%
  mutate(value = case_when(str_detect(siteid, 'WILKIN') & value == 'BDL' & var_NEW == 'WAT22' ~ '<0.1',
                           str_detect(siteid, 'WILKIN') & value == 'BDL' & var_NEW == 'WAT30' ~ '<0.4',
                           str_detect(siteid, 'WILKIN') & value == 'BDL' & var_NEW == 'WAT36' ~ '<0.1',
                           str_detect(siteid, 'WILKIN') & value == 'BDL' & var_NEW == 'WAT40' ~ '<5',
                           TRUE ~ value)) %>%
  select(siteid, plotid, location, height, date, time, sample_type, var_NEW, value, comments) %>%
  # standardize timestamp
  mutate(timestamp_type = ifelse(is.na(time), 'D', 'I')) %>%
  mutate(tmsp = ifelse(timestamp_type == 'I', 
                       update(date, 
                              hour = as.numeric(str_sub(time, 1, 2)),
                              minute = as.numeric(str_sub(time, -2, -1))),
                       NA),
         tmsp = as_datetime(tmsp),
         UTC = case_when(siteid %in% c('DEFI_R', 'FULTON', 'UBWC', 'VANWERT') ~ tmsp + hours(5),
                         siteid == 'FAIRM' & timestamp_type == 'I' ~ tmsp + hours(5),
                         siteid %in% c('HICKS_B', ' WILKIN1', ' WILKIN2') ~ tmsp + hours(6)),
         UTC = format(UTC, '%Y-%m-%dT%H:%M:%S+00:00')) %>%
  ungroup() %>%
  # handle replicated measurements
  group_by(siteid, plotid, location, height, date, time, var_NEW) %>%
  mutate(subsample = ifelse(siteid == 'CLAY_R',  1:n(), NA),
         subsample = ifelse(siteid == 'STJOHNS', 1:n(), subsample),
         subsample = ifelse(str_detect(siteid, 'WILKIN'), 1:n(), subsample),
         subsample = ifelse(siteid == 'DEFI_R' & sample_type == 'Mast', 1:n(), subsample)) %>%
  select(siteid, plotid, location, height, subsample, tmsp, date, time, UTC, timestamp_type,
         sample_type, var_NEW, value, comments) %>%
  ungroup() -> wq_ALL_standard



# Save standardized data --------------------------------------------------

write_rds(wq_ALL_standard, 'Standard_Data/wq_ALL.rds', compress = 'xz')

