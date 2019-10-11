# Initialize functions 
source('00_project_settings.R')



# Read All water quality data
wq_ALL <- read_rds('Inter_Data/wq_ALL.rds')



# Standardize water quality data
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
                                 siteid == 'FAIRM'  ~ 'Grab',
                                 siteid == 'SERF_IA' ~ 'Grab',
                                 siteid == 'SERF_SD' ~ 'Grab',
                                 siteid == 'SWROC' ~ 'Grab',
                                 siteid == 'TIDE' ~ 'Flow Proportional',
                                 siteid == 'UBWC' & month(date) > 2 ~ 'Time Proportional',         # ISCO
                                 siteid == 'UBWC' & month(date) < 3 ~ 'Grab',
                                 siteid == 'UBWC' & month(date) == 12 & day(date) > 15 ~ 'Grab',
                                 siteid == 'WILKIN1' ~ 'Flow Proportional',
                                 siteid == 'WILKIN2' ~ 'Flow Proportional',
                                 siteid == 'WILKIN3' ~ 'Grab',
                                 TRUE ~ sample_type)) %>%
  # correct plot IDs
  mutate(plotid = case_when(siteid == 'FAIRM' & plotid == 'Sump1' ~ 'West',
                            siteid == 'FAIRM' & plotid == 'Sump2' ~ 'East',
                            siteid == 'ACRE' & str_detect(location, 'Inlet') ~ location,
                            TRUE ~ plotid)) %>%
  # correct measurement units
  mutate(value_temp = as.numeric(value),
         # this is to fix P concentration at WRSIS
         value_temp = ifelse(str_detect(var_NEW, '^WAT4'), value_temp * 1000, value_temp),
         value = ifelse(siteid %in% c('DEFI_R', 'FULTON', 'VANWERT') & 
                          str_detect(var_NEW, '^WAT4') & 
                          !is.na(value_temp),
                        as.character(value_temp),
                        value)) %>%
  # standartize BDLs
  mutate(comments = ifelse(value == 'no water', value, comments),
         value = ifelse(value == 'no water', NA_character_, value),    # at WILKIN3
         value = ifelse(value == 'NV', NA_character_, value),          # at WRSIS
         comments = str_replace_all(comments, 'changed to .* from', 'changed to NA from')) %>%
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
                         siteid %in% c('HICKS_B', ' WILKIN1', ' WILKIN2') ~ tmsp + hours(6)),
         UTC = format(UTC, '%Y-%m-%dT%H:%M:%S+00:00')) %>%
  select(siteid, plotid, location, height, date, time, UTC, timestamp_type,
         sample_type, var_NEW, value, comments) %>%
  ungroup() -> temp


temp %>%
  filter(is.na(date)) %>% View() 


