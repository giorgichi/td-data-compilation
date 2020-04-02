# This script compiles finilized (clean) tile flow and discharge data of hieghest temporal resolution
# for future preservation.
# This data does not goes to the repository, but rather kept locally at ISU Agronomy server and Gio's desktop

# NOTE: this scrips should be run after 01_read_tile_flow_data.R without cleaning its environment!



# Combine all discharge data ----------------------------------------------

# Original discharge data are all at hourly or sub-hourly intervals, 
# but units varies from m3/h to gpm.
ALL_Discharge <-
  bind_rows(tf_ACRE_hourly,
            tf_BEAR_hourly,
            tf_BEAR2_hourly,
            tf_BENTON_hourly,
            tf_DIKE_hourly,
            tf_HICKORY_hourly,
            tf_MAASS_hourly,
            tf_SHEARER_hourly,
            tf_WILKIN3_hourly,
            tf_DEFI_R_houly,
            tf_FULTON_houly,
            tf_VANWERT_houly) %>%
  mutate(var_NEW = 'WAT05',
         var_name = 'Discharge',
         # convert to standard units
         value = ifelse(siteid %in% c('BEAR', 'BEAR2', 'BENTON', 'DIKE', 
                                      'HICKORY', 'MAASS', 'SHEARER', 'WILKIN3'),
                        value * 0.2271247, 
                        value),
         units = 'm3/h') %>%
  # standardize timestamp
  mutate(timestamp_type = 'I',
         date = as_date(tmsp),
         time = format(tmsp, '%H:%M'), 
         UTC = case_when(siteid %in% c('ACRE', 'DEFI_R', 'FULTON', 'VANWERT') ~ tmsp + hours(5),
                         siteid %in% c('BEAR', 'BEAR2', 'BENTON', 'DIKE', 'HICKORY', 'MAASS',
                                       'SHEARER', 'WILKIN3') ~ tmsp + hours(6)),
         # format according to ISO 8601 standard
         UTC = format(UTC, '%Y-%m-%dT%H:%M:%S+00:00')) %>%
  select(siteid, plotid, location, date, time, UTC, timestamp_type, 
         var_NEW, var_name, value, units, tmsp)



# Combine all daily tile flow data ----------------------------------------
DAILY_Tile_Flow <-
  bind_rows(tf_AUGLA_daily,
            tf_CRAWF_daily,
            tf_DEFI_M_daily,
            tf_DPAC_daily,  # for 2012-2017 overlaps with hourly data
            tf_FAIRM_daily,
            tf_HARDIN_NW_daily,
            tf_HENRY_daily,
            tf_HICKS_B_daily,
            tf_MUDS2_daily,
            tf_MUDS3_OLD_daily,
            tf_MUDS4_daily,
            tf_STORY_daily,
            tf_SWROC_daily,
            tf_TIDE_daily,
            tf_UBWC_daily) %>%
  mutate(location = NA_character_, 
         time = NA_character_,
         UTC = NA_character_,
         timestamp_type = 'D',
         var_NEW = 'WAT06',
         var_name = 'Tile Flow',
         units = 'mm') %>% 
  select(siteid, plotid, location, date, time, UTC, timestamp_type, 
         var_NEW, var_name, value, units)



# Combine all hourly tile flow data ---------------------------------------
HOURLY_Tile_Flow <-
  bind_rows(tf_CLAY_C_hourly,
            tf_CLAY_R_hourly,
            tf_DPAC_hourly,     # for 2012-2017 overlaps with daily data
            tf_HARDIN_hourly,   # NOTE this contains both DAILY and HOURLY data
            tf_MUDS3_NEW_hourly,
            tf_SERF_IA_hourly,
            tf_SERF_SD_hourly,  # NEET TO look for final version in my laptop
            tf_STJOHNS_hourly,
            tf_WILKIN1_hourly,
            tf_WILKIN2_hourly) %>%
  mutate(var_NEW = 'WAT06',
         var_name = 'Tile Flow',
         location = NA_character_,
         units = 'mm') %>%
  # standardize timestamp
  mutate(timestamp_type = ifelse(siteid == 'HARDIN' & is.na(tmsp), 'D', 'I')) %>%
  mutate(date = ifelse(timestamp_type == 'I', as_date(tmsp), date),
         date = as_date(date),
         time = ifelse(timestamp_type == 'I', format(tmsp, '%H:%M'), NA_character_), 
         UTC = case_when(siteid %in% c('DPAC', 'HARDIN', 'STJOHNS') ~ tmsp + hours(5),
                         siteid %in% c('CLAY_C', 'CLAY_R', 'SERF_IA', 'SERF_SD', 'MUDS3_NEW',
                                       'WILKIN1', 'WILKIN2') ~ tmsp + hours(6)),
         # format according to ISO 8601 standard
         UTC = format(UTC, '%Y-%m-%dT%H:%M:%S+00:00')) %>%
  select(siteid, plotid, location, date, time, UTC, timestamp_type, 
         var_NEW, var_name, value, units, tmsp)

  

# Combine and Standardize tile flow and discharge data
bind_rows(ALL_Discharge, HOURLY_Tile_Flow, DAILY_Tile_Flow) %>%
  # standardized plotids and locations
  mutate(location = case_when(siteid %in% c('BEAR', 'BEAR2', 'BENTON', 'DIKE', 
                                            'HICKORY', 'MAASS', 'SHEARER', 'WILKIN3') ~ plotid,
                              siteid == 'ACRE' & plotid == 'Outlet' ~ plotid,
                              siteid == 'DEFI_R' & location == 'L' ~ 'Offsite',
                              siteid == 'DEFI_R' & location == 'O' ~ 'Wetland Out',
                              siteid == 'FULTON' & location == 'A' ~ 'Wetland In',
                              siteid == 'FULTON' & location == 'B' ~ 'Wetland Out',
                              siteid == 'FULTON' & location == 'C' ~ 'Stream',
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
  arrange(siteid) %>%
  ungroup() -> tf_ORIGINAL_hourly_standard



# Save standardized data --------------------------------------------------
tf_ORIGINAL_hourly_standard %>%
  write_rds('Standard_Data/tf_ALL_hourly_ORIGINAL.rds', compress = 'xz')

# after saving it was gzip-ed via shell command line  
tf_ORIGINAL_hourly_standard %>%
  write_csv('Standard_Data/CSV/tile_flow_and_discharge_all_ORIGINAL.csv')


