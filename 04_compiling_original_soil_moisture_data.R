# This script compiles finilized (clean) soil moisture and temperature (and EC) data of hieghest temporal resolution
# for future preservation.
# This data does not goes to the repository, but rather kept locally at ISU Agronomy server and Gio's desktop

# NOTE: this scrips should be run after 01_read_soil_moisture_data.R without cleaning its environment!



# Combine all hourly soil sensor data -------------------------------------
bind_rows(soil_ACRE_sm,
          soil_BEAR_sm,
          soil_BENTON_sm,
          soil_CLAY_R_sm,
          soil_CLAY_U_sm,
          soil_DIKE_sm,
          soil_FAIRM_sm,
          soil_MAASS_sm,
          soil_DPAC_sm,
          soil_HICKS_B_sm,
          soil_SERF_IA_sm,
          soil_STJOHNS_sm) %>%
  # standardize timestamp
  mutate(timestamp_type = 'I',
         date = as_date(tmsp),
         time = format(tmsp, '%H:%M'), 
         UTC = case_when(siteid %in% c('ACRE', 'DPAC', 'STJOHNS') ~ tmsp + hours(5),
                         TRUE ~ tmsp + hours(6)),
         # format according to ISO 8601 standard
         UTC = format(UTC, '%Y-%m-%dT%H:%M:%S+00:00')) %>%
  select(siteid, plotid, location, depth, date, time, UTC, timestamp_type, 
         soil_moisture, soil_temp, soil_ec, tmsp) -> HOURLY_sm
  

# Combine all hourly soil sensor data -------------------------------------
bind_rows(soil_HICKS_B_daily_sm,
          soil_HICKS_P_daily_sm,
          soil_SERF_SD_daily_sm,
          soil_SWROC_daily_sm) %>%
  # standardize timestamp
  mutate(timestamp_type = 'D',
         time = NA_character_, 
         UTC = NA_character_) %>%
  select(siteid, plotid, location, depth, date, time, UTC, timestamp_type, 
         soil_moisture) -> DAILY_sm



# Combine daily and hourly data -------------------------------------------
bind_rows(HOURLY_sm, DAILY_sm) %>%
  write_rds('Inter_Data/sm_ALL_hourly_ORIGINAL.rds')

# bind_rows(ALL_Discharge, HOURLY_Tile_Flow, DAILY_Tile_Flow) %>%
#   select(-tmsp) %>%
#   write.csv('Output_Data/tile_flow_and_discharge_all_ORIGINAL.csv')
