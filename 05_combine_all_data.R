# Load the RSQLite Library
library(RSQLite)
library(tidyverse)
library(lubridate)
library(janitor)



# Create a connection to a new database for storing all data
conn <- dbConnect(RSQLite::SQLite(), 'Final_Database//TD_ALL_Data.db')
conn_final <- dbConnect(RSQLite::SQLite(), 'Final_Database//TD_FINAL_Data.db')



# Keys
tribble(
  ~ table_name, ~var_code, ~var_name, ~var_units,
  
  'soil_moisture',   'SOIL09',   'soil_moisture_content',                 'cm3/cm3',
  'soil_moisture',   'SOIL10',   'soil_temperature',                      'degree C',
  'soil_moisture',   'SOIL12',   'soil_bulk_electrical_conductivity',     'dS/m',
  
  'soil_properties', 'SOIL01',       'soil_texture',                      '',
  'soil_properties', 'SOIL02.01',    'precent_sand',                      '%',
  'soil_properties', 'SOIL02.02',    'precent_silt',                      '%',
  'soil_properties', 'SOIL02.03',    'precent_clay',                      '%',
  'soil_properties', 'SOIL03',       'soil_bulk_density',                 'g/cm3',
  'soil_properties', 'SOIL06.01',    'saturated_hydraulic_conductivity',  'cm/hr',
  'soil_properties', 'SOIL06.02',    'infiltration_rate',                 'cm/hr',
  'soil_properties', 'SOIL08.01',    'matric_potential',                  'bar',
  'soil_properties', 'SOIL08.02',    'water_content',                     'cm3/cm3',
  'soil_properties', 'SOIL15',       'soil_organic_matter',               'g/kg',
  'soil_properties', 'SOIL20.03',    'soil_pH_water',                     '',
  'soil_properties', 'SOIL20.07',    'soil_pH_salt',                      '',
  'soil_properties', 'SOIL21.01',    'lime_test_index',                   '',
  'soil_properties', 'SOIL21.02',    'neutralizable_acidity',             'cmol/kg',
  'soil_properties', 'SOIL22',       'cation_exchange_capacity',          'cmol/kg',
  'soil_properties', 'SOIL23.01',    'K_saturation',                      '%',
  'soil_properties', 'SOIL23.02',    'Ca_saturation',                     '%',
  'soil_properties', 'SOIL23.03',    'Mg_saturation',                     '%',
  'soil_properties', 'SOIL24.01',    'K_content',                         'g/kg',
  'soil_properties', 'SOIL24.02',    'Ca_content',                        'g/kg',
  'soil_properties', 'SOIL24.03',    'Mg_content',                        'g/kg',
  'soil_properties', 'SOIL25.01',    'K_amount',                          'kg/ha',
  'soil_properties', 'SOIL25.02',    'Ca_amount',                         'kg/ha',
  'soil_properties', 'SOIL25.03',    'Mg_amount',                         'kg/ha',
  'soil_properties', 'SOIL26',       'sodium_adsorption_ratio',           '',
  'soil_properties', 'SOIL27.02',    'soil_salinity_in_saturated_paste_extract', 'dS/m',
  'soil_properties', 'SOIL27.03',    'soil_salinity_in_water',            'dS/m',
  'soil_properties', 'SOIL30.01',    'soil_organic_carbon',     '%',   # <- THIS - units to change to g/kg?
  'soil_properties', 'SOIL32.01',    'soil_total_nitrogen',     '%',   # <- THIS - units to change to g/kg?
  'soil_properties', 'SOIL32.04',    'soil_nitrate_content',              'mg N/kg',
  'soil_properties', 'SOIL32.05',    'soil_ammonium_content',             'mg N/kg',
  'soil_properties', 'SOIL33.04',    'soil_nitrate_amount',               'kg N/ha',
  'soil_properties', 'SOIL33.05',    'soil_ammonium_amount',              'kg N/ha',
  'soil_properties', 'SOIL34.01',    'soil_phosphorus_content',           'ug P/kg',
  'soil_properties', 'SOIL34.01.01', 'soil_phosphorus_content_Bray_1',    'ug P/kg',
  'soil_properties', 'SOIL34.01.04', 'soil_phosphorus_content_Mehlich_3', 'ug P/kg',
  'soil_properties', 'SOIL35.01.01', 'soil_phosphorus_amount_Bray_1',     'kg P/ha',
  'soil_properties', 'SOIL35.01.04', 'soil_phosphorus_amount_Mehlich_3',  'kg P/ha',
  
  'weather_daily', 'CLIM01',       'Precipitation',       'mm',
  'weather_daily', 'CLIM03.01',    'Air Temperature',     'degree C',
  'weather_daily', 'CLIM04.01',    'Relative Humidity',   '%',
  'weather_daily', 'CLIM05.01',    'Solar Irradiance',    'W/m2',
  'weather_daily', 'CLIM06.01',    'Wind Speed',          'm/s',
  'weather_daily', 'CLIM06.02',    'Wind Direction',      'degree',
  'weather_daily', 'CLIM06.03',    'Wind Gust',           'm/s',
  
  'weather_hourly', 'CLIM01',       'Precipitation',                           'mm',
  'weather_hourly', 'CLIM01.02',    'Snowfall',                                'mm',
  'weather_hourly', 'CLIM03.01.01', 'Ave Air Temperature',                     'degree C',
  'weather_hourly', 'CLIM03.01.02', 'Min Air Temperature',                     'degree C',
  'weather_hourly', 'CLIM03.01.03', 'Max Air Temperature',                     'degree C',
  'weather_hourly', 'CLIM03.02',    'Dew-Point Temperature',                   'degree C',
  'weather_hourly', 'CLIM04.01.01', 'Ave Relative Humidity',                   '%',
  'weather_hourly', 'CLIM04.01.02', 'Min Relative Humidity',                   '%',
  'weather_hourly', 'CLIM04.01.03', 'Max Relative Humidity',                   '%',
  'weather_hourly', 'CLIM05.01.01', 'Ave Solar Irradiance',                    'W/m2',
  'weather_hourly', 'CLIM05.01.02', 'Min Solar Irradiance',                    'W/m2',
  'weather_hourly', 'CLIM05.01.03', 'Max Solar Irradiance',                    'W/m2',
  'weather_hourly', 'CLIM05.02',    'Solar Radiation',                         'MJ/m2',
  'weather_hourly', 'CLIM05.03',    'Photosynthetically Active Radiation',     'micromol/s m2',
  'weather_hourly', 'CLIM06.01',    'Wind Speed',                              'm/s',
  'weather_hourly', 'CLIM06.02',    'Wind Direction',                          'degree',
  'weather_hourly', 'CLIM06.03',    'Wind Gust',                               'm/s',
  'weather_hourly', 'CLIM06.04',    'Wind Run',                                'km',
  'weather_hourly', 'CLIM06.05',    'Max Wind Speed',                          'm/s',
  'weather_hourly', 'CLIM07.01',    'Pan Evaporation',                         'mm',
  'weather_hourly', 'CLIM07.02',    'Reference ET',                            'mm',
  'weather_hourly', 'CLIM07.02.01', 'Reference ET (Penman-Monteith)',          'mm',
  'weather_hourly', 'CLIM07.02.02', 'Reference ET (Short Grass)',              'mm',
  'weather_hourly', 'CLIM07.02.03', 'Reference ET (Tall Grass)',               'mm',
  'weather_hourly', 'CLIM07.02.04', 'Reference ET (Short Crop)',               'mm',
  'weather_hourly', 'CLIM08.01.01', 'Ave Bare Soil Temperature (10 cm depth)', 'degree C',
  'weather_hourly', 'CLIM08.01.02', 'Min Bare Soil Temperature (10 cm depth)', 'degree C',
  'weather_hourly', 'CLIM08.01.03', 'Max Bare Soil Temperature (10 cm depth)', 'degree C',
  'weather_hourly', 'CLIM08.02.02', 'Min Bare Soil Temperature (2" depth)',    'degree C',
  'weather_hourly', 'CLIM08.02.03', 'Max Bare Soil Temperature (2" depth)',    'degree C',
  'weather_hourly', 'CLIM08.03.02', 'Min Bare Soil Temperature (4" depth)',    'degree C',
  'weather_hourly', 'CLIM08.03.03', 'Max Bare Soil Temperature (4" depth)',    'degree C',
  'weather_hourly', 'CLIM08.04.02', 'Min Bare Soil Temperature (8" depth)',    'degree C',
  'weather_hourly', 'CLIM08.04.03', 'Max Bare Soil Temperature (8" depth)',    'degree C',
  
  'water_table',    'WAT01', 'Water Table Depth',     'm',
  'water_table',    'WAT02', 'Groundwater Level',     'm',
  'water_table',    'WAT03', 'Piezometric Head',      'm',
  
  'water_stage',    'WAT04', 'Stage',                 'm',
  'water_stage',    'WAT14', 'Water Storage',         'm3'
  
  )




# Water Table and Stage Data ----------------------------------------------

# Water table, water level and piezometric head data
read_rds('Output_Data/water_table_daily_ALL.rds') -> wt_daily
read_rds('Output_Data/water_table_hourly_ALL.rds') -> wt_hourly

bind_rows(wt_daily, wt_hourly) %>%
  select(siteid, plotid, location, reading_type, date, time, UTC, timestamp_type, tmsp,
         WAT01, WAT02, WAT03) -> wt

wt %>%
  mutate(date = format(date, '%Y-%m-%d')) -> 
  wt_DB

dbWriteTable(conn, "water_table", wt_DB)


# Save selected data (variables) for FINAL DB. > NOTE: only DAILY values goes to the FINAL DB
wt_DB %>%
  # calculate daily data
  group_by(siteid, plotid, location, reading_type, date, timestamp_type) %>%
  summarise(WAT01 = mean(WAT01, na.rm = TRUE)) %>%
  arrange(siteid, plotid, location, reading_type, date) %>%
  ungroup() %>%
  select(-timestamp_type) %>%
  # remove empty rows resulted from other (not WAT01) variables
  group_by(siteid, plotid, location, reading_type) %>% 
  mutate(CHECK = sum(!is.na(WAT01))) %>%
  filter(CHECK != 0) %>%
  ungroup() %>%
  select(-CHECK) ->
  wt_FINAL_DB

dbWriteTable(conn_final, "water_table", wt_FINAL_DB)


# Stage and water storage data
read_rds('Output_Data/stage_hourly_ALL.rds') -> stage_hourly

stage_hourly %>%
  mutate(date = format(date, '%Y-%m-%d')) %>%
  select(siteid, plotid, location, reading_type, date, time, UTC, timestamp_type, tmsp,
         WAT04, WAT14) ->
  stage_hourly_DB

dbWriteTable(conn, "water_stage", stage_hourly_DB)


# Save selected data (variables) for FINAL DB. > NOTE: only DAILY values goes to the FINAL DB
stage_hourly_DB %>%
  # calculate daily data
  group_by(siteid, plotid, location, reading_type, date) %>%
  summarise(WAT04 = mean(WAT04, na.rm = TRUE)) %>%
  arrange(siteid, plotid, location, reading_type, date) %>%
  ungroup() ->
  stage_FINAL_DB

dbWriteTable(conn_final, "water_stage", stage_FINAL_DB)


rm(wt, wt_daily, wt_hourly, wt_DB, wt_FINAL_DB,
   stage_hourly, stage_hourly_DB, stage_FINAL_DB)



# Weather Data ------------------------------------------------------------

# Hourly weather data
read_rds('Output_Data/weather_hourly_all_variable.rds') -> weather_hourly

weather_hourly %>% 
  mutate(date = format(date, '%Y-%m-%d')) ->
  weather_hourly_DB

dbWriteTable(conn, "weather_hourly", weather_hourly_DB)


# Daily weather data
read_rds('Output_Data/weather_daily_all_variable.rds') -> weather_daily

weather_daily %>% 
  mutate(date = format(date, '%Y-%m-%d')) ->
  weather_daily_DB

dbWriteTable(conn, "weather_daily", weather_daily_DB)


# Save selected data (variables) for FINAL DB. > NOTE: only DAILY WEATHER goes to the FINAL DB
weather_daily %>%
  select(siteid:date, CLIM01, CLIM03.01.01, CLIM03.01.02, CLIM03.01.03,
         CLIM04.01.01, CLIM05.02, CLIM06.01, CLIM06.02, starts_with('CLIM07.02')) %>%
  # remove erroneous readings from WRSIS sites
  mutate(CLIM05.02 = case_when(siteid == 'DEFI_R' & year(date) %in% 2003:2006 ~ NA_real_,
                               siteid == 'FULTON' & year(date) %in% 2004:2006 ~ NA_real_,
                               siteid == 'VANWERT' ~ NA_real_,
                               TRUE ~ CLIM05.02)) %>%
  arrange(siteid, station, date) %>%
  mutate(date = as.character(date)) ->
  weather_daily_FINAL_DB

dbWriteTable(conn_final, "weather", weather_daily_FINAL_DB, overwrite = TRUE)



# Soil Moisture, Temperature and EC ---------------------------------------

read_rds('Inter_Data/sm_ALL_hourly_ORIGINAL.rds') -> sm

sm %>%
  mutate(date = format(date, '%Y-%m-%d')) -> 
  sm_DB

dbWriteTable(conn, "soil_moisture", sm_DB)


# calculate daily data
sm_DB %>%
  filter(!(siteid == 'HICKS_B' & timestamp_type == 'I' & year(tmsp) > 2013)) %>%
  group_by(siteid, plotid, location, depth, date, timestamp_type) %>%
  summarise(SOIL09 = mean(SOIL09, na.rm = TRUE),
            SOIL10 = mean(SOIL10, na.rm = TRUE),
            SOIL12 = mean(SOIL12, na.rm = TRUE)) %>%
  arrange(siteid, plotid, location, depth, date) %>%
  ungroup() %>%
  mutate(location = ifelse(str_detect(siteid, 'CLAY'), str_sub(location, 4, 5), location)) %>%
  select(-timestamp_type) ->
  sm_FINAL_DB

dbWriteTable(conn_final, "soil_moisture", sm_FINAL_DB)

rm(sm_DB, sm_FINAL_DB)



# Soil Properties Data ----------------------------------------------------

read_rds('Output_Data/soil_properties_ALL.rds') -> soil_properties

soil_properties %>%
  mutate(year = as.integer(year),
         date = as.character(date)) ->
  soil_properties_DB

dbWriteTable(conn, "soil_properties", soil_properties_DB, overwrite = TRUE)

# Save selected data (variables) for FINAL DB
soil_properties %>%
  rownames_to_column() %>%
  # remove variables that are measures < 3 sites
  select(-SOIL04, -SOIL07, 
         -SOIL05.01, -SOIL05.02,
         -SOIL20.02,
         -SOIL23.05,
         -SOIL23.04, -SOIL24.04, 
         -SOIL31.02, -SOIL32.03) %>%
  filter_at(vars(starts_with('SOIL')), any_vars(!is.na(.))) -> temp

soil_properties %>%
  rownames_to_column() %>%
  # remove variables that are measures < 3 sites
  select(-SOIL04, -SOIL07, 
         -SOIL05.01, -SOIL05.02,
         -SOIL20.02,
         -SOIL23.05,
         -SOIL23.04, -SOIL24.04, 
         -SOIL31.02, -SOIL32.03) %>%
  filter_at(vars(starts_with('SOIL')), all_vars(is.na(.))) %>%
  filter(!siteid %in% c('ACRE', 'CLAY_R', 'DPAC', 'FAIRM', 'MUDS1', 'SERF_SD')) %>%
  filter(!(siteid == 'STJOHNS' & year == 2013 & depth %in% c('20 to 40 cm', '40 to 60 cm'))) %>%
  bind_rows(temp) %>%
  arrange(as.numeric(rowname)) %>%
  select(-rowname) %>%
  arrange(siteid, plotid, location, subsample, depth, year, date) %>%
  ungroup() %>%
  mutate(year = as.integer(year),
         date = as.character(date)) ->
  soil_properties_FINAL_DB

dbWriteTable(conn_final, "soil_properties", soil_properties_FINAL_DB, overwrite = TRUE)

