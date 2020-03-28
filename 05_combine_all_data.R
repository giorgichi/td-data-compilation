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
  
  'water_table',    'WAT01', 'Water Table Depth',          'm',
  'water_table',    'WAT02', 'Groundwater Level',          'm',
  'water_table',    'WAT03', 'Piezometric Head',           'm',
  
  'water_stage',    'WAT04', 'Stage',                      'm',
  'water_stage',    'WAT14', 'Water Storage',              'm3',
  
  
  'water_quality',  'WAT15', 'Water Electrical Conductivity',                             'uS/cm',
  'water_quality',  'WAT16', 'Water Temperature',                                         'degree C',
  'water_quality',  'WAT20', 'Turbidity',                                                 'NTU',
  'water_quality',  'WAT22', 'Total Suspended Solids',                                    'mg/l',
  'water_quality',  'WAT23', 'Total Filterable Solids',                                   'mg/l',
  'water_quality',  'WAT30', 'Nitrate-Nitrite (NO3-N + NO2-N) Concentration',             'mg N/l',
  'water_quality',  'WAT31', 'Nitrate (NO3-N) Concentration',                             'mg N/l',
  'water_quality',  'WAT33', 'Ammonia (NH3-N) Concentration',                             'mg N/l',
  'water_quality',  'WAT34', 'Total N Concentration (filtered)',                          'mg N/l',
  'water_quality',  'WAT35', 'Total N Concentration (unfiltered)',                        'mg N/l',
  'water_quality',  'WAT36', 'Total Kjeldhal N Concentration',                            'mg N/l',
  'water_quality',  'WAT37', 'Organic N Concentration',                                   'mg N/l',
  'water_quality',  'WAT38', 'Particulate N Concentration',                               'mg N/l',
  'water_quality',  'WAT40', 'Dissolved Reactive P (ortho-P) Concentration (filtered)',   'ug P/l',
  'water_quality',  'WAT41', 'Reactive P (ortho-P) Concentration (unfiltered)',           'ug P/l',
  'water_quality',  'WAT42', 'Total Dissolved P Concentration (filtered)',                'ug P/l',
  'water_quality',  'WAT43', 'Total P Concentration (unfiltered)',                        'ug P/l',
  'water_quality',  'WAT44', 'Organic P Concentration',                                   'ug P/l',
  'water_quality',  'WAT45', 'Particulate P Concentration',                               'ug P/l',
  'water_quality',  'WAT50', 'Dissolved Organic Carbon (DOC) (filtered)',                 'mg C/l',
  'water_quality',  'WAT51', 'Total Organic Carbon (TOC) (unfiltered)',                   'mg C/l',
  'water_quality',  'WAT52', 'Inorganic Carbon (filtered)',                               'mg C/l',
  'water_quality',  'WAT60', 'Water pH',                                                  ''
  
  )



# Agronomic Data ----------------------------------------------------------

read_rds('Output_Data/agro_ALL.rds') -> agr

agr %>%
  # standardize plot and locations names for FAIRM
  mutate(location = ifelse(siteid == 'FAIRM' & plotid == 'CD/SI', 'East & West plots', location),
         plotid = ifelse(siteid == 'FAIRM' & plotid == 'CD/SI', 'SI', plotid),
         plotid = ifelse(siteid == 'FAIRM' & str_detect(plotid, 'CD'), word(plotid, 2), plotid)) %>%
  mutate(year = as.integer(year),
         date = as.character(date)) %>%
  spread(var_NEW, value) -> agr_DB

dbWriteTable(conn, "agronomic", agr_DB, overwrite = TRUE)
  

# Save selected data (variables) for FINAL DB.
agr_DB %>%
  # select plots and locations of interest
  mutate(action = ifelse(is.na(action), 'keep', action)) %>%
  filter(action != 'remove') %>%
  # remove plots that are under more than one drainage system
  filter(plotid != 'Inlet_A, Inlet_B') %>%
  # select variables of high value, quality and abundance 
  select(-action, -harvested_area, -AGR90.01.10) %>%
  # aggregate Ease and West into SI at FAIRM
  mutate(location = ifelse(siteid == 'FAIRM', NA_character_, location),
         AGR20.01.60 = ifelse(siteid == 'FAIRM' & plotid == 'East', '9675.74', AGR20.01.60),
         plotid = ifelse(siteid == 'FAIRM' & plotid == 'East', 'SI', plotid)) %>% 
  filter(!(siteid == 'FAIRM' & plotid == 'West')) %>%
  mutate(year = as.integer(year),
         date = as.character(date)) -> agr_FINAL_DB

dbWriteTable(conn_final, "agronomic", agr_FINAL_DB, overwrite = TRUE)



# Water Quality Data ------------------------------------------------------
read_rds('Output_Data/wq_ALL.rds') -> wq_hourly

wq_hourly %>%
  spread(var_NEW, value) %>%
  mutate(date = format(date, '%Y-%m-%d')) %>%
  select(siteid, plotid, location, height, subsample, sample_type, tmsp, date, time, UTC, timestamp_type,
         starts_with('WAT'), comments) -> wq_DB

dbWriteTable(conn, "water_quality", wq_DB)



# Save selected data (variables) for FINAL DB. > NOTE: only DAILY values goes to the FINAL DB
wq_hourly %>%
  # select variables of high value, quality and abundance 
  filter(var_NEW %in% c('WAT15',
                        'WAT30', 'WAT31', 'WAT33', 'WAT34', 'WAT35',
                        'WAT40', 'WAT41', 'WAT42', 'WAT43',
                        'WAT60')) %>%
  select(-comments, -UTC, -tmsp) %>%
  # calculate daily values
  mutate(value_parse = parse_number(value)) %>%
  # FIRST - average replicated measurements
  mutate(subsample = ifelse(is.na(subsample), 1, subsample)) %>%
  filter(!(subsample > 1 & value =='BDL')) %>%
  group_by(siteid, plotid, location, height, date, time, timestamp_type, sample_type, var_NEW) %>%
  summarise(value = first(value),
            subsample = max(subsample),
            value_parse = mean(value_parse, na.rm = TRUE)) %>%
  mutate(value = ifelse(subsample > 1 & value != 'BDL', as.character(round(value_parse, 4)), value)) %>%
  ungroup() %>%
  select(-subsample) %>%
  # SECOND - average over day
  mutate(value_temp = as.numeric(value),
         value_BDL = ifelse(value == 'BDL', 1, 0),
         value_LES = ifelse(str_detect(value, '<'), 1, 0)) %>%
  group_by(siteid, plotid, location, height, date, timestamp_type, sample_type, var_NEW) %>%
  summarise(value = first(value),
            value_parse = mean(value_parse, na.rm = TRUE),
            value_temp = mean(value_temp),
            value_BDL = max(value_BDL),
            value_LES = max(value_LES)) %>% 
  ungroup() %>%
  # THIRD - substitude average of BDLs with BDL for HOURLY data
  mutate(value_parse = ifelse(is.nan(value_parse), NA_real_, round(value_parse, 5)),
         value = ifelse(timestamp_type == 'I', as.character(value_parse), value),
         value = ifelse(timestamp_type == 'I' & value_BDL == 1 & is.na(value), 'BDL', value)) %>%
  mutate(date = format(date, '%Y-%m-%d')) %>%
  select(siteid, plotid, location, height, date, sample_type, var_NEW, value) %>%
  spread(var_NEW, value) -> wq_FINAL_DB


dbWriteTable(conn_final, "water_quality", wq_FINAL_DB)



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
  mutate(plotid = ifelse(siteid == 'FAIRM' & plotid == 'CD/SI', 'SI', plotid)) %>%
  mutate(year = as.integer(year),
         date = as.character(date)) ->
  soil_properties_DB

dbWriteTable(conn, "soil_properties", soil_properties_DB, overwrite = TRUE)

# Save selected data (variables) for FINAL DB
soil_properties %>%
  mutate(plotid = ifelse(siteid == 'FAIRM' & plotid == 'CD/SI', 'SI', plotid)) %>%
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
  # remove plots that are outside of research boundaries or shared by drainage areas at ACRE 
  filter(!(siteid == 'ACRE' & is.na(plotid))) %>%
  filter(!(siteid == 'ACRE' & str_detect(location, '22-E'))) %>%
  mutate(year = as.integer(year),
         date = as.character(date)) ->
  soil_properties_FINAL_DB

dbWriteTable(conn_final, "soil_properties", soil_properties_FINAL_DB, overwrite = TRUE)

