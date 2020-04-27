# Load the RSQLite Library
library(RSQLite)
library(tidyverse)
library(lubridate)
library(janitor)



# Create a connection to a new database for storing all data
conn <- dbConnect(RSQLite::SQLite(), 'Final_Database//TD_ALL_Data.db')
conn_final <- dbConnect(RSQLite::SQLite(), 'Final_Database//TD_FINAL_Data.db')



# Field Management --------------------------------------------------------

write_rds(dwm_standard, 'Standard_Data/dwm_ALL.rds', compress = 'xz')
write_rds(fertilizing_standard, 'Standard_Data/fertilizing_ALL.rds', compress = 'xz')
write_rds(irrigation_standard, 'Standard_Data/irrigation_ALL.rds', compress = 'xz')
write_rds(notes_standard, 'Standard_Data/notes_ALL.rds', compress = 'xz')
write_rds(planting_standard, 'Standard_Data/planting_ALL.rds', compress = 'xz')



# Agronomic Data ----------------------------------------------------------

read_rds('Standard_Data/agro_ALL.rds') -> agr

agr %>%
  mutate(year = as.integer(year),
         date = as.character(date)) %>%
  spread(var_NEW, value) -> agr_DB

dbWriteTable(conn, "agronomic", agr_DB, overwrite = TRUE)
  

# count sites per variable
agr_DB %>%
  gather(code, value, starts_with('AGR')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         CROP, 
         VARIABLE_NAME = NEW_VAR_NAME,
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/agr_variable_count.csv')


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
read_rds('Standard_Data/wq_ALL.rds') -> wq_hourly

wq_hourly %>%
  spread(var_NEW, value) %>%
  mutate(date = format(date, '%Y-%m-%d')) %>%
  select(siteid, plotid, location, height, subsample, sample_type, tmsp, date, time, UTC, timestamp_type,
         starts_with('WAT'), comments) -> wq_DB

dbWriteTable(conn, "water_quality", wq_DB, overwrite = TRUE)


# count sites per variable
wq_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/wq_variable_count.csv')


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


dbWriteTable(conn_final, "water_quality", wq_FINAL_DB, overwrite = TRUE)



# Tile Flow ---------------------------------------------------------------
read_rds('Standard_Data/tf_ALL_hourly_ORIGINAL.rds') -> tf

tf %>%
  select(-units, -var_name) %>%
  spread(var_NEW, value) %>%
  mutate(date = format(date, '%Y-%m-%d')) ->
  tf_DB

dbWriteTable(conn, "tile_flow", tf_DB)


# read daily data
read_rds('Standard_Data/tf_ALL_daily.rds') -> tf_daily

tf_daily %>%
  spread(var_NEW, value) ->
  tf_FINAL_DB

dbWriteTable(conn_final, "tile_flow", tf_FINAL_DB)


# count sites per variable
tf_FINAL_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/tf_variable_count.csv')



# Irrigation ---------------------------------------------------------------
read_rds('Standard_Data/irr_ALL_daily.rds') -> irr

irr %>%
  spread(var_NEW, value) %>%
  remove_empty(which = 'cols') %>%
  mutate(date = format(date, '%Y-%m-%d')) ->
  irr_DB

dbWriteTable(conn, "irrigation", irr_DB)


# save daily data
irr_DB -> irr_FINAL_DB

dbWriteTable(conn_final, "irrigation", irr_FINAL_DB)


# count sites per variable
irr_FINAL_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/irr_variable_count.csv')




# N Loads ---------------------------------------------------------------
read_rds('Standard_Data/nl_ALL_daily.rds') -> nl

nl %>%
  spread(var_NEW, value) %>%
  remove_empty("cols") %>%
  mutate(date = format(date, '%Y-%m-%d')) ->
  nl_DB

dbWriteTable(conn, "n_loads", nl_DB)


# save daily data
nl_DB -> nl_FINAL_DB

dbWriteTable(conn_final, "n_loads", nl_FINAL_DB)


# count sites per variable
nl_FINAL_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/nl_variable_count.csv')




# Water Table and Stage Data ----------------------------------------------

# Water table, water level and piezometric head data
read_rds('Standard_Data/water_table_daily_ALL.rds') -> wt_daily
read_rds('Standard_Data/water_table_hourly_ALL.rds') -> wt_hourly

bind_rows(wt_daily, wt_hourly) %>%
  select(siteid, plotid, location, reading_type, date, time, UTC, timestamp_type, tmsp,
         WAT01, WAT02, WAT03) -> wt

wt %>%
  mutate(date = format(date, '%Y-%m-%d')) -> 
  wt_DB

dbWriteTable(conn, "water_table", wt_DB)


# count sites per variable
wt_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/wt_variable_count.csv')



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
  # replace NaN with NAs
  mutate(WAT01 = ifelse(is.nan(WAT01), NA_real_, WAT01)) %>%
  select(-CHECK) ->
  wt_FINAL_DB

dbWriteTable(conn_final, "water_table", wt_FINAL_DB)


# Stage and water storage data
read_rds('Standard_Data/stage_hourly_ALL.rds') -> stage_hourly

stage_hourly %>%
  mutate(date = format(date, '%Y-%m-%d')) %>%
  select(siteid, plotid, location, reading_type, date, time, UTC, timestamp_type, tmsp,
         WAT04, WAT14) ->
  stage_hourly_DB

dbWriteTable(conn, "water_stage", stage_hourly_DB)


# count sites per variable
stage_hourly_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/st_variable_count.csv')


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
read_rds('Standard_Data/weather_hourly_all_variable.rds') -> weather_hourly

weather_hourly %>% 
  # replace NaN with NA
  gather(key, value, starts_with('CLIM')) %>%
  mutate(value = ifelse(is.nan(value), NA_real_, value)) %>%
  spread(key, value) %>%
  mutate(date = format(date, '%Y-%m-%d')) ->
  weather_hourly_DB

dbWriteTable(conn, "weather_hourly", weather_hourly_DB)

# count sites per variable
weather_hourly_DB %>%
  gather(code, value, starts_with('CLIM')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv') %>%
              filter(CROP == 'HOURLY'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/weather_hourly_variable_count.csv')


# Daily weather data
read_rds('Standard_Data/weather_daily_all_variable.rds') -> weather_daily

weather_daily %>% 
  gather(key, value, starts_with('CLIM')) %>% 
  mutate(value = ifelse(is.nan(value), NA_real_, value)) %>%
  spread(key, value) %>%
  mutate(date = format(date, '%Y-%m-%d')) ->
  weather_daily_DB


dbWriteTable(conn, "weather_daily", weather_daily_DB, overwrite = TRUE)

# count sites per variable
weather_daily_DB %>%
  gather(code, value, starts_with('CLIM')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv') %>%
              filter(CROP == 'DAILY'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/weather_daily_variable_count.csv')


# Save selected data (variables) for FINAL DB. > NOTE: only DAILY WEATHER goes to the FINAL DB
weather_daily_DB %>%
  select(siteid:date, CLIM01, CLIM03.01.01, CLIM03.01.02, CLIM03.01.03,
         CLIM04.01.01, CLIM05.02, CLIM06.01, CLIM06.02, starts_with('CLIM07.02')) %>%
  # remove erroneous readings from WRSIS sites
  mutate(DATE = ymd(date),
         YEAR = year(DATE),
         CLIM05.02 = case_when(siteid == 'DEFI_R' & YEAR %in% 2003:2006 ~ NA_real_,
                               siteid == 'FULTON' & YEAR %in% 2004:2006 ~ NA_real_,
                               siteid == 'VANWERT' ~ NA_real_,
                               TRUE ~ CLIM05.02)) %>%
  arrange(siteid, station, DATE) %>%
  select(-DATE, -YEAR) ->
  weather_daily_FINAL_DB

dbWriteTable(conn_final, "weather", weather_daily_FINAL_DB, overwrite = TRUE)



# Soil Moisture, Temperature and EC ---------------------------------------

read_rds('Standard_Data/sm_ALL_hourly_ORIGINAL.rds') -> sm

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


# count sites per variable
sm_FINAL_DB %>%
  gather(code, value, starts_with('SOIL')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/sm_variable_count.csv')


rm(sm_DB, sm_FINAL_DB)



# Soil Properties Data ----------------------------------------------------

read_rds('Standard_Data/soil_properties_ALL.rds') -> soil_properties

soil_properties %>%
  mutate(plotid = ifelse(siteid == 'FAIRM' & plotid == 'CD/SI', 'SI', plotid)) %>%
  mutate(year = as.integer(year),
         date = as.character(date)) ->
  soil_properties_DB

dbWriteTable(conn, "soil_properties", soil_properties_DB, overwrite = TRUE)


# count sites per variable
soil_properties_DB %>%
  gather(code, value, starts_with('SOIL')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/sp_variable_count.csv')


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

