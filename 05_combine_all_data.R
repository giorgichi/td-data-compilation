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
  
  'soil_moisture', 'SOIL09', 'soil_moisture_content', 'cm3/cm3',
  'soil_moisture', 'SOIL10', 'soil_temperature', 'degree C',
  'soil_moisture', 'SOIL12', 'soil_bulk_electrical_conductivity', 'dS/m',
  
  'soil_properties', 'SOIL01', 'soil_texture', '',
  'soil_properties', 'SOIL02.01', 'precent_sand', '%',
  'soil_properties', 'SOIL02.02', 'precent_silt', '%',
  'soil_properties', 'SOIL02.03', 'precent_clay', '%',
  'soil_properties', 'SOIL03', 'soil_bulk_density', 'g/cm3',
  'soil_properties', 'SOIL06.01', 'saturated_hydraulic_conductivity', 'cm/hr',
  'soil_properties', 'SOIL06.02', 'infiltration_rate', 'cm/hr',
  'soil_properties', 'SOIL08.01', 'matric_potential', 'bar',
  'soil_properties', 'SOIL08.02', 'water_content', 'cm3/cm3',
  'soil_properties', 'SOIL15', 'soil_organic_matter', 'g/kg',
  'soil_properties', 'SOIL20.03', 'soil_pH_water', '',
  'soil_properties', 'SOIL20.07', 'soil_pH_salt', '',
  'soil_properties', 'SOIL21.01', 'lime_test_index', '',
  'soil_properties', 'SOIL21.02', 'neutralizable_acidity', 'cmol/kg',
  'soil_properties', 'SOIL22', 'cation_exchange_capacity', 'cmol/kg',
  'soil_properties', 'SOIL23.01', 'K_saturation', '%',
  'soil_properties', 'SOIL23.02', 'Ca_saturation', '%',
  'soil_properties', 'SOIL23.03', 'Mg_saturation', '%',
  'soil_properties', 'SOIL24.01', 'K_content', 'g/kg',
  'soil_properties', 'SOIL24.02', 'Ca_content', 'g/kg',
  'soil_properties', 'SOIL24.03', 'Mg_content', 'g/kg',
  'soil_properties', 'SOIL25.01', 'K_amount', 'kg/ha',
  'soil_properties', 'SOIL25.02', 'Ca_amount', 'kg/ha',
  'soil_properties', 'SOIL25.03', 'Mg_amount', 'kg/ha',
  'soil_properties', 'SOIL26', 'sodium_adsorption_ratio', '',
  'soil_properties', 'SOIL27.02', 'soil_salinity_in_saturated_paste_extract', 'dS/m',
  'soil_properties', 'SOIL27.03', 'soil_salinity_in_water', 'dS/m',
  'soil_properties', 'SOIL30.01', 'soil_organic_carbon', '%',   # <- THIS - units to change to g/kg?
  'soil_properties', 'SOIL32.01', 'soil_total_nitrogen', '%',   # <- THIS - units to change to g/kg?
  'soil_properties', 'SOIL32.04', 'soil_nitrate_content', 'mg N/kg',
  'soil_properties', 'SOIL32.05', 'soil_ammonium_content', 'mg N/kg',
  'soil_properties', 'SOIL33.04', 'soil_nitrate_amount', 'kg N/ha',
  'soil_properties', 'SOIL33.05', 'soil_ammonium_amount', 'kg N/ha',
  'soil_properties', 'SOIL34.01', 'soil_phosphorus_content', 'ug P/kg',
  'soil_properties', 'SOIL34.01.01', 'soil_phosphorus_content_Bray_1', 'ug P/kg',
  'soil_properties', 'SOIL34.01.04', 'soil_phosphorus_content_Mehlich_3', 'ug P/kg',
  'soil_properties', 'SOIL35.01', 'soil_phosphorus_amount', 'kg P/ha',
  'soil_properties', 'SOIL35.01.01', 'soil_phosphorus_amount_Bray_1', 'kg P/ha',
  'soil_properties', 'SOIL35.01.04', 'soil_phosphorus_amount_Mehlich_3', 'kg P/ha'
  )



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



# Read Soil Properties Data -----------------------------------------------

read_rds('Output_Data/soil_properties_ALL.rds') -> soil_properties

dbWriteTable(conn, "soil_properties", soil_properties)

soil_properties %>%
  add_rownames() %>%
  # remove variables that are measures < 3 sites
  select(-SOIL04, -SOIL07, 
         -SOIL05.01, -SOIL05.02,
         -SOIL20.02,
         -SOIL23.05,
         -SOIL23.04, -SOIL24.04, 
         -SOIL31.02, -SOIL32.03) %>%
  filter_at(vars(starts_with('SOIL')), any_vars(!is.na(.))) -> temp

soil_properties %>%
  add_rownames() %>%
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
  soil_properties_DB

dbWriteTable(conn_final, "soil_properties", soil_properties_DB, overwrite = TRUE)

