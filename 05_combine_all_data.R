# Load the RSQLite Library
library(RSQLite)
library(tidyverse)
library(lubridate)



# Create a connection to a new database for storing all data
conn <- dbConnect(RSQLite::SQLite(), 'Final_Database//TD_ALL_Data.db')
conn_final <- dbConnect(RSQLite::SQLite(), 'Final_Database//TD_FINAL_Data.db')



# Keys
tribble(
  ~ table_name, ~var_code, ~var_name, ~var_units,
  'soil_moisture', 'SOIL09', 'soil_moisture_content', 'cm3/cm3',
  'soil_moisture', 'SOIL10', 'soil_temperature', 'degree C',
  'soil_moisture', 'SOIL12', 'soil_bulk_electrical_conductivity', 'dS/m'
  )



# Soil Moisture, Temperature and EC ---------------------------------------

read_rds('Inter_Data/sm_ALL_hourly_ORIGINAL.rds') -> sm

sm %>%
  mutate(date = format(date, '%Y-%m-%d')) -> 
  sm_DB

# Write the mtcars dataset into a table named mtcars_data
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
  select(-timestamp_type) ->
  sm_FINAL_DB

# Write the mtcars dataset into a table named mtcars_data
dbWriteTable(conn_final, "soil_moisture", sm_FINAL_DB)
