# Initialize functions 
source('00_project_settings.R')



# DOWNLOAD ................................................................
# Download all weather data

gs_ls('Weather$') %>%
  pull(sheet_title) -> sheets

for (i in sheets) {
  DownloadGoogleSheet(TITLE = i, FOLDER = 'WEATHER')
}



# READ ....................................................................
# Read each site-data separately


# ACRE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/ACRE Weather.xlsx') %>%
  pluck(1) -> weather_ACRE_hourly

ReadExcelSheets('Input_Data/WEATHER/ACRE Weather.xlsx') %>%
  pluck(2) -> weather_ACRE_daily


# AUGLA -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/AUGLA Weather.xlsx') %>%
  pluck(2) -> weather_AUGLA_hourly

ReadExcelSheets('Input_Data/WEATHER/AUGLA Weather.xlsx') %>%
  pluck(1) -> weather_AUGLA_daily


# BATH_A ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/BATH_A Weather.xlsx')


# BEAR --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/BEAR Weather.xlsx') %>%
  pluck(1) -> weather_BEAR_hourly

ReadExcelSheets('Input_Data/WEATHER/BEAR Weather.xlsx') %>%
  pluck(2) -> weather_BEAR_daily


# BEAR2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/BEAR2 Weather.xlsx') %>%
  pluck(1) -> weather_BEAR2_hourly

ReadExcelSheets('Input_Data/WEATHER/BEAR2 Weather.xlsx') %>%
  pluck(2) -> weather_BEAR2_daily


# BENTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/BENTON Weather.xlsx') %>%
  pluck(1) -> weather_BENTON_hourly

# Daily is not calculated
ReadExcelSheets('Input_Data/WEATHER/BENTON Weather.xlsx') %>%
  pluck(2) #-> weather_BENTON_daily


# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/CLAY_C Weather.xlsx') %>%
  pluck(1) -> weather_CLAY_C_daily


# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/CLAY_R Weather.xlsx') %>%
  bind_rows() %>%
  mutate(Station = sheet,
         sheet = 'DAILY') %>%
  select(Date, Station, Precipitation, sheet) -> weather_CLAY_R_daily


# CLAY_U ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/CLAY_U Weather.xlsx') %>%
  pluck(1) -> weather_CLAY_U_hourly

ReadExcelSheets('Input_Data/WEATHER/CLAY_U Weather.xlsx') %>%
  pluck(2) -> weather_CLAY_U_daily


# CRAWF -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/CRAWF Weather.xlsx') %>%
  pluck(2) -> weather_CRAWF_hourly 
# hourly data covers < 1 year and was used to calculate daily for that year only
# should be removed from the db

ReadExcelSheets('Input_Data/WEATHER/CRAWF Weather.xlsx') %>%
  pluck(1) -> weather_CRAWF_daily


# DEFI_M ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/DEFI_M Weather.xlsx') %>%
  pluck(2) -> weather_DEFI_M_hourly 
# hourly data covers 2 years and was used to calculate daily for those years only
# can be removed from the db

ReadExcelSheets('Input_Data/WEATHER/DEFI_M Weather.xlsx') %>%
  pluck(1) -> weather_DEFI_M_daily


# DEFI_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/DEFI_R Weather.xlsx') %>%
  .[1:2] %>%
  bind_rows() %>%
  mutate(Station = word(sheet, 2),
         sheet = 'HOURLY') %>%
  select(Date, Time, Station, everything()) %>%
  # correct measurement units
  mutate(
    # from km/h to m/s
    `Wind Speed` = `Wind Speed` / 3.6,
    `Wind Gust`  = `Wind Gust`  / 3.6
  ) -> weather_DEFI_R_hourly

ReadExcelSheets('Input_Data/WEATHER/DEFI_R Weather.xlsx') %>%
  pluck(3) %>%
  mutate(Station = word(sheet, 2),
         sheet = 'DAILY')  %>%
  # correct measurement units
  mutate(
    # from km/h to m/s
    `Wind Speed` = `Wind Speed` / 3.6
  ) -> weather_DEFI_R_daily


# DIKE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/DIKE Weather.xlsx') %>%
  pluck(1) -> weather_DIKE_hourly

# Daily is not calculated
ReadExcelSheets('Input_Data/WEATHER/DIKE Weather.xlsx') %>%
  pluck(2) #-> weather_DIKE_daily


# DPAC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/DPAC Weather.xlsx') %>%
  pluck(1) -> weather_DPAC_hourly

ReadExcelSheets('Input_Data/WEATHER/DPAC Weather.xlsx') %>%
  pluck(2) -> weather_DPAC_daily


# FAIRM ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/FAIRM Weather.xlsx') %>%
  pluck(1) %>%
  select(1:4) %>%
  gather(Station, Precipitation, -Date) %>%
  mutate(Station = str_replace(Station, 'Precipitation \\(', 'MANUAL at '),
         Station = str_remove(Station, '\\)'),
         sheet = 'DAILY') -> temp

ReadExcelSheets('Input_Data/WEATHER/FAIRM Weather.xlsx') %>%
  .[2:4] %>%
  bind_rows() %>%
  separate(sheet, into = c('sheet', 'Station'), extra = 'merge') %>%
  bind_rows(temp) %>%
  select(Date, Station, Precipitation, sheet) -> weather_FAIRM_daily


# FULTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/FULTON Weather.xlsx') %>%
  pluck(1) %>%
  # correct measurement units
  mutate(
    # from km/h to m/s
    `Wind Speed` = `Wind Speed` / 3.6,
    `Wind Gust`  = `Wind Gust`  / 3.6
  ) -> weather_FULTON_hourly

ReadExcelSheets('Input_Data/WEATHER/FULTON Weather.xlsx') %>%
  pluck(2) %>%
  # correct measurement units
  mutate(
    # from km/h to m/s
    `Wind Speed` = `Wind Speed` / 3.6
  ) -> weather_FULTON_daily


# HARDIN ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/HARDIN Weather.xlsx') %>%
  pluck(1) -> weather_HARDIN_daily


# HARDIN_NW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/HARDIN_NW Weather.xlsx') %>% 
  pluck(2) -> weather_HARDIN_NW_hourly

ReadExcelSheets('Input_Data/WEATHER/HARDIN_NW Weather.xlsx') %>% 
  pluck(1) -> weather_HARDIN_NW_daily


# HENRY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/HENRY Weather.xlsx') %>%
  pluck(2) -> weather_HENRY_hourly

ReadExcelSheets('Input_Data/WEATHER/HENRY Weather.xlsx') %>%
  pluck(1) -> weather_HENRY_daily


# HICKORY -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/HICKORY Weather.xlsx') %>%
  pluck(1) -> weather_HICKORY_hourly


# HICKS_B -----------------------------------------------------------------
# should we rename it to HICKS_P?
ReadExcelSheets('Input_Data/WEATHER/HICKS_B Weather.xlsx') %>%
  pluck(1) -> weather_HICKS_B_hourly

ReadExcelSheets('Input_Data/WEATHER/HICKS_B Weather.xlsx') %>%
  pluck(2) -> weather_HICKS_B_daily


# MAASS -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/MAASS Weather.xlsx') %>%
  pluck(1) -> weather_MAASS_hourly

ReadExcelSheets('Input_Data/WEATHER/MAASS Weather.xlsx') %>%
  pluck(2) -> weather_MAASS_daily


# MUDS1 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/MUDS1 Weather.xlsx') %>%
  pluck(1) -> weather_MUDS1_daily


# MUDS2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/MUDS2 Weather.xlsx') %>%
  pluck(1) -> weather_MUDS2_daily


# MUDS3_NEW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/MUDS3_NEW Weather.xlsx') %>%
  pluck(1) -> weather_MUDS3_NEW_daily


# MUDS3_OLD ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/MUDS3_OLD Weather.xlsx') %>%
  pluck(1) -> weather_MUDS3_OLD_daily


# MUDS4 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/MUDS4 Weather.xlsx') %>%
  pluck(1) -> weather_MUDS4_daily


# SERF_IA -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/SERF_IA Weather.xlsx') %>%
  bind_rows() %>%
  mutate(Station = str_remove(sheet, 'DAILY '),
         sheet = 'DAILY') %>%
  select(Date, Station, sheet, everything()) %>%
  select(-Comments) -> weather_SERF_IA_daily


# SERF_SD -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/SERF_SD Weather.xlsx') %>%
  pluck(2) -> weather_SERF_SD_hourly

ReadExcelSheets('Input_Data/WEATHER/SERF_SD Weather.xlsx') %>%
  pluck(1) -> weather_SERF_SD_daily


# SHEARER -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/SHEARER Weather.xlsx') %>%
  pluck(1) -> weather_SHEARER_hourly

# Daily is not calculated
ReadExcelSheets('Input_Data/WEATHER/SHEARER Weather.xlsx') %>%
  pluck(2) #-> weather_SHEARER_daily


# STJOHNS -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/STJOHNS Weather.xlsx') %>%
  pluck(2) -> weather_STJOHNS_hourly

ReadExcelSheets('Input_Data/WEATHER/STJOHNS Weather.xlsx') %>%
  pluck(1) -> weather_STJOHNS_daily


# STORY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/STORY Weather.xlsx') %>%
  pluck(1) -> weather_STORY_daily


# SWROC -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/SWROC Weather.xlsx') %>%
  pluck(1) -> weather_SWROC_daily


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/TIDE Weather.xlsx') %>%
  pluck(1) -> weather_TIDE_hourly

ReadExcelSheets('Input_Data/WEATHER/TIDE Weather.xlsx') %>%
  pluck(2) -> weather_TIDE_daily


# UBWC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/UBWC Weather.xlsx') %>%
  pluck(1) -> weather_UBWC_daily


# VANWERT -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/VANWERT Weather.xlsx') %>%
  pluck(1) %>%
  # correct measurement units
  mutate(
    # from km/h to m/s
    `Wind Speed` = `Wind Speed` / 3.6,
    `Wind Gust`  = `Wind Gust`  / 3.6
    ) -> weather_VANWERT_hourly

ReadExcelSheets('Input_Data/WEATHER/VANWERT Weather.xlsx') %>%
  pluck(2) %>%
  # correct measurement units
  mutate(
    # from km/h to m/s
    `Wind Speed` = `Wind Speed` / 3.6
    ) -> weather_VANWERT_daily


# WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WEATHER/WILKIN1 Weather.xlsx') 


# WILKIN2 -----------------------------------------------------------------
# ReadExcelSheets('Input_Data/WEATHER/WILKIN2 Weather.xlsx') 


# WILKIN3 -----------------------------------------------------------------
# ReadExcelSheets('Input_Data/WEATHER/WILKIN3 Weather.xlsx') 



# ALL ---------------------------------------------------------------------
# COMBINE .................................................................
# Combnine all hourly weather data
rm(weather_ALL_hourly) 
mget(ls(pattern = 'weather_[[:graph:]]+_hourly')) %>%
  map(~ .x %>% gather(key, value, -Date, -Time, -sheet, -starts_with('Station'))) %>%
  bind_rows(.id = 'site') %>%
  filter(site != 'weater_ALL_hourly') %>%
  mutate(siteid = str_remove(site, 'weather_'),
         siteid = str_remove(siteid, '_hourly'),
         tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(siteid, station = Station, tmsp, key, value) %>%
  # only HICKS_B has Min and Max Hourly Air Temp
  filter(!key %in% c('Min Air Temperature', 'Max Air Temperature')) %>%
  mutate(key = ifelse(key == 'Ave Air Temperature', 'Air Temperature', key)) -> weather_ALL_hourly


# Combnine all daily weather data
rm(weather_ALL_daily)
mget(ls(pattern = 'weather_[[:graph:]]+_daily')) %>%
  map(~ .x %>% gather(key, value, -Date, -sheet, -starts_with('Station'))) %>%
  bind_rows(.id = 'site') %>%
  filter(site != 'weater_ALL_daily') %>%
  mutate(siteid = str_remove(site, 'weather_'),
         siteid = str_remove(siteid, '_daily'),
         date = as_date(Date)) %>%
  select(siteid, station = Station, date, key, value) %>%
  # correct spelling error
  mutate(key = str_replace(key, 'Refrence', 'Reference')) ->
  weather_ALL_daily




  
