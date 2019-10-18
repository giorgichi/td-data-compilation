# Initialize functions 
source('00_project_settings.R')



# DOWNLOAD ................................................................
# Download all tile flow data

gs_ls('Tile Flow') %>%
  pull(sheet_title) -> sheets

for (i in sheets) {
  DownloadGoogleSheet(TITLE = i, FOLDER = 'WATER/TILE_FLOW')
}


# Download all irrigation data

gs_ls('Irri') %>%
  pull(sheet_title) -> sheets

for (i in sheets) {
  DownloadGoogleSheet(TITLE = i, FOLDER = 'WATER/TILE_FLOW')
}


# Download WRSIS csv files for tile flow data



# READ ....................................................................
# Read each site-data separately


# ACRE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/ACRE Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT16')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'ACRE') -> tf_ACRE_hourly


# AUGLA -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/GV - AUGLA Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'AUGLA') -> tf_AUGLA_daily

# NEED TO download once gap-filled data is entered in the Google Sheet
DownloadGoogleSheet('FILLED - AUGLA Tile Flow', FOLDER = 'WATER/TILE_FLOW')
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - AUGLA Tile Flow.xlsx') 


# BATH_A ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/BATH_A Tile Flow.xlsx') 


# BEAR --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/BEAR Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = Time)) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'BEAR') -> tf_BEAR_hourly


# BEAR2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/BEAR2 Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = Time)) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'BEAR2') -> tf_BEAR2_hourly


# BENTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/BENTON Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = Time)) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'BENTON') -> tf_BENTON_hourly


# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/CLAY_C Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'CLAY_C') -> tf_CLAY_C_hourly


# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/CLAY_R Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(!is.na(tmsp)) %>%
  mutate(siteid = 'CLAY_R') -> tf_CLAY_R_hourly


# CRAWF -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/CRAWF Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'CRAWF') -> 
  # !!! DO NOT COMPILE THIS DATA !!!
  tf_CRAWF_hourly

ReadExcelSheets('Input_Data/WATER/TILE_FLOW/GV - CRAWF Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'CRAWF') -> tf_CRAWF_daily

# NEED TO download once gap-filled data is entered in the Google Sheet
DownloadGoogleSheet('FILLED - CRAWF Tile Flow', FOLDER = 'WATER/TILE_FLOW')
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - CRAWF Tile Flow.xlsx') 


# DEFI_M ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/GV - DEFI_M Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'DEFI_M') -> tf_DEFI_M_daily

# NEED TO download once gap-filled data is entered in the Google Sheet
DownloadGoogleSheet('FILLED - DEFI_M Tile Flow', FOLDER = 'WATER/TILE_FLOW')
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - DEFI_M Tile Flow.xlsx') 


# DEFI_R ------------------------------------------------------------------
read_csv('Input_Data/WATER/TILE_FLOW/DEFI_R_flow_data_1999-2008_2018-10-01.csv') %>%
  select(tmsp = timestamp, siteid = site_ID, location, value = flow) %>%
  mutate(var = 'WAT16', plotid = NA) -> 
  # !!! DO NOT COMPILE THIS DATA !!!
  tf_DEFI_R_houly

tf_DEFI_R_houly %>%
  mutate(date = as_date(tmsp)) %>%
  group_by(siteid, plotid, location, var, date) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(var_name = 'Discharge') %>%
  ungroup() -> tf_DEFI_R_daily


# DIKE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/DIKE Tile Flow.xlsx') %>%
  bind_rows() %>%
  select(tmsp = Date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'DIKE') -> tf_DIKE_hourly


# DPAC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/DPAC Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'DPAC') -> tf_DPAC_hourly

ReadExcelSheets('Input_Data/WATER/TILE_FLOW/DAILY CLEAN - DPAC Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20060615)) %>%
  mutate(siteid = 'DPAC') -> tf_DPAC_daily

# FAIRM -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FAIRM Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'FAIRM') -> tf_FAIRM_daily


# FULTON ------------------------------------------------------------------
read_csv('Input_Data/WATER/TILE_FLOW/FULTON_flow_data_2000-2011_2018-10-01.csv') %>%
  select(tmsp = timestamp, siteid = site_ID, location, value = flow) %>%
  mutate(var = 'WAT16', plotid = NA) -> 
  # !!! DO NOT COMPILE THIS DATA !!!
  tf_FULTON_houly

tf_FULTON_houly %>%
  # remove flow measurement from the nearby stream
  filter(location != 'C') %>% 
  mutate(date = as_date(tmsp)) %>%
  group_by(siteid, plotid, location, var, date) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(var_name = 'Discharge') %>%
  ungroup() -> tf_FULTON_daily


# HARDIN ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/HARDIN Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time)),
         date = as_date(Date)) %>%
  select(date, tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'HARDIN') -> 
  # !!! DO NOT COMPILE THIS DATA !!!
  tf_HARDIN_hourly

ReadExcelSheets('Input_Data/WATER/TILE_FLOW/DAILY CLEAN - HARDIN Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20081001)) %>%
  mutate(siteid = 'HARDIN') -> tf_HARDIN_daily

# NEED TO download once gap-filled data is entered in the Google Sheet
DownloadGoogleSheet('FILLED - HARDIN Tile Flow', FOLDER = 'WATER/TILE_FLOW')
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - HARDIN Tile Flow.xlsx') 


# HARDIN_NW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/GV - HARDIN_NW Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'HARDIN_NW') -> tf_HARDIN_NW_daily

# NEED TO download once gap-filled data is entered in the Google Sheet
DownloadGoogleSheet('FILLED - HARDIN_NW Tile Flow', FOLDER = 'WATER/TILE_FLOW')
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - HARDIN_NW Tile Flow.xlsx') 


# HENRY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/DAILY CLEAN - HENRY Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20080516)) %>%
  mutate(siteid = 'HENRY') -> tf_HENRY_daily


# HICKORY -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/HICKORY Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = Time)) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'HICKORY') -> tf_HICKORY_hourly


# HICKS_B -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/HICKS_B Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'HICKS_B') -> tf_HICKS_B_daily

# NEED TO download once gap-filled data is entered in the Google Sheet
DownloadGoogleSheet('FILLED - HICKS_B Tile Flow', FOLDER = 'WATER/TILE_FLOW')
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - HICKS_B Tile Flow.xlsx') 


# MAASS -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/MAASS Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = Time)) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'MAASS') -> tf_MAASS_hourly


# MUDS2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/COMPLETE - MUDS2 Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20100703)) %>%
  mutate(siteid = 'MUDS2') -> tf_MUDS2_daily


# MUDS3_NEW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/MUDS3_NEW Tile Flow.xlsx') 


# MUDS3_OLD ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/COMPLETE - MUDS3_OLD Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20100524)) %>%
  mutate(siteid = 'MUDS3_OLD') -> tf_MUDS3_OLD_daily


# MUDS4 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/COMPLETE - MUDS4 Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20100705)) %>%
  mutate(siteid = 'MUDS4') -> tf_MUDS4_daily


# SERF_IA -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/SERF_IA Tile Flow.xlsx') %>%
  bind_rows() %>%
  select(tmsp = Date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'SERF_IA') -> 
  # !!! DO NOT COMPILE THIS DATA !!!
  tf_SERF_IA_hourly

ReadExcelSheets('Input_Data/WATER/TILE_FLOW/DAILY CLEAN - SERF_IA Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20070419)) %>%
  mutate(siteid = 'SERF_IA') -> tf_SERF_IA_daily

# NEED TO download once gap-filled data is entered in the Google Sheet
DownloadGoogleSheet('FILLED - SERF_IA Tile Flow', FOLDER = 'WATER/TILE_FLOW')
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - SERF_IA Tile Flow.xlsx') 


# SERF_SD -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/SERF_SD Tile Flow.xlsx') %>%
  .[1:3] %>%
  map(., ~ .x %>% mutate_at(vars(contains('WAT')), as.numeric)) %>%
  bind_rows() %>%
  select(tmsp = Date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'SERF_SD') -> 
  # !!! DO NOT COMPILE THIS DATA !!!
  tf_SERF_SD_hourly

# get the dates when data was actually collected
tf_SERF_SD_hourly %>%
  filter(!is.na(value) & var == 'WAT1') %>%
  distinct(date = as_date(tmsp), plotid) %>%
  mutate(check = 1) -> temp

ReadExcelSheets('Input_Data/WATER/TILE_FLOW/DAILY CLEAN - SERF_SD Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20150413)) %>%
  mutate(siteid = 'SERF_SD') %>%
  # remove imputed 0s for no-flow winter periods
  full_join(temp, by = c('plotid', 'date')) %>%
  mutate(value = ifelse(is.na(check) & !is.na(value), NA_real_, value)) %>%
  select(-check) -> tf_SERF_SD_daily

# NEED TO download once gap-filled data is entered in the Google Sheet
DownloadGoogleSheet('FILLED - SERF_SD Tile Flow', FOLDER = 'WATER/TILE_FLOW')
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - SERF_SD Tile Flow.xlsx') 


# SHEARER -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/SHEARER Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'SHEARER') -> tf_SHEARER_hourly


# STJOHNS -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/STJOHNS Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(!is.na(value)) %>%
  mutate(siteid = 'STJOHNS') -> 
  # !!! DO NOT COMPILE THIS DATA !!!
  tf_STJOHNS_hourly

ReadExcelSheets('Input_Data/WATER/TILE_FLOW/GV - STJOHNS Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20090324)) %>%
  mutate(siteid = 'STJOHNS') -> tf_STJOHNS_daily

# NEED TO download once gap-filled data is entered in the Google Sheet
DownloadGoogleSheet('FILLED - STJOHNS Tile Flow', FOLDER = 'WATER/TILE_FLOW')
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - STJOHNS Tile Flow.xlsx') 


# STORY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/COMPLETE - STORY Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'STORY') -> tf_STORY_daily


# SWROC -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/SWROC Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'SWROC') -> tf_SWROC_daily


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/TIDE Tile Flow.xlsx') %>%
  map(., ~ .x %>% mutate_at(vars(contains('WAT1')), as.numeric)) %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'TIDE') -> tf_TIDE_daily

ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - TIDE Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'TIDE') -> tf_TIDE_daily_FILLED


# UBWC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/COMPLETE - UBWC Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'UBWC') -> tf_UBWC_daily


# VANWERT -----------------------------------------------------------------
read_csv('Input_Data/WATER/TILE_FLOW/VANWERT_flow_data_2001-2009.csv') %>%
  select(tmsp = timestamp, siteid = site_ID, location, value = flow) %>%
  mutate(var = 'WAT16', plotid = NA) -> 
  # !!! DO NOT COMPILE THIS DATA !!!
  tf_VANWERT_houly

tf_VANWERT_houly %>%
  mutate(date = as_date(tmsp)) %>%
  group_by(siteid, plotid, location, var, date) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(var_name = 'Discharge') %>%
  ungroup() -> tf_VANWERT_daily


# WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/WILKIN1 Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'WILKIN1') -> tf_WILKIN1_hourly


# WILKIN2 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/WILKIN2 Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'WILKIN2') -> tf_WILKIN2_hourly


# WILKIN3 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/WILKIN3 Tile Flow.xlsx') %>%
  bind_rows() %>%
  select(tmsp = Date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'WILKIN3') -> tf_WILKIN3_hourly




# Irrigation data ---------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/CLAY_R Irrigation Water Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  group_by(date, plotid, var, var_name) %>%
  summarise(value = sum(value)) %>%
  mutate(siteid = 'CLAY_R') -> irr_CLAY_R_daily


ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FAIRM Irrigation Water Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  group_by(date, plotid, var, var_name) %>%
  summarise(value = sum(value)) %>%
  mutate(siteid = 'FAIRM') -> irr_FAIRM_daily


ReadExcelSheets('Input_Data/WATER/TILE_FLOW/SWROC Irrigation Water Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  group_by(date, plotid, var, var_name) %>%
  summarise(value = sum(value)) %>%
  mutate(siteid = 'SWROC') -> irr_SWROC_daily

