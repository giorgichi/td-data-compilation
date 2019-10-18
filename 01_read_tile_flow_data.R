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

tf_ACRE_hourly %>%
  group_by(siteid, plotid, var, var_name, date = as_date(tmsp)) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  ungroup() %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_ACRE_daily_GOOD
  

# AUGLA -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/GV - AUGLA Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'AUGLA') -> tf_AUGLA_daily

tf_AUGLA_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_AUGLA_daily_GOOD

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

tf_BEAR_hourly %>% 
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  # convert from gpm to m3/h
  mutate(value = value * 0.2271247) %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_BEAR_daily_GOOD


# BEAR2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/BEAR2 Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = Time)) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'BEAR2') -> tf_BEAR2_hourly

tf_BEAR2_hourly %>% 
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  # convert from gpm to m3/h
  mutate(value = value * 0.2271247) %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_BEAR2_daily_GOOD


# BENTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/BENTON Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = Time)) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'BENTON') -> tf_BENTON_hourly

tf_BENTON_hourly %>% 
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  # convert from gpm to m3/h
  mutate(value = value * 0.2271247) %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_BENTON_daily_GOOD


# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/CLAY_C Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'CLAY_C') -> tf_CLAY_C_hourly

tf_CLAY_C_hourly %>%
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(check = sum(!is.na(value)),
            value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(value = ifelse(check == 0, NA_real_, value)) %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_CLAY_C_daily_GOOD


# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/CLAY_R Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(!is.na(tmsp)) %>%
  mutate(siteid = 'CLAY_R') -> tf_CLAY_R_hourly

tf_CLAY_R_hourly %>% 
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(check = sum(!is.na(value)),
            value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(value = ifelse(check == 0, NA_real_, value)) %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_CLAY_R_daily_GOOD


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

tf_CRAWF_daily %>% 
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_CRAWF_daily_GOOD

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

tf_DEFI_M_daily %>% 
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_DEFI_M_daily_GOOD

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

tf_DEFI_R_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> tf_DEFI_R_daily_GOOD


# DIKE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/DIKE Tile Flow.xlsx') %>%
  bind_rows() %>%
  select(tmsp = Date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'DIKE') -> tf_DIKE_hourly

tf_DIKE_hourly %>% 
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  # convert from gpm to m3/h
  mutate(value = value * 0.2271247) %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  filter(date > ymd(20160425)) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_DIKE_daily_GOOD


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

tf_DPAC_daily %>% 
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_DPAC_daily_GOOD

# NEED TO download once gap-filled data is entered in the Google Sheet
DownloadGoogleSheet('FILLED - DPAC Tile Flow', FOLDER = 'WATER/TILE_FLOW')
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FILLED - DPAC Tile Flow.xlsx') 


# FAIRM -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FAIRM Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'FAIRM') -> tf_FAIRM_daily

tf_FAIRM_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_FAIRM_daily_GOOD


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

tf_FULTON_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> tf_FULTON_daily_GOOD


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

tf_HARDIN_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_HARDIN_daily_GOOD

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

tf_HARDIN_NW_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_HARDIN_NW_daily_GOOD

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

tf_HENRY_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_HENRY_daily_GOOD


# HICKORY -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/HICKORY Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = Time)) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'HICKORY') -> tf_HICKORY_hourly

tf_HICKORY_hourly %>% 
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  # convert from gpm to m3/h
  mutate(value = value * 0.2271247) %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_HICKORY_daily_GOOD


# HICKS_B -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/HICKS_B Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'HICKS_B') -> tf_HICKS_B_daily

tf_HICKS_B_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_HICKS_B_daily_GOOD

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

tf_MAASS_hourly %>% 
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  # convert from gpm to m3/h
  mutate(value = value * 0.2271247) %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_MAASS_daily_GOOD


# MUDS2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/COMPLETE - MUDS2 Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20100703)) %>%
  mutate(siteid = 'MUDS2') -> tf_MUDS2_daily

tf_MUDS2_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value)  -> tf_MUDS2_daily_GOOD


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

tf_MUDS3_OLD_daily %>% 
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_MUDS3_OLD_daily_GOOD


# MUDS4 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/COMPLETE - MUDS4 Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  filter(date > ymd(20100705)) %>%
  mutate(siteid = 'MUDS4') -> tf_MUDS4_daily

tf_MUDS4_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_MUDS4_daily_GOOD


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

tf_SERF_IA_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_SERF_IA_daily_GOOD

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

tf_SERF_SD_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_SERF_SD_daily_GOOD

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

tf_SHEARER_hourly %>% 
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  # convert from gpm to m3/h
  mutate(value = value * 0.2271247) %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_SHEARER_daily_GOOD


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

tf_STJOHNS_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_STJOHNS_daily_GOOD

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

tf_STORY_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_STORY_daily_GOOD


# SWROC -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/SWROC Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'SWROC') -> tf_SWROC_daily

tf_SWROC_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_SWROC_daily_GOOD


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/TIDE Tile Flow.xlsx') %>%
  map(., ~ .x %>% mutate_at(vars(contains('WAT1')), as.numeric)) %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'TIDE') -> tf_TIDE_daily

tf_TIDE_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_TIDE_daily_GOOD

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

tf_UBWC_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_UBWC_daily_GOOD


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

tf_VANWERT_daily %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> tf_VANWERT_daily_GOOD


# WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/WILKIN1 Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'WILKIN1') -> tf_WILKIN1_hourly

tf_WILKIN1_hourly %>%
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(check = sum(!is.na(value)),
            value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(value = ifelse(check == 0, NA_real_, value)) %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_WILKIN1_daily_GOOD


# WILKIN2 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/WILKIN2 Tile Flow.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'WILKIN2') -> tf_WILKIN2_hourly

tf_WILKIN2_hourly %>%
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(check = sum(!is.na(value)),
            value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(value = ifelse(check == 0, NA_real_, value)) %>%
  mutate(var_NEW = ifelse(var == 'WAT1', 'WAT06', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_WILKIN2_daily_GOOD


# WILKIN3 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/WILKIN3 Tile Flow.xlsx') %>%
  bind_rows() %>%
  select(tmsp = Date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  mutate(siteid = 'WILKIN3') -> tf_WILKIN3_hourly

tf_WILKIN3_hourly %>%
  group_by(date = as_date(tmsp), siteid, plotid, var) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  # convert from gpm to m3/h
  mutate(value = value * 0.2271247) %>%
  mutate(var_NEW = ifelse(var == 'WAT16', 'WAT05', 'HELP')) %>%
  select(siteid, plotid, date, var_NEW, value) -> tf_WILKIN3_daily_GOOD


# IRRIGATION DATA ---------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/CLAY_R Irrigation Water Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  group_by(date, plotid, var, var_name) %>%
  summarise(value = sum(value)) %>%
  mutate(siteid = 'CLAY_R') -> irr_CLAY_R_daily_GOOD


ReadExcelSheets('Input_Data/WATER/TILE_FLOW/FAIRM Irrigation Water Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  group_by(date, plotid, var, var_name) %>%
  summarise(value = sum(value)) %>%
  mutate(siteid = 'FAIRM') -> irr_FAIRM_daily_GOOD


ReadExcelSheets('Input_Data/WATER/TILE_FLOW/SWROC Irrigation Water Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as_date(Date)) %>%
  select(date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  group_by(date, plotid, var, var_name) %>%
  summarise(value = sum(value)) %>%
  mutate(siteid = 'SWROC') -> irr_SWROC_daily_GOOD


# ALL ---------------------------------------------------------------------
# Combine IRRIGATION DATA .................................................
mget(ls(pattern = 'irr_[[:graph:]]+_daily_GOOD')) %>%
  bind_rows() %>%
  ungroup() %>%
  mutate(var_NEW = ifelse(var == 'WAT17', 'WAT11', 'HELP')) %>%
  select(siteid, plotid, var_NEW, date, value) ->
  irr_ALL_daily

write_csv(irr_ALL_daily, 'Output_Data/irrigation_daily_all.csv')


# Combine TILE FLOW & DISCHARGE DATA ......................................
mget(ls(pattern = 'tf_[[:graph:]]+_daily_GOOD')) %>%
  bind_rows() %>%
  select(siteid, plotid, location, var_NEW, date, value) ->
  tf_ALL_daily

write_csv(tf_ALL_daily, 'Output_Data/tile_flow_daily_all.csv')


# Save for later analysis
write_rds(irr_ALL_daily, 'Inter_Data/irr_ALL_daily.rds')
write_rds(tf_ALL_daily, 'Inter_Data/tf_ALL_daily.rds')







