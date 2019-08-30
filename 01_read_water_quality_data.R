# Initialize functions 
source('00_project_settings.R')



# DOWNLOAD ................................................................
# Download all weather data

gs_ls('WQ') %>%
  filter(!str_detect(sheet_title, 'WQFS')) %>%
  pull(sheet_title) -> sheets

for (i in sheets) {
  DownloadGoogleSheet(TITLE = i, FOLDER = 'WATER/WQ')
}



# READ ....................................................................
# Read each site-data separately


# ACRE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/ACRE Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_ACRE_hourly


# AUGLA -------------------------------------------------------------------


# BATH_A ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/BATH_A Water Table Depth.xlsx')


# BEAR --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/BEAR Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as.Date(Date)) %>%
  select(date, contains('WAT4 Water')) -> wq_BEAR_daily


# BEAR2 -------------------------------------------------------------------
# Water table is reported from the ground surface -> Dan is going to update it
# Also, there are 4 measurements per day with no clear timestamp - Need to ask Dan
ReadExcelSheets('Input_Data/WATER/WQ/BEAR2 Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as.Date(Date)) %>%
  select(date, contains('WAT4 Water')) #-> wq_BEAR2_daily


# BENTON ------------------------------------------------------------------


# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/CLAY_C Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_CLAY_C_hourly


# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/CLAY_R Water Table Depth.xlsx') %>%
  bind_rows()  %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_CLAY_R_hourly


# CRAWF -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/CLAY_U Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_CRAWF_hourly


# DPAC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DPAC Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_DPAC_hourly


# DEFI_M ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DEFI_R Water Table Depth 2003-2007 from leveloggers.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = `DATE&TIME`) %>%
  select(tmsp, contains('WAT4')) -> wq_DEFI_R_hourly


# DEFI_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DEFI_R Water Table Depth 2000-2007 manual readings.xlsx') %>%
  bind_rows() -> wq_DEFI_R_daily


# DIKE --------------------------------------------------------------------


# FAIRM ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/FAIRM Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_FAIRM_hourly


# FULTON ------------------------------------------------------------------


# HARDIN ------------------------------------------------------------------


# HARDIIN_NW --------------------------------------------------------------


# HENRY -------------------------------------------------------------------


# HICKS_B -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HICKS_B Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_HICKS_B_hourly


# HICKORY -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HICKORY Water Table Depth.xlsx') 


# MAASS -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MAASS Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as.Date(Date)) %>%
  select(date, contains('WAT4 Water')) -> wq_MAASS_daily


# MUDS2 -------------------------------------------------------------------


# MUDS3_NEW ---------------------------------------------------------------


# MUDS3_OLD ---------------------------------------------------------------


# MUDS4 -------------------------------------------------------------------


# SERF_IA -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/SERF_IA Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_SERF_IA_hourly


# SERF_SD -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/SERF_SD Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_SERF_SD_hourly


# SHEARER -----------------------------------------------------------------


# STJOHNS -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/STJOHNS Water Table Depth.xlsx') %>%
  # NEED TO remove 2010 since it was impossible to calculate water table from elevation data
  .[-1] %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_STJOHNS_hourly


# STORY -------------------------------------------------------------------


# SWROC -------------------------------------------------------------------


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/TIDE Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wq_TIDE_hourly


# UBWC --------------------------------------------------------------------


# VANWERT -----------------------------------------------------------------


# WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/WILKIN1 Water Table Depth.xlsx') 


# WILKIN2 -----------------------------------------------------------------


# WILKIN3 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/WILKIN3 Water Table Depth.xlsx') 



# ALL ---------------------------------------------------------------------
# COMBINE .................................................................


# Combnine all hourly water table data
rm(wq_ALL_hourly) 
mget(ls(pattern = 'wq_[[:graph:]]+_hourly')) %>%
  map(~ .x %>% gather(key, value, -tmsp)) %>%
  bind_rows(.id = 'site') %>%
  filter(site != 'wq_ALL_hourly') %>%
  mutate(siteid = str_remove(site, 'wq_'),
         siteid = str_remove(siteid, '_hourly')) %>%
  separate(key, into = c('plotid', 'rest'), sep = ' ', extra = 'merge') %>%
  select(siteid, plotid, tmsp, value) -> 
  wq_ALL_hourly



# Combnine all daily weather data
rm(wq_ALL_daily)
mget(ls(pattern = 'wq_[[:graph:]]+_daily')) %>%
  map(~ .x %>% gather(key, value, -date)) %>%
  bind_rows(.id = 'site') %>%
  filter(site != 'weater_ALL_daily') %>%
  mutate(siteid = str_remove(site, 'wq_'),
         siteid = str_remove(siteid, '_daily')) %>%
  separate(key, into = c('plotid', 'rest'), sep = ' ', extra = 'merge') %>%
  select(siteid, plotid, date, value) ->
  wq_ALL_daily



# Save for later analysis
write_rds(wq_ALL_hourly, 'Inter_Data/wq_ALL_hourly.rds')
write_rds(wq_ALL_daily, 'Inter_Data/wq_ALL_daily.rds')






