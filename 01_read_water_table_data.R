# Initialize functions 
source('00_project_settings.R')



# DOWNLOAD ................................................................
# Download all weather data

gs_ls('Water Table Depth') %>%
  pull(sheet_title) -> sheets

for (i in sheets) {
  DownloadGoogleSheet(TITLE = i, FOLDER = 'WATER/WATER_TABLE')
}


# Download piezometric data that are not in the standard sheets
DownloadGoogleSheet('ACRE Piezometer', FOLDER = 'WATER/WATER_TABLE')
DownloadGoogleSheet('WILKIN2 Piezometer', FOLDER = 'WATER/WATER_TABLE')



# READ ....................................................................
# Read each site-data separately


# ACRE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/ACRE Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_ACRE_hourly

ReadExcelSheets('Input_Data/WATER/WATER_TABLE/ACRE Piezometer.xlsx') %>%
  bind_rows() %>%
  mutate(date = as.Date(Date)) %>%
  select(date, contains('WATXX')) -> pz_ACRE_daily


# BATH_A ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/BATH_A Water Table Depth.xlsx')


# BEAR --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/BEAR Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as.Date(Date)) %>%
  select(date, contains('WAT4 Water')) -> wt_BEAR_daily


# BEAR2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/BEAR2 Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_BEAR2_hourly


# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/CLAY_C Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_CLAY_C_hourly


# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/CLAY_R Water Table Depth.xlsx') %>%
  bind_rows()  %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_CLAY_R_hourly


# CLAY_U ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/CLAY_U Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_CLAY_U_hourly


# DPAC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/DPAC Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_DPAC_hourly


# DEFI_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/DEFI_R Water Table Depth 2000-2007 manual readings.xlsx') %>%
  bind_rows() %>%
  mutate(date = as.Date(DATE)) %>%
  # Remove data from frozen wells (see the commetns)
  gather(key, value, contains('WAT4')) %>%
  mutate(value = ifelse(date %in% ymd(20001122, 20010109), NA, value),
         value = ifelse(date == ymd(20010117) & 
                          key %in% paste0(c(1,2,3,8), 'a WAT4'), NA, value),
         value = ifelse(date %in% ymd(20010124, 20010131, 20010207) & 
                          key %in% paste0(c(1,2,3,5), 'a WAT4'), NA, value),
         value = ifelse(date == ymd(20010214) & key == '6a WAT4', NA, value),
         value = ifelse(date %in% ymd(20010228, 20010309) & key == '5a WAT4', NA, value)) %>%
  spread(key, value) %>%
  select(date, contains('WAT4')) -> wt_DEFI_R_daily

ReadExcelSheets('Input_Data/WATER/WATER_TABLE/DEFI_R Water Table Depth 2003-2007 from leveloggers.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = `DATE&TIME`) %>%
  select(tmsp, contains('WAT4')) -> wt_DEFI_R_hourly


# FAIRM ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/FAIRM Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_FAIRM_hourly


# HICKS_B -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/HICKS_B Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_HICKS_B_hourly


# HICKS_P -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/HICKS_P Water Table Depth.xlsx') 


# MAASS -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/MAASS Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(date = as.Date(Date)) %>%
  select(date, contains('WAT4 Water')) -> wt_MAASS_daily


# SERF_IA -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/SERF_IA Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_SERF_IA_hourly


# SERF_SD -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/SERF_SD Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = update(Date, hour = hour(Time), minute = minute(Time))) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_SERF_SD_hourly


# STJOHNS -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/STJOHNS Water Table Depth.xlsx') %>%
  # Remove 2010 since it was impossible to calculate water table from elevation data
  .[-1] %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_STJOHNS_hourly


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/TIDE Water Table Depth.xlsx') %>%
  bind_rows() %>%
  mutate(tmsp = Date) %>%
  select(tmsp, contains('WAT4 Water')) -> wt_TIDE_hourly


# WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/WILKIN1 Water Table Depth.xlsx') 


# WILKIN2 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/WILKIN2 Piezometer.xlsx') #%>%
  # bind_rows() %>%
  # mutate(date = as.Date(Date)) %>%
  # select(date, contains('WATXX')) -> pz_WILKIN2_daily


# WILKIN3 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WATER_TABLE/WILKIN3 Water Table Depth.xlsx') 



# ALL ---------------------------------------------------------------------
# COMBINE .................................................................


# Combnine all hourly water table data
rm(wt_ALL_hourly) 
mget(ls(pattern = 'wt_[[:graph:]]+_hourly')) %>%
  map(~ .x %>% gather(key, value, -tmsp)) %>%
  bind_rows(.id = 'site') %>%
  filter(site != 'wt_ALL_hourly') %>%
  mutate(siteid = str_remove(site, 'wt_'),
         siteid = str_remove(siteid, '_hourly')) %>%
  separate(key, into = c('plotid', 'rest'), sep = ' ', extra = 'merge') %>%
  select(siteid, plotid, tmsp, value) -> 
  wt_ALL_hourly

write_csv(wt_ALL_hourly, 'Output_Data/water_table_hourly_all.csv')


# Combnine all daily weather data
rm(wt_ALL_daily)
mget(ls(pattern = 'wt_[[:graph:]]+_daily')) %>%
  map(~ .x %>% gather(key, value, -date)) %>%
  bind_rows(.id = 'site') %>%
  filter(site != 'weater_ALL_daily') %>%
  mutate(siteid = str_remove(site, 'wt_'),
         siteid = str_remove(siteid, '_daily')) %>%
  separate(key, into = c('plotid', 'rest'), sep = ' ', extra = 'merge') %>%
  select(siteid, plotid, date, value) ->
  wt_ALL_daily

write_csv(wt_ALL_daily, 'Output_Data/water_table_daily_all.csv')


# Save for later analysis
write_rds(wt_ALL_hourly, 'Inter_Data/wt_ALL_hourly.rds')
write_rds(wt_ALL_daily, 'Inter_Data/wt_ALL_daily.rds')





