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
ReadExcelSheets('Input_Data/WATER/WQ/ACRE WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows()  -> wq_ACRE


# AUGLA -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/AUGLA WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_AUGLA


# BATH_A ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/BATH_A WQ.xlsx')


# BEAR --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/BEAR WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_BEAR


# BEAR2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/BEAR2 WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_BEAR2


# BENTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/BENTON WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_BENTON


# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/CLAY_C WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  filter(!is.na(Date)) -> wq_CLAY_C


# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/CLAY_R WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_CLAY_R


# CRAWF -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/CRAWF WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_CRAWF


# DPAC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DPAC WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_DPAC


# DEFI_M ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DEFI_M WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_DEFI_M


# Help >>> DEFI_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DEFI_R WQ 1999-2008.xlsx') %>%
  # NEED a lot to standardize
  bind_rows() #-> wq_DEFI_R

ReadExcelSheets('Input_Data/WATER/WQ/DEFI_R WQ 2000-2007 Lysimeter.xlsx') %>%
  # NEED This is unique data, maybe with soil data?
  bind_rows() #-> wq_DEFI_R_lysimeter


# DIKE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DIKE WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_DIKE


# FAIRM ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/FAIRM WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_FAIRM


# Help >>> FULTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/FULTON WQ 2000-2009.xlsx') %>%
  # NEED a lot of standardization
  bind_rows() #-> wq_FULTON


# HARDIN ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HARDIN WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_HARDIN


# HARDIIN_NW --------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HARDIN_NW WQ.xlsx') 


# HENRY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HENRY WQ.xlsx')


# Help >>> HICKS_B -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HICKS_B WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED some modification and UPDATED data
  bind_rows() %>%
  filter(!is.na(Date)) -> wq_HICKS_B


# HICKORY -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HICKORY WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_HICKORY


# MAASS -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MAASS WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_MAASS


# MUDS2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MUDS2 WQ.xlsx')


# MUDS3_NEW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MUDS3_NEW WQ.xlsx')


# MUDS3_OLD ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MUDS3_OLD WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_MUDS3_OLD


# MUDS4 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MUDS4 WQ.xlsx')


# SERF_IA -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/SERF_IA WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_SERF_IA


# SERF_SD -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/SERF_SD WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_SERF_SD


# SHEARER -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/SHEARER WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_SHEARER


# Help >>> STJOHNS -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/STJOHNS WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED to handle sample types, Maybe
  bind_rows() -> wq_STJOHNS


# Help >>> STORY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/STORY WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED to remove plots that are not part of the TD Project
  bind_rows() -> wq_STORY


# Help >>> SWROC -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/SWROC WQ.xlsx') %>%
  # remove sheet with metadata
  .[-1] %>% 
  # NEED TO CHECK METADATA PAGE
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() -> wq_SWROC


# Help >>> TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/TIDE WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED to remove variables that are not reported for any year
  bind_rows() -> wq_TIDE


# Help >>> UBWC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/UBWC WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED to handle timestamp, otherwise it's ok
  bind_rows() -> wq_UBWC


# Help >>> VANWERT -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/VANWERT WQ 2001-2009.xlsx') %>%
  # NEED a lot of standardization
  bind_rows() #-> wq_VANWERT


# Help >>> WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/WILKIN1 WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED to handle 'BDL' values
  # Has timestamp as a separate option 
  bind_rows() -> wq_WILKIN1


# Help >>> WILKIN2 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/WILKIN2 WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED to handle 'BDL' values
  # Has timestamp as a separate option 
  bind_rows() -> wq_WILKIN2


# Help >>> WILKIN3 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/WILKIN3 WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED to handle 'BDL' and other comments like 'no water' 
  bind_rows() -> wq_WILKIN3



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






