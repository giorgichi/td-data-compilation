# Initialize functions 
source('00_project_settings.R')
library(googledrive)
library(janitor)



# DOWNLOAD ................................................................
# Download all Soil data

sheets <- drive_find(pattern = 'Soil Data', type = 'spreadsheet')

sheets %>%
  filter(str_detect(name, '. Soil Data$')) %>%
  pull(name) -> sheet_names

for (i in sheet_names) {
  DownloadGoogleSheet(TITLE = i, FOLDER = 'SOIL')
}


# READ ....................................................................
# Read each site-data separately


# ACRE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/ACRE Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_ACRE

# format tables
soil_ACRE %>%
  mutate(date = as.Date(Date),
         location = plotid, 
         var_OLD = word(var),
         siteid = "ACRE") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  mutate(var_NEW = ifelse(var_NEW == 'WAT30' & date < ymd(20160301), 'WAT31', var_NEW),
         var_NEW = ifelse(var_NEW == 'WAT70' & date < ymd(20160301), 'WAT71', var_NEW)) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_ACRE_new


# AUGLA -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/AUGLA Soil Data.xlsx') 


# BATH_A ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/BATH_A Soil Data.xlsx')



# BEAR --------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/BEAR Soil Data.xlsx') %>%
  pluck(1) -> soil_BEAR

# format tables
soil_BEAR %>% 
  remove_empty('cols') %>%
  mutate(siteid = "BEAR",
         year = date,
         date = as.Date(date, origin = "1899-12-30")) %>%
  # correct years 
  mutate(year = ifelse(year > 2020, year(date), year),
         date = ifelse(year == 2013, NA, date),
         date = as.Date(date, origin = origin)) %>% 
  # add unique identifier to 2013 rep samples
  mutate(location = subsample,
         subsample = ifelse(year == 2013, 1:2, NA)) %>%
  select(siteid, location, subsample, depth, year, date, 
         everything(), -uniqueid, -sheet) -> 
  soil_BEAR_new



# BEAR2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/BEAR2 Soil Data.xlsx') %>%
  pluck(1) -> soil_BEAR2

# format tables
soil_BEAR2 %>%
  remove_empty('cols') %>%
  mutate(siteid = "BEAR2",
         location = tolower(subsample),
         subsample = NA_character_,
         date = as.Date(date),
         year = year(date)) %>%
  select(siteid, location, subsample, depth, year, date, 
         everything(), -uniqueid, -sheet) -> 
  soil_BEAR2_new



# BENTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/BENTON Soil Data.xlsx') %>%
  pluck(1) -> soil_BENTON

# format tables
soil_BENTON %>%
  remove_empty('cols') %>%
  mutate(siteid = "BENTON",
         location = subsample,
         subsample = NA_character_,
         year = date,
         date = NA) %>%
  # convert depth from in to cm
  separate(depth, into = c('d1', 'd2'), sep = '-', convert = TRUE) %>%
  mutate(d1 = round(parse_number(d1)*2.54),
         d2 = round(d2*2.54),
         depth = ifelse(is.na(d2), d1, paste0(d1, '-', d2))) %>%
  select(siteid, location, subsample, depth, year, date, 
         everything(), -uniqueid, -sheet) -> 
  soil_BENTON_new



# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/CLAY_C Soil Data.xlsx') %>%
  bind_rows() %>%
  filter(!is.na(Date)) %>%
  transform_WAT_df() -> soil_CLAY_C

# format tables
soil_CLAY_C %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "CLAY_C") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_CLAY_C_new



# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/CLAY_R Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_CLAY_R

# format tables
soil_CLAY_R %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "CLAY_R") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_CLAY_R_new



# CLAY_U ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/CLAY_U Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_CLAY_U

# format tables
soil_CLAY_U %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "CLAY_U") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_CLAY_U_new



# CRAWF -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/CRAWF Soil Data.xlsx') 


# DEFI_M ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/DEFI_M Soil Data.xlsx') 



# DEFI_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/DEFI_R Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_DEFI_R



# DIKE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/DIKE Soil Data.xlsx') %>%
  pluck(1) -> soil_DIKE

# format tables
soil_DIKE %>%
  remove_empty('cols') %>%
  mutate(siteid = "DIKE",
         location = uniqueid,
         subsample = NA_character_,
         date = as.Date(date),
         year = year(date)) %>%
  # convert depth from in to cm
  separate(depth, into = c('d1', 'd2'), sep = '-', convert = TRUE) %>%
  mutate(d1 = round(parse_number(d1)*2.54),
         d2 = round(d2*2.54)) %>%
  mutate(depth = ifelse(is.na(d2), paste0(d1, '+'), paste0(d1, '-', d2))) %>%
  # there were two samples collected in middle at max depth, 
  # for simplicity of the database use only sand loam sample as it feets the crossectional trend
  filter(!(location == 'middle' & SOIL26 > 80)) %>%
  select(siteid, location, subsample, depth, year, date, 
         everything(), -uniqueid, -sheet) ->
  soil_DIKE_new



# DPAC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/DPAC Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_DPAC

# format tables
soil_DPAC %>%
  mutate(date = as.Date(Date),
         location = NA_character_,
         var_OLD = word(var),
         siteid = "DPAC") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT20' ~ 'WAT70',
                             var_OLD == 'WAT9'  ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_DPAC_new



# FAIRM ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/FAIRM Soil Data.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_WAT_df() %>%
  mutate(value = str_remove(value, '<')) %>%
  mutate(value = as.numeric(value)) -> soil_FAIRM

# format tables
soil_FAIRM %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "FAIRM") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_FAIRM_new



# FULTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/FULTON Soil Data.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_WAT_df() %>%
  mutate(value = str_remove(value, '<')) %>%
  mutate(value = as.numeric(value)) -> soil_FULTON


# HARDIN ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/HARDIN Soil Data.xlsx')


# HARDIIN_NW --------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/HARDIN_NW Soil Data.xlsx') 


# HENRY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/HENRY Soil Data.xlsx')



# HICKORY -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/HICKORY Soil Data.xlsx') %>%
  pluck(1) -> soil_HICKORY

# format tables
soil_HICKORY %>%
  remove_empty('cols') %>%
  mutate(siteid = "HICKORY",
         location = subsample,
         subsample = NA_character_,
         date = as.Date(date),
         year = year(date)) %>%
  # convert depth from in to cm
  separate(depth, into = c('d1', 'd2'), sep = '-', convert = TRUE) %>%
  mutate(depth = paste0(round(d1*2.54), '-', round(d2*2.54))) %>%
  select(siteid, location, subsample, depth, year, date, 
         everything(), -uniqueid, -sheet) ->
  soil_HICKORY_new



# HICKS_B -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/HICKS_B Soil Data.xlsx') %>%
  # NEED UPDATED data
  bind_rows() %>%
  filter(!is.na(Date)) %>% 
  transform_WAT_df() -> soil_HICKS_B

# format tables
soil_HICKS_B %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "HICKS_B") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_HICKS_B_new



# HICKS_P -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/HICKS_P Soil Data.xlsx') %>%
  # NEED UPDATED data
  bind_rows() %>%
  filter(!is.na(Date)) %>% 
  transform_WAT_df() -> soil_HICKS_P



# MAASS -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MAASS Soil Data.xlsx') %>%
  pluck(1) -> soil_MAASS

# format tables
soil_MAASS %>%
  remove_empty('cols') %>%
  mutate(siteid = "MAASS",
         location = subsample,
         subsample = NA_character_,
         date = as.Date(date),
         year = year(date)) %>%
  # standardize location names
  mutate(location = case_when(location %in% c('MWA', 'MWB') ~ 'west',
                              location %in% c('MEA', 'MEB') ~ 'east',
                              location %in% c('MCA', 'MCB') ~ 'middle',
                              TRUE ~ NA_character_)) %>% 
  select(siteid, location, subsample, depth, year, date, 
         everything(), -uniqueid, -sheet) ->
  soil_MAASS_new



# MUDS1 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MUDS1 Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_MUDS1




# MUDS2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MUDS2 Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_MUDS2

# format tables
soil_MUDS2 %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "MUDS2") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_MUDS2_new


# MUDS3_NEW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MUDS3_NEW Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_MUDS3_NEW


# MUDS3_OLD ---------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MUDS3_OLD Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_MUDS3_OLD

# format tables
soil_MUDS3_OLD %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "MUDS3_OLD") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_MUDS3_OLD_new


# MUDS4 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MUDS4 Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_MUDS4

# format tables
soil_MUDS4 %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "MUDS4") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_MUDS4_new



# SERF_IA -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/SERF_IA Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_SERF_IA

# format tables
soil_SERF_IA %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "SERF_IA") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT20' ~ 'WAT70',
                             var_OLD == 'WAT9'  ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_SERF_IA_new



# SERF_SD -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/SERF_SD Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_SERF_SD

# format tables
soil_SERF_SD %>%
  mutate(date = as.Date(Date),
         location = ifelse(str_length(plotid) > 5, plotid, NA), 
         var_OLD = word(var),
         siteid = "SERF_SD") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD == 'WAT02' ~ 'WAT31',
                             var_OLD == 'WAT20' ~ 'WAT71',
                             var_OLD == 'WAT09' ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_SERF_SD_new



# SHEARER -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/SHEARER Soil Data.xlsx') %>%
  pluck(1) -> soil_SHEARER

# format tables
soil_SHEARER %>%
  remove_empty('cols') %>%
  mutate(siteid = "SHEARER",
         location = subsample,
         subsample = NA_character_,
         date = as.Date(date),
         year = year(date)) %>%
  select(siteid, location, subsample, depth, year, date, 
         everything(), -uniqueid, -sheet) ->
  soil_SHEARER_new



# STJOHNS -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/STJOHNS Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_STJOHNS

# format tables
soil_STJOHNS %>%
  mutate(date = as.Date(Date),
         location = NA_character_,
         var_OLD = word(var),
         siteid = "STJOHNS") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT20' ~ 'WAT70',
                             var_OLD == 'WAT9'  ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_STJOHNS_new


# STORY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/STORY Soil Data.xlsx') %>%
  bind_rows() %>%
  # select plots that are part of TD Project (2, 3, 5, 8, 9, 11)
  select(Date, 
         matches('(^2 WAT.*)|(^3 WAT.*)|(^5 WAT.*)|(^8 WAT.*)|(^9 WAT.*)|(^11 WAT.*)'), 
         sheet) %>%
  transform_WAT_df() -> soil_STORY

# format tables
soil_STORY %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "STORY") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_STORY_new



# SWROC -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/SWROC Soil Data.xlsx') %>%
  bind_rows() #%>%
# transform_WAT_df() -> soil_SWROC

# # format tables
# soil_SWROC %>%
#   mutate(date = as.Date(Date),
#          location = NA_character_,
#          var_OLD = word(var),
#          siteid = "SWROC") %>%
#   select(siteid, plotid, location, date, var_OLD, var, value) %>%
#   mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
#                              var_OLD == 'WAT20' ~ 'WAT70',
#                              var_OLD == 'WAT9'  ~ 'WAT40',
#                              var_OLD == 'WAT26' ~ 'WAT80',
#                              TRUE ~ 'TBD')) %>%
#   select(siteid, plotid, location, date, var_NEW, value) -> soil_SWROC_new


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/TIDE Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() %>%
  # remove '-WQ' from the end of plotid
  mutate(plotid = str_remove_all(plotid, '-WQ')) -> soil_TIDE

# format tables
soil_TIDE %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "TIDE") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT20' ~ 'WAT70',
                             var_OLD == 'WAT9'  ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_TIDE_new



# UBWC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/UBWC Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_UBWC

# format tables
soil_UBWC %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "UBWC") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_UBWC_new




# VANWERT -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/VANWERT Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_VANWERT



# WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/WILKIN1 Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_WILKIN1

# format tables
soil_WILKIN1 %>%
  group_by(date = as.Date(Date), plotid, var) %>%
  summarise(SUMs = sum(value, na.rm = TRUE),
            MEANs = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(value = case_when(str_detect(var, 'WAT20|WAT26|WATXX') ~ SUMs,
                           str_detect(var, 'WAT2 |WAT9 |WAT8 ') ~ MEANs,
                           TRUE ~ NA_real_)) %>%
  # convert units from g to kg
  mutate(value = ifelse(str_detect(var, 'WAT26|WATXX'), value / 1000, value)) %>%
  mutate(location = NA_character_, 
         var_OLD = word(var),
         siteid = "WILKIN1") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT20' ~ 'WAT70',
                             var_OLD == 'WAT9'  ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             var_OLD == 'WAT8'  ~ 'WAT42',  # unsure, maybe WAT43?
                             var_OLD == 'WATXX' ~ 'WAT82',  # unsure, maybe WAT83?
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_WILKIN1_new



# WILKIN2 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/WILKIN2 Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_WILKIN2

# format tables
soil_WILKIN2 %>%
  group_by(date = as.Date(Date), plotid, var) %>%
  summarise(SUMs = sum(value, na.rm = TRUE),
            MEANs = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(value = case_when(str_detect(var, 'WAT20|WAT26|WATXX') ~ SUMs,
                           str_detect(var, 'WAT2 |WAT9 |WAT8 ') ~ MEANs,
                           TRUE ~ NA_real_)) %>%
  # convert units from g to kg
  mutate(value = ifelse(str_detect(var, 'WAT26|WATXX'), value / 1000, value)) %>%
  mutate(location = NA_character_, 
         var_OLD = word(var),
         siteid = "WILKIN2") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT20' ~ 'WAT70',
                             var_OLD == 'WAT9'  ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             var_OLD == 'WAT8'  ~ 'WAT42',  # unsure, maybe WAT43?
                             var_OLD == 'WATXX' ~ 'WAT82',  # unsure, maybe WAT83?
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_WILKIN2_new


# WILKIN3 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/WILKIN3 Soil Data.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> soil_WILKIN3

# format tables
soil_WILKIN3 %>%
  group_by(date = as.Date(Date), plotid, var) %>%
  summarise(SUMs = sum(value, na.rm = TRUE),
            MEANs = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(value = case_when(str_detect(var, 'WAT20 Tile') ~ SUMs,
                           str_detect(var, 'WAT2 Tile ') ~ MEANs,
                           TRUE ~ NA_real_)) %>%
  mutate(siteid = "WILKIN3",
         plotid = case_when(plotid %in% c('CS03', 'From_Field') ~ 'Field',
                            plotid %in% 'To_Buffer' ~ 'Buffer',
                            plotid %in% 'To_Stream' ~ 'Stream',
                            TRUE ~ 'HELP'),
         location = NA_character_, 
         var_OLD = word(var)) %>%
  # convert N loads from kg/ha to mass based kg
  mutate(value = case_when(var_OLD == 'WAT20' ~ value * 4,
                           TRUE ~ value)) %>%
  # format tables
  mutate(var_NEW = case_when(plotid == 'Field' & var_OLD == 'WAT2'  ~ 'WAT30',
                             plotid == 'Field' & var_OLD == 'WAT20' ~ 'WAT90',
                             plotid == 'Buffer' & var_OLD == 'WAT20' ~ 'WAT90',
                             plotid == 'Stream' & var_OLD == 'WAT20' ~ 'WAT90',
                             str_detect(var, 'removed') ~ 'WAT91', 
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> soil_WILKIN3_new



# ALL ---------------------------------------------------------------------
# COMBINE .................................................................



# Combnine all hourly water table data
mget(ls(pattern = 'soil_[[:graph:]]+_new')) %>%
  bind_rows() -> soil_ALL


# Save for later analysis
write_rds(soil_ALL, 'Inter_Data/soil_ALL.rds')


