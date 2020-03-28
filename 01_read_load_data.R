# Initialize functions 
source('00_project_settings.R')



# DOWNLOAD ................................................................
# Download all N load data

sheets <- drive_find(pattern = 'Nitrate-N Load', type = 'spreadsheet')


for (i in sheets$name) {
  DownloadGoogleSheet(TITLE = i, FOLDER = 'WATER/LOAD')
}



# READ ....................................................................
# Read each site-data separately


# ACRE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/ACRE Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_ACRE

# assign NEW var codes
nl_ACRE %>%
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
  select(siteid, plotid, location, date, var_NEW, value) -> nl_ACRE_new


# AUGLA -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/AUGLA Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_AUGLA

# assign NEW var codes
nl_AUGLA %>%
  mutate(date = as.Date(Date),
         location = NA_character_,
         var_OLD = word(var),
         siteid = "AUGLA") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT20' ~ 'WAT70',
                             var_OLD == 'WAT9'  ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_AUGLA_new



# BATH_A ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/BATH_A Tile Nitrate-N Load.xlsx')



# BEAR --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/BEAR Tile Nitrate-N Load.xlsx') %>%
  bind_rows()  %>%
  transform_WAT_df() -> nl_BEAR

# assign NEW var codes
nl_BEAR %>% 
  mutate(date = as.Date(Date),
         plotid = ifelse(plotid == 'tile1', 'Field', 'Buffer'), 
         var_OLD = word(var),
         siteid = "BEAR",
         location = case_when(plotid %in% c('Field', 'Buffer') ~ NA_character_,
                              TRUE ~ 'HELP')) %>%
  # convert N loads from kg/ha to mass based kg
  mutate(value = case_when(var_OLD == 'WAT20' & date < ymd(20131101) ~ value * 10.1,
                           var_OLD == 'WAT20' & date > ymd(20131031) ~ value * 5.9,
                           TRUE ~ value)) %>%
  # assign NEW var codes
  mutate(var_NEW = case_when(plotid == 'Field' & var_OLD == 'WAT2'  ~ 'WAT30',
                             plotid == 'Field' & var_OLD == 'WAT20' ~ 'WAT90',
                             str_detect(var, 'N diverted to SB') ~ 'WAT90', 
                             str_detect(var, 'N removed') ~ 'WAT91', 
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_BEAR_new



# BEAR2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/BEAR2 Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_BEAR2

# assign NEW var codes
nl_BEAR2 %>%
  mutate(date = as.Date(Date),
         plotid = ifelse(plotid == 'tile1', 'Field', 'Buffer'), 
         var_OLD = word(var),
         siteid = "BEAR2",
         location = case_when(plotid %in% c('Field', 'Buffer') ~ NA_character_,
                              TRUE ~ 'HELP')) %>%
  # convert N loads from kg/ha to mass based kg
  mutate(value = case_when(var_OLD == 'WAT20' ~ value * 40.5,
                           TRUE ~ value)) %>%
  # assign NEW var codes
  mutate(var_NEW = case_when(plotid == 'Field' & var_OLD == 'WAT2'  ~ 'WAT30',
                             plotid == 'Field' & var_OLD == 'WAT20' ~ 'WAT90',
                             str_detect(var, 'N diverted to SB') ~ 'WAT90', 
                             str_detect(var, 'N removed') ~ 'WAT91', 
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_BEAR2_new



# BENTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/BENTON Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_BENTON

# assign NEW var codes
nl_BENTON %>%
  mutate(date = as.Date(Date),
         plotid = ifelse(plotid == 'tile1', 'Field', 'Buffer'), 
         var_OLD = word(var),
         siteid = "BENTON",
         location = case_when(plotid %in% c('Field', 'Buffer') ~ NA_character_,
                              TRUE ~ 'HELP')) %>%
  # convert N loads from kg/ha to mass based kg
  mutate(value = case_when(var_OLD == 'WAT20' ~ value * 7.1,
                           TRUE ~ value)) %>%
  # assign NEW var codes
  mutate(var_NEW = case_when(plotid == 'Field' & var_OLD == 'WAT2'  ~ 'WAT30',
                             plotid == 'Field' & var_OLD == 'WAT20' ~ 'WAT90',
                             str_detect(var, 'diverted to SB') ~ 'WAT90', 
                             str_detect(var, 'removed') ~ 'WAT91', 
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_BENTON_new



# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/CLAY_C Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  filter(!is.na(Date)) %>%
  transform_WAT_df() -> nl_CLAY_C

# assign NEW var codes
nl_CLAY_C %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "CLAY_C") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_CLAY_C_new



# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/CLAY_R Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_CLAY_R

# assign NEW var codes
nl_CLAY_R %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "CLAY_R") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_CLAY_R_new



# CRAWF -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/CRAWF Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_CRAWF

# assign NEW var codes
nl_CRAWF %>%
  mutate(date = as.Date(Date),
         location = NA_character_,
         var_OLD = word(var),
         siteid = "CRAWF") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT20' ~ 'WAT70',
                             var_OLD == 'WAT9'  ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             TRUE ~ 'TBD')) %>% 
  select(siteid, plotid, location, date, var_NEW, value) -> nl_CRAWF_new



# DPAC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/DPAC Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_DPAC

# assign NEW var codes
nl_DPAC %>%
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
  select(siteid, plotid, location, date, var_NEW, value) -> nl_DPAC_new



# DEFI_M ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/DEFI_M Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_DEFI_M

# assign NEW var codes
nl_DEFI_M %>%
  mutate(date = as.Date(Date),
         location = NA_character_,
         var_OLD = word(var),
         siteid = "DEFI_M") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT20' ~ 'WAT70',
                             var_OLD == 'WAT9'  ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_DEFI_M_new



# DIKE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/DIKE Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_DIKE

# assign NEW var codes
nl_DIKE %>%
  mutate(date = as.Date(Date),
         plotid = ifelse(plotid == 'tile1', 'Field', 'Buffer'), 
         var_OLD = word(var),
         siteid = "DIKE",
         location = case_when(plotid %in% c('Field', 'Buffer') ~ NA_character_,
                              TRUE ~ 'HELP')) %>%
  # convert N loads from kg/ha to mass based kg
  mutate(value = case_when(var_OLD == 'WAT20' ~ value * 28.6,
                           TRUE ~ value)) %>%
  # assign NEW var codes
  mutate(var_NEW = case_when(plotid == 'Field' & var_OLD == 'WAT2'  ~ 'WAT30',
                             plotid == 'Field' & var_OLD == 'WAT20' ~ 'WAT90',
                             str_detect(var, 'diverted to SB') ~ 'WAT90', 
                             str_detect(var, 'removed') ~ 'WAT91', 
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_DIKE_new



# FAIRM ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/FAIRM Tile Nitrate-N Load.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_WAT_df() %>%
  mutate(value = str_remove(value, '<')) %>%
  mutate(value = as.numeric(value)) -> nl_FAIRM

# assign NEW var codes
nl_FAIRM %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "FAIRM") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_FAIRM_new


# HARDIN ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/HARDIN Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_HARDIN

# assign NEW var codes
nl_HARDIN %>%
  mutate(date = as.Date(Date),
         location = NA_character_,
         var_OLD = word(var),
         siteid = "HARDIN") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT20' ~ 'WAT70',
                             var_OLD == 'WAT9'  ~ 'WAT40',
                             var_OLD == 'WAT26' ~ 'WAT80',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_HARDIN_new


# HARDIIN_NW --------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/HARDIN_NW Tile Nitrate-N Load.xlsx') 


# HENRY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/HENRY Tile Nitrate-N Load.xlsx')


# HICKS_B -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/HICKS_B Tile Nitrate-N Load.xlsx') %>%
  # NEED UPDATED data
  bind_rows() %>%
  filter(!is.na(Date)) %>% 
  transform_WAT_df() -> nl_HICKS_B

# assign NEW var codes
nl_HICKS_B %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "HICKS_B") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_HICKS_B_new


# HICKORY -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/HICKORY Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_HICKORY

# assign NEW var codes
nl_HICKORY %>%
  mutate(date = as.Date(Date),
         plotid = ifelse(plotid == 'tile1', 'Field', 'Buffer'), 
         var_OLD = word(var),
         siteid = "HICKORY",
         location = case_when(plotid %in% c('Field', 'Buffer') ~ NA_character_,
                              TRUE ~ 'HELP')) %>%
  # convert N loads from kg/ha to mass based kg
  mutate(value = case_when(var_OLD == 'WAT20' ~ value * 21.8,
                           TRUE ~ value)) %>%
  # assign NEW var codes
  mutate(var_NEW = case_when(plotid == 'Field' & var_OLD == 'WAT2'  ~ 'WAT30',
                             plotid == 'Field' & var_OLD == 'WAT20' ~ 'WAT90',
                             str_detect(var, 'diverted to') ~ 'WAT90', 
                             str_detect(var, 'removed') ~ 'WAT91', 
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_HICKORY_new



# MAASS -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/MAASS Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_MAASS

# assign NEW var codes
nl_MAASS %>%
  mutate(date = as.Date(Date),
         plotid = ifelse(plotid == 'tile1', 'Field', 'Buffer'), 
         var_OLD = word(var),
         siteid = "MAASS",
         location = case_when(plotid %in% c('Field', 'Buffer') ~ NA_character_,
                              TRUE ~ 'HELP')) %>%
  # convert N loads from kg/ha to mass based kg
  mutate(value = case_when(var_OLD == 'WAT20' ~ value * 4.7,
                           TRUE ~ value)) %>%
  # assign NEW var codes
  mutate(var_NEW = case_when(plotid == 'Field' & var_OLD == 'WAT2'  ~ 'WAT30',
                             plotid == 'Field' & var_OLD == 'WAT20' ~ 'WAT90',
                             str_detect(var, 'diverted to') ~ 'WAT90', 
                             str_detect(var, 'removed') ~ 'WAT91', 
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_MAASS_new



# MUDS2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/MUDS2 Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_MUDS2

# assign NEW var codes
nl_MUDS2 %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "MUDS2") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_MUDS2_new


# MUDS3_NEW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/MUDS3_NEW Tile Nitrate-N Load.xlsx')  %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_MUDS3_NEW

# assign NEW var codes
nl_MUDS3_NEW %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "MUDS3_NEW") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             var_OLD == 'WAT8'  ~ 'WAT42',  # unsure, maybe WAT43?
                             var_OLD == 'WATXX' ~ 'WAT82',  # unsure, maybe WAT83?
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_MUDS3_NEW_new


# MUDS3_OLD ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/MUDS3_OLD Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_MUDS3_OLD

# assign NEW var codes
nl_MUDS3_OLD %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "MUDS3_OLD") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_MUDS3_OLD_new


# MUDS4 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/MUDS4 Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_MUDS4

# assign NEW var codes
nl_MUDS4 %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "MUDS4") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_MUDS4_new


# SERF_IA -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/SERF_IA Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_SERF_IA

# assign NEW var codes
nl_SERF_IA %>%
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
  select(siteid, plotid, location, date, var_NEW, value) -> nl_SERF_IA_new



# SERF_SD -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/SERF_SD Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_SERF_SD

# assign NEW var codes
nl_SERF_SD %>%
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
  select(siteid, plotid, location, date, var_NEW, value) -> nl_SERF_SD_new



# SHEARER -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/SHEARER Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_SHEARER

# assign NEW var codes
nl_SHEARER %>%
  mutate(date = as.Date(Date),
         plotid = ifelse(plotid == 'tile1', 'Field', 'Buffer'), 
         var_OLD = word(var),
         siteid = "SHEARER",
         location = case_when(plotid %in% c('Field', 'Buffer') ~ NA_character_,
                              TRUE ~ 'HELP')) %>%
  # convert N loads from kg/ha to mass based kg
  mutate(value = case_when(var_OLD == 'WAT20' ~ value * 3.4,
                           TRUE ~ value)) %>%
  # assign NEW var codes
  mutate(var_NEW = case_when(plotid == 'Field' & var_OLD == 'WAT2'  ~ 'WAT30',
                             plotid == 'Field' & var_OLD == 'WAT20' ~ 'WAT90',
                             str_detect(var, 'diverted to') ~ 'WAT90', 
                             str_detect(var, 'removed') ~ 'WAT91', 
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_SHEARER_new



# STJOHNS -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/STJOHNS Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_STJOHNS

# assign NEW var codes
nl_STJOHNS %>%
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
  select(siteid, plotid, location, date, var_NEW, value) -> nl_STJOHNS_new


# STORY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/STORY Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  # select plots that are part of TD Project (2, 3, 5, 8, 9, 11)
  select(Date, 
         matches('(^2 WAT.*)|(^3 WAT.*)|(^5 WAT.*)|(^8 WAT.*)|(^9 WAT.*)|(^11 WAT.*)'), 
         sheet) %>%
  transform_WAT_df() -> nl_STORY

# assign NEW var codes
nl_STORY %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "STORY") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_STORY_new



# SWROC -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/SWROC Tile Nitrate-N Load.xlsx') %>%
  bind_rows() #%>%
# transform_WAT_df() -> nl_SWROC

# # assign NEW var codes
# nl_SWROC %>%
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
#   select(siteid, plotid, location, date, var_NEW, value) -> nl_SWROC_new


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/TIDE Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() %>%
  # remove '-WQ' from the end of plotid
  mutate(plotid = str_remove_all(plotid, '-WQ')) -> nl_TIDE

# assign NEW var codes
nl_TIDE %>%
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
  select(siteid, plotid, location, date, var_NEW, value) -> nl_TIDE_new



# UBWC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/UBWC Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_UBWC

# assign NEW var codes
nl_UBWC %>%
  mutate(date = as.Date(Date),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "UBWC") %>%
  select(siteid, plotid, location, date, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2')  ~ 'WAT30',
                             var_OLD %in% c('WAT20') ~ 'WAT70',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_UBWC_new


# WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/WILKIN1 Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_WILKIN1

# assign NEW var codes
nl_WILKIN1 %>%
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
  select(siteid, plotid, location, date, var_NEW, value) -> nl_WILKIN1_new



# WILKIN2 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/WILKIN2 Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_WILKIN2

# assign NEW var codes
nl_WILKIN2 %>%
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
  select(siteid, plotid, location, date, var_NEW, value) -> nl_WILKIN2_new


# WILKIN3 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/LOAD/WILKIN3 Tile Nitrate-N Load.xlsx') %>%
  bind_rows() %>%
  transform_WAT_df() -> nl_WILKIN3

# assign NEW var codes
nl_WILKIN3 %>%
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
  # assign NEW var codes
  mutate(var_NEW = case_when(plotid == 'Field' & var_OLD == 'WAT2'  ~ 'WAT30',
                             plotid == 'Field' & var_OLD == 'WAT20' ~ 'WAT90',
                             plotid == 'Buffer' & var_OLD == 'WAT20' ~ 'WAT90',
                             plotid == 'Stream' & var_OLD == 'WAT20' ~ 'WAT90',
                             str_detect(var, 'removed') ~ 'WAT91', 
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, var_NEW, value) -> nl_WILKIN3_new



# ALL ---------------------------------------------------------------------
# COMBINE .................................................................



# Combnine all hourly water table data
mget(ls(pattern = 'nl_[[:graph:]]+_new')) %>%
  bind_rows() -> nl_ALL


# Save for later analysis
write_rds(nl_ALL, 'Inter_Data/nl_ALL.rds', compress = 'xz')

