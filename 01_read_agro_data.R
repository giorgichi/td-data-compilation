# Initialize functions 
source('00_project_settings.R')



# DOWNLOAD ................................................................
# Download all agronomic data

gs_ls('Crop Yield') %>%
  pull(sheet_title) -> sheets

for (i in sheets) {
  DownloadGoogleSheet(TITLE = i, FOLDER = 'AGR')
}



# READ ....................................................................
# Read each site-data separately


# ACRE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/ACRE Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  mutate(location = ifelse(is.na(`Sub Field`), Field, paste(`Field`, `Sub Field`, sep = '-'))) %>%
  select(plotid, location, harvested_area = `Harvested area`, starts_with('AGR')) %>%
  gather(key, value, starts_with('AGR')) %>%
  filter(is.na(harvested_area)) -> agr_ACRE


# AUGLA -------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/AUGLA Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_AUGLA


# BATH_A ------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/BATH_A Crop Yield Data.xlsx')


# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/CLAY_C Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_CLAY_C


# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/CLAY_R Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_CLAY_R


# CLAY_U ------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/CLAY_U Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_CLAY_U


# CRAWF -------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/CRAWF Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_CRAWF


# DEFI_M ------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/DEFI_M Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_DEFI_M


# DEFI_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/DEFI_R Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_DEFI_R


# DPAC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/DPAC Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_DPAC


# FAIRM ------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/FAIRM Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_FAIRM


# FULTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/FULTON Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_FULTON


# HARDIN ------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/HARDIN Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_HARDIN


# HARDIN_NW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/HARDIN_NW Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_HARDIN_NW


# HENRY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/HENRY Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_HENRY


# HICKS_B -----------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/HICKS_B Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  mutate(location = parse_number(plotid) %>% as.character(),
         plotid = ifelse(is.na(location), plotid, str_sub(plotid, 1, 2))) %>%
  gather(key, value, -plotid, -location, -year) -> agr_HICKS_B


# MUDS1 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/MUDS1 Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  # remove redundant and non-standard variable
  select(-`AGR25_X Corn grain total nitrogen at R6`, -`Subplot ID`) %>%
  gather(key, value, starts_with('AGR')) %>%
  filter(!is.na(value)) %>% 
  mutate(crop = word(key, 2),
         plotid = as.character(plotid)) %>%
  select(action, plotid, year, planting_date = `Planting date`, crop, key, value, everything()) %>%
  gather(trt, trt_value, 8:11) %>%
  arrange(plotid, year, key) %>%
  # remove treatment name if there was no treatment applied
  mutate(trt = ifelse(is.na(trt_value), NA_character_, trt)) %>%
  distinct() %>%  
  group_by(plotid, year, crop) %>%
  # checking redundunt dups created as an artifact in year with treatments
  mutate(CHECK = sum(!is.na(trt))) %>%
  filter(!(CHECK > 0 & is.na(trt))) %>%
  select(plotid, year, crop, everything(), -CHECK) %>% 
  ungroup() -> agr_MUDS1


# MUDS2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/MUDS2 Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  select(-contains('N-treatment')) %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  mutate(plotid = as.character(plotid)) %>%
  gather(key, value, -plotid, -year) -> agr_MUDS2


# MUDS3_NEW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/MUDS3_NEW Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  mutate(plotid = as.character(plotid)) %>%
  gather(key, value, -plotid, -year) -> agr_MUDS3_NEW


# MUDS3_OLD ---------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/MUDS3_OLD Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  mutate(plotid = as.character(plotid)) %>%
  gather(key, value, -plotid, -year) %>%
  # extract date from the variable names
  mutate(date = word(key, -1) %>% parse_date(format = '%m/%d/%Y')) %>%
  # get rid of dates from variable names
  mutate(key = str_replace(key, " (\\/?[[0-9]]*){3}$", "")) %>%
  filter(!is.na(value)) %>%
  select(plotid, year, date, key, value) -> agr_MUDS3_OLD


# MUDS4 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/MUDS4 Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  mutate(plotid = as.character(plotid)) %>%
  gather(key, value, -plotid, -year) -> agr_MUDS4


# SERF_IA -----------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/SERF_IA Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_SERF_IA


# SERF_SD -----------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/SERF_SD Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_SERF_SD


# STJOHNS -----------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/STJOHNS Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_STJOHNS


# STORY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/STORY Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  filter(plotid %in% c(2, 3, 5, 8, 9, 11)) %>%
  mutate(plotid = as.character(plotid)) %>%
  gather(key, value, -plotid, -year) %>%
  # extract date from the variable names
  mutate(date = ifelse(str_detect(key, 'AGR55'), word(key, -1), NA), 
         date = parse_date(date, format = '%Y-%m-%d')) %>% 
  # get rid of dates from variable names
  mutate(key = str_replace(key, " \\(weekly\\) (\\-?[[0-9]]*){3}$", "")) %>%
  filter(!is.na(value)) %>%
  select(plotid, year, date, key, value) -> agr_STORY


# SWROC -------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/SWROC Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  mutate(plotid = `Plot ID`,
         location = paste(parse_number(`Subplot ID`), 'N'),
         year = sheet) %>%
  gather(key, value, starts_with('AGR')) %>%
  # remove variables with unresolved issues
  filter(!(str_detect(key, 'AGR4 |AGRXX |AGRYY |AGRZZ '))) %>%
  select(plotid:value) %>%
  # there were no N treatments in 2015, hense soybean plots in 2016 did not have carry-over effect
  mutate(location = ifelse(year == 2016 & str_detect(key, 'Soybean'), 
                           NA_character_,
                           location)) %>%
  filter(!(year == 2016 & is.na(value))) -> temp_SWROC

read_excel('Input_Data/AGR/SWROC LAI.xlsx', sheet = '2016', skip = 2,
           col_names = c('crop', 'plotid', 'N_trt', 'date', 'value')) %>%
  mutate(N_trt = as.integer(N_trt),
         location = ifelse(is.na(N_trt), NA_character_, paste(N_trt, 'N')),
         year = as.character(year(date)),
         date = as.Date(date),
         key = 'AGR56 Leaf Area Index (LAI)') %>%
  select(plotid, location, year, date, crop, key, value) %>%
  bind_rows(temp_SWROC) -> agr_SWROC


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/TIDE Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_TIDE


# UBWC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/UBWC Crop Yield Data.xlsx') 


# VANWERT -----------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/VANWERT Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, starts_with('AGR')) %>%
  mutate(crop = word(key, 2),
         `Hybrid Corn` = ifelse(crop == 'Corn',  `Hybrid Corn`, NA_character_),
         `Hybrid Popcorn` = ifelse(crop == 'Popcorn',  `Hybrid Popcorn`, NA_character_),
         `Cultivar Soybean` = ifelse(crop == 'Soybean',  `Cultivar Soybean`, NA_character_))  %>%
  filter(!is.na(value)) %>% 
  gather(trt, trt_value, 3:7) %>%
  arrange(plotid, year, key) %>%
  # remove treatment name if there was no treatment applied
  mutate(trt = ifelse(is.na(trt_value), NA_character_, trt)) %>%
  distinct() %>%
  group_by(plotid, year, crop) %>%
  # checking redundunt dups created as an artifact in year with treatments
  mutate(CHECK = sum(!is.na(trt))) %>%
  filter(!(CHECK > 0 & is.na(trt))) %>%
  select(plotid, year, crop, everything(), -CHECK) %>% 
  ungroup() -> agr_VANWERT


# WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/WILKIN1 Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_WILKIN1


# WILKIN2 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/WILKIN2 Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_WILKIN2


# WILKIN3 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/WILKIN3 Crop Yield Data.xlsx')



# COMBINE .................................................................
# Combnine all agronomic data
mget(ls(pattern = 'agr_')) %>%
  map(.x = ., .f = ~ .x %>% mutate(value = as.character(value))) %>%
  bind_rows(.id = 'site') %>%
  filter(site != 'agr_ALL') %>%
  mutate(siteid = str_remove(site, 'agr_')) %>%
  select(siteid, plotid, location,
         action, harvested_area, planting_date, crop, trt, trt_value, 
         year, date, key, value) -> agr_ALL

# Save for farther analysis
write_rds(agr_ALL, 'Inter_Data/agr_ALL.rds')

