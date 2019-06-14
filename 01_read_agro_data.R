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
ReadExcelSheets('Input_Data/AGR/ACRE Crop Yield Data.xlsx')


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
  gather(key, value, -plotid, -year) -> agr_HICKS_B


# MUDS1 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/MUDS1 Crop Yield Data.xlsx') 


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
  gather(key, value, -plotid, -year) -> agr_MUDS3_OLD


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
  select(-contains('Development')) %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  mutate(plotid = as.character(plotid)) %>%
  gather(key, value, -plotid, -year) -> agr_STORY


# SWROC -------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/SWROC Crop Yield Data.xlsx') 


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/TIDE Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_TIDE


# UBWC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/UBWC Crop Yield Data.xlsx') 


# VANWERT -----------------------------------------------------------------
ReadExcelSheets('Input_Data/AGR/VANWERT Crop Yield Data.xlsx')


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
ReadExcelSheets('Input_Data/AGR/WILKIN3 Crop Yield Data.xlsx') %>%
  bind_rows() %>%
  rename(year = sheet, plotid = `Plot ID`) %>%
  gather(key, value, -plotid, -year) -> agr_WILKIN3



# COMBINE .................................................................
# Combnine all agronomic data
mget(ls(pattern = 'agr_')) %>%
  bind_rows(.id = 'siteid') %>%
  mutate(siteid = str_remove(siteid, 'agr_')) -> agr_ALL


