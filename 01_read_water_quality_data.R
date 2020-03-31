# Initialize functions 
source('00_project_settings.R')

transform_df <- function(df) {
  df %>%
    gather(key, value, contains('WAT')) %>%
    separate(key, into = c('plotid', 'var'), extra = 'merge', sep = ' ') %>%
    filter(!is.na(value))
}


# DOWNLOAD ................................................................
# Download all water quality data

gs_ls('WQ') %>%
  filter(!str_detect(sheet_title, 'WQFS')) %>%
  pull(sheet_title) -> sheets

for (i in sheets) {
  DownloadGoogleSheet(TITLE = i, FOLDER = 'WATER/WQ')
}


# Download WQ data that are not in the standard sheets
DownloadGoogleSheet('CLAY_R Turbidity', FOLDER = 'WATER/WQ')
DownloadGoogleSheet('CLAY_R Tile Water EC', FOLDER = 'WATER/WQ')
DownloadGoogleSheet('FAIRM Tile Water EC', FOLDER = 'WATER/WQ')



# READ ....................................................................
# Read each site-data separately


# ACRE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/ACRE WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_ACRE

# assign NEW var codes
wq_ACRE %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = plotid, 
         var_OLD = word(var),
         siteid = "ACRE",
         plotid = NA_character_) %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             var_OLD == 'WATXX' ~ 'WAT33',
                             TRUE ~ 'TBD')) %>%
  mutate(var_NEW = ifelse(var_NEW == 'WAT30' & date < ymd(20160301), 'WAT31', var_NEW),
         var_NEW = ifelse(var_NEW == 'WAT40' & date < ymd(20160301), 'WAT41', var_NEW)) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_ACRE_new


# AUGLA -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/AUGLA WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_AUGLA

# assign NEW var codes
wq_AUGLA %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "AUGLA") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_AUGLA_new



# BATH_A ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/BATH_A WQ.xlsx')



# BEAR --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/BEAR WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows()  %>%
  transform_df() -> wq_BEAR

# assign NEW var codes
wq_BEAR %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = ifelse(plotid != 'tile1', plotid, NA), 
         var_OLD = word(var),
         siteid = "BEAR",
         plotid = ifelse(plotid == 'tile1', plotid, NA)) %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_BEAR_new



# BEAR2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/BEAR2 WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_BEAR2

# assign NEW var codes
wq_BEAR2 %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = ifelse(plotid != 'tile1', plotid, NA), 
         var_OLD = word(var),
         siteid = "BEAR2",
         plotid = ifelse(plotid == 'tile1', plotid, NA)) %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_BEAR2_new



# BENTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/BENTON WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_BENTON

# assign NEW var codes
wq_BENTON %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = ifelse(plotid != 'tile1', plotid, NA), 
         var_OLD = word(var),
         siteid = "BENTON",
         plotid = ifelse(plotid == 'tile1', plotid, NA)) %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_BENTON_new



# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/CLAY_C WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  filter(!is.na(Date)) %>%
  transform_df() -> wq_CLAY_C

# assign NEW var codes
wq_CLAY_C %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "CLAY_C") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             var_OLD %in% c('WAT14') ~ 'WAT60',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_CLAY_C_new



# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/CLAY_R Turbidity.xlsx') %>%
  bind_rows() %>%
  mutate(date = as.Date(Date),
         time = NA) %>%
  gather(key, value, contains('Turbidity')) %>%
  mutate(siteid = 'CLAY_R',
         plotid = str_remove(key, ' WAT13 Turbidity'),
         var_NEW = 'WAT20',
         location = word(plotid),
         value = as.character(value)) %>%
  # get rid of the North field as it is not part of TD
  filter(plotid != 'Control 3') %>%
  select(siteid, plotid, location, date, time, var_NEW, value) %>%
  mutate(location = ifelse(location == 'Control', NA, location),
         plotid = case_when(plotid == 'Control 1' ~ 'SI',
                            plotid == 'Control 2' ~ 'CD',
                            TRUE ~ NA_character_)) -> turbidity_CLAY_R


ReadExcelSheets('Input_Data/WATER/WQ/CLAY_R Tile Water EC.xlsx') %>%
  bind_rows() %>%
  mutate(date = as.Date(Date),
         time = NA) %>%
  gather(key, value, contains('Water EC')) %>%
  mutate(siteid = 'CLAY_R',
         plotid = str_remove(key, ' WAT10 Tile Water EC'),
         var_NEW = 'WAT15',
         location = NA,
         value = as.character(value)) %>%
  filter(!is.na(value)) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> waterEC_CLAY_R


ReadExcelSheets('Input_Data/WATER/WQ/CLAY_R WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_CLAY_R

# assign NEW var codes
wq_CLAY_R %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "CLAY_R") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             var_OLD %in% c('WAT14') ~ 'WAT60',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) %>%
  bind_rows(turbidity_CLAY_R, waterEC_CLAY_R) -> wq_CLAY_R_new



# CRAWF -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/CRAWF WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_CRAWF

# assign NEW var codes
wq_CRAWF %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "CRAWF") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_CRAWF_new



# DPAC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DPAC WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_DPAC

# assign NEW var codes
wq_DPAC %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "DPAC") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             var_OLD %in% c('WAT8', 'WAT22') ~ 'WAT42',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_DPAC_new



# DEFI_M ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DEFI_M WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_DEFI_M

# assign NEW var codes
wq_DEFI_M %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "DEFI_M") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_DEFI_M_new



# DEFI_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DEFI_R WQ 1999-2008.xlsx') %>%
  # NEED a lot to standardize
  bind_rows() %>%
  # add pick-up time to missing dates in comments
  mutate(pickupdate = ifelse(is.na(`Pick Up DATE`), 
                             paste(month.abb[MONTH], YEAR, sep = ', '),
                             format(`Pick Up DATE`, '%b %d, %Y')),
         comm = ifelse(is.na(Date), paste('Pick-up date:', pickupdate), NA),
         Comments = ifelse(!is.na(comm) & !is.na(Comments), paste0(comm, '; ', Comments), Comments),
         Comments = ifelse(!is.na(comm) & is.na(Comments), comm, Comments),
         Comments = ifelse(str_detect(Comments, 'No sample'), NA, Comments)) %>%
  # add estimated dates for samples with no tmsp
  mutate(Comments = ifelse(is.na(Date) & !is.na(`Pick Up DATE`), 'estimated from Pick-Up date', Comments),
         Date = ifelse(is.na(Date) & !is.na(`Pick Up DATE`), `Pick Up DATE` - days(1), Date),
         Date = as_datetime(Date)) %>%
  select(-comm, - pickupdate) %>%
  # refine locations based on pH and TFS measurements 
  mutate(location = ifelse(location == 'W' & method == 'G' & is.na(Time) & YEAR == 2003,
                           c('WETIN', 'WETOUT'), location),
         location = ifelse(location == 'RESIN', 'R', location)) %>%
  arrange(location, method, YEAR, MONTH, Date, Time, `Bottle Number`) %>%
  transform_df() %>%
  select(Date, Time, sample_type = method, location, bottle = `Bottle Number`, height,
         sheet, plotid, var, value, Comments) -> wq_DEFI_R

# assign NEW var codes
wq_DEFI_R %>%
  unite(var, plotid, var, sep = ' ') %>%
  mutate(date = as.Date(Date),
         time = format(Time, '%H:%M'), 
         var_NEW = word(var),
         siteid = "DEFI_R",
         plotid = NA_character_) %>% 
  mutate(sample_type = case_when(sample_type == 'I' ~ 'ISCO',
                                 sample_type == 'G' ~ 'Grab',
                                 sample_type == 'M' ~ 'Mast',
                                 TRUE ~ 'TBD')) %>% 
  select(siteid, plotid, location, date, time, sample_type, height, var_NEW, value, 
         comments = Comments) -> wq_DEFI_R_new


# WQ from lysimeter 
ReadExcelSheets('Input_Data/WATER/WQ/DEFI_R WQ 2000-2007 Lysimeter.xlsx') %>%
  bind_rows() %>%
  transform_df() %>%
  select(Date, sheet, location = plotid, everything()) -> wq_DEFI_R_lysimeter

# assign NEW var codes
wq_DEFI_R_lysimeter %>%
  rename(var_NEW = var) %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         siteid = "DEFI_R",
         plotid = as.numeric(location),
         location = paste('lysimeter', location)) %>% 
  mutate(plotid = case_when(plotid %in% 1:4 ~ '8',
                            plotid %in% 5:8 ~ '9',
                            plotid %in% 9:12 ~ '6',
                            plotid %in% 13:16 ~ '7',
                            plotid %in% 17:24 ~ '11',
                            plotid %in% 25:30 ~ '10',
                            TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_DEFI_R_lysimeter_new



# DIKE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/DIKE WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_DIKE

# assign NEW var codes
wq_DIKE %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = ifelse(plotid != 'tile1', plotid, NA), 
         var_OLD = word(var),
         siteid = "DIKE",
         plotid = ifelse(plotid == 'tile1', plotid, NA)) %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_DIKE_new



# FAIRM ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/FAIRM Tile Water EC.xlsx') %>%
  bind_rows() %>%
  gather(key, value, contains('Water EC')) %>%
  mutate(date = as.Date(Date),
         time = format(Time, '%H:%M'),
         siteid = "FAIRM",
         plotid = str_remove(key, ' WAT10 Tile Water EC'),
         var_NEW = 'WAT15',
         location = NA,
         value = as.character(value)) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> waterEC_FAIRM

ReadExcelSheets('Input_Data/WATER/WQ/FAIRM WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_FAIRM

# assign NEW var codes
wq_FAIRM %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "FAIRM") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) %>%
  bind_rows(waterEC_FAIRM) -> wq_FAIRM_new


# FULTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/FULTON WQ 2000-2009.xlsx') %>%
  bind_rows() %>%
  transform_df() %>%
  select(Date, Time, sample_type = method, location, bottle = `Bottle Number`,
         sheet, plotid, var, value, Comments) -> wq_FULTON

# assign NEW var codes
wq_FULTON %>%
  unite(var, plotid, var, sep = ' ') %>%
  mutate(date = as.Date(Date),
         time = format(Time, '%H:%M'), 
         var_NEW = word(var),
         siteid = "FULTON",
         plotid = NA_character_) %>% 
  mutate(sample_type = case_when(sample_type == 'I' ~ 'ISCO',
                                 sample_type == 'G' ~ 'Grab',
                                 sample_type == 'M' ~ 'Mast',
                                 TRUE ~ 'TBD')) %>% 
  select(siteid, plotid, location, date, time, sample_type, var_NEW, value, 
         comments = Comments) -> wq_FULTON_new



# HARDIN ------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HARDIN WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_HARDIN

# assign NEW var codes
wq_HARDIN %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "HARDIN") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_HARDIN_new



# HARDIIN_NW --------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HARDIN_NW WQ.xlsx') 


# HENRY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HENRY WQ.xlsx')


# HICKS_B -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HICKS_B WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED UPDATED data
  bind_rows() %>%
  filter(!is.na(Date)) %>% 
  transform_df() %>%
  select(Date, sheet, plotid, var, value) -> wq_HICKS_B

# assign NEW var codes
wq_HICKS_B %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "HICKS_B") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) %>%
  # handle replicated measurements
  group_by(siteid, plotid, location, date, time, var_NEW) %>%
  summarise(value = as.character(mean(as.numeric(value)))) -> wq_HICKS_B_new



# HICKORY -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/HICKORY WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_HICKORY

# assign NEW var codes
wq_HICKORY %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = ifelse(plotid != 'tile1', plotid, NA), 
         var_OLD = word(var),
         siteid = "HICKORY",
         plotid = ifelse(plotid == 'tile1', plotid, NA)) %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_HICKORY_new



# MAASS -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MAASS WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_MAASS

# assign NEW var codes
wq_MAASS %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = ifelse(plotid != 'tile1', plotid, NA), 
         var_OLD = word(var),
         siteid = "MAASS",
         plotid = ifelse(plotid == 'tile1', plotid, NA)) %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_MAASS_new



# MUDS2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MUDS2 WQ.xlsx')


# MUDS3_NEW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MUDS3_NEW WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_MUDS3_NEW

# assign NEW var codes
wq_MUDS3_NEW %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "MUDS3_NEW") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             var_OLD %in% c('WAT8', 'WAT22') ~ 'WAT42',
                             var_OLD == 'WAT12' ~ 'WAT22',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_MUDS3_NEW_new


# MUDS3_OLD ---------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MUDS3_OLD WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_MUDS3_OLD

# assign NEW var codes
wq_MUDS3_OLD %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "MUDS3_OLD") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_MUDS3_OLD_new



# MUDS4 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/MUDS4 WQ.xlsx')


# SERF_IA -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/SERF_IA WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_SERF_IA

# assign NEW var codes
wq_SERF_IA %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "SERF_IA") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_SERF_IA_new



# SERF_SD -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/SERF_SD WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_SERF_SD

# assign NEW var codes
wq_SERF_SD %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = ifelse(str_length(plotid) > 5, plotid, NA), 
         var_OLD = word(var),
         siteid = "SERF_SD",
         plotid = str_sub(plotid, 1, 5)) %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT31',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_SERF_SD_new

# read tile water temperature data
ReadExcelSheets('Input_Data/WATER/TILE_FLOW/SERF_SD Tile Flow.xlsx') %>%
  .[1:3] %>%
  map(., ~ .x %>% mutate_at(vars(contains('WAT')), as.numeric)) %>%
  bind_rows() %>%
  select(Date, contains('WAT')) %>%
  gather(key, value, contains('WAT')) %>%
  separate(key, into = c('plotid', 'var', 'var_name'), sep = ' ', extra = 'merge') %>%
  # select only tile water temp 
  filter(var == 'WAT11') -> temp_SERF_SD

# assign NEW var codes
temp_SERF_SD %>%
  filter(!is.na(value)) %>%
  mutate(date = as.Date(Date),
         time = format(Date, '%H:%M'),
         location = NA_character_, 
         var_OLD = var,
         value = as.character(value),
         siteid = "SERF_SD") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD == 'WAT11' ~ 'WAT16',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> temp_SERF_SD_new




# SHEARER -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/SHEARER WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_SHEARER

# assign NEW var codes
wq_SHEARER %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = ifelse(plotid != 'tile1', plotid, NA), 
         var_OLD = word(var),
         siteid = "SHEARER",
         plotid = ifelse(plotid == 'tile1', plotid, NA)) %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_SHEARER_new



# STJOHNS -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/STJOHNS WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() %>%
  # handle sample types
  mutate(sample_type = ifelse(plotid == 'WS', `WS Sample Type`, `WN Sample Type`)) %>%
  select(Date, sheet, plotid, sample_type, var, value) -> wq_STJOHNS

# assign NEW var codes
wq_STJOHNS %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "STJOHNS") %>%
  select(siteid, plotid, location, date, time, sample_type, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, sample_type, var_NEW, value) -> wq_STJOHNS_new



# STORY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/STORY WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  # select plots that are part of TD Project (2, 3, 5, 8, 9, 11)
  select(Date, 
         matches('(^2 WAT.*)|(^3 WAT.*)|(^5 WAT.*)|(^8 WAT.*)|(^9 WAT.*)|(^11 WAT.*)'), 
         sheet) %>%
  transform_df() -> wq_STORY

# assign NEW var codes
wq_STORY %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "STORY") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_STORY_new



# SWROC -------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/SWROC WQ.xlsx') %>%
  # remove sheet with metadata
  .[-1] %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_SWROC

# read pond WQ data
read_excel('Input_Data/WATER/WQ/SWROC WQ.xlsx', 
           sheet = 1, skip = 8, col_names = c('plotid', 'Date', 'value')) %>%
  add_column(var = 'WAT2 Tile Nitrate-N concentration') %>%
  mutate(value = as.character(value)) ->
  wq_SWROC_pond
  
# assign NEW var codes
wq_SWROC_pond %>% 
  bind_rows(wq_SWROC) %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = ifelse(str_detect(plotid, 'NPM'), NA, plotid), 
         var_OLD = word(var),
         siteid = "SWROC",
         plotid = ifelse(str_detect(plotid, 'NPM'), plotid, NA)) %>% 
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_SWROC_new


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/TIDE WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  # remove variables that are not reported for any year
  select(-contains('WAT21')) %>%
  transform_df() %>%
  # remove '-WQ' from the end of plotid
  mutate(plotid = str_remove_all(plotid, '-WQ')) -> wq_TIDE

# assign NEW var codes
wq_TIDE %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "TIDE") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_TIDE_new



# UBWC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/UBWC WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  bind_rows() %>%
  transform_df() -> wq_UBWC

# assign NEW var codes
wq_UBWC %>%
  mutate(date = as.Date(Date),
         time = format(Date, '%H:%M'),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "UBWC") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD %in% c('WAT2', 'WAT21') ~ 'WAT30',
                             var_OLD %in% c('WAT9', 'WAT23') ~ 'WAT40',
                             var_OLD %in% c('WAT8', 'WAT22') ~ 'WAT43',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_UBWC_new


# VANWERT -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/VANWERT WQ 2001-2009.xlsx') %>%
  bind_rows() %>%
  # add pick-up time to missing dates in comments
  mutate(comm = ifelse(is.na(Date), paste('Pick-up date:', as.Date(`Pick Up DATE`)), NA),
         Comments = ifelse(!is.na(comm) & !is.na(Comments), paste0(comm, '; ', Comments), Comments),
         Comments = ifelse(!is.na(comm) & is.na(Comments), comm, Comments),
         Comments = ifelse(str_detect(Comments, 'No sample'), NA, Comments)) %>%
  select(-comm) %>%
  # add estimated dates for samples with no tmsp
  mutate(Comments = ifelse(is.na(Date), 'estimated from Pick-Up date', Comments),
         Date = ifelse(is.na(Date), `Pick Up DATE` - days(1), Date),
         Date = as_datetime(Date)) %>%
  transform_df() %>%
  select(Date, Time, sample_type = method, location, bottle = `Bottle Number`,
         sheet, plotid, var, value, Comments) -> wq_VANWERT

# assign NEW var codes
wq_VANWERT %>%
  unite(var, plotid, var, sep = ' ') %>%
  # after observing data it seems that two unkown samples belong to OFFSITE
  mutate(location = ifelse(location == 'unknown', 'OFFSITE', location)) %>%
  mutate(date = as.Date(Date),
         time = format(Time, '%H:%M'), 
         var_NEW = word(var),
         siteid = "VANWERT",
         plotid = NA_character_) %>% 
  mutate(sample_type = case_when(sample_type == 'I' ~ 'ISCO',
                                 sample_type == 'G' ~ 'Grab',
                                 sample_type == 'M' ~ 'Mast',
                                 TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, sample_type, var_NEW, value, 
         comments = Comments) -> wq_VANWERT_new


# WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/WILKIN1 WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED to handle 'BDL' values
  bind_rows() %>%
  gather(var, value, contains('WAT')) %>%
  select(Date, sheet, plotid = PlotID, var, value) -> wq_WILKIN1

# assign NEW var codes
wq_WILKIN1 %>%
  mutate(date = as.Date(Date),
         time = format(Date, '%H:%M'),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "WILKIN1") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD == 'WAT2' ~ 'WAT30',
                             var_OLD == 'WAT9' ~ 'WAT40',
                             var_OLD == 'WAT8' ~ 'WAT42', # unsure, maybe WAT43?
                             var_OLD == 'WAT12' ~ 'WAT22',
                             var_OLD == 'WATXX' ~ 'WAT36',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_WILKIN1_new



# WILKIN2 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/WILKIN2 WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED to handle 'BDL' values
  bind_rows() %>%
  gather(var, value, contains('WAT')) %>%
  select(Date, sheet, plotid = PlotID, var, value) -> wq_WILKIN2

# assign NEW var codes
wq_WILKIN2 %>%
  mutate(date = as.Date(Date),
         time = format(Date, '%H:%M'),
         location = NA_character_, 
         var_OLD = word(var),
         siteid = "WILKIN2") %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>%
  mutate(var_NEW = case_when(var_OLD == 'WAT2' ~ 'WAT30',
                             var_OLD == 'WAT9' ~ 'WAT40',
                             var_OLD == 'WAT8' ~ 'WAT42', # unsure, maybe WAT43?
                             var_OLD == 'WAT12' ~ 'WAT22',
                             var_OLD == 'WATXX' ~ 'WAT36',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_WILKIN2_new



# WILKIN3 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/WATER/WQ/WILKIN3 WQ.xlsx') %>%
  map(.x = ., .f =  ~ .x %>% mutate_at(vars(contains("WAT")), as.character)) %>%
  # NEED to handle 'BDL' and other comments like 'no water' 
  bind_rows() %>%
  transform_df() -> wq_WILKIN3

# assign NEW var codes
wq_WILKIN3 %>%
  mutate(date = as.Date(Date),
         time = NA_character_,
         location = ifelse(plotid != 'CS03', plotid, NA), 
         var_OLD = word(var),
         siteid = "WILKIN3",
         plotid = ifelse(plotid == 'CS03', plotid, NA)) %>%
  select(siteid, plotid, location, date, time, var_OLD, var, value) %>% 
  mutate(var_NEW = case_when(var_OLD == 'WAT2'  ~ 'WAT30',
                             var_OLD == 'WAT21' ~ 'WAT30',
                             TRUE ~ 'TBD')) %>%
  select(siteid, plotid, location, date, time, var_NEW, value) -> wq_WILKIN3_new



# ALL ---------------------------------------------------------------------
# COMBINE .................................................................


# Combnine all hourly water table data
mget(ls(pattern = 'wq_[[:graph:]]+_new')) %>%
  bind_rows() %>%
  bind_rows(temp_SERF_SD_new) %>% 
  arrange(siteid, plotid, location, var_NEW, sample_type, height, date, time) ->
  wq_ALL


# Save for later analysis
write_rds(wq_ALL, 'Inter_Data/wq_ALL.rds', compress = 'xz')



