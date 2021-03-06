# Load the RSQLite Library
library(RSQLite)
library(tidyverse)
library(lubridate)
library(janitor)



# Connect to SQLite DB -----------------------------------------------------------------------

# Create a connection to a new database for storing all data
conn <- dbConnect(RSQLite::SQLite(), 'Final_Database//TD_ALL_Data.db')
conn_final <- dbConnect(RSQLite::SQLite(), 'Final_Database//TD_FINAL_Data.db')

# Create list of private farms
private_farms <- c("AUGLA", "CRAWF", "DEFI_M", "HARDIN", "HARDIN_NW", "HENRY", "STJOHNS",
                   "BEAR", "BEAR2", "BENTON", "DIKE", "HICKORY", "MAASS", "SHEARER", "STORY", 
                   #"CLAY_C", "CLAY_R", "CLAY_U", "FAIRM", "UBWC", 
                   #"DEFI_R",  "FULTON", "VANWERT", "WILKIN1", "WILKIN2", "WILKIN3"
                   "HICKS_B", "HICKS_P", "MUDS4")



# MetaData ----------------------------------------------------------------
# ... Methods -------------------------------------------------------------
methods <- read_rds('Standard_Data/meta_methods.rds')
dbWriteTable(conn, "meta_methods", methods, overwrite = TRUE)
dbWriteTable(conn_final, "meta_methods", methods, overwrite = TRUE)

# ... History -------------------------------------------------------------
site_history <- read_rds('Standard_Data/meta_site_history.rds')
site_history %>%
  mutate(drainage_retention_practice = ifelse(siteid == 'FAIRM', 
                                              'Other', 
                                              drainage_retention_practice),
         drainage_retention_practice =
           ifelse(drainage_retention_practice == "DW Recycle",
                  "Drainage Water Recycling",
                  drainage_retention_practice),
         drainage_retention_practice =
           ifelse(drainage_retention_practice == "Saturated Buffers",
                  "Saturated Buffer",
                  drainage_retention_practice)) -> site_history_DB
dbWriteTable(conn, "meta_site_history", site_history_DB, overwrite = TRUE)

site_history_DB %>%
  filter(!siteid %in% c('BATH_A', 'BATH_R', 'HICKS_S')) %>%
  # standardize number of decimal points
  mutate(drainage_coefficient = 
           case_when(drainage_coefficient == '12.38 and 18.89' ~ '12.4 and 18.9', 
                     drainage_coefficient == '4.54 and 7.10' ~ '4.5 and 7.1',
                     drainage_coefficient == '8.95 and 15.41' ~ '9.0 and 15.4',
                     drainage_coefficient == '4.28 and 3.25' ~ '4.3 and 3.3',
                     drainage_coefficient == '20.71 and 17.26' ~ '20.7 and 17.3',
                     drainage_coefficient == '13.95 and 13.19' ~ '14.0 and 13.2',
                     drainage_coefficient == '20.22 and 36.87' ~ '20.2 and 36.9',
                     TRUE ~ drainage_coefficient),
         drain_depth =
           case_when(drain_depth == '1.0' ~ '1.00',
                     drain_depth == '1.2192' ~ '1.22',
                     drain_depth == '0.9144' ~ '0.91',
                     TRUE ~ drain_depth),
         drain_spacing = 
           case_when(drain_spacing == '10' ~ '10.0',
                     drain_spacing == '2.4 and 4.9 (subirrigated); 6 and 12 (conventional)' ~
                       '2.4 and 4.9 (subirrigated); 6.0 and 12.0 (conventional)',
                     TRUE ~ drain_spacing),
         latitude = 
           case_when(str_length(latitude) == 2 ~ paste0(latitude, ".00"),
                     str_length(latitude) == 4 ~ paste0(latitude, "0"),
                     TRUE ~ latitude),
         longitude = 
           case_when(str_length(longitude) == 3 ~ paste0(longitude, ".00"),
                     str_length(longitude) == 5 ~ paste0(longitude, "0"),
                     TRUE ~ longitude)) %>%
  # mask locations of private farms
  mutate(latitude = ifelse(siteid %in% private_farms,
                           paste0(str_sub(latitude, 1, -2), "X"),
                           latitude),
         longitude = ifelse(siteid %in% private_farms,
                            paste0(str_sub(longitude, 1, -2), "X"),
                            longitude)) %>% 
  select(-PI_institution_unit, -drainage_intensity,
         -kirkham_coefficient) -> site_history_FINAL_DB
dbWriteTable(conn_final, "meta_site_history", site_history_FINAL_DB, overwrite = TRUE)

# ... Plot ID -------------------------------------------------------------
plot_ids <- read_rds('Standard_Data/meta_plot_ids.rds')
plot_ids %>%
  # fix spelling error in the database
  mutate(drain_material = 
           ifelse(drain_material == "pvs", "pvc", drain_material)) -> plot_ids_DB
dbWriteTable(conn, "meta_plotids", plot_ids_DB, overwrite = TRUE)

plot_ids_DB %>%
  select(-(dwm_treatment:irrigation_type), -comments_dwm) -> plot_ids_FINAL_DB
dbWriteTable(conn_final, "meta_plotids", plot_ids_FINAL_DB, overwrite = TRUE)

# ... Treatment ID (by year) ----------------------------------------------
trt_by_year <- read_rds('Standard_Data/meta_plot_treatments_annual.rds')
dbWriteTable(conn, "meta_treatment_years", trt_by_year, overwrite = TRUE)

trt_by_year %>%
  left_join(plot_ids %>% select(siteid:irrigation_type, comments_dwm)) %>%
  mutate(comments = 
           case_when(siteid == 'AUGLA' & year == 2012 ~ comments_dwm,
                     siteid == 'DEFI_M' & year == 2012 ~ comments_dwm,
                     TRUE ~ NA_character_)) %>%
  mutate(dwm_treatment = ifelse(dwm_treatment == "varies" & is.na(comments),
                                str_to_lower(dwm), dwm_treatment)) %>%
  select(siteid, plotid, year, dwm_abb, 
         drainage_water_management = dwm_treatment, 
         irrigation = irrigation_type, comments) -> trt_by_year_FINAL_DB
dbWriteTable(conn_final, "meta_treatment_years", trt_by_year_FINAL_DB, overwrite = TRUE)



# Field Management --------------------------------------------------------

# ... Planting ------------------------------------------------------------
planting <- read_rds('Standard_Data/planting_ALL.rds')

planting %>%
  mutate(plant_rate_units = ifelse(is.na(plant_rate_units),
                                   "n/a",
                                   paste(plant_rate_units, "per ha"))) %>%
  mutate_at(vars(starts_with("date")), as.character) -> planting_DB
dbWriteTable(conn, "mngt_planting", planting_DB, overwrite = TRUE)

planting_DB %>%
  filter(action != "remove") %>%
  # this location was removed due to harvest area criteria
  filter(!(siteid == 'ACRE' & location == 'Field 8')) %>%
  select(-action, -operation) -> planting_FINAL_DB
dbWriteTable(conn_final, "mngt_planting", planting_FINAL_DB, overwrite = TRUE)

# ... Harvesting ----------------------------------------------------------
harvesting <- read_rds('Standard_Data/harvesting_ALL.rds')

harvesting %>%
  mutate_at(vars(starts_with("date")), as.character) -> harvesting_DB
dbWriteTable(conn, "mngt_harvesting", harvesting_DB, overwrite = TRUE)

harvesting_DB %>%
  filter(action != "remove") %>%
  # this location was removed due to harvest area criteria
  filter(!(siteid == 'ACRE' & location == 'Field 8')) %>%
  select(-action, -operation) -> harvesting_FINAL_DB
dbWriteTable(conn_final, "mngt_harvesting", harvesting_FINAL_DB, overwrite = TRUE)

# ... Fertilizing ---------------------------------------------------------
fertilizing <- read_rds('Standard_Data/fertilizing_ALL.rds')

fertilizing %>%
  # standardize location names for SWROC
  mutate(location = ifelse(siteid == 'SWROC', str_remove(location, "^0{1,2}"), location)) %>%
  mutate_at(vars(starts_with("date")), as.character) -> fertilizing_DB
dbWriteTable(conn, "mngt_fertilizing", fertilizing_DB, overwrite = TRUE)

fertilizing_DB %>%
  filter(action != "remove") %>%
  # this location was removed due to harvest area criteria
  filter(!(siteid == 'ACRE' & location == 'Field 8')) %>%
  # select variables of high value, quality and abundance 
  select(-action) -> fertilizing_FINAL_DB
dbWriteTable(conn_final, "mngt_fertilizing", fertilizing_FINAL_DB, overwrite = TRUE)

# ... Tillage -------------------------------------------------------------
tillage <- read_rds('Standard_Data/tillage_ALL.rds')

tillage %>%
  rename(tillage_type = operation_type) %>%
  # standardize location names for SWROC
  mutate(location = ifelse(siteid == 'SWROC', str_remove(location, "^0{1,2}"), location)) %>%
  mutate_at(vars(starts_with("date")), as.character) -> tillage_DB
dbWriteTable(conn, "mngt_tillage", tillage_DB, overwrite = TRUE)

tillage_DB %>%
  filter(action != "remove") %>%
  # this location was removed due to harvest area criteria
  filter(!(siteid == 'ACRE' & location == 'Field 8')) %>%
  # select variables of high value, quality and abundance 
  select(-action, -operation) -> tillage_FINAL_DB
dbWriteTable(conn_final, "mngt_tillage", tillage_FINAL_DB, overwrite = TRUE)

# ... DWM -----------------------------------------------------------------
dwm <- read_rds('Standard_Data/dwm_ALL.rds')

dwm %>%
  mutate_at(vars(starts_with("date")), as.character) -> dwm_DB
dbWriteTable(conn, "mngt_dwm", dwm_DB, overwrite = TRUE)

dwm_DB %>%
  filter(action != "remove") %>%
  select(-action, -time) -> dwm_FINAL_DB
dbWriteTable(conn_final, "mngt_dwm", dwm_FINAL_DB, overwrite = TRUE)

# ... Irrigation ----------------------------------------------------------
irrigation <- read_rds('Standard_Data/irrigation_ALL.rds')

irrigation %>%
  mutate_at(vars(starts_with("date_")), as.character) -> irrigation_DB
dbWriteTable(conn, "mngt_irrigation", irrigation_DB, overwrite = TRUE)

irrigation_DB %>%
  filter(action != "remove") %>%
  # after removing hours some entries become redundant and need to remove
  filter(!(siteid == 'DEFI_R' & date_irrigation_end == '2000-08-22')) %>%
  select(-action, -starts_with('time_irr')) -> irrigation_FINAL_DB
dbWriteTable(conn_final, "mngt_irrigation", irrigation_FINAL_DB, overwrite = TRUE)

# ... Notes ---------------------------------------------------------------
notes <- read_rds('Standard_Data/notes_ALL.rds')

notes -> notes_DB
dbWriteTable(conn, "mngt_notes", notes_DB, overwrite = TRUE)

notes %>%
  filter(action == "keep") %>%
  select(-action) %>%
  filter(!siteid %in% c('BATH_A', 'HICKS_S')) -> notes_FINAL_DB
dbWriteTable(conn_final, "mngt_notes", notes_FINAL_DB, overwrite = TRUE)

# ... Reside --------------------------------------------------------------
residue <- read_rds('Standard_Data/residue_ALL.rds')

residue -> residue_DB
dbWriteTable(conn, "mngt_residue", residue_DB, overwrite = TRUE)

residue_DB %>%
  filter(action == 'keep') %>%
  select(-action)  -> residue_FINAL_DB
dbWriteTable(conn_final, "mngt_residue", residue_FINAL_DB, overwrite = TRUE)

# ... Pesticide (not included in public dataset) --------------------------
pesticide <- read_rds('Standard_Data/pesticide_ALL.rds')

pesticide %>%
  mutate_at(vars(starts_with("date")), as.character) %>%
  select(action, everything()) -> pesticide_DB
dbWriteTable(conn, "mngt_pesticide", pesticide_DB, overwrite = TRUE)



# Agronomic Data ----------------------------------------------------------

read_rds('Standard_Data/agro_ALL.rds') -> agr

agr %>%
  mutate(year = as.integer(year),
         date = as.character(date)) %>%
  spread(var_NEW, value) %>%
  arrange(siteid, plotid, year, date) -> agr_DB

dbWriteTable(conn, "agronomic", agr_DB, overwrite = TRUE)
  

# count sites per variable
agr_DB %>%
  gather(code, value, starts_with('AGR')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         CROP, 
         VARIABLE_NAME = NEW_VAR_NAME,
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/agr_variable_count.csv')


# Save selected data (variables) for FINAL DB.
agr_DB %>%
  # select plots and locations of interest
  mutate(action = ifelse(is.na(action), 'keep', action)) %>%
  filter(action != 'remove') %>%
  # only data before 2019 goes to public db
  filter(year < 2019) %>%
  # remove plots that are under more than one drainage system
  filter(plotid != 'Inlet_A, Inlet_B') %>%
  # remove 2012 data for CLAY_R due to poor field management
  # see GitHub datateam/issues/276
  filter(!(siteid == 'CLAY_R' & year == 2012)) %>%
  # remove CRAWF data due to quality issues noted by Ehsan Ghane
  filter(siteid != 'CRAWF') %>%
  # see GitHub datatea/issues/343
  mutate(trt = ifelse(siteid == 'MUDS1' & year %in% 2004:2005, NA, trt),
         trt_value = ifelse(siteid == 'MUDS1' & year %in% 2004:2005, NA, trt_value),
         trt_value = 
           ifelse(siteid == 'MUDS1' & year %in% 2006:2007 & trt == 'N-treatment Corn',
                  NA, trt_value),
         trt = 
           ifelse(siteid == 'MUDS1' & year %in% 2006:2007 & trt == 'N-treatment Corn', 
                  NA, trt),
         trt_value = 
           ifelse(siteid == 'MUDS1' & year %in% 2009:2010 & str_detect(trt, 'Fungicide'),
                  NA, trt_value),
         trt = 
           ifelse(siteid == 'MUDS1' & year %in% 2009:2010 & str_detect(trt, 'Fungicide'), 
                  NA, trt)) %>%
  # select variables of high value, quality and abundance 
  select(-action, -harvested_area, 
         -AGR90.01.10, 
         -AGR46.21.10,
         -AGR48.21.20,
         -AGR48.21.25,
         -AGR58.21.20,
         -AGR58.21.60,
         -AGR66.21.10,
         -AGR66.21.60,
         -AGR68.21.20) %>%
  # remove rows with no data (after implementing selection of variables)
  filter_at(vars(starts_with('AGR')), any_vars(!is.na(.))) %>%
  mutate(year = as.integer(year),
         date = as.character(date)) -> agr_FINAL_DB

dbWriteTable(conn_final, "agronomic", agr_FINAL_DB, overwrite = TRUE)



# Water Quality Data ------------------------------------------------------
read_rds('Standard_Data/wq_ALL.rds') -> wq_hourly

wq_hourly %>%
  spread(var_NEW, value) %>%
  mutate(date = format(date, '%Y-%m-%d')) %>%
  select(siteid, plotid, location, height, subsample, sample_type, tmsp, date, time, UTC, timestamp_type,
         starts_with('WAT'), comments) -> wq_DB

dbWriteTable(conn, "water_quality", wq_DB, overwrite = TRUE)


# count sites per variable
wq_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/wq_variable_count.csv')


# Save selected data (variables) for FINAL DB. > NOTE: only DAILY values goes to the FINAL DB
wq_hourly %>%
  # remove locations due to questionable flow data
  filter(!(siteid == 'DEFI_R' & location %in% c("Reservoir Out", "A", "B", "M", "Offsite"))) %>%
  filter(!(siteid == 'VANWERT' & location %in% c("Reservoir Out", "Offsite"))) %>%
  # select variables of high value, quality and abundance 
  filter(var_NEW %in% c('WAT15',
                        'WAT30', 'WAT31', 'WAT33', 'WAT34', 'WAT35',
                        'WAT40', 'WAT41', 'WAT42', 'WAT43',
                        'WAT60')) %>%
  select(-comments, -UTC, -tmsp) %>%
  # calculate daily values
  mutate(value_parse = parse_number(value)) %>%
  # FIRST - average replicated measurements
  mutate(subsample = ifelse(is.na(subsample), 1, subsample)) %>%
  filter(!(subsample > 1 & value =='BDL')) %>%
  group_by(siteid, plotid, location, height, date, time, timestamp_type, sample_type, var_NEW) %>%
  summarise(value = first(value),
            subsample = max(subsample),
            value_parse = mean(value_parse, na.rm = TRUE)) %>%
  mutate(value = ifelse(subsample > 1 & value != 'BDL', as.character(round(value_parse, 4)), value)) %>%
  ungroup() %>%
  select(-subsample) %>%
  # SECOND - average over day
  mutate(value_temp = as.numeric(value),
         value_BDL = ifelse(value == 'BDL', 1, 0),
         value_LES = ifelse(str_detect(value, '<'), 1, 0)) %>%
  group_by(siteid, plotid, location, height, date, timestamp_type, sample_type, var_NEW) %>%
  summarise(value = first(value),
            value_parse = mean(value_parse, na.rm = TRUE),
            value_temp = mean(value_temp),
            value_BDL = max(value_BDL),
            value_LES = max(value_LES)) %>% 
  ungroup() %>%
  # THIRD - substitute average of BDLs with BDL for HOURLY data
  mutate(value_parse = ifelse(is.nan(value_parse), NA_real_, round(value_parse, 5)),
         value = ifelse(timestamp_type == 'I', as.character(value_parse), value),
         value = ifelse(timestamp_type == 'I' & value_BDL == 1 & is.na(value), 'BDL', value)) %>%
  mutate(date = format(date, '%Y-%m-%d')) %>%
  select(siteid, plotid, location, height, date, sample_type, var_NEW, value) %>%
  spread(var_NEW, value) -> wq_FINAL_DB


dbWriteTable(conn_final, "water_quality", wq_FINAL_DB, overwrite = TRUE)



# Tile Flow ---------------------------------------------------------------
read_rds('Standard_Data/tf_ALL_hourly_ORIGINAL.rds') -> tf

tf %>%
  select(-units, -var_name) %>%
  spread(var_NEW, value) %>%
  mutate(date = format(date, '%Y-%m-%d')) ->
  tf_DB

dbWriteTable(conn, "tile_flow", tf_DB)


# read daily data
read_rds('Standard_Data/tf_ALL_daily.rds') -> tf_daily
read_csv('Input_Data/WATER/tile_flow_for_ANOVA_2020-03-09.csv') -> tf_filled

tf_daily %>%
  # remove locations with questionable data
  filter(!(siteid == 'DEFI_R' & location %in% c("A", "B", "Offsite"))) %>%
  filter(!(siteid == 'VANWERT' & location %in% c("Reservoir Out", "Offsite"))) %>%
  # cut off any questionable high flow (some are double of Des Moines river!)
  mutate(value = case_when(
    siteid == 'DEFI_R' & location == "I" & value > 1000 ~ NA_real_,
    siteid == 'DEFI_R' & location == "J" & value > 1000 ~ NA_real_,
    siteid == 'DEFI_R' & location == "C" & value > 1500 ~ NA_real_,
    siteid == 'DEFI_R' & location == "D" & value > 5000 ~ NA_real_,
    siteid == 'DEFI_R' & location == "Wetland Out" & value > 25000 ~ NA_real_,
    TRUE ~ value)) %>%
  select(-time, -UTC, -timestamp_type) %>%
  spread(var_NEW, value) %>%
  full_join(tf_filled %>% select(-trt, -rep, -year, -season), 
            by = c('siteid' = 'site', 'plotid' = 'plot', 'date')) %>%
  arrange(siteid, plotid, location, date) %>%
  # standardize comments
  mutate(comments = case_when(str_detect(comments, 'predicted') ~ 'imputed',
                              str_detect(comments, 'filled') ~ 'imputed zero',
                              TRUE ~ comments)) %>%
  rename(WAT06x = flow) ->
  tf_FINAL_DB

dbWriteTable(conn_final, "tile_flow", tf_FINAL_DB, overwrite = TRUE)


# count sites per variable
tf_FINAL_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/tf_variable_count.csv')



# Irrigation ---------------------------------------------------------------
read_rds('Standard_Data/irr_ALL_daily.rds') -> irr

irr %>%
  spread(var_NEW, value) %>%
  remove_empty(which = 'cols') %>%
  mutate(date = format(date, '%Y-%m-%d')) ->
  irr_DB

dbWriteTable(conn, "irrigation", irr_DB)


# save daily data
irr_DB %>%
  select(-timestamp_type) -> irr_FINAL_DB

dbWriteTable(conn_final, "irrigation", irr_FINAL_DB, overwrite = TRUE)


# count sites per variable
irr_FINAL_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/irr_variable_count.csv')




# N Loads ---------------------------------------------------------------
read_rds('Standard_Data/nl_ALL_daily.rds') -> nl

nl %>%
  spread(var_NEW, value) %>%
  remove_empty("cols") %>%
  mutate(date = format(date, '%Y-%m-%d')) ->
  nl_DB

dbWriteTable(conn, "n_loads", nl_DB, overwrite = TRUE)


# read filled daily data
read_csv('Input_Data/WATER/loads_for_ANOVA_2020-03-09.csv') -> nl_filled

nl_DB %>%
  select(-timestamp_type) %>%
  # select variables of high value, quality and abundance 
  select(-WAT80, -WAT83) %>%
  mutate(date = ymd(date)) %>%
  full_join(nl_filled %>% select(-trt, -rep, -year, -season), 
            by = c('siteid' = 'site', 'plotid' = 'plot', 'date')) %>%
  arrange(siteid, plotid, location, date) %>%
  rename(WAT70x = loads) ->
  nl_FINAL_DB

dbWriteTable(conn_final, "n_loads", nl_FINAL_DB, overwrite = TRUE)


# count sites per variable
nl_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/nl_variable_count.csv')




# Water Table -------------------------------------------------------------

# Water table, water level and piezometric head data
read_rds('Standard_Data/water_table_daily_ALL.rds') -> wt_daily
read_rds('Standard_Data/water_table_hourly_ALL.rds') -> wt_hourly

bind_rows(wt_daily, wt_hourly) %>%
  select(siteid, plotid, location, reading_type, date, time, UTC, timestamp_type, tmsp,
         WAT01, WAT02, WAT03) -> wt

wt %>%
  mutate(date = format(date, '%Y-%m-%d')) -> 
  wt_DB

dbWriteTable(conn, "water_table", wt_DB)


# count sites per variable
wt_DB %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/wt_variable_count.csv')



# Save selected data (variables) for FINAL DB. > NOTE: only DAILY values goes to the FINAL DB
wt_DB %>%
  # calculate daily data
  group_by(siteid, plotid, location, reading_type, date, timestamp_type) %>%
  summarise(WAT01 = mean(WAT01, na.rm = TRUE)) %>%
  arrange(siteid, plotid, location, reading_type, date) %>%
  ungroup() %>%
  select(-timestamp_type) %>%
  # remove empty rows resulted from other (not WAT01) variables
  group_by(siteid, plotid, location, reading_type) %>% 
  mutate(CHECK = sum(!is.na(WAT01))) %>%
  filter(CHECK != 0) %>%
  ungroup() %>%
  # replace NaN with NAs
  mutate(WAT01 = ifelse(is.nan(WAT01), NA_real_, WAT01)) %>%
  select(-CHECK) ->
  wt_FINAL_DB

dbWriteTable(conn_final, "water_table", wt_FINAL_DB, overwrite = TRUE)


# Stage Data --------------------------------------------------------------

# Stage and water storage data
read_rds('Standard_Data/stage_hourly_ALL.rds') -> stage_hourly

stage_hourly %>%
  mutate(date = format(date, '%Y-%m-%d')) %>%
  select(siteid, plotid, location, reading_type, date, time, UTC, timestamp_type, tmsp,
         WAT04, WAT14) ->
  stage_hourly_DB

dbWriteTable(conn, "water_stage", stage_hourly_DB)


# count sites per variable
stage_hourly_DB %>%
  # leave only wetland
  filter(location != 'Reservoir') %>%
  gather(code, value, starts_with('WAT')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/st_variable_count.csv')


# Save selected data (variables) for FINAL DB. > NOTE: only DAILY values goes to the FINAL DB
stage_hourly_DB %>%
  # leave only wetland
  filter(location != 'Reservoir') %>%
  # calculate daily data
  group_by(siteid, plotid, location, reading_type, date) %>%
  summarise(WAT04 = mean(WAT04, na.rm = TRUE)) %>%
  arrange(siteid, plotid, location, reading_type, date) %>%
  ungroup() ->
  stage_FINAL_DB

dbWriteTable(conn_final, "water_stage", stage_FINAL_DB, overwrite = TRUE)


rm(wt, wt_daily, wt_hourly, wt_DB, wt_FINAL_DB,
   stage_hourly, stage_hourly_DB, stage_FINAL_DB)



# Weather Data ------------------------------------------------------------

# Hourly weather data
read_rds('Standard_Data/weather_hourly_all_variable.rds') -> weather_hourly

weather_hourly %>% 
  # replace NaN with NA
  gather(key, value, starts_with('CLIM')) %>%
  mutate(value = ifelse(is.nan(value), NA_real_, value)) %>%
  spread(key, value) %>%
  mutate(date = format(date, '%Y-%m-%d')) ->
  weather_hourly_DB

dbWriteTable(conn, "weather_hourly", weather_hourly_DB, overwrite = TRUE)

# count sites per variable
weather_hourly_DB %>%
  gather(code, value, starts_with('CLIM')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv') %>%
              filter(CROP == 'HOURLY'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/weather_hourly_variable_count.csv')


# Daily weather data
read_rds('Standard_Data/weather_daily_all_variable.rds') -> weather_daily

weather_daily %>% 
  gather(key, value, starts_with('CLIM')) %>% 
  mutate(value = ifelse(is.nan(value), NA_real_, value)) %>%
  spread(key, value) %>%
  mutate(date = format(date, '%Y-%m-%d')) ->
  weather_daily_DB


dbWriteTable(conn, "weather_daily", weather_daily_DB, overwrite = TRUE)

# count sites per variable
weather_daily_DB %>%
  gather(code, value, starts_with('CLIM')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv') %>%
              filter(CROP == 'DAILY'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/weather_daily_variable_count.csv')


# Save selected data (variables) for FINAL DB. > NOTE: only DAILY WEATHER goes to the FINAL DB
weather_daily_DB %>%
  select(siteid:date, CLIM01, CLIM03.01.01, CLIM03.01.02, CLIM03.01.03, CLIM03.02.01, 
         CLIM04.01.01, CLIM05.02, CLIM06.01, CLIM06.02, 
         CLIM07.03.01, CLIM07.03.03, CLIM07.04.01) %>%
  # remove 0 solar radiation readings
  mutate(CLIM05.02 = ifelse(!is.na(CLIM05.02) & CLIM05.02 == 0, NA, CLIM05.02)) %>%
  # remove erroneous readings from WRSIS sites
  mutate(DATE = ymd(date),
         YEAR = year(DATE),
         CLIM05.02 = case_when(siteid == 'DEFI_R' & YEAR %in% 2003:2006 ~ NA_real_,
                               siteid == 'FULTON' & YEAR %in% 2004:2006 ~ NA_real_,
                               siteid == 'VANWERT' ~ NA_real_,
                               TRUE ~ CLIM05.02)) %>%
  arrange(siteid, station, DATE) %>%
  select(-DATE, -YEAR) ->
  weather_daily_FINAL_DB

dbWriteTable(conn_final, "weather", weather_daily_FINAL_DB, overwrite = TRUE)



# Soil Moisture, Temperature and EC ---------------------------------------

read_rds('Standard_Data/sm_ALL_hourly_ORIGINAL.rds') -> sm

sm %>%
  mutate(date = format(date, '%Y-%m-%d')) -> 
  sm_DB

dbWriteTable(conn, "soil_moisture", sm_DB)


# calculate daily data
sm_DB %>%
  filter(!(siteid == 'HICKS_B' & timestamp_type == 'I' & year(tmsp) > 2013)) %>%
  group_by(siteid, plotid, location, depth, date, timestamp_type) %>%
  summarise(SOIL09 = mean(SOIL09, na.rm = TRUE),
            SOIL10 = mean(SOIL10, na.rm = TRUE),
            SOIL12 = mean(SOIL12, na.rm = TRUE)) %>%
  arrange(siteid, plotid, location, depth, date) %>%
  ungroup() %>%
  mutate(location = ifelse(str_detect(siteid, 'CLAY'), str_sub(location, 4, 5), location)) %>%
  # remove negative soil moisture readings
  mutate(SOIL09 = ifelse(SOIL09 < 0 & !is.na(SOIL09), 0, SOIL09)) %>%
  select(-timestamp_type) ->
  sm_FINAL_DB

dbWriteTable(conn_final, "soil_moisture", sm_FINAL_DB)


# count sites per variable
sm_FINAL_DB %>%
  gather(code, value, starts_with('SOIL')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/sm_variable_count.csv')


rm(sm_DB, sm_FINAL_DB)



# Soil Properties Data ----------------------------------------------------

read_rds('Standard_Data/soil_properties_ALL.rds') -> soil_properties

soil_properties %>%
  mutate(year = as.integer(year),
         date = as.character(date)) ->
  soil_properties_DB

dbWriteTable(conn, "soil_properties", soil_properties_DB, overwrite = TRUE)


# count sites per variable
soil_properties_DB %>%
  gather(code, value, starts_with('SOIL')) %>%
  filter(!is.na(value)) %>%
  distinct(siteid, code) %>%
  group_by(code) %>%
  nest() %>%
  mutate(n = map_dbl(data, nrow),
         sites = map_chr(data, ~ .x %>% pull(siteid) %>% paste(collapse = ', '))) %>%
  ungroup() %>%
  left_join(read_csv('Final_Database/var_code_key.csv'), 
            by = c('code' = 'NEW_CODE')) %>%
  select(CODE = code, 
         VARIABLE_NAME = NEW_VAR_NAME, 
         UNITS,
         NUMBER_OF_SITES = n, 
         LIST_OF_SITES = sites) %>%
  write_csv('Final_Database/summaries/sp_variable_count.csv')


# Save selected data (variables) for FINAL DB
soil_properties %>%
  rownames_to_column() %>%
  # remove variables that are measures < 3 sites
  select(-SOIL04, -SOIL07, 
         -SOIL05.01, -SOIL05.02,
         -SOIL20.02,
         -SOIL23.05, 
         -SOIL31.02, -SOIL32.03) %>%
  filter_at(vars(starts_with('SOIL')), any_vars(!is.na(.))) -> temp

soil_properties %>%
  rownames_to_column() %>%
  # remove variables that are measures < 3 sites
  select(-SOIL04, -SOIL07, 
         -SOIL05.01, -SOIL05.02,
         -SOIL20.02,
         -SOIL23.05,
         -SOIL31.02, -SOIL32.03) %>%
  filter_at(vars(starts_with('SOIL')), all_vars(is.na(.))) %>%
  filter(!siteid %in% c('ACRE', 'CLAY_R', 'DPAC', 'FAIRM', 'MUDS1', 'SERF_SD')) %>%
  filter(!(siteid == 'STJOHNS' & year == 2013 & depth %in% c('20 to 40 cm', '40 to 60 cm'))) %>%
  bind_rows(temp) %>%
  arrange(as.numeric(rowname)) %>%
  select(-rowname) %>%
  arrange(siteid, plotid, location, subsample, depth, year, date) %>%
  ungroup() %>%
  # remove plots that are outside of research boundaries or shared by drainage areas at ACRE 
  filter(!(siteid == 'ACRE' & is.na(plotid))) %>%
  filter(!(siteid == 'ACRE' & str_detect(location, '22-E'))) %>%
  mutate(year = as.integer(year),
         date = as.character(date)) %>%
  mutate(depth = 
           case_when(depth == '0 to 22.86 cm'       ~ '0 to 23 cm',
                     depth == '22.86 to 55.88 cm'   ~ '23 to 56 cm',
                     depth == '55.88 to 121.92 cm'  ~ '56 to 122 cm',
                     depth == '3.81 to 11.43 cm'    ~ '4 to 11 cm',
                     depth == '11.43 to 19.05 cm'   ~ '11 to 19 cm',
                     depth == '26.67 to 34.29 cm'   ~ '27 to 34 cm',
                     depth == '57.15 to 64.77 cm'   ~ '57 to 65 cm',
                     depth == '87.63 to 95.25 cm'   ~ '88 to 95 cm',
                     depth == '118.11 to 125.73 cm' ~ '118 to 126 cm',
                     depth == '148.59 to 156.21 cm' ~ '149 to 156 cm',
                     depth == '179.07 to 186.69 cm' ~ '179 to 187 cm',
                     depth == '209.55 to 217.17 cm' ~ '210 to 217 cm',
                     depth == '240.03 to 247.65 cm' ~ '240 to 248 cm',
                     depth == '24.38 cm'     ~ '24 cm',
                     depth == '48.77 cm'     ~ '49 cm',
                     depth == '73.15 cm'     ~ '73 cm',
                     depth == '97.54 cm'     ~ '98 cm',
                     depth == '121.92 cm'    ~ '122 cm',
                     depth == '2.5 cm'       ~ '3 cm',
                     depth == '13.0 cm'      ~ '13 cm',
                     depth == '28.0 cm'      ~ '28 cm',
                     depth == '48.0 cm'      ~ '48 cm',
                     depth == '7.5 cm'       ~ '8 cm',
                     depth == '12.5 cm'      ~ '13 cm',
                     depth == '17.5 cm'      ~ '18 cm',
                     depth == '22.5 cm'      ~ '23 cm',
                     depth == '27.5 cm'      ~ '28 cm',
                     depth == '32.5 cm'      ~ '33 cm',
                     depth == '37.5 cm'      ~ '38 cm',
                     depth == '42.5 cm'      ~ '43 cm',
                     depth == '47.5 cm'      ~ '48 cm',
                     TRUE ~ depth)) ->
  soil_properties_FINAL_DB

dbWriteTable(conn_final, "soil_properties", soil_properties_FINAL_DB, overwrite = TRUE)

