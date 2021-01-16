# Load the RSQLite Library
library(RSQLite)
library(tidyverse)
library(lubridate)
library(janitor)
library(googledrive)



# Create a connection to a new database for storing all data
conn_final <- dbConnect(RSQLite::SQLite(), 'Final_Database//TD_FINAL_Data.db')

# Read var codes
codes <- read_csv('Final_Database/var_code_key.csv')

# Read site ids
siteids <- read_csv('Ag_Commons_Data/siteids.csv')




# Function to replace old IDs with new ones
ReplaceIDs <- function(df){
  df %>%
    left_join(siteids, by = 'siteid') %>%
    mutate(siteid = ID) %>%
    select(-ID, -KEY)
}

# Function to round numbers in mixed column (so you can retain non-numeric values)
RoundNumber <- function(df, COL, ROUND = 2) {
  COL <- sym(COL)
  df %>% 
    mutate(temp = as.numeric(!!COL),
           temp = round(temp, ROUND),
           !!COL := ifelse(is.na(temp), !!COL, as.character(temp)))
}

# Function to get table from the database
GetSQLiteTable <- function(){
  tables  <- dbListTables(conn_final) %>%
    sort()
  tables_list <- tables %>%
    paste0("   ", seq_along(.), '. ', .) %>%
    paste(collapse = '\n')
  
  cat('List of tables available in the Database: \n')
  cat(tables_list)
  tn <- readline('enter number for table to read: \n') %>%
    as.numeric()
  
  if (tn %in% seq_along(tables)) {
    df <- dbReadTable(conn_final, tables[tn]) %>%
      as_tibble()
  } else {
    print('You have entered incorrect number')
  }
  return(df)
}


# Metadata --------------------------------------------------------
# ....  Site History ---------
meta_site_history <- dbReadTable(conn_final, 'meta_site_history') %>% as_tibble()


# Format Site History data
meta_site_history %>%
  ReplaceIDs() %>%
  arrange(siteid) -> site_history_EXP

write_csv(site_history_EXP, 'Ag_Commons_Data/meta_site_characteristics.csv')

drive_upload(media = 'Ag_Commons_Data/meta_site_characteristics.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



# ....  Plot IDs ---------
meta_plotids <- dbReadTable(conn_final, 'meta_plotids') %>% as_tibble()
plotids_locations <- read_excel('Final_Database/summaries/IDs.xlsx')

# Format Plot ID data
plotids_locations %>%
  full_join(meta_plotids, by = c('siteid', 'plotid')) %>%
  select(-ID) %>%
  ReplaceIDs() %>%
  mutate(comments = str_replace_all(comments, '\n', '; ')) %>%
  select(siteid, plotid, 
         # dwm_treatment, dwmid, # THIS IS MOVED TO treatment_identifier
         # irrigation_type, irrid, # THIS IS MOVED TO treatment_identifier
         drainage_area:tile_material,
         starts_with('location'),
         comments) %>%
  arrange(siteid, plotid) -> plotids_EXP

write_csv(plotids_EXP, 'Ag_Commons_Data/meta_plot_characteristics.csv')

drive_upload(media = 'Ag_Commons_Data/meta_plot_characteristics.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



# ....  Treatment by Year ---------
meta_trt_year <- dbReadTable(conn_final, 'meta_treatment_years') %>% as_tibble()

# Format Treatments data
meta_trt_year %>%
  left_join(plotids_locations %>% 
              select(siteid, plotid, dwmid, irrid)) %>%
  select(-dwm_abb) %>%
  mutate(SITEID = siteid) %>%
  ReplaceIDs() %>%
  select(SITEID, siteid, plotid, dwmid, irrid, 
         year, drainage_water_management, irrigation, comments) %>%
  arrange(siteid, plotid) -> treatment_EXP

treatment_EXP %>% 
  select(-SITEID) %>%
  write_csv('Ag_Commons_Data/meta_treatment_identifier.csv')

drive_upload(media = 'Ag_Commons_Data/meta_treatment_identifier.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'),
             overwrite = TRUE)



# ....  Methods ---------
meta_methods <- dbReadTable(conn_final, 'meta_methods') %>% as_tibble()


# Format Methods data
meta_methods %>%
  # combine water NO3-N concentration and NO3-N + NO2-N 
  mutate(NEW_CODE = ifelse(NEW_CODE == 'WAT30', 'WAT31', NEW_CODE)) %>%
  group_by(siteid, data_category, NEW_CODE) %>%
  mutate(count = 1:n(),
         count = ifelse(siteid == 'ACRE' & NEW_CODE == 'WAT31', -1, count)) %>%
  group_by(siteid, data_category, NEW_CODE, count) %>%
  summarise(method_description = paste(method_description, collapse = ' ')) %>% 
  ungroup() %>%
  # remove entries without method description
  filter(method_description != 'Method not available') %>%
  left_join(codes %>% select(-TYPE, -CROP, -(UNITS:EXPORT_VAR_NAME)), 
            by = 'NEW_CODE') %>%
  # remove variables not included in the public database
  filter(ACTION == 'YES' | is.na(ACTION)) %>%
  select(siteid, data_category, variable_name = NEW_VAR_NAME, method_description) %>%
  ReplaceIDs() %>%
  arrange(siteid, data_category) -> methods_EXP

write_csv(methods_EXP, 'Ag_Commons_Data/meta_methods.csv')

drive_upload(media = 'Ag_Commons_Data/meta_methods.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



# Agronomic data ----------------------------------------------------------
agr <- dbReadTable(conn_final, 'agronomic') %>% as_tibble()

# Format data
agr %>%
  gather(key, value, starts_with('AGR')) %>%
  filter(!is.na(value)) %>%
  left_join(codes %>% select(-TYPE, -(CROP:UNITS)), 
            by = c('key' = 'NEW_CODE')) %>%
  # FYI - all values are numeric only
  mutate(value = round(as.numeric(value), DIGITS)) %>%
  select(-key, -DIGITS) %>%
  spread(EXPORT_VAR_NAME, value) %>%
  ReplaceIDs() %>%
  arrange(siteid, plotid, location, year) %>%
  select(siteid:date,
         leaf_area_index,
         final_plant_population,
         grain_moisture,
         crop_yield,
         biomass_yield,
         whole_plant_biomass,
         vegetative_biomass,
         grain_biomass,
         corn_cob_biomass,
         whole_plant_total_N,
         vegetative_total_N,
         grain_total_N,
         corn_cob_total_N,
         vegetative_total_C,
         grain_total_C,
         corn_cob_total_C) -> agr_EXP

write_csv(agr_EXP, 'Ag_Commons_Data/agronomic_data.csv')

drive_upload(media = 'Ag_Commons_Data/agronomic_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



# Soil Properties Data ----------------------------------------------------
sp <- dbReadTable(conn_final, "soil_properties") %>% as_tibble()

# Format data
sp %>%
  rownames_to_column() %>%
  gather(key, value, starts_with('SOIL')) %>%
  filter(!is.na(value)) %>%
  mutate(value2 = as.numeric(value)) %>% 
  # round up results of particle analysis
  mutate(value3 = ifelse(str_detect(key, 'SOIL02'), round(value2, 1), NA)) %>%
  # correct errors introduced du to rounding by adjusting sand content
  group_by(siteid, plotid, location, subsample, depth, year, date) %>%
  mutate(value4 = sum(value3, na.rm = TRUE),
         value4 = ifelse(key == 'SOIL02.01', round(value4, 1), NA)) %>%
  mutate(value3 = ifelse(value4 != 100 & !is.na(value3), value3 + 100 - value4, value3),
         value2 = ifelse(!is.na(value3) & str_detect(key, 'SOIL02'), value3, value2)) %>%
  ungroup() %>%
  # round up the rest
  left_join(codes %>% select(-TYPE, -(CROP:UNITS)),
            by = c('key' = 'NEW_CODE')) %>%
  mutate(value2 = as.character(round(value2, DIGITS)),
         value  = ifelse(is.na(value2), value, as.character(value2))) %>%
  select(-value2, -value3, -value4, -DIGITS, -key) %>%
  spread(EXPORT_VAR_NAME, value) %>%
  select(-rowname) %>%
  # remove measurements after 2018
  filter(year < 2019) %>%
  ReplaceIDs() %>%
  arrange(siteid, plotid, location, subsample, depth, year, date) %>%
  select(siteid:date,
         soil_texture,
         percent_sand,
         percent_silt,
         percent_clay,
         bulk_density,
         hydraulic_conductivity,
         infiltration_rate,
         matric_potential,
         water_content,
         som,
         pH_water,
         pH_salt,
         lime_index,
         neutralizable_acidity,
         cec,
         K_saturation,
         Ca_saturation,
         Mg_saturation,
         Na_saturation,
         K_concentration,
         Ca_concentation,
         Mg_concentration,
         Na_concentration,
         K_amount,
         Ca_amount,
         Mg_amount,
         sar,
         salinity_paste,
         salinity_water,
         soc,
         total_N,
         NO3_concentration,
         NH4_concentration,
         NO3_amount,
         NH4_amount,
         P_B1_concentration,
         P_M3_concentration,
         P_B1_amount,
         P_M3_amount) -> sp_EXP

write_csv(sp_EXP, 'Ag_Commons_Data/soil_properties_data.csv')

drive_upload(media = 'Ag_Commons_Data/soil_properties_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



# Soil Moisture data ------------------------------------------------------
sm <- dbReadTable(conn_final, 'soil_moisture') %>% as_tibble()

# Format data
sm %>%
  gather(key, value, starts_with('SOIL')) %>%
  left_join(codes %>% select(-TYPE, -(CROP:UNITS)), 
            by = c('key' = 'NEW_CODE')) %>%
  mutate(value = round(value, DIGITS)) %>%
  select(-key, -DIGITS) %>%
  spread(EXPORT_VAR_NAME, value) %>%
  # remove measurements after 2018
  mutate(date = ymd(date)) %>%
  filter(year(date) < 2019) %>%
  ReplaceIDs() %>%
  arrange(siteid, plotid, location, depth, date) %>%
  select(siteid:date,
         soil_moisture,
         soil_temperature,
         soil_ec) -> sm_EXP


write_csv(sm_EXP, 'Ag_Commons_Data/soil_moisture_data.csv')

drive_upload(media = 'Ag_Commons_Data/soil_moisture_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



# Water Table data --------------------------------------------------------
wt <- dbReadTable(conn_final, 'water_table') %>% as_tibble()

# Format data
wt %>%
  gather(key, value, starts_with('WAT')) %>%
  left_join(codes %>% select(-TYPE, -(CROP:UNITS)), 
            by = c('key' = 'NEW_CODE')) %>%
  mutate(value = round(value, DIGITS)) %>%
  select(-key, -DIGITS) %>%
  spread(EXPORT_VAR_NAME, value) %>%
  # remove measurements after 2018
  mutate(date = ymd(date)) %>%
  filter(year(date) < 2019) %>%
  ReplaceIDs() %>%
  arrange(siteid, plotid, location, date) %>%
  select(siteid:date,
         water_table_depth) -> wt_EXP


write_csv(wt_EXP, 'Ag_Commons_Data/water_table_data.csv')

drive_upload(media = 'Ag_Commons_Data/water_table_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



# Water Stage Data --------------------------------------------------------
st <- dbReadTable(conn_final, 'water_stage') %>% as_tibble()

# Format data
st %>%
  gather(key, value, starts_with('WAT')) %>%
  left_join(codes %>% select(-TYPE, -(CROP:UNITS)), 
            by = c('key' = 'NEW_CODE')) %>%
  mutate(value = round(value, DIGITS)) %>%
  select(-key, -DIGITS) %>%
  spread(EXPORT_VAR_NAME, value) %>%
  # remove measurements after 2018
  mutate(date = ymd(date)) %>%
  filter(year(date) < 2019) %>%
  ReplaceIDs() %>%
  arrange(siteid, plotid, location, reading_type, date) %>%
  select(siteid:date,
         stage) -> st_EXP


write_csv(st_EXP, 'Ag_Commons_Data/water_stage_data.csv')

drive_upload(media = 'Ag_Commons_Data/water_stage_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



# Water Quality data ------------------------------------------------------
wq <- dbReadTable(conn_final, 'water_quality') %>% as_tibble()

# Format data
wq %>%
  gather(key, value, starts_with('WAT')) %>%
  left_join(codes %>% select(-TYPE, -(CROP:UNITS)), 
            by = c('key' = 'NEW_CODE')) %>%
  mutate(value2 = as.numeric(value),
         value2 = round(value2, DIGITS),
         value = ifelse(is.na(value2), value, as.character(value2))) %>% 
  select(-key, -DIGITS, -value2) %>%
  filter(!is.na(value)) %>%
  spread(EXPORT_VAR_NAME, value) %>%
  mutate(nitrate_N_concentration = ifelse(nitrate_N_concentration == '<0.030', 
                                          '<0.03', 
                                          nitrate_N_concentration),
         ortho_P_filtered_concentration = ifelse(ortho_P_filtered_concentration == '<2.0', 
                                          '<2', 
                                          ortho_P_filtered_concentration)) %>%
  # remove measurements after 2018
  mutate(date = ymd(date)) %>%
  filter(year(date) < 2019) %>%
  ReplaceIDs() %>%
  arrange(siteid, plotid, location, height, sample_type, date) %>% 
  select(siteid:sample_type,
         nitrate_N_concentration,
         ammonia_N_concentration,
         total_N_filtered_concentration,
         total_N_unfiltered_concentration,
         ortho_P_filtered_concentration,
         ortho_P_unfiltered_concentration,
         total_P_filtered_concentration,
         total_P_unfiltered_concentration,
         pH,
         water_ec) -> wq_EXP


write_csv(wq_EXP, 'Ag_Commons_Data/water_quality_data.csv')

drive_upload(media = 'Ag_Commons_Data/water_quality_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)




# Tile Flow and N Load data -----------------------------------------------
tf <- dbReadTable(conn_final, 'tile_flow') %>% as_tibble() %>% mutate(date = as_date(date))
nl <- dbReadTable(conn_final, 'n_loads') %>% as_tibble() %>% mutate(date = as_date(date)) %>% filter(!is.na(date))

tf %>% 
  full_join(nl, by = c('siteid', 'plotid', 'location', 'date')) %>%
  gather(key, value, starts_with('WAT')) %>%
  left_join(codes %>% select(-TYPE, -(CROP:UNITS)), 
            by = c('key' = 'NEW_CODE')) %>%
  mutate(value2 = as.numeric(value),
         value2 = round(value2, DIGITS),
         value = ifelse(is.na(value2), value, as.character(value2))) %>% 
  select(-key, -DIGITS, -value2, -ACTION) %>%
  filter(!is.na(value)) %>%
  spread(EXPORT_VAR_NAME, value) %>%
  # return back dates with missing measurements removed due to data transformations
  full_join(tf %>% select(siteid:date)) %>%
  full_join(nl %>% select(siteid:date)) %>%
  # remove measurements after 2018
  filter(year(date) < 2019) %>%
  ReplaceIDs() %>%
  # add dwm treatment
  mutate(year = year(date)) %>%
  left_join(treatment_EXP %>% select(-dwr)) %>%
  mutate(dwm = ifelse(SITEID == 'AUGLA' & plotid == 'East' & year == 2012 & date > ymd(20120617),
                      'Controlled Drainage', dwm),
         dwm = ifelse(SITEID == 'AUGLA' & plotid == 'West' & year == 2012 & date > ymd(20120617),
                      'Free Drainage', dwm),
         dwm = ifelse(SITEID == 'DEFI_M' & plotid == 'East' & year == 2012 & date < ymd(20120105),
                      'Controlled Drainage', dwm),
         dwm = ifelse(SITEID == 'DEFI_M' & plotid == 'West' & year == 2012 & date < ymd(20120105),
                      'Free Drainage', dwm)) %>%
  left_join(treatment_EXP %>% 
              mutate(location = case_when(SITEID == 'DEFI_R' & plotid == 6 ~ "I",
                                          SITEID == 'DEFI_R' & plotid == 8 ~ "J",
                                          TRUE ~ NA_character_)) %>% 
              filter(!is.na(location)) %>%
              select(siteid, location, year, dwm2 = dwm)) %>%
  mutate(dwm = ifelse(is.na(dwm2), dwm, dwm2)) %>%
  arrange(siteid, plotid, location, date) %>% 
  select(siteid:date,
         dwm_treatment = dwm,
         tile_flow,
         discharge,
         nitrate_N_load,
         nitrate_N_removed,
         tile_flow_filled,
         nitrate_N_load_filled,
         comments) %>%
  # CLAY_C has couple years with 0 drainage, but loads are missing
  # add 0 loads for days when flow was measured at CLAY_C
  mutate(nitrate_N_load = ifelse(siteid == 'MN_Clay1' & 
                                   year(date) %in% 2015:2018 &
                                   tile_flow == 0, 
                                 0, 
                                 nitrate_N_load)) -> tf_EXP


write_csv(tf_EXP, 'Ag_Commons_Data/tile_flow_and_N_loads_data.csv')

drive_upload(media = 'Ag_Commons_Data/tile_flow_and_N_loads_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)

  

# Irrigation data ---------------------------------------------------------
irr <- dbReadTable(conn_final, 'irrigation') %>% as_tibble()

# Format data
irr %>%
  gather(key, value, starts_with('WAT')) %>%
  left_join(codes %>% select(-TYPE, -(CROP:UNITS)), 
            by = c('key' = 'NEW_CODE')) %>%
  mutate(value2 = as.numeric(value),
         value2 = round(value2, DIGITS),
         value = ifelse(is.na(value2), value, as.character(value2))) %>%
  select(-key, -DIGITS, -value2) %>%
  filter(!is.na(value)) %>%
  spread(EXPORT_VAR_NAME, value) %>%
  # remove measurements after 2018
  mutate(date = ymd(date)) %>%
  filter(year(date) < 2019) %>%
  ReplaceIDs() %>%
  arrange(siteid, plotid, date) %>% 
  select(siteid:date,
         irrigation_amount) -> irr_EXP


write_csv(irr_EXP, 'Ag_Commons_Data/irrigation_data.csv')

drive_upload(media = 'Ag_Commons_Data/irrigation_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)


# Weather data -------------------------------------------------------
clim <- dbReadTable(conn_final, 'weather') %>% as_tibble()

# Format data
clim %>%
  gather(key, value, starts_with('CLIM')) %>%
  left_join(codes %>% filter(CROP == 'DAILY') %>% select(-TYPE, -(CROP:UNITS)), 
            by = c('key' = 'NEW_CODE')) %>%
  mutate(value = round(value, DIGITS)) %>%
  select(-key, -DIGITS) %>%
  spread(EXPORT_VAR_NAME, value) %>%
  # combine ET data for export
  mutate(et = ifelse(is.na(reference_ET_pm_sg),
                     reference_ET_pm_sc, reference_ET_pm_sg),
         et_method = ifelse(!is.na(et), 'Penman-Monteith (Short Grass)', NA),
         et = ifelse(is.na(reference_ET_t_g), et, reference_ET_t_g),
         et_method = ifelse(is.na(et_method) & !is.na(et), 'Thornthwaite (Grass)', et_method)) %>%
  select(!contains("_ET_")) %>%
  # remove measurements after 2018
  mutate(date = ymd(date)) %>%
  filter(year(date) < 2019) %>%
  ReplaceIDs() %>%
  arrange(siteid, station, date) %>%
  select(siteid:date,
         precipitation,
         relative_humidity = relative_humidity_avg,
         air_temp_avg, air_temp_min, air_temp_max,
         solar_radiation,
         wind_speed, wind_direction,
         et, et_method) %>% 
  # remove date with no readings
  filter(!(siteid == 'IA_Boone' & date == ymd(20160609))) -> clim_EXP


write_csv(clim_EXP, 'Ag_Commons_Data/weather_data.csv')

drive_upload(media = 'Ag_Commons_Data/weather_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



# Field Management --------------------------------------------------------
mngt_planting <- dbReadTable(conn_final, 'mngt_planting') %>% as_tibble()
mngt_fertilizing <- dbReadTable(conn_final, 'mngt_fertilizing') %>% as_tibble()
mngt_irrigation <- dbReadTable(conn_final, 'mngt_irrigation') %>% as_tibble()
mngt_residue <- dbReadTable(conn_final, 'mngt_residue') %>% as_tibble()
mngt_dwm <- dbReadTable(conn_final, 'mngt_dwm') %>% as_tibble()
mngt_notes <- dbReadTable(conn_final, 'mngt_notes') %>% as_tibble()

# Format Planting data
mngt_planting %>%
  # round up variables
  mutate(plant_maturity = ifelse(str_length(plant_maturity) > 3, 
                                 str_remove(plant_maturity, '\\.0$'),
                                 plant_maturity),
         plant_maturity_GDD_F = as.character(round(as.numeric(plant_maturity_GDD_F), 0)),
         temp = as.numeric(plant_rate),
         temp = case_when(plant_rate_units == "seeds" ~ round(temp, -1),
                          plant_rate_units == "kg"    ~ round(temp, 1),
                          TRUE ~ temp),
         plant_rate = ifelse(is.na(temp), plant_rate, as.character(temp))) %>%
  ReplaceIDs() %>%
  arrange(siteid, plotid, location, date) %>%
  select(-temp) -> planting_EXP

write_csv(planting_EXP, 'Ag_Commons_Data/mngt_planting_and_harvesting_data.csv')

drive_upload(media = 'Ag_Commons_Data/mngt_planting_and_harvesting_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



# Format Fertilizing data
mngt_fertilizing %>%
  # round up variables
  mutate(lime_rate = as.character(round(lime_rate, 2)),
         temp = as.numeric(fertilizer_rate),
         fertilizer_rate = ifelse(is.na(temp), 
                                  fertilizer_rate, 
                                  as.character(round(temp, 1)))) %>% 
  RoundNumber(COL = "nitrogen_elem") %>%
  RoundNumber(COL = "phosphorus_elem") %>%
  RoundNumber(COL = "potassium_elem") %>%
  RoundNumber(COL = "sulfur_elem") %>%
  RoundNumber(COL = "zinc_elem") %>%
  RoundNumber(COL = "magnesium_elem") %>%
  RoundNumber(COL = "calcium_elem") %>%
  RoundNumber(COL = "iron_elem") %>%
  ReplaceIDs() %>%
  arrange(siteid, plotid, location, date) %>%
  select(-temp) -> fertilizing_EXP

write_csv(fertilizing_EXP, 'Ag_Commons_Data/mngt_fertilizing_and_tillage_data.csv')

drive_upload(media = 'Ag_Commons_Data/mngt_fertilizing_and_tillage_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)


# Format Residue data
mngt_residue %>%
  ReplaceIDs() %>%
  arrange(siteid, year_calendar) -> residue_EXP

write_csv(residue_EXP, 'Ag_Commons_Data/mngt_residue_data.csv')

drive_upload(media = 'Ag_Commons_Data/mngt_residue_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)


# Format Irrigation data
mngt_irrigation %>%
  # round up variables
  RoundNumber(COL = "irrigation_amount") %>%
  ReplaceIDs() %>%
  arrange(siteid, irrigation_structure, date_irrigation_start) %>%
  select(-temp) -> irrigation_EXP

write_csv(irrigation_EXP, 'Ag_Commons_Data/mngt_irrigation_data.csv')

drive_upload(media = 'Ag_Commons_Data/mngt_irrigation_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)


# Format DWM data
mngt_dwm %>%
  # round up variables
  RoundNumber(COL = "outlet_depth", ROUND = 1) %>%
  ReplaceIDs() %>%
  arrange(siteid, control_structure, date) %>%
  select(-temp) -> dwm_EXP

write_csv(dwm_EXP, 'Ag_Commons_Data/mngt_dwm_data.csv')

drive_upload(media = 'Ag_Commons_Data/mngt_dwm_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)


# Format Notes data
mngt_notes %>%
  ReplaceIDs() %>%
  arrange(siteid, year_calendar) -> notes_EXP

write_csv(notes_EXP, 'Ag_Commons_Data/mngt_notes_data.csv')

drive_upload(media = 'Ag_Commons_Data/mngt_notes_data.csv',
             path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
             overwrite = TRUE)



