# Initialize functions 
source('00_project_settings.R')
library(janitor)



# READ ....................................................................
# Read each site-data separately



# ACRE --------------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/ACRE_soil_moisture_and_temp.csv') %>%
  select(-X1) -> sm_ACRE

# format table
sm_ACRE %>%
  mutate(location = str_replace(location, 'Plot', 'Field'),
         tmsp = date,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, depth, date, tmsp,
         soil_moisture = Soil_Moisture, soil_temp = Soil_Temp) -> 
  soil_ACRE_sm



# BEAR --------------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/BEAR_soil_moisture_HOURLY_2015_2018.csv',
         guess_max = 300000) -> sm_BEAR

# format table
sm_BEAR %>% 
  mutate(plotid = NA_character_,
         depth = paste(round(depth_in * 2.54, 2), 'cm'),
         tmsp = date,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, depth, date, tmsp,
         soil_moisture = sm, soil_temp = temp_C) -> 
  soil_BEAR_sm



# BENTON ------------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/BENTON_soil_moisture.csv') -> sm_BENTON

# format table
sm_BENTON %>%
  mutate(location = NA_character_,
         plotid = NA_character_,
         depth = paste(round(depth_in * 2.54, 2), 'cm'),
         tmsp = date,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, depth, date, tmsp,
         soil_moisture = moisture, soil_temp = temperature) -> 
  soil_BENTON_sm



# CLAY_R ------------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/CALY_R_Soil_Moisture_DARYL.csv') ->  sm_CLAY_R

# format table
sm_CLAY_R %>%
  mutate(location = paste0(plotid, '_M', location),
         tmsp = timestamp,
         date = as.Date(timestamp)) %>%
  select(siteid = uniqueid, plotid, location, depth, date, tmsp,
         soil_moisture, soil_temp, soil_ec) -> 
  soil_CLAY_R_sm



# CLAY_U ------------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/CALY_U_Soil_Moisture_DARYL.csv') ->  sm_CLAY_U

# format table
sm_CLAY_U %>%
  mutate(location = ifelse(plotid == 'East', 'UD_M1', 'UD_M2'),
         plotid = 'UD',
         tmsp = timestamp,
         date = as.Date(timestamp)) %>%
  select(siteid = uniqueid, plotid, location, depth, date, tmsp,
         soil_moisture, soil_temp, soil_ec) -> 
  soil_CLAY_U_sm



# DIKE --------------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/DIKE_soil_moisture.csv', guess_max = 100000) -> sm_DIKE

# format table
sm_DIKE %>%
  mutate(location = NA_character_,
         plotid = NA_character_,
         depth = paste(round(depth_in * 2.54, 2), 'cm'),
         tmsp = date,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, depth, date, tmsp,
         soil_moisture = moisture) -> 
  soil_DIKE_sm



# DPAC --------------------------------------------------------------------
read_rds('Input_Data/SOIL/SM/FROM_WEB/Output_Data/dpac_correct.rds') -> sm_DPAC

# format table
sm_DPAC %>%
  mutate(date = as.Date(st_tmsp)) %>%
  select(siteid, plotid, depth, date, tmsp = st_tmsp,
         soil_moisture = Moisture, soil_temp = Temp) ->
  soil_DPAC_sm



# FAIRM ------------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/FAIRM_Soil_Moisture_DARYL.csv') -> sm_FAIRM

# format table
sm_FAIRM %>%
  mutate(location = ifelse(plotid == 'North', '1', '2'),
         plotid = 'West',
         tmsp = timestamp,
         date = as.Date(timestamp)) %>%
  select(siteid = uniqueid, plotid, location, depth, date, tmsp,
         soil_moisture, soil_temp, soil_ec) -> 
  soil_FAIRM_sm



# HICKS_B -----------------------------------------------------------------
read_rds('Input_Data/SOIL/SM/FROM_WEB/Output_Data/hicks_correct.rds') -> sm_HICKS_B

# read daily Soil Moisture
read_csv('Input_Data/SOIL/SM/HICKS_B_soil_moisture_DAILY_2014-2017.csv') -> sm_daily_HICKS_B

# format table
sm_HICKS_B %>%
  mutate(date = as.Date(st_tmsp)) %>%
  select(siteid, plotid, depth, date, tmsp = st_tmsp,
         soil_moisture = Moisture, soil_temp = Temp) %>%
  full_join(sm_daily_HICKS_B, by = c('siteid', 'plotid' = 'plotID', 'depth', 'date')) %>%
  filter(!is.na(tmsp)) %>% tail(10)
  # soil_HICKS_B_sm

 

# HICKS_P -----------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/HICKS_P_soil_moisture.csv')-> sm_HICKS_P

# format table
sm_HICKS_P %>%
  mutate(location = NA_character_,
         plotid = NA_character_,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, depth, date,
         soil_moisture = sm) -> 
  soil_HICKS_P_sm



# MAASS -------------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/MAASS_soil_moisture_HOURLY_2015_2018.csv',
         guess_max = 400000) -> sm_MAASS

# format tables
sm_MAASS %>%
  mutate(plotid = NA_character_,
         depth = paste(round(depth_in * 2.54, 2), 'cm'),
         tmsp = date,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, depth, date, tmsp,
         soil_moisture = sm, soil_temp = temp_C) -> 
  soil_MAASS_sm



# SERF_IA -----------------------------------------------------------------
read_rds('Input_Data/SOIL/SM/FROM_WEB/Output_Data/serfia_correct.rds') -> sm_SERF_IA

# format table
sm_SERF_IA %>%
  mutate(date = as.Date(st_tmsp),
         plotid = paste0('S', plotid)) %>%
  select(siteid, plotid, depth, date, tmsp = st_tmsp,
         soil_moisture = Moisture, soil_temp = Temp) ->
  soil_SERF_IA_sm



# SERF_SD -----------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/SERF_SD_soil_moisture.csv') -> sm_SERF_SD

# format table
sm_SERF_SD %>%
  select(siteid, plotid = plotID, location = subsample, depth, date,
         soil_moisture = sm) ->
  soil_SERF_SD_sm



# STJOHNS -----------------------------------------------------------------
read_rds('Input_Data/SOIL/SM/FROM_WEB/Output_Data/stjohns_correct.rds') -> sm_STJOHNS

# format table
sm_STJOHNS %>%
  mutate(date = as.Date(st_tmsp),
         location = plotid,
         plotid = str_sub(plotid, 1, 2)) %>%
  select(siteid, plotid, location, depth, date, tmsp = st_tmsp,
         soil_moisture = Moisture, soil_temp = Temp) ->
  soil_STJOHNS_sm



# SWROC -------------------------------------------------------------------
read_csv('Input_Data/SOIL/SM/SWROC_soil_moisture.csv') -> sm_SWROC

# format table
sm_SWROC %>%
    mutate(location = ifelse(is.na(treatment), treatment, paste(treatment, 'N'))) %>%
    select(siteid, plotid = plotID, location, depth, date,
           soil_moisture = sm) ->
    soil_SWROC_sm

  

# ALL ---------------------------------------------------------------------
# COMBINE .................................................................



# Combnine all hourly water table data
mget(ls(pattern = 'soil_[[:graph:]]+_sm')) %>%
  bind_rows() -> sm_ALL


# Save for later analysis
write_rds(sm_ALL, 'Inter_Data/sm_ALL.rds')


