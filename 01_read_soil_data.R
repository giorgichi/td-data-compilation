# Initialize functions 
source('00_project_settings.R')
library(janitor)



# READ ....................................................................
# Read each site-data separately


# ACRE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/ACRE Soil Data.xlsx') %>%
  pluck(1) -> soil_ACRE

# format Soil Properties tables
soil_ACRE %>%
  remove_empty('cols') %>%
  mutate(siteid = "ACRE",
         location = plotID,
         plotID = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_ACRE_properties


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
  soil_BEAR_properties



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
  soil_BEAR2_properties



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
         everything(), -uniqueid, -sheet, -d1, -d2) -> 
  soil_BENTON_properties



# CLAY_C ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/CLAY_C Soil Data.xlsx') %>%
  pluck(1) -> soil_CLAY_C

# format Soil Properties tables
soil_CLAY_C %>%
  remove_empty('cols') %>%
  mutate(siteid = "CLAY_C",
         location = subsample,
         subsample = NA_character_,
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_CLAY_C_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/CLAY_C Soil Data.xlsx') %>%
  pluck(3) %>%
  remove_empty('cols') %>%
  mutate(siteid = "CLAY_C",
         location = subsample,
         subsample = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_CLAY_C_nitrate

# format Soil Water Retention tables
ReadExcelSheets('Input_Data/SOIL/CLAY_C Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  fill(plotID:date) %>%
  mutate(siteid = "CLAY_C",
         location = subsample,
         subsample = NA_character_,
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_CLAY_C_wr



# CLAY_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/CLAY_R Soil Data.xlsx') %>%
  pluck(1) ->  soil_CLAY_R

# format Soil Properties tables
soil_CLAY_R %>%
  remove_empty('cols') %>%
  mutate(siteid = "CLAY_R",
         location = subsample,
         subsample = NA_character_,
         year = ifelse(date < 2020, date, year(as.Date(date, origin = "1899-12-30"))),
         date = ifelse(date < 2020, NA, as.Date(date, origin = "1899-12-30")),
         date = as.Date(date, origin = origin)) %>%
  # correct location names at 0-5 depth in 2017
  mutate(location = ifelse(year == 2017 & depth == '0 - 5', paste0(location, '-2'), location)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_CLAY_R_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/CLAY_R Soil Data.xlsx') %>%
  pluck(3) %>%
  remove_empty('cols') %>%
  mutate(siteid = "CLAY_R",
         location = subsample,
         subsample = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_CLAY_R_nitrate

# format Soil Water Retention tables
ReadExcelSheets('Input_Data/SOIL/CLAY_R Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  fill(plotID:date) %>%
  mutate(siteid = "CLAY_R",
         location = subsample,
         subsample = NA_character_,
         depth = as.character(depth),
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_CLAY_R_wr


# CLAY_U ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/CLAY_U Soil Data.xlsx') %>%
  pluck(1) ->  soil_CLAY_U

# format Soil Properties tables
soil_CLAY_U %>%
  remove_empty('cols') %>%
  mutate(siteid = "CLAY_U",
         location = subsample,
         subsample = NA_character_,
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_CLAY_U_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/CLAY_U Soil Data.xlsx') %>%
  pluck(3) %>%
  remove_empty('cols') %>%
  mutate(siteid = "CLAY_U",
         location = subsample,
         subsample = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_CLAY_U_nitrate

# format Soil Water Retention tables
ReadExcelSheets('Input_Data/SOIL/CLAY_U Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  fill(plotID:date) %>%
  mutate(siteid = "CLAY_U",
         location = subsample,
         subsample = NA_character_,
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_CLAY_U_wr



# CRAWF -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/CRAWF Soil Data.xlsx') 


# DEFI_M ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/DEFI_M Soil Data.xlsx') 



# DEFI_R ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/DEFI_R Soil Data.xlsx') %>%
  pluck(1) -> soil_DEFI_R

# format Soil Properties tables
soil_DEFI_R %>%
  remove_empty('cols') %>%
  mutate(siteid = "DEFI_R",
         location = plotID,
         plotID = NA_character_,
         subsample = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_DEFI_R_properties



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
  filter(!(location == 'middle' & SOIL02.01 > 80)) %>%
  select(siteid, location, subsample, depth, year, date, 
         everything(), -uniqueid, -sheet, -d1, -d2) ->
  soil_DIKE_properties



# DPAC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/DPAC Soil Data.xlsx') %>%
  pluck(1) -> soil_DPAC

# format Soil Properties tables
soil_DPAC %>%
  remove_empty('cols') %>%
  mutate(siteid = "DPAC",
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_DPAC_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/DPAC Soil Data.xlsx') %>%
  pluck(4) %>%
  remove_empty('cols') %>%
  mutate(siteid = "DPAC",
         subsample = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_DPAC_nitrate

# format Soil Water Retention tables
ReadExcelSheets('Input_Data/SOIL/DPAC Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "DPAC",
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_DPAC_wr



# FAIRM ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/FAIRM Soil Data.xlsx') %>%
  pluck(1) -> soil_FAIRM

# format Soil Properties tables
soil_FAIRM %>%
  remove_empty('cols') %>%
  mutate(siteid = "FAIRM",
         location = subsample,
         subsample = NA_character_,
         year = ifelse(date < 2020, date, year(as.Date(date, origin = "1899-12-30"))),
         date = ifelse(date < 2020, NA, as.Date(date, origin = "1899-12-30")),
         date = as.Date(date, origin = origin)) %>%
  # correct location names at 0-5 depth in 2017
  mutate(location = case_when(year == 2017 & location == 'A2' ~ paste0(location, '-2'), 
                              year == 2017 & location %in% c('B1', 'C2') ~ paste0(location, '-1'), 
                              TRUE ~ location)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_FAIRM_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/FAIRM Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "FAIRM",
         location = subsample,
         subsample = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_FAIRM_nitrate


# FULTON ------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/FULTON Soil Data.xlsx') %>%
  pluck(1) -> soil_FULTON

# format Soil Properties tables
soil_FULTON %>%
  remove_empty('cols') %>%
  mutate(siteid = "FULTON",
         location = plotID,
         plotID = NA_character_,
         subsample = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_FULTON_properties


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
         everything(), -uniqueid, -sheet, -d1, -d2) ->
  soil_HICKORY_properties



# HICKS_B -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/HICKS_B Soil Data.xlsx') %>%
  pluck(1) -> soil_HICKS_B

# format Soil Properties tables
soil_HICKS_B %>%
  remove_empty('cols') %>%
  mutate(siteid = "HICKS_B",
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_HICKS_B_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/HICKS_B Soil Data.xlsx') %>%
  pluck(4) %>%
  remove_empty('cols') %>%
  mutate(siteid = "HICKS_B",
         subsample = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_HICKS_B_nitrate

# format Soil Water Retention tables
ReadExcelSheets('Input_Data/SOIL/HICKS_B Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "HICKS_B",
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_HICKS_B_wr



# HICKS_P -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/HICKS_P Soil Data.xlsx') %>%
  pluck(1) -> soil_HICKS_P

# format Soil Properties tables
soil_HICKS_P %>%
  remove_empty('cols') %>%
  mutate(siteid = "HICKS_P",
         location = plotID,
         plotID = NA_character_,
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_HICKS_P_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/HICKS_P Soil Data.xlsx') %>%
  pluck(4) %>%
  remove_empty('cols') %>%
  mutate(siteid = "HICKS_P",
         location = plotID,
         plotID = NA_character_,
         subsample = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_HICKS_P_nitrate

# format Soil Water Retention tables
ReadExcelSheets('Input_Data/SOIL/HICKS_P Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "HICKS_P",
         location = plotID,
         plotID = NA_character_,
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_HICKS_P_wr



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
  soil_MAASS_properties



# MUDS1 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MUDS1 Soil Data.xlsx') %>%
  pluck(1) -> soil_MUDS1

# format Soil Properties tables
soil_MUDS1 %>%
  remove_empty('cols') %>%
  mutate(siteid = "MUDS1",
         plotid = as.character(plotID),
         location = subsample,
         subsample = NA_character_,
         date = as.Date(date)) %>%
  # SOC in 2017 is actually SOM > correct and convert accordingly
  mutate(SOIL15 = ifelse(year == 2017, SOIL30.01 * 10, SOIL15),
         SOIL30.01 = ifelse(year == 2017, NA_real_, SOIL30.01)) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet, -plotID, -COMMENTS) -> 
  soil_MUDS1_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/MUDS1 Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "MUDS1",
         plotid = as.character(plotID),
         location = NA_character_,
         subsample = NA_character_,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet, -plotID) -> 
  soil_MUDS1_nitrate


# MUDS2 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MUDS2 Soil Data.xlsx') %>%
  pluck(1) -> soil_MUDS2

# format Soil Properties tables
soil_MUDS2 %>%
  remove_empty('cols') %>%
  mutate(siteid = "MUDS2",
         plotid = as.character(plotID),
         location = subsample,
         subsample = NA_character_,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet, -plotID, -COMMENTS) -> 
  soil_MUDS2_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/MUDS2 Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "MUDS2",
         plotid = as.character(plotID),
         location = subsample,
         subsample = NA_character_,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet, -plotID) -> 
  soil_MUDS2_nitrate


# MUDS3_NEW ---------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MUDS3_NEW Soil Data.xlsx') %>%
  pluck(1) -> soil_MUDS3_NEW

# format Soil Properties tables
soil_MUDS3_NEW %>%
  remove_empty('cols') %>%
  mutate(siteid = "MUDS3_NEW",
         plotid = as.character(plotID),
         location = NA_character_,
         subsample = NA_character_,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet, -plotID) -> 
  soil_MUDS3_NEW_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/MUDS3_NEW Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "MUDS3_NEW",
         plotid = as.character(plotID),
         location = NA_character_,
         subsample = NA_character_,
         date = as.Date(date)) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet, -plotID) -> 
  soil_MUDS3_NEW_nitrate


# MUDS3_OLD ---------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MUDS3_OLD Soil Data.xlsx') %>%
  pluck(1) -> soil_MUDS3_OLD

# format Soil Properties tables
soil_MUDS3_OLD %>%
  remove_empty('cols') %>%
  mutate(siteid = "MUDS3_OLD",
         plotid = NA_character_, 
         location = subsample,
         subsample = NA_character_,
         date = NA) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_MUDS3_OLD_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/MUDS3_OLD Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "MUDS3_OLD",
         plotid = NA_character_, 
         location = subsample,
         subsample = NA_character_,
         date = NA) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_MUDS3_OLD_nitrate



# MUDS4 -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/MUDS4 Soil Data.xlsx') %>%
  pluck(1) -> soil_MUDS4

# format Soil Properties tables
soil_MUDS4 %>%
  remove_empty('cols') %>%
  mutate(siteid = "MUDS4",
         plotid = NA_character_, 
         location = subsample,
         subsample = NA_character_,
         date = NA) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_MUDS4_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/MUDS4 Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "MUDS4",
         plotid = NA_character_, 
         location = subsample,
         subsample = NA_character_,
         date = NA) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_MUDS4_nitrate



# SERF_IA -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/SERF_IA Soil Data.xlsx') %>%
  pluck(1) -> soil_SERF_IA

# format Soil Properties tables
soil_SERF_IA %>%
  remove_empty('cols') %>%
  mutate(siteid = "SERF_IA",
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_SERF_IA_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/SERF_IA Soil Data.xlsx') %>%
  pluck(4) %>%
  remove_empty('cols') %>%
  mutate(siteid = "SERF_IA",
         subsample = NA_character_,
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_SERF_IA_nitrate

# format Soil Water Retention tables
ReadExcelSheets('Input_Data/SOIL/SERF_IA Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "SERF_IA",
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_SERF_IA_wr



# SERF_SD -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/SERF_SD Soil Data.xlsx') %>%
  pluck(1) -> soil_SERF_SD

# format Soil Properties tables
soil_SERF_SD %>%
  remove_empty('cols') %>%
  mutate(siteid = "SERF_SD",
         location = ifelse(year == 2017, plotID, NA_character_),
         plotID = ifelse(year == 2017, NA_character_, plotID),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_SERF_SD_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/SERF_SD Soil Data.xlsx') %>%
  pluck(4) %>%
  remove_empty('cols') %>%
  mutate(siteid = "SERF_SD",
         location = NA_character_,
         subsample = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_SERF_SD_nitrate

# format Soil Penetration Resistance tables
ReadExcelSheets('Input_Data/SOIL/SERF_SD Soil Data.xlsx') %>%
  pluck(5) %>%
  remove_empty('cols') %>%
  mutate(siteid = "SERF_SD",
         location = NA_character_,
         subsample = NA_character_,
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_SERF_SD_pr



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
  soil_SHEARER_properties



# STJOHNS -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/STJOHNS Soil Data.xlsx') %>%
  pluck(1) -> soil_STJOHNS

# format Soil Properties tables
soil_STJOHNS %>%
  remove_empty('cols') %>%
  mutate(siteid = "STJOHNS",
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_STJOHNS_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/STJOHNS Soil Data.xlsx') %>%
  pluck(4) %>%
  remove_empty('cols') %>%
  mutate(siteid = "STJOHNS",
         subsample = NA_character_,
         date = as.Date(date)) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_STJOHNS_nitrate

# format Soil Water Retention tables
ReadExcelSheets('Input_Data/SOIL/STJOHNS Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  mutate(siteid = "STJOHNS",
         year = date,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_STJOHNS_wr

# STORY -------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/STORY Soil Data.xlsx') %>%
  pluck(1) -> soil_STORY

# format Soil Properties tables
soil_STORY %>%
  remove_empty('cols') %>%
  mutate(siteid = "STORY",
         plotid = NA_character_,
         location = subsample,
         subsample = NA_character_,
         year = date,
         date = NA) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_STORY_properties

# format Soil Nitrate tables
ReadExcelSheets('Input_Data/SOIL/STORY Soil Data.xlsx') %>%
  pluck(2) %>%
  remove_empty('cols') %>%
  # select only relevant plots
  filter(plotID %in% c(2, 3, 5, 8, 9, 11)) %>%
  mutate(siteid = "STORY",
         plotid = as.character(plotID),
         location = NA_character_,
         year = year(date),
         date = as.Date(date)) %>%
  select(siteid, plotid, location, subsample, depth, year, date, 
         everything(), -sheet, -plotID) -> 
  soil_STORY_nitrate


# SWROC -------------------------------------------------------------------
# format Soil Moisture tables
ReadExcelSheets('Input_Data/SOIL/SWROC Soil Data.xlsx') # %>%
  # pluck(2) %>%
  # mutate(date = as.Date(date),
  #        location = treatment,
  #        subsample = NA_character_,
  #        siteid = "SWROC") %>%
  # mutate(location = ifelse(is.na(location), NA, paste(location , 'N'))) %>%
  # select(siteid, plotid = plotID, location, subsample, depth, date, 
  #        everything(), -sheet, -treatment) -> 
  # soil_SWROC_sm


# TIDE --------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/TIDE Soil Data.xlsx') %>%
  pluck(1) -> soil_TIDE

# format Soil Properties tables
soil_TIDE %>%
  remove_empty('cols') %>%
  mutate(siteid = "TIDE",
         location = NA_character_,
         subsample = NA_character_,
         year = NA,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet, -COMMENTS) -> 
  soil_TIDE_properties



# UBWC --------------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/UBWC Soil Data.xlsx') %>%
  pluck(1) -> soil_UBWC

# format Soil Properties tables
soil_UBWC %>%
  remove_empty('cols') %>%
  mutate(siteid = "UBWC",
         location = subsample,
         subsample = NA_character_,
         year = NA,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_UBWC_properties


# VANWERT -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/VANWERT Soil Data.xlsx') %>%
  pluck(1) -> soil_VANWERT

# format Soil Properties tables
soil_VANWERT %>%
  remove_empty('cols') %>%
  mutate(siteid = "VANWERT",
         location = plotID,
         plotID = NA_character_,
         subsample = NA_character_) %>%
  # correct years 
  mutate(year = ifelse(date > 2020, year(as.Date(date, origin = "1899-12-30")), date),
         date = ifelse(date < 2020, NA, as.Date(date, origin = "1899-12-30")),
         date = as.Date(date, origin = origin)) %>% 
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_VANWERT_properties


# WILKIN1 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/WILKIN1 Soil Data.xlsx') %>%
  pluck(1) -> soil_WILKIN1

# format Soil Properties tables
soil_WILKIN1 %>%
  remove_empty('cols') %>%
  mutate(siteid = "WILKIN1",
         # location = NA_character_,
         subsample = NA_character_,
         year = NA,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_WILKIN1_properties

# WILKIN2 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/WILKIN2 Soil Data.xlsx') %>%
  pluck(1) -> soil_WILKIN2

# format Soil Properties tables
soil_WILKIN2 %>%
  remove_empty('cols') %>%
  mutate(siteid = "WILKIN2",
         # location = NA_character_,
         subsample = NA_character_,
         year = NA,
         date = NA) %>%
  select(siteid, plotid = plotID, location, subsample, depth, year, date, 
         everything(), -sheet) -> 
  soil_WILKIN2_properties


# WILKIN3 -----------------------------------------------------------------
ReadExcelSheets('Input_Data/SOIL/WILKIN3 Soil Data.xlsx')



# ALL ---------------------------------------------------------------------
# COMBINE .................................................................



# Combnine all soil properties
mget(ls(pattern = 'soil_[[:graph:]]+_properties')) %>%
  map(~ .x %>% 
        mutate(subsample = as.character(subsample),
               location = as.character(location)) %>%
        mutate_at(vars(starts_with('SOIL')), as.character)) %>%
  bind_rows() -> soil_properties_ALL

# Combnine all soil nitrates
mget(ls(pattern = 'soil_[[:graph:]]+_nitrate')) %>%
  map(~ .x %>%
        mutate(subsample = as.character(subsample),
               location = as.character(location))%>%
        mutate_at(vars(starts_with('SOIL')), as.character)) %>%
  bind_rows() -> soil_nitrate_ALL

# Combnine all soil water retensions
mget(ls(pattern = 'soil_[[:graph:]]+_wr')) %>%
  map(~ .x %>%
        mutate(subsample = as.character(subsample),
               location = as.character(location))%>%
        mutate_at(vars(starts_with('SOIL')), as.character)) %>%
  bind_rows() -> soil_wr_ALL

# Combnine all soil penetration resistance
mget(ls(pattern = 'soil_[[:graph:]]+_pr$')) %>%
  map(~ .x %>%
        mutate(depth = as.character(depth))%>%
        mutate_at(vars(starts_with('SOIL')), as.character)) %>%
  bind_rows() -> soil_pr_ALL


# Save for later analysis
write_rds(soil_properties_ALL, 'Inter_Data/soil_properties_ALL.rds', compress = 'xz')
write_rds(soil_nitrate_ALL, 'Inter_Data/soil_nitrate_ALL.rds', compress = 'xz')
write_rds(soil_wr_ALL, 'Inter_Data/soil_wr_ALL.rds', compress = 'xz')
write_rds(soil_pr_ALL, 'Inter_Data/soil_pr_ALL.rds', compress = 'xz')


