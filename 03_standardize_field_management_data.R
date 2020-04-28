# Initialize functions 
source('00_project_settings.R')



# Read All field management data -----
planting <- read_rds('Inter_Data/fm_planting.rds')
fertilizing <- read_rds('Inter_Data/fm_fertilizing.rds')
pesticide <- read_rds('Inter_Data/fm_pesticide.rds')
residue <- read_rds('Inter_Data/fm_residue.rds')
dwm <- read_rds('Inter_Data/fm_dwm.rds')
irrigation <- read_rds('Inter_Data/fm_irrigation.rds')
notes <- read_rds('Inter_Data/fm_notes.rds')

# Get plot and location identifiers for agronomic data
agr_ids <- 
  read_rds('Standard_Data/agro_ALL.rds') %>%
  distinct(siteid, plotid , location) %>%
  rename(PLOT = plotid) %>%
  # plots that does not have crop yield data
  add_row(siteid = c('DEFI_R', 'UBWC', 'UBWC'), 
          PLOT = c('10', 'B2', 'B4'))



# Function to standardize plotids at ACRE
rename_ACRE_plots <- function(df){
  df %>%
    mutate(plotid = ifelse(siteid == 'ACRE' & plotid == '22E/W', '22E', plotid)) %>%
    mutate(plotid = ifelse(siteid == 'ACRE' & str_detect(plotid, '[:alpha:]'), 
                           str_replace(plotid, '[:alpha:]', 
                                       paste0('-', str_extract(plotid, "[:alpha:]"))),
                           plotid),
           location = ifelse(siteid == 'ACRE', paste('Field', plotid), NA_character_)) %>%
    left_join(agr_ids %>% filter(siteid == 'ACRE'),
              by = c('siteid', 'location')) %>%
    mutate(plotid = ifelse(siteid == 'ACRE', PLOT, plotid)) %>%
    select(action:plotid, location, everything(), - PLOT)
}

# Function to identify additional location-years to remove at ACRE
correct_ACRE_action <- function(df){
  REMOVE <- read_rds('Standard_Data/agro_ALL.rds') %>%
    filter(siteid == 'ACRE' & action == 'remove') %>%
    mutate(year_calendar = as.numeric(year)) %>%
    distinct(siteid, plotid, location, year_calendar, REMOVE = 'yes')
  df %>%
    left_join(REMOVE, by = c('siteid', 'plotid', 'location', 'year_calendar')) %>%
    mutate(action = ifelse(!is.na(REMOVE), 'remove', action)) %>%
    select(-REMOVE)
  }

# Function to standardize plotids at DEFI_R
rename_DEFI_R_plots <- function(df) {
  df %>%
    mutate(location = ifelse(siteid == 'DEFI_R', 
                             str_extract(plotid, '[:alpha:]'), 
                             location),
           plotid = ifelse(siteid == 'DEFI_R' & !is.na(location), 
                           str_remove(plotid, '[:alpha:]'), 
                           plotid),
           location = case_when(siteid == 'DEFI_R' & location == 'N' ~ 'North half',
                                siteid == 'DEFI_R' & location == 'S' ~ 'South half',
                                TRUE ~ location))
  }


# Function to standardize plotids at SWROC
rename_SWROC_plots <- function(df) {
  df %>%
    separate(plotid, into = c('PLOT', 'LOC'), sep = "_", remove = FALSE, convert = TRUE) %>%
    mutate(plotid   = ifelse(siteid == 'SWROC', PLOT, plotid),
           location = ifelse(siteid == 'SWROC', paste(LOC, 'N'), location)) %>%
    select(-PLOT, - LOC)
}

# Function to identify non-relevant plots at STORY
correct_STORY_action <- function(df){
  df %>%
    mutate(action = ifelse(siteid == 'STORY' & plotid %in% c(1, 4, 6, 7, 10, 12), 
                           'remove', action))
}


# Standardize Planting Data ----------

# standardize plot ids
planting %>%
  rename_ACRE_plots() %>%
  rename_DEFI_R_plots() %>%
  correct_ACRE_action() -> planting_standard



# Standardize Fertilizer Data ----------

# correct rates
fertilizing %>%
  # convert manure rate from gpa to L/ha
  mutate(temp = round(as.numeric(manure_rate) * 9.354, 0),
         manure_rate = ifelse(is.na(temp), manure_rate, as.character(temp)),
         temp = NULL) %>%
  # calculate elemental rate of synthetic fertilizer 
  mutate(check_n = as.numeric(fertilizer_rate) * as.numeric(nitrogen_percent) / 100,
         check_p = as.numeric(fertilizer_rate) * as.numeric(phosphate_percent) / 100 / 2.2915,
         check_k = as.numeric(fertilizer_rate) * as.numeric(potash_percent) / 100 / 1.2047,
         check_s = as.numeric(fertilizer_rate) * as.numeric(sulfur_percent) / 100 ,
         check_z = as.numeric(fertilizer_rate) * as.numeric(zinc_percent) / 100,
         check_m = as.numeric(fertilizer_rate) * as.numeric(magnesium_percent) / 100,
         check_c = as.numeric(fertilizer_rate) * as.numeric(calcium_percent) / 100,
         check_i = as.numeric(fertilizer_rate) * as.numeric(iron_percent) / 100) %>%
  mutate_at(vars(starts_with('check')), round, digits = 2) %>%
  mutate_at(vars(starts_with('check')), as.character) %>%
  mutate(nitrogen_elem = ifelse(!is.na(as.numeric(fertilizer_rate)), check_n, nitrogen_elem),
         phosphorus_elem = ifelse(!is.na(as.numeric(fertilizer_rate)), check_p, phosphorus_elem),
         potassium_elem = ifelse(!is.na(as.numeric(fertilizer_rate)), check_k, potassium_elem),
         sulfur_elem = ifelse(!is.na(as.numeric(fertilizer_rate)), check_s, sulfur_elem),
         zinc_elem = ifelse(!is.na(as.numeric(fertilizer_rate)), check_z, zinc_elem),
         magnesium_elem = ifelse(!is.na(as.numeric(fertilizer_rate)), check_m, magnesium_elem),
         calcium_elem = ifelse(!is.na(as.numeric(fertilizer_rate)), check_c, calcium_elem),
         iron_elem = ifelse(!is.na(as.numeric(fertilizer_rate)), check_i, iron_elem)) %>%
  select(-ends_with('percent'),-starts_with('check')) %>%
  # variable 'stabilizer' is redundant - NEED TO ASK LORI
  select(everything()) -> fertilizing_corrected


# standardize plot ids
fertilizing_corrected %>%
  rename_ACRE_plots() %>%
  correct_STORY_action() %>%
  rename_SWROC_plots() %>% 
  rename_DEFI_R_plots() %>%
  # remove erroneous plot id and correct locations at VANWERT
  mutate(plotid = ifelse(siteid == 'VANWERT' & plotid == '68', NA, plotid),
         location = case_when(siteid == 'VANWERT' & plotid == 1 ~ 'SE',
                              siteid == 'VANWERT' & plotid == 2 ~ 'SW',
                              TRUE ~ location),
         plotid =  case_when(siteid == 'VANWERT' & plotid == 1 ~ 'S',
                             siteid == 'VANWERT' & plotid == 2 ~ 'S',
                             siteid == 'VANWERT' & plotid == 3 ~ 'D',
                             TRUE ~ plotid)) -> fertilizing_standard



# Standardize Pesticide Data ----------

pesticide -> pesticide_standard
  


# Standardize Residue Data ----------

residue -> 
  # NEED to move crop from comments to cashcrop column
  residue_standard



# Standardize DWM Data ----------

dwm %>%
  mutate(temp = round(as.numeric(outlet_depth), 2),
         outlet_depth = ifelse(!is.na(temp), as.character(temp), outlet_depth),
         temp = NULL) %>%
  rename(controlled_plotids = plotid) -> dwm_standard


# Standardize Irrigation Data ----------
irrigation %>%
  mutate(temp = round(as.numeric(irrigation_amount), 2),
         irrigation_amount = ifelse(!is.na(temp), as.character(temp), irrigation_amount),
         temp = NULL) %>%
  rename(irrigated_plotids = plotid) -> irrigation_standard
  


# Standardize Notes Data ----------
notes ->
  notes_standard




# Save standardized data --------------------------------------------------

write_rds(dwm_standard, 'Standard_Data/dwm_ALL.rds', compress = 'xz')
write_rds(fertilizing_standard, 'Standard_Data/fertilizing_ALL.rds', compress = 'xz')
write_rds(irrigation_standard, 'Standard_Data/irrigation_ALL.rds', compress = 'xz')
write_rds(notes_standard, 'Standard_Data/notes_ALL.rds', compress = 'xz')
write_rds(planting_standard, 'Standard_Data/planting_ALL.rds', compress = 'xz')



#########################
##### CHEK PLOT IDs #####
#########################
dwm %>%
  filter(!is.na(plotid)) %>%
  anti_join(agr_ids, by = c('siteid', 'plotid' = 'PLOT')) %>%
  distinct(action, siteid, plotid) %>%
  View()

