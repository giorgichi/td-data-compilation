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



# DOWNLOAD ................................................................

DownloadGoogleSheet('TD Site Keys', FOLDER = 'Metadata',
                    ID = "1PjB63WtYRyYnasm5mmLUF2-WdHuc_3dA_q8iEVV_C28")



# READ ....................................................................

read_excel('Input_Data/Metadata/TD Site Keys.xlsx', sheet = 'AGRO ID', skip = 1,
           col_types = 'text') ->
  agro_plots


# Get plot and location identifiers for agronomic data
agr_ids <- 
  read_rds('Standard_Data/agro_ALL.rds') %>%
  distinct(siteid, plotid , location) %>%
  rename(PLOT = plotid) %>%
  # plots that does not have crop yield data
  add_row(siteid = c('DEFI_R', 'UBWC', 'UBWC'), 
          PLOT = c('10', 'B2', 'B4'))


# Get plot and crop identifiers for agronomic data
agro_plot_crop_ids <-
  agro_plots %>%
  filter(FILTER != "NO") %>%
  select(-KEY_PLOT, -FILTER, -CR, -Comments) %>%
  gather(year, crop, starts_with('CY_')) %>%
  mutate(year = parse_number(year)) %>%
  filter(!crop %in% c('NA')) %>% 
  # some sites have multiple crops planted in a plot within a year
  mutate(crop = str_split(crop, "")) %>%
  unnest(crop) %>%
  mutate(crop = case_when(crop == 'B' ~ "sugar beet",
                          crop == 'C' ~ "corn",
                          crop == 'F' ~ "forage",
                          crop == 'O' ~ "oats",
                          crop == 'P' ~ "popcorn",
                          crop == 'S' ~ "soybean",
                          crop == 'W' ~ "wheat",
                          TRUE ~ "help")) %>%
  select(siteid = Site_ID, Agro_ID, Agro_Location, year_crop = year, crop) %>%
  arrange(siteid, year_crop) %>%
  # no need for ACRE since plot
  filter(siteid != 'ACRE') 



# FUNCTIONS ----------------------------------------------------------------------------------

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
           location = case_when(siteid == 'DEFI_R' & location == 'N' ~ 'NE',
                                siteid == 'DEFI_R' & location == 'S' ~ 'SE',
                                TRUE ~ location))
  }


# Function to standardize plotids at SWROC
rename_SWROC_plots <- function(df) {
  df %>%
    separate(plotid, into = c('PLOT', 'LOC'), sep = "_", remove = FALSE, convert = TRUE) %>%
    mutate(plotid   = ifelse(siteid == 'SWROC', PLOT, plotid),
           location = ifelse(siteid == 'SWROC' & !is.na(LOC), 
                             paste(LOC, 'N'), location)) %>%
    select(-PLOT, - LOC)
}

# Function to identify non-relevant plots at STORY
correct_STORY_action <- function(df){
  df %>%
    mutate(action = ifelse(siteid == 'STORY' & plotid %in% c(1, 4, 6, 7, 10, 12), 
                           'remove', action))
}

# Function to identify delayed planted plots at MUDS1
delayed_plots <- 
  planting %>% 
  filter(siteid == 'MUDS1' & action == 'remove') %>% 
  distinct(siteid, plotid, year_crop) %>%
  mutate(NEW_action = 'remove')

correct_MUDS1_action <- function(df){
  df %>%
    left_join(delayed_plots, by = c("siteid", "plotid", "year_crop")) %>%
    mutate(action = ifelse(!is.na(NEW_action), NEW_action, action)) %>%
    select(-NEW_action)
}

# Function to standardize plotids at VANVERT
rename_VANWERT_plots <- function(df) {
  df %>%
    mutate(location = case_when(siteid == 'VANWERT' & plotid == 1 ~ 'SE',
                                siteid == 'VANWERT' & plotid == 2 ~ 'SW',
                                siteid == 'VANWERT' & plotid == 3 ~ 'DE',
                                siteid == 'VANWERT' & plotid == 4 ~ 'DW',
                                TRUE ~ location),
           plotid =  case_when(siteid == 'VANWERT' & plotid == 1 ~ 'S',
                               siteid == 'VANWERT' & plotid == 2 ~ 'S',
                               siteid == 'VANWERT' & plotid == 3 ~ 'D',
                               siteid == 'VANWERT' & plotid == 4 ~ 'D',
                               TRUE ~ plotid))
}



# Standardize Planting Data ----------

# standardize plot ids for planting data 
planting %>%
  rename_ACRE_plots() %>%
  rename_DEFI_R_plots() %>%
  mutate(crop = ifelse(siteid == 'MUDS3_OLD', 'forage', cashcrop)) %>%
  filter(operation == 'planting') -> planting_ONLY

# standardize plot ids
planting_ONLY %>%
  filter(!is.na(plotid) | !is.na(location)) %>%
  distinct(siteid, 
           Agro_ID = plotid, 
           Agro_Location = location, 
           year_crop, 
           crop) %>%
  # select ids that has no site-plot-location-year already defined 
  anti_join(agro_plot_crop_ids, .) %>%
  # this code below is redundant and should not effect input df
  # unless there is some disagreement between management and agronomic plot identifiers
  filter(!siteid %in% c('ACRE', 'MUDS1')) %>%
  # add the ids for entries with missing site-plot-location for corresponding crop year
  right_join(planting_ONLY) %>%
  mutate(plotid = ifelse(!is.na(Agro_ID), Agro_ID, plotid),
         location = ifelse(!is.na(Agro_Location), Agro_Location, location)) %>%
  select(names(planting_ONLY), -crop) %>%
  correct_ACRE_action() %>%
  correct_MUDS1_action() %>%
  arrange(siteid) -> planting_standard



# standardize plot ids for harvesting data
planting %>%
  rename_ACRE_plots() %>%
  rename_DEFI_R_plots() %>%
  mutate(crop = ifelse(siteid == 'MUDS3_OLD', 'forage', cashcrop)) %>%
  filter(operation == 'harvesting') -> harvesting_ONLY

# standardize plot ids
harvesting_ONLY %>%
  filter(!is.na(plotid) | !is.na(location)) %>%
  distinct(siteid, 
           Agro_ID = plotid, 
           Agro_Location = location, 
           year_crop, 
           crop) %>%
  # select ids that has no site-plot-location-year already defined 
  anti_join(agro_plot_crop_ids, .) %>%
  filter(!siteid %in% c('ACRE', 'MUDS1')) %>%
  # add the ids for entries with missing site-plot-location for corresponding crop year
  right_join(harvesting_ONLY) %>%
  mutate(plotid = ifelse(!is.na(Agro_ID), Agro_ID, plotid),
         location = ifelse(!is.na(Agro_Location), Agro_Location, location)) %>%
  select(names(harvesting_ONLY)) %>%
  correct_ACRE_action() %>%
  correct_MUDS1_action() %>%
  select(action, siteid, plotid, location, 
         year_calendar, year_crop, date, operation, cashcrop, comments) %>%
  arrange(siteid) -> harvesting_standard



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
  select(everything()) -> fertilizing_corrected



# standardize plot ids for fertilizing data 
fertilizing_corrected %>%
  rename_ACRE_plots() %>%
  rename_DEFI_R_plots() %>%
  rename_SWROC_plots() %>% 
  rename_VANWERT_plots() %>%
  mutate(crop = ifelse(siteid == 'MUDS3_OLD', 'forage', cashcrop)) %>%
  filter(operation %in% c('fertilizing', 'soil amendment')) -> fertilizing_ONLY

# standardize plot ids
fertilizing_ONLY %>%
  filter(!is.na(plotid) | !is.na(location)) %>%
  distinct(siteid, 
           Agro_ID = plotid, 
           Agro_Location = location, 
           year_crop, 
           crop) %>%
  # select ids that has no site-plot-location-year already defined 
  anti_join(agro_plot_crop_ids, .) %>%
  # this code below remove plots at MUDS1 with delayed planting
  filter(!siteid %in% c('ACRE', 'MUDS1')) %>%
  # add the ids for entries with missing site-plot-location for corresponding crop year
  right_join(fertilizing_ONLY) %>%
  mutate(plotid = ifelse(!is.na(Agro_ID), Agro_ID, plotid),
         location = ifelse(!is.na(Agro_Location), Agro_Location, location)) %>%
  select(names(fertilizing_ONLY), -crop) %>%
  correct_ACRE_action() %>%
  correct_STORY_action() %>%
  correct_MUDS1_action() %>%
  arrange(siteid) -> fertilizing_standard



# standardize plot ids for tillage data 
fertilizing_corrected %>%
  rename_ACRE_plots() %>%
  rename_DEFI_R_plots() %>%
  rename_SWROC_plots() %>% 
  rename_VANWERT_plots() %>%
  mutate(crop = ifelse(siteid == 'MUDS3_OLD', 'forage', cashcrop)) %>%
  filter(operation == 'tillage') -> tillage_ONLY

# standardize plot ids
tillage_ONLY %>%
  filter(!is.na(plotid) | !is.na(location)) %>%
  distinct(siteid, 
           Agro_ID = plotid, 
           Agro_Location = location, 
           year_crop, 
           crop) %>%
  # select ids that has no site-plot-location-year already defined 
  anti_join(agro_plot_crop_ids, .) %>%
  # this code below remove plots at MUDS1 with delayed planting
  filter(!siteid %in% c('ACRE', 'MUDS1')) %>%
  # add the ids for entries with missing site-plot-location for corresponding crop year
  right_join(tillage_ONLY) %>%
  mutate(plotid = ifelse(!is.na(Agro_ID), Agro_ID, plotid),
         location = ifelse(!is.na(Agro_Location), Agro_Location, location)) %>%
  select(names(tillage_ONLY), -crop) %>%
  correct_ACRE_action() %>%
  correct_STORY_action() %>%
  correct_MUDS1_action() %>%
  select(action:depth, comments) %>%
  arrange(siteid) -> tillage_standard



# Standardize Pesticide Data ----------

pesticide %>%
  mutate(CROP = crop,
         crop = ifelse(siteid == 'MUDS3_OLD', 'forage', crop)) %>%
  full_join(agro_plot_crop_ids) %>% 
  filter(!is.na(action)) %>%
  mutate(crop = CROP) %>%
  select(plotid = Agro_ID, location = Agro_Location, 
         names(pesticide), -CROP) %>%
  correct_ACRE_action() %>%
  correct_MUDS1_action() %>%
  select(action, siteid, plotid, location, 
         year_calendar, year_crop, date, crop, 
         operation, operation_type, method, timing, total_rate, 
         product1_name, product1_rate, product1_form, 
         product2_name, product2_rate, product2_form, 
         product3_name, product3_rate, product3_form, 
         product4_name, product4_rate, product4_form, 
         adjuvant1, adjuvant2, comments) %>%
  arrange(siteid) -> pesticide_standard
  


# Standardize Residue Data ----------

residue %>%
  mutate(CROP = crop,
         crop = ifelse(siteid == 'MUDS3_OLD', 'forage', crop)) %>%
  full_join(agro_plot_crop_ids) %>% 
  filter(!is.na(action)) %>%
  mutate(crop = CROP) %>%
  select(plotid = Agro_ID, location = Agro_Location, 
         names(residue), -CROP) %>%
  correct_ACRE_action() %>%
  correct_MUDS1_action() %>%
  select(action, siteid, plotid, location, 
         year_calendar, year_crop, crop,
         notill, comments) %>%
  arrange(siteid, year_calendar) -> residue_standard



# Standardize DWM Data ----------

dwm %>%
  mutate(temp = round(as.numeric(outlet_depth), 2),
         outlet_depth = ifelse(!is.na(temp), as.character(temp), outlet_depth),
         temp = round(as.numeric(outlet_height), 2),
         outlet_height = ifelse(!is.na(temp), as.character(temp), outlet_height),
         temp = NULL) %>%
  # remove comments about time stamp as there is only date used in final table
  mutate(comments = str_remove(comments, " Time stamp is approximate."),
         comments = str_remove(comments, "Time stamp is approximate. "),
         comments = ifelse(comments == "Time stamp is approximate.", NA_character_, comments)) %>%
  # replace 'NA' with 'n/a' at TIDE and SERF_IA for year when boards were not changed
  mutate(date = ifelse(is.na(date), "n/a", date)) -> dwm_standard


# Standardize Irrigation Data ----------
irrigation %>%
  mutate(temp = round(as.numeric(irrigation_amount), 2),
         irrigation_amount = ifelse(!is.na(temp), as.character(temp), irrigation_amount),
         temp = NULL) -> irrigation_standard
  


# Standardize Notes Data ----------
notes %>%
  mutate(siteid = str_replace(siteid, "WILKINS", "WILKIN")) -> notes_standard




# SAVE standardized data --------------------------------------------------

write_rds(planting_standard, 'Standard_Data/planting_ALL.rds', compress = 'xz')
write_rds(harvesting_standard, 'Standard_Data/harvesting_ALL.rds', compress = 'xz')
write_rds(fertilizing_standard, 'Standard_Data/fertilizing_ALL.rds', compress = 'xz')
write_rds(tillage_standard, 'Standard_Data/tillage_ALL.rds', compress = 'xz')
write_rds(pesticide_standard, 'Standard_Data/pesticide_ALL.rds', compress = 'xz')
write_rds(residue_standard, 'Standard_Data/residue_ALL.rds', compress = 'xz')
write_rds(irrigation_standard, 'Standard_Data/irrigation_ALL.rds', compress = 'xz')
write_rds(dwm_standard, 'Standard_Data/dwm_ALL.rds', compress = 'xz')
write_rds(notes_standard, 'Standard_Data/notes_ALL.rds', compress = 'xz')


