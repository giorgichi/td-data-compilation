# Initialize functions 
source('00_project_settings.R')



# Read All agr data
agr_ALL <- read_rds('Inter_Data/agr_ALL.rds')
codes <- read_csv('Input_Data/codes.csv')


# Correct measurements
agr_ALL %>%
  select(-planting_date) %>%
  mutate(crop = ifelse(is.na(crop), word(key, 2), crop),
         crop = str_to_lower(crop),
         # correct crops at SERF_SD
         crop = case_when(crop == 'leaf' & year == 2016 ~ 'corn', 
                          crop == 'leaf' & year == 2017 ~ 'soybean',
                          TRUE ~ crop)) %>%
  # add location for Yield from Zone of DWM influence
  mutate(location = ifelse(str_detect(key, 'DWM'), 'DWM zone of influence', location)) %>%
  # add treatment for SWROC
  mutate(trt = ifelse(siteid == 'SWROC' & crop == 'corn', 'N-treatment Corn', trt),
         trt_value = case_when(siteid == 'SWROC' & crop == 'corn' & location == '0 N' ~ 'Non-fertilized',
                               siteid == 'SWROC' & crop == 'corn' & location != '0 N' ~ 
                                 paste('Urea at', word(location, 1), 'lbs N/acre'),
                               TRUE ~ trt_value)) %>%
  # coorect units 
  mutate(value_new = as.numeric(value),
         # corn final population is in plant per acre - convert to per ha
         value = ifelse(siteid == 'VANWERT' & str_detect(key, 'AGR1 '), as.character(value_new * 2.47105), value),
         value = ifelse(siteid == 'VANWERT' & str_detect(key, 'AGR2 '), as.character(value_new * 2.47105), value),
         # corn yield reported at 15% - adjust to 15.5%
         value = ifelse(siteid == 'DPAC' & str_detect(key, 'AGR17') & !year %in% c(2011:2015), 
                        as.character(round(value_new*0.85/0.845 ,2)), value),
         value = ifelse(siteid %in% c('MUDS4', 'MUDS2', 'MUDS3_NEW', 'MUDS1') & str_detect(key, 'AGR17'), 
                        as.character(round(value_new*0.85/0.845 ,2)), value),
         # corn grain carbon is calculated based on crop yield - recalculate based on corn grain biomass
         value = ifelse(siteid == 'STJOHNS' & str_detect(key, 'AGR23 ') & year == 2013, as.character(value_new * 0.845), value)) %>%
  select(siteid, plotid, location, trt, trt_value, crop, year, date, key, value, everything(), -value_new) -> 
  agr_ALL_correct


# Standardize agronomic data
agr_ALL_correct %>%
  # standardize plot and location names
  mutate(location = ifelse(siteid == 'ACRE', paste('Field', location), location)) %>%
  # standardize crop names
  mutate(crop = ifelse(crop == 'soy', 'soybean', crop)) %>%
  # update variable names
  left_join(codes, by = 'key') %>%
  mutate(NEW_CODE = case_when(CROP == 'Any' & crop == 'corn' ~ 
                                str_replace(NEW_CODE, '80.00.', '80.01.'),
                              CROP == 'Any' & crop == 'soybean' ~ 
                                str_replace(NEW_CODE, '80.00.', '80.21.'),
                              TRUE ~ NEW_CODE)) %>%
  # mark fileds with < 0.81 ha areas at ACRE for farther filter
  mutate(action = ifelse(siteid == 'ACRE' & harvested_area < 0.81, 'remove', action)) %>%
  select(siteid, plotid, location, crop, trt, trt_value, year, date, 
         var_NEW = NEW_CODE, value, action, harvested_area) %>%
  filter(!is.na(value)) ->
  agr_ALL_standard


# Save standardized data --------------------------------------------------

write_rds(agr_ALL_standard, 'Standard_Data/agro_ALL.rds', compress = 'xz')


