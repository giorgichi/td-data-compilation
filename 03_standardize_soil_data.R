# Initialize functions 
source('00_project_settings.R')



# Read All soil data
soil_properties_ALL <- read_rds('Inter_Data/soil_properties_ALL.rds')
soil_nitrate_ALL <- read_rds('Inter_Data/soil_nitrate_ALL.rds')
soil_wr_ALL <- read_rds('Inter_Data/soil_wr_ALL.rds')
soil_pr_ALL <- read_rds('Inter_Data/soil_pr_ALL.rds')



# Correct water retension curves at CLAY sites
soil_wr_ALL %>%
  group_by(siteid, plotid, location, subsample, depth, year, date) %>%
  mutate(S1 = ifelse(str_detect(siteid, 'CLAY'), sort(as.numeric(SOIL08.01), decreasing = TRUE), -9999),
         S2 = ifelse(str_detect(siteid, 'CLAY'), sort(as.numeric(SOIL08.02), decreasing = TRUE), -9999)) %>%
  ungroup() %>%
  mutate(S2 = ifelse(siteid == 'CLAY_U' & location == 'M2' & depth == '97.54' & S1 < -1, S2 - 10, S2)) %>%
  mutate(SOIL08.01 = ifelse(str_detect(siteid, 'CLAY'), as.character(S1), SOIL08.01),
         SOIL08.02 = ifelse(str_detect(siteid, 'CLAY'), as.character(S2), SOIL08.02)) %>%
  select(siteid:SOIL08.02) %>%
  distinct() -> soil_wr_clean



# Combine all soil data
soil_properties_ALL %>%
  # remove redundant variables reported at MUDS1
  select(-ends_with('_2')) %>%
  bind_rows(soil_pr_ALL, soil_nitrate_ALL, soil_wr_clean) %>%
  select(names(.) %>% sort()) %>%
  select(siteid, plotid, location, subsample, depth, year, date, everything()) -> soil_ALL



# Correct soil variables
soil_ALL %>%
  # fix soil texture that does not sum up to 100%
  mutate(SOIL02.01 = case_when(siteid == 'BENTON' & SOIL02.01 == '3.1' ~ '3.2',
                               siteid == 'BENTON' & SOIL02.01 == '19' ~ '19.1',
                               siteid == 'BENTON' & SOIL02.01 == '23.4' ~ '23.5',
                               siteid == 'BENTON' & SOIL02.01 == '4.9' ~ '4.8',
                               siteid == 'STJOHNS' & plotid == 'WN' & SOIL02.01 == '13.9' ~ '13.8',
                               TRUE ~ SOIL02.01)) %>%
  # make Matric Potentail negative
  mutate(temp = as.numeric(SOIL08.01),
         SOIL08.01 = ifelse(temp > 0 & !is.na(temp), paste0('-', SOIL08.01), SOIL08.01)) %>%
  # convert Soil Water Content to decimal
  mutate(temp = ifelse(str_detect(siteid, 'CLAY'), as.numeric(SOIL08.02), NA),
         SOIL08.02 = ifelse(str_detect(siteid, 'CLAY'), as.character(temp/100), SOIL08.02)) %>%
  # calculate Base Saturation at STJOHNS
  mutate(temp = as.numeric(SOIL23.01) + as.numeric(SOIL23.02) + as.numeric(SOIL23.03),
         temp = ifelse(temp > 100, 100, temp),
         SOIL23.05 = ifelse(is.na(SOIL23.05), 
                            as.character(temp),
                            SOIL23.05)) %>%
  # correct type at ACRE
  mutate(SOIL25.01 = ifelse(SOIL25.01 == '2014', '204', SOIL25.01)) %>%
  select(-temp) -> soil_ALL_correct



# Standardize soil data
soil_ALL_correct %>%
  # remove a digit signifying sample depth at FAIRM and CLAY sites
  mutate(location = ifelse(str_detect(siteid, 'CLAY') & str_length(location) == 4, 
                           str_sub(location, 1, 2), location),
         location = ifelse(siteid == 'FAIRM', str_remove(location, '-6$'), location),
         location = ifelse(siteid == 'FAIRM', str_remove(location, '-18$'), location)) %>%
  # correct plotid and locations at ACRE
  mutate(plotid = ifelse(siteid == 'ACRE', location, plotid),
         plotid = ifelse(siteid == 'ACRE' & str_detect(subsample, '10-'), NA_character_, plotid),
         plotid = ifelse(siteid == 'ACRE' & subsample %in% c('21-B', '21-C'), NA_character_, plotid),
         location = ifelse(siteid == 'ACRE', NA_character_, location),
         location = ifelse(siteid == 'ACRE', subsample, location),
         subsample = ifelse(siteid == 'ACRE', NA_character_, subsample),
         subsample = ifelse(siteid == 'ACRE', word(location, 2), subsample),
         location = ifelse(siteid == 'ACRE', word(location, 1), location),
         location = ifelse(siteid == 'ACRE', paste('Field', location), location)) %>%
  # standardize depth 
  mutate(depth = case_when(str_detect(depth, 'cm') ~ str_replace(depth, '-', ' to '),
                           !str_detect(depth, 'cm') & str_detect(depth, ' to ') & str_length(depth) < 20 ~ paste(depth, 'cm'), 
                           TRUE ~ depth),
         depth = case_when(!str_detect(depth, ' to ') ~ str_replace(depth, ' - ', ' to '),
                           TRUE ~ depth),
         depth = case_when(!str_detect(depth, ' to ') ~ str_replace(depth, '-', ' to '),
                           TRUE ~ depth),
         depth = case_when(depth == '205+' ~ '>205 cm',
                           depth == '213+' ~ '>213 cm',
                           depth == '231+' ~ '>231 cm',
                           TRUE ~ depth),
         depth = ifelse(siteid != 'VANWERT' & !is.na(depth) & !str_ends(depth, ' cm'), paste(depth, 'cm'), depth),
         depth = case_when(depth == '0 to 22.86 cm' ~ '0 to 23 cm',
                           depth == '22.86 to 55.88 cm' ~ '23 to 56 cm',
                           depth == '55.88 to 121.92 cm' ~ '56 to 122 cm',
                           TRUE ~ depth),
         depth = case_when(siteid == 'STORY' & !is.na(SOIL32.04) & depth == '0 to 6 cm' ~ '0 to 15 cm',
                           siteid == 'STORY' & !is.na(SOIL32.04) & depth == '6 to 12 cm' ~ '15 to 30 cm',
                           siteid == 'STORY' & !is.na(SOIL32.04) & depth == '12 to 18 cm' ~ '30 to 46 cm',
                           siteid == 'STORY' & !is.na(SOIL32.04) & depth == '18 to 24 cm' ~ '46 to 61 cm',
                           siteid == 'STORY' & !is.na(SOIL32.04) & depth == '24 to 30 cm' ~ '61 to 76 cm',
                           siteid == 'STORY' & !is.na(SOIL32.04) & depth == '30 to 36 cm' ~ '76 to 91 cm',
                           siteid == 'STORY' & !is.na(SOIL32.04) & depth == '36 to 42 cm' ~ '91 to 107 cm',
                           siteid == 'STORY' & !is.na(SOIL32.04) & depth == '42 to 48 cm' ~ '107 to 122 cm',
                           TRUE ~ depth)) -> soil_ALL_standard


# Save standardized data --------------------------------------------------

write_rds(soil_ALL_standard, 'Output_Data/soil_properties_ALL.rds')
write_csv(soil_ALL_standard, 'Output_Data/soil_properties_ALL.csv', na = '')


# check/visualize data
soil_ALL_standard %>%
  select(siteid:date, starts_with('SOIL35')) %>%
  mutate_at(vars(starts_with('SOIL')), as.numeric) %>%
  # mutate(SOIL23.05 = ifelse(is.na(SOIL23.05), SOIL23.01 + SOIL23.02 + SOIL23.03, SOIL23.05)) %>%
  gather(key, value, starts_with('SOIL')) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(siteid, value)) +
  geom_boxplot() +
  facet_grid(key ~ ., scales = 'free') +
  theme_gio
  
  



