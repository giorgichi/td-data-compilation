# Initialize functions 
source('00_project_settings.R')


# Read weather data
weather_ALL_hourly <- read_rds('Inter_Data/weather_ALL_hourly.rds')
weather_ALL_daily <- read_rds('Inter_Data/weather_ALL_daily.rds')



# HOURLY WEATHER DATA -----------------------------------------------------

# Standartize timestamps (make hourly steps)
# do sites with identical temporal data together
# NOTE: check is there are some missing Precipitation data since it will take extra step so no 0 is inserted when all are NAs

weather_ALL_hourly %>%
  filter(siteid %in% c('AUGLA', 'DEFI_M', 'HARDIN_NW', 'HENRY',
                       'CLAY_U')) %>%
  spread(key, value) %>%
  mutate(date = floor_date(tmsp, unit = 'hour')) %>%
  group_by(siteid, station, date) %>%
  summarise(Precipitation = sum(Precipitation)) %>%
  ungroup() %>%
  gather(key, value, -siteid, -station, -date) -> df1

weather_ALL_hourly %>%
  filter(siteid %in% c('CRAWF')) %>%
  spread(key, value) %>%
  mutate(date = floor_date(tmsp, unit = 'hour')) %>%
  group_by(siteid, station, date) %>%
  summarise(Precipitation = sum(Precipitation)) %>%
  ungroup() %>%
  full_join(tibble(siteid = 'CRAWF', date = seq(ymd_h(2010031800), ymd_h(2010093013), by = 'hour')), 
            by = c('siteid', 'date')) %>%
  arrange(date) %>%
  gather(key, value, -siteid, -station, -date) -> df2

weather_ALL_hourly %>%
  filter(siteid %in% c('FULTON', 'VANWERT')) %>%
  spread(key, value) %>%
  mutate(date = floor_date(tmsp, unit = 'hour')) %>%
  group_by(siteid, station, date) %>%
  summarise(Precipitation = sum(Precipitation),
            `Air Temperature` = mean(`Air Temperature`, na.rm = TRUE),
            `Relative Humidity` = mean(`Relative Humidity`, na.rm = TRUE),
            `Solar Radiation` = mean(`Solar Radiation`, na.rm = TRUE),
            `Wind Speed` = mean(`Wind Speed`, na.rm = TRUE),
            `Wind Gust` = max(`Wind Gust`, na.rm = TRUE),
            `Wind Direction` = first(`Wind Direction`)) %>%
  ungroup() %>%
  # correct for glitch in calculating Wind Gust when there is no data
  mutate(`Wind Gust` = ifelse(is.infinite(`Wind Gust`), NA, `Wind Gust`)) %>%
  gather(key, value, -siteid, -station, -date) -> df3

weather_ALL_hourly %>%
  filter(siteid %in% c('DEFI_R') & station == 'Station') %>%
  spread(key, value) %>%
  mutate(date = floor_date(tmsp, unit = 'hour')) %>%
  group_by(siteid, station, date) %>%
  # # check is there are some missing values
  # filter(is.na(Precipitation))
  summarise(Precip_CHECK = mean(Precipitation, na.rm = TRUE),
            Precipitation = sum(Precipitation, na.rm = TRUE),
            `Air Temperature` = mean(`Air Temperature`, na.rm = TRUE),
            `Relative Humidity` = mean(`Relative Humidity`, na.rm = TRUE),
            `Solar Radiation` = mean(`Solar Radiation`, na.rm = TRUE),
            `Wind Speed` = mean(`Wind Speed`, na.rm = TRUE),
            `Wind Gust` = max(`Wind Gust`, na.rm = TRUE),
            `Wind Direction` = first(`Wind Direction`)) %>%
  ungroup() %>%
  # correct for glitch in calculating Precipitation and Wind Gust when there is no data
  mutate(`Wind Gust` = ifelse(is.infinite(`Wind Gust`), NA, `Wind Gust`),
         Precipitation = ifelse(is.na(Precip_CHECK), NA, Precipitation)) %>%
  select(-Precip_CHECK) %>%
  gather(key, value, -siteid, -station, -date) -> df4
  
bind_rows(df1, df2, df3, df4) %>%
  rename(tmsp = date) -> df_all
  


# Combine datasets with uniform timestamp
weather_ALL_hourly %>%
  filter(!siteid %in% c('AUGLA', 'CRAWF', 'DEFI_M', 'HARDIN_NW', 'HENRY',
                       'CLAY_U', 'FULTON', 'VANWERT')) %>%
  filter(!(siteid == 'DEFI_R' & station == 'Station')) %>%
  bind_rows(df_all) %>%
  # remove questionable data
  mutate(value = ifelse(siteid == 'DPAC' & between(tmsp, ymd_h(2017051712), ymd_h(2017061412)) & 
                          key == 'Wind Speed', NA_real_, value)) ->
  weather_ALL_hourly_correct



# Standardize hourly weather data
weather_ALL_hourly_correct %>% 
  # add variable codes
  left_join(tibble(key = c('Precipitation',    
                           'Air Temperature',
                           'Relative Humidity',
                           'Solar Radiation',
                           'Wind Speed',
                           'Wind Direction',
                           'Wind Gust'),
                   code = c('CLIM01',
                            'CLIM03.01',
                            'CLIM04.01',
                            'CLIM05.01',
                            'CLIM06.01',
                            'CLIM06.02',
                            'CLIM06.03')),
            by = "key") %>%
  select(-key) %>%
  spread(code, value) %>%
  # standardize timestamp
  mutate(date = as_date(tmsp),
         time = format(tmsp, '%H:%M'),
         UTC = case_when(siteid %in% c('ACRE', 'DPAC', 
                                       'AUGLA', 'CRAWF', 'DEFI_M', 'HARDIN_NW', 'HENRY', 'STJOHNS',
                                       'DEFI_R', 'FULTON', 'VANWERT',
                                       'TIDE') ~ tmsp + hours(5), 
                         siteid %in% c('BEAR', 'BEAR2', 'BENTON', 'DIKE', 'HICKORY', 'MAASS', 'SHEARER',
                                       'HICKS_B', 'CLAY_U', 'SERF_SD') ~ tmsp + hours(6)),
         UTC = format(UTC, '%Y-%m-%dT%H:%M:%S+00:00'),     # format according to ISO 8601 standard 
         timestamp_type = 'I') %>%
  select(siteid, station, date, time, UTC, timestamp_type, tmsp, 
         starts_with('CLIM')) -> weather_ALL_hourly_standard



# save hourly weather data
weather_ALL_hourly_standard %>%
  arrange(siteid, station, tmsp) %>%
  write_csv('Standard_Data/CSV/weather_hourly_all_variable.csv')

weather_ALL_hourly_standard %>%
  arrange(siteid, station, tmsp) %>%
  write_rds('Standard_Data/weather_hourly_all_variable.rds', compress = 'xz')



# DAILY WEATHER DATA ------------------------------------------------------

# there are some sites that has only hourly data
weather_ALL_hourly %>%
  filter(!is.na(value)) %>%
  mutate(year = year(tmsp)) %>%
  distinct(siteid, station, key, year) %>%
  mutate(hourly = 'yes') %>%
  full_join(weather_ALL_daily %>%
              filter(!str_detect(key, 'Reference ET'),
                     !str_detect(key, 'Dew|Run|Density|Photo'),
                     !str_detect(key, 'Min Solar'),
                     !str_detect(key, 'Max Solar'),
                     !str_detect(key, 'Min Rel'),
                     !str_detect(key, 'Max Rel'),
                     !str_detect(key, 'Max Wind'),
                     !str_detect(key, 'Evaporation'),
                     !str_detect(key, 'Snow'),
                     !str_detect(key, 'Soil')) %>%
              mutate(key = str_remove(key, 'Ave ')) %>%
              mutate(key = str_remove(key, 'Min ')) %>%
              mutate(key = str_remove(key, 'Max ')) %>%
              mutate(year = year(date)) %>%
              distinct(siteid, station, key, year) %>%
              mutate(daily = 'yes'),
            by = c('siteid', 'station', 'key', 'year')) %>%
  filter(is.na(daily) & !is.na(hourly)) %>%
  select(-daily) %>%
  spread(key, hourly) #%>% View()

# calculate daily values based on hourly weather measurements for missing sites
weather_ALL_hourly %>%
  filter(siteid %in% c('BENTON', 'DIKE', 'HICKORY', 'SHEARER')) %>%
  spread(key, value) %>%
  mutate(date = as_date(tmsp)) %>%
  group_by(siteid, station, date) %>%
  summarise(count = n(),
            rain_CHECK = sum(!is.na(Precipitation)),
            Precipitation = sum(Precipitation, na.rm = TRUE),
            temp_CHECK = sum(!is.na(`Air Temperature`)),
            `Ave Air Temperature` = mean(`Air Temperature`, na.rm = TRUE),
            `Min Air Temperature` = min(`Air Temperature`, na.rm = TRUE),
            `Max Air Temperature` = max(`Air Temperature`, na.rm = TRUE)) %>%
  mutate(Precipitation = ifelse(rain_CHECK < 19, NA, Precipitation),
         `Ave Air Temperature` = ifelse(temp_CHECK < 19, NA, `Ave Air Temperature`),
         `Min Air Temperature` = ifelse(temp_CHECK < 19, NA, `Min Air Temperature`),
         `Max Air Temperature` = ifelse(temp_CHECK < 19, NA, `Max Air Temperature`)) %>%
  select(-count, -contains('CHECK')) %>%
  ungroup() %>%
  gather(key, value, -siteid, -station, -date) -> df5
  

weather_ALL_hourly %>%
  filter(siteid == 'DEFI_R' & station == 'OnSite') %>%
  group_by(key) %>%
  mutate(value = zoo::na.approx(value, na.rm = FALSE, maxgap = 4)) %>%
  ungroup() %>%
  spread(key, value) %>%
  mutate(date = as_date(tmsp)) %>%
  group_by(siteid, station, date) %>%
  summarise(count = n(),
            rain_CHECK = sum(!is.na(Precipitation)),
            Precipitation = sum(Precipitation, na.rm = TRUE),
            temp_CHECK = sum(!is.na(`Air Temperature`)),
            `Ave Air Temperature` = mean(`Air Temperature`, na.rm = TRUE),
            `Min Air Temperature` = min(`Air Temperature`, na.rm = TRUE),
            `Max Air Temperature` = max(`Air Temperature`, na.rm = TRUE)) %>%
  mutate(Precipitation = ifelse(rain_CHECK < 19, NA, Precipitation),
         `Ave Air Temperature` = ifelse(temp_CHECK < 19, NA, `Ave Air Temperature`),
         `Min Air Temperature` = ifelse(temp_CHECK < 19, NA, `Min Air Temperature`),
         `Max Air Temperature` = ifelse(temp_CHECK < 19, NA, `Max Air Temperature`)) %>%
  select(-count, -contains('CHECK')) %>%
  ungroup() %>%
  gather(key, value, -siteid, -station, -date) -> df6

# calculate daily wind gust for WIRSIS sites
weather_ALL_hourly %>%
  filter(siteid %in% c('DEFI_R', 'FULTON', 'VANWERT'),
         str_detect(key, 'Wind Gust')) %>%
  filter(!(siteid == 'DEFI_R' & station == 'OnSite')) %>% 
  mutate(date = as_date(tmsp)) %>%
  group_by(siteid, station, date, key) %>%
  summarise(count = n(),
            CHECK = sum(!is.na(value)),
            value = max(value, na.rm = TRUE)) %>%
  mutate(value = ifelse(count < 19, NA, value),
         value = ifelse(is.infinite(value), NA, value)) %>%
  select(-count, -CHECK) %>%
  # remove erroneous 0 readings
  mutate(value = case_when(siteid == 'DEFI_R' & date == ymd(20031102) ~ NA_real_,
                           siteid == 'DEFI_R' & date == ymd(20070508) ~ NA_real_,
                           siteid == 'DEFI_R' & date %in% ymd(20040105):ymd(20040107) ~ NA_real_,
                           TRUE ~ value)) %>%
  ungroup() -> df7

# calculate daily wind direction for WIRSIS sites
weather_ALL_hourly %>%
  filter(siteid %in% c('DEFI_R', 'FULTON', 'VANWERT'),
         key %in% c('Wind Direction', 'Wind Speed')) %>%
  filter(!(siteid == 'DEFI_R' & station == 'OnSite')) %>% 
  mutate(date = as_date(tmsp)) %>%
  spread(key, value) %>%
  # calculate wind components
  mutate(x = `Wind Speed` * cospi(`Wind Direction`/180),
         y = `Wind Speed` * sinpi(`Wind Direction`/180)) %>%
  group_by(siteid, station, date) %>%
  summarise(count = n(),
            `Wind Direction` = mean(`Wind Direction`, na.rm = TRUE),
            `Wind Speed` = mean(`Wind Speed`, na.rm = TRUE),
            x = mean(x, na.rm = TRUE),
            y = mean(y, na.rm = TRUE)) %>%
  # combine components of wind diraction and convert to degrees
  mutate(WD = atan2(y, x)/pi*180,
         WD = ifelse(WD < 0, 360 + WD, WD)) %>%
  # after checking that they are close to simple average substitude them
  mutate(`Wind Direction` = WD) %>%
  select(-x, -y, -WD) %>%
  gather(key, value, 5:6) %>%
  mutate(value = ifelse(count < 19, NA, value),
         value = ifelse(is.infinite(value), NA, value)) %>%
  select(-count) %>%
  # remove erroneous 0 readings
  mutate(value = case_when(siteid == 'DEFI_R' & date %in% ymd(20040105):ymd(20040107) ~ NA_real_,
                           TRUE ~ value)) %>%
  ungroup() -> df8

# calculate daily solar radiation at DPAC
weather_ALL_hourly %>%
  filter(siteid == 'DPAC',
         key %in% 'Solar Radiation') %>% 
  mutate(date = as_date(tmsp)) %>%
  group_by(siteid, station, date, key) %>%
  summarise(count = n() ,
            CHECK = sum(!is.na(value)),
            value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  # convert solar radiation to MJ/m2
  mutate(value = value  * 60 * 60 * 24 / 10^6) %>%
  mutate(value = ifelse(count < 19, NA, value),
         value = ifelse(is.infinite(value), NA, value)) %>%
  select(-count, -CHECK) -> df9
  


# Standardize daily weather data
weather_ALL_daily %>%
  # correct ET names to reflect the method used
  mutate(key = case_when(str_detect(key, 'Penman') ~ str_replace(key, 'Monteith', 'Monteith Short Grass'),
                         str_detect(key, 'Short Crop') ~ str_replace(key, 'Short', 'Penman-Monteith Short'),
                         str_detect(key, 'Grass') ~ str_replace(key, '\\(', '\\(Penman-Monteith '),
                         str_detect(key, 'ET$') ~ str_replace(key, 'ET', 'ET (Thornthwaite Grass)'),   # NEED to check again
                         TRUE ~ key)) %>%
  # get rid of duplicated WIND SPEED measurements at WIRSIS
  left_join(df8 %>% mutate(value = 'Y') %>% select(everything(), DUPS = value), 
            by = c("siteid", "station", "date", "key")) %>%
  filter(is.na(DUPS)) %>%
  select(-DUPS) %>%
  # combine with the rest of the daily data
  bind_rows(df5, df6, df7, df8, df9) %>%
  # standardize variable names (some variables have two names)
  mutate(key = case_when(key == 'Relative Humidity' ~ 'Ave Relative Humidity',
                         TRUE ~ key)) %>%
  # add variable codes
  left_join(tibble(key = c('Precipitation',
                           'Snowfall',
                           'Ave Air Temperature',
                           'Min Air Temperature',
                           'Max Air Temperature',
                           'Dew-Point Temperature',
                           'Ave Relative Humidity',
                           'Min Relative Humidity',
                           'Max Relative Humidity',
                           'Ave Solar Irradiance',
                           'Min Solar Irradiance',
                           'Max Solar Irradiance',
                           'Solar Radiation',
                           'Photosynthetically Active Radiation',
                           'Wind Speed',
                           'Wind Direction',
                           'Wind Gust',
                           'Wind Run',
                           'Max Wind Speed',
                           'Pan Evaporation',
                           'Reference ET (Penman-Monteith Short Grass)',
                           'Reference ET (Penman-Monteith Tall Grass)',
                           'Reference ET (Penman-Monteith Short Crop)',
                           'Reference ET (Thornthwaite Grass)',   
                           'Ave Bare Soil Temperature (10 cm depth)',
                           'Min Bare Soil Temperature (10 cm depth)',
                           'Max Bare Soil Temperature (10 cm depth)',
                           'Min Bare Soil Temperature (2" depth)',
                           'Max Bare Soil Temperature (2" depth)',
                           'Min Bare Soil Temperature (4" depth)',
                           'Max Bare Soil Temperature (4" depth)',
                           'Min Bare Soil Temperature (8" depth)',
                           'Max Bare Soil Temperature (8" depth)'),
                   code = c('CLIM01',
                            'CLIM01.02',
                            'CLIM03.01.01',
                            'CLIM03.01.02',
                            'CLIM03.01.03',
                            'CLIM03.02',
                            'CLIM04.01.01',
                            'CLIM04.01.02',
                            'CLIM04.01.03',
                            'CLIM05.01.01',
                            'CLIM05.01.02',
                            'CLIM05.01.03',
                            'CLIM05.02',
                            'CLIM05.03',
                            'CLIM06.01',
                            'CLIM06.02',
                            'CLIM06.03',
                            'CLIM06.04',
                            'CLIM06.05',
                            'CLIM07.01',
                            'CLIM07.03.01',
                            'CLIM07.03.02',
                            'CLIM07.03.03',
                            'CLIM07.04.01',
                            'CLIM08.01.01',
                            'CLIM08.01.02',
                            'CLIM08.01.03',
                            'CLIM08.02.02',
                            'CLIM08.02.03',
                            'CLIM08.03.02',
                            'CLIM08.03.03',
                            'CLIM08.04.02',
                            'CLIM08.04.03')),
            by = "key") %>%
  select(-key) %>% 
  spread(code, value) %>%
  # standardize timestamp
  mutate(timestamp_type = 'D') %>%
  select(siteid, station, date, timestamp_type,
         starts_with('CLIM')) -> weather_ALL_daily_standard



# save daily weather data
weather_ALL_daily_standard %>%
  arrange(siteid, station, date) %>%
  write_csv('Standard_Data/CSV/weather_daily_all_variable.csv')

weather_ALL_daily_standard %>%
  arrange(siteid, station, date) %>%
  write_rds('Standard_Data/weather_daily_all_variable.rds', compress = 'xz')






