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
  bind_rows(df_all) ->
  weather_ALL_hourly_standard

# THE HOURLY DATA NEEDS to UPDATE ACRE WEATHER and READY TO OUTPUT




# DAILY WEATHER DATA ------------------------------------------------------

# there are some sites that has only hourly data
weather_ALL_hourly_standard %>%
  mutate(year = year(tmsp)) %>%
  distinct(siteid, station, year) %>%
  mutate(hourly = 'yes') %>%
  full_join(weather_ALL_daily %>%
              mutate(year = year(date)) %>%
              distinct(siteid, station, year) %>%
              mutate(daily = 'yes'),
            by = c('siteid', 'station', 'year')) %>%
  filter(is.na(daily) & !is.na(hourly))

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
  mutate(date = as_date(tmsp)) %>%
  group_by(siteid, station, date, key) %>%
  summarise(count = n(),
            CHECK = sum(!is.na(value)),
            value = max(value, na.rm = TRUE)) %>%
  filter(count > 24 & count < 36)
  filter(CHECK > 0 & CHECK != count)
  mutate(value = ifelse(count < 19, NA, value),
         value = ifelse(is.infinite(value), NA, value)) %>%
  select(-count) %>%
  ungroup() -> df7

# calculate daily wind direction for WIRSIS and HICKS_B sites
weather_ALL_hourly %>%
  filter(siteid %in% c('DEFI_R', 'FULTON', 'VANWERT', 'HICKS_B'),
         key %in% c('Wind Direction', 'Wind Speed')) %>%
  mutate(date = as_date(tmsp)) %>%
  spread(key, value) %>%
  mutate(x = `Wind Speed` * cospi(`Wind Direction`/180),
         y = `Wind Speed` * sinpi(`Wind Direction`/180)) %>%
  group_by(siteid, station, date) %>%
  summarise(count = n(),
            value = max(value, na.rm = TRUE)) %>%
  mutate(value = ifelse(count < 19, NA, value),
         value = ifelse(is.infinite(value), NA, value)) %>%
  select(-count) %>%
  ungroup() -> df8

# combine with the rest of the daily data
weather_ALL_daily %>%
  bind_rows(df5, df6, df7) %>%
  write_csv('Output_Data/weather_daily_all_variable.csv')
  
  # distinct(key, siteid) %>%
  # count(key) %>% 
  # write.csv('Output_Data/weather_daily_variable_count_NEW.txt', 
  #           row.names = FALSE, quote = FALSE)

  # IMPORTANT NOTE: >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # SERF_IA ----
    # there are multiple weather stations to represent on-site data
    # quality of precipitation data varies and availibility varies accross stations and years
    # use:
    # DAILY ISUnetwork (mesonet) for 2014 and forward (station was upgraded in late 2013 to improve data quality)
    # MANUAL from 2007 to 2013
    # DAILY LevelRain is actual on-site weather station which was often clogged by bird droppings and other,
    # hence qaulity of this data is always questionable (Carl does not trust it)

  
  
  # drop variables of NO interest
  filter(!key %in% c('Photosynthetically Active Radiation', 
                     'Dew-Point Temperature',
                     'Max Relative Humidity',
                     'Min Relative Humidity',
                     'Ave Solar Radiation',
                     'Max Solar Radiation',
                     'Min Solar Radiation',
                     'Max Bare Soil Temperature (2\" depth)',
                     'Min Bare Soil Temperature (2\" depth)',
                     'Max Bare Soil Temperature (8\" depth)',
                     'Min Bare Soil Temperature (8\" depth)')) %>%
  
  # filter(key == 'Total Energy Density')
  count(key) %>% pull(key)
