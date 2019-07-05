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
  bind_rows(df_all)
  

