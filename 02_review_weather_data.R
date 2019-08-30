# Initialize functions 
source('00_project_settings.R')


# Read weather data
weather_ALL_hourly <- read_rds('Inter_Data/weather_ALL_hourly.rds')
weather_ALL_daily <- read_rds('Inter_Data/weather_ALL_daily.rds')



# HOURLY WEATHER DATA -----------------------------------------------------

# number of sites with hourly variables measured
weather_ALL_hourly %>%
  distinct(siteid, key) %>%
  count(key)

# 1 Air Temperature      13
# 2 Precipitation        22
# 3 Relative Humidity     5
# 4 Solar Radiation       6
# 5 Wind Direction        4
# 6 Wind Gust             3
# 7 Wind Speed            6


# Plot Hourly Weather by Variable ------------------------------------------------

# Wind Speed 
# 6 sites: ACRE, DPAC, HICKS_B, DEFI_R, FULTON, VANWERT
weather_ALL_hourly %>%
  filter(str_detect(key, 'Speed')) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x=tmsp, y=value, col=siteid)) +
  geom_point(na.rm = TRUE, alpha = 0.5) +
  facet_grid(siteid ~ ., scales = 'free') +
  ggtitle('Wind Speed') +
  theme_gio2
ggsave(paste0('Figs/WEATHER/weather_hourly_WIND_SPEED_', Sys.Date(),'.png'), 
       width = 16, height = 8)

# Wind Direction 
# 4 sites: HICKS_B, DEFI_R, FULTON, VANWERT
weather_ALL_hourly %>%
  filter(str_detect(key, 'Wind Direction')) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x=tmsp, y=value, col=siteid)) +
  geom_point(na.rm = TRUE, alpha = 0.5) +
  facet_grid(siteid ~ ., scales = 'free') +
  ggtitle('Wind Direction') +
  theme_gio2
ggsave(paste0('Figs/WEATHER/weather_hourly_WIND_DIRECTION_', Sys.Date(),'.png'), 
       width = 16, height = 8)

# Wind Gust
# 3 sites: DEFI_R, FULTON, VANWERT
weather_ALL_hourly %>%
  filter(str_detect(key, 'Wind Gust')) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x=tmsp, y=value, col=siteid)) +
  geom_point(na.rm = TRUE, alpha = 0.5) +
  facet_grid(siteid ~ ., scales = 'free') +
  ggtitle('Wind Gust') +
  theme_gio2
ggsave(paste0('Figs/WEATHER/weather_hourly_WIND_GUST_', Sys.Date(),'.png'), 
       width = 16, height = 8)

# Relative Humidity
# 5 sites: ACRE, HICKS_B, DEFI_R, FULTON, VANWERT
weather_ALL_hourly %>%
  filter(str_detect(key, 'Relative Humidity')) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x=tmsp, y=value, col=siteid)) +
  geom_point(na.rm = TRUE, alpha = 0.5) +
  facet_grid(siteid ~ ., scales = 'free') +
  ggtitle('Relative Humidity') +
  theme_gio2
ggsave(paste0('Figs/WEATHER/weather_hourly_RH_', Sys.Date(),'.png'), 
       width = 16, height = 8)

# Solar Radiation
# 6 sites: ACRE, DPAC, HICKS_B, DEFI_R, FULTON, VANWERT
weather_ALL_hourly %>%
  filter(str_detect(key, 'Solar Radiation')) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x=tmsp, y=value, col=siteid)) +
  geom_point(na.rm = TRUE, alpha = 0.5) +
  facet_grid(siteid ~ ., scales = 'free') +
  ggtitle('Solar Radiation') +
  theme_gio2
ggsave(paste0('Figs/WEATHER/weather_hourly_SR_', Sys.Date(),'.png'), 
       width = 16, height = 8)

# Air Temperature
# 13 sites: ACRE, DPAC, HICKS_B, DEFI_R, FULTON, VANWERT
weather_ALL_hourly %>%
  filter(str_detect(key, 'Air Temperature')) %>% 
  filter(siteid %in% c('ACRE', 'DPAC', 'HICKS_B', 'DEFI_R', 'FULTON', 'VANWERT')) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x=tmsp, y=value, col=siteid)) +
  geom_point(na.rm = TRUE, alpha = 0.5) +
  facet_grid(siteid ~ ., scales = 'free') +
  ggtitle('Air Temperature') +
  theme_gio2
ggsave(paste0('Figs/WEATHER/weather_hourly_AIR_TEMP_OH_', Sys.Date(),'.png'), 
       width = 16, height = 8)


# selecet sites for plotting
meta_key_site %>%
  select(Site_ID, State, PI_Main, DWR_Type) %>% 
  filter(State %in% c('OH')) %>%
  pull(Site_ID) -> my_sites

# plot all data by selected sites
weather_ALL_hourly %>%
  filter(siteid %in% my_sites) %>%
  ggplot(aes(x=tmsp, y=value, col=siteid)) +
  geom_point(na.rm = TRUE, alpha = 0.5) +
  facet_grid(key ~ ., scales = 'free') +
  theme_gio2
ggsave(paste0('Figs/WEATHER/weather_hourly_OH_', Sys.Date(),'.png'), 
       width = 16, height = 8)
