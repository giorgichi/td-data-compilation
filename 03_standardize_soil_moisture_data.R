# Initialize functions 
source('00_project_settings.R')



# Read All soil data
sm_ALL <- read_rds('Inter_Data/sm_ALL.rds')



# Calculate daily values
sm_ALL %>%
  # calculate daily data
  select(-tmsp) %>%
  group_by(siteid, plotid, location, depth, date) %>%
  summarise(soil_moisture = mean(soil_moisture, na.rm = TRUE),
            soil_temp = mean(soil_temp, na.rm = TRUE),
            soil_ec = mean(soil_ec, na.rm = TRUE)) %>%
  ungroup() -> 
  sm_ALL_daily


# visualize data
sm_ALL_daily %>%
  filter(siteid == 'DPAC') %>% filter(depth == '10 cm') %>%
  mutate(year = year(date),
         tmsp = update(date, year = 2012)) %>%
  ggplot(aes(tmsp, soil_moisture, col = plotid)) +
  geom_point() +
  facet_grid(year ~ .) +
  theme_gio



