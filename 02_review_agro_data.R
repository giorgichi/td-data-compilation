# count variables
agr_ALL %>% 
  count(key) %>%
  separate(key, into = c('code', 'var'), extra = 'merge') %>%
  arrange(var) %>%
  View()


# prepare df for plotting
agr_ALL %>%
  filter(!is.na(value)) %>%
  # update variable names
  mutate(key = case_when(str_detect(key, 'AGRXX Soybean Yield') ~ 
                           'AGR69 Soybean grain yield at 13.0 % MB in DWM zone of influence',
                         str_detect(key, 'AGRXX Corn Yield') ~ 
                           'AGR67 Corn grain yield at 15.5% MB in DWM zone of influence',
                         TRUE ~ key)) %>%
  # count(key) %>%
  separate(key, into = c('code', 'var'), extra = 'merge') %>%
  separate(var, into = c('crop', 'var'), extra = 'merge') -> df


# Check Leaf Area Index
df %>%
  filter(str_detect(var, 'Area Index'))


# Plot variables collected at each site
df %>% 
  filter(!is.na(value)) %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  count(siteid, code, crop, var) %>%
  ggplot(aes(siteid, var)) +
  geom_point() +
    facet_grid(~ crop, scales = 'free', space = 'free_x') +
  theme_gio2 +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('Figs/AGR/agr_vars_list.png', width = 16, height = 8)

# Plot plant population for Corn and Soybean
df %>%
  filter(crop %in% c('Corn', 'Popocorn', 'Soybean')) %>%
  filter(var == 'final plant population') %>%
  ggplot(aes(x=siteid, y=value)) +
  geom_boxplot() +
  ggtitle('Final Plant Population') +
  facet_grid(crop ~ ., scales = "free_y") +
  theme_gio2
ggsave('Figs/AGR/final_plant_population.png', width = 16, height = 8)

# check the years with low plant population
df %>%
  filter(crop %in% c('Corn', 'Popocorn', 'Soybean')) %>%
  filter(var == 'final plant population') %>%
  filter(crop == 'Corn' & value < 35000)


# Plot Grain Moisture
df %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(var == 'grain moisture') %>%
  ggplot(aes(x=siteid, y=value)) +
  geom_boxplot() +
  ggtitle('Grain Moisture') +
  facet_grid(crop ~ .) +
  theme_gio2
ggsave('Figs/AGR/grain_moisture.png', width = 16, height = 8)

# check the years with low plant population
df %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(var == 'grain moisture') %>%
  filter(crop == 'Corn' & value > 300)


# Plot Grain Yield
df %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(str_detect(var, 'grain yield')) %>%
  ggplot(aes(x=siteid, y=value)) +
  geom_boxplot() +
  ggtitle('Grain Yield') +
  facet_grid(crop ~ ., scales = 'free') +
  theme_gio2 +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('Figs/AGR/grain_yield.png', width = 16, height = 8)



# Plot Corn Cob data
df %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(str_detect(var, 'cob')) %>%
  ggplot(aes(x=siteid, y=value)) +
  geom_boxplot() +
  ggtitle('Corb Cob') +
  facet_grid(var ~ ., scales = 'free') +
  theme_gio2
ggsave('Figs/AGR/corn_cob.png', width = 16, height = 8)


# Plot Grain Biomass
df %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(str_detect(var, 'grain biomass')) %>%
  ggplot(aes(x=siteid, y=value)) +
  geom_boxplot() +
  ggtitle('Grain Biomass') +
  facet_grid(crop ~ ., scales = 'free') +
  theme_gio2
ggsave('Figs/AGR/grain_biomass.png', width = 16, height = 8)

# check Grain Biomass
df %>%
  filter(crop %in% c('Corn', 'Soybean')) %>%
  filter(str_detect(var, 'grain yield')) %>%
  filter(!str_detect(var, 'zone')) %>%
  mutate(biomass_calc = (100 - parse_number(var))*value/100) %>%
  select(-var, -value) %>%
  full_join(df, by = c('siteid', 'plotid', 'year', 'crop')) %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(str_detect(var, 'grain biomass')) %>%
  ggplot(aes(x=biomass_calc, y=value, col=crop)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(na.rm = TRUE) +
  theme_gio2


<<<<<<< HEAD
# Plot Grain TN
df %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(str_detect(var, 'grain total n')) %>%
  ggplot(aes(x=siteid, y=value)) +
  geom_boxplot() +
  labs(title = 'Grain Total Nitrogen', 
       x = NULL,
       y = 'Grain N, kg N/ha') +
  facet_grid(crop ~ ., scales = 'free') +
  theme_gio2
ggsave('Figs/AGR/grain_TN.png', width = 10, height = 8)

# Check Grain TN concentration
df %>%
  filter(crop %in% c('Corn')) %>%
  filter(var %in% c('grain total nitrogen at R6', 
                    'grain yield at 15.5% MB')) %>%
  mutate(var = ifelse(str_detect(var, 'yield'), 'yield', 'grain_TN')) %>%
  select(-code, -crop) %>%
  spread(var, value) %>%
  mutate(biomass = yield *0.845) %>%
  filter(!is.na(grain_TN)) %>%
  ggplot(aes(x=biomass, y=grain_TN, col=siteid)) +
  geom_point() +
  geom_point(aes(size = str_detect(siteid, 'MUDS')), show.legend = FALSE) +
  scale_size_discrete(range = c(2, 4)) +
  labs(title = 'Corn Grain Biomass vs Total N', 
       x = 'Grain Biomass, kg/ha',
       y = 'Grain Total Nitrogen, kg N/ha') +
  theme_gio2
ggsave('Figs/AGR/grain_TN_vs_Biomass.png', width = 10, height = 8)



# Plot Grain TC
df %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(str_detect(var, 'grain total c')) %>%
  ggplot(aes(x=siteid, y=value)) +
  geom_boxplot() +
  labs(title = 'Grain Total Carbon', 
       x = NULL,
       y = 'Grain Carbon, kg C/ha') +
  facet_grid(crop ~ ., scales = 'free') +
  theme_gio2
ggsave('Figs/AGR/grain_TC.png', width = 10, height = 8)


=======
>>>>>>> b649bbbb2b854c38b47a79bf0d39c3ce4944201b

# Plot Veg Biomass
df %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(str_detect(var, 'vegetative biomass at')) %>%
  ggplot(aes(x=siteid, y=value)) +
  geom_boxplot() +
<<<<<<< HEAD
  labs(title = 'Vegetative Biomass', 
          x = NULL,
          y = 'Vegetative Biomass, kg/ha') +
  facet_grid(crop ~ ., scales = 'free') +
  theme_gio2
ggsave('Figs/AGR/vegetative_biomass.png', width = 10, height = 8)


# Plot Veg Biomass TN
df %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(str_detect(var, 'vegetative biomass total n')) %>%
  ggplot(aes(x=siteid, y=value)) +
  geom_boxplot() +
  labs(title = 'Vegetative Biomass Total Nitrogen', 
       x = NULL,
       y = 'Vegetative Biomass, kg N/ha') +
  facet_grid(crop ~ ., scales = 'free') +
  theme_gio2
ggsave('Figs/AGR/vegetative_biomass_TN.png', width = 10, height = 8)


# Plot Veg Biomass TC
df %>%
  filter(!crop %in% c('Forage', 'Leaf', 'Sugarbeet')) %>%
  filter(str_detect(var, 'vegetative biomass total c')) %>%
  ggplot(aes(x=siteid, y=value)) +
  geom_boxplot() +
  labs(title = 'Vegetative Biomass Total Carbon', 
       x = NULL,
       y = 'Vegetative Biomass, kg C/ha') +
  facet_grid(crop ~ ., scales = 'free') +
  theme_gio2
ggsave('Figs/AGR/vegetative_biomass_TC.png', width = 10, height = 8)
=======
  ggtitle('Vegetative Biomass') +
  facet_grid(crop ~ ., scales = 'free') +
  theme_gio2
ggsave('Figs/AGR/vegetative_biomass.png', width = 16, height = 8)
>>>>>>> b649bbbb2b854c38b47a79bf0d39c3ce4944201b
