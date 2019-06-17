# Standardize agronomic data
agr_ALL %>%
  # update variable names
  mutate(key = case_when(str_detect(key, 'AGRXX Soybean Yield') ~ 
                           'AGR69 Soybean grain yield at 13.0 % MB in DWM zone of influence',
                         str_detect(key, 'AGRXX Corn Yield') ~ 
                           'AGR67 Corn grain yield at 15.5% MB in DWM zone of influence',
                         TRUE ~ key)) 
