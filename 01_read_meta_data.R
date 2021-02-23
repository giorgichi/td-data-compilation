# Initialize functions 
source('00_project_settings.R')



# DOWNLOAD ................................................................

# Site History
DownloadGoogleSheet('TD Site Metadata + History (Master)', FOLDER = 'Metadata',
                    ID = "1oZ2NEmoa0XHSGTWKaBLt0DJK1kIbpWO6iSZ0I2OE2gA")

# Master Key
DownloadGoogleSheet('TD Site Keys', FOLDER = 'Metadata',
                    ID = "1PjB63WtYRyYnasm5mmLUF2-WdHuc_3dA_q8iEVV_C28")



# READ ....................................................................

# Site History
read_excel('Input_Data/Metadata/TD Site Metadata + History (Master).xlsx') %>%
  slice(-1) -> site_history


# Site Key
read_excel('Input_Data/Metadata/TD Site Keys.xlsx',
           sheet = 'SITES',
           skip = 1) -> site_keys

read_excel('Input_Data/Metadata/TD Site Keys.xlsx',
           sheet = 'PLOTS',
           skip = 1) -> plot_keys

read_excel('Input_Data/Metadata/TD Site Keys.xlsx',
           sheet = 'DWM ID',
           skip = 1) -> dwm_keys


# Methods
read_excel('Input_Data/Metadata/TD Sampling Methods.xlsx',
           sheet = 'UPDATED') -> methods


# Data Dictionary
read_excel('Input_Data/Metadata/TD Data Dictionary.xlsx',
           sheet = 'FINAL') -> dd



# Format data -------------------------------------------------------------

# ... Site History  -------------------------------------------------------
site_history %>%
  select(siteid = UniqueID,
         lead_PI = `Lead PI`,
         co_leaders = `Co-Leaders`,
         # PI_state = `State PI`,
         PI_institution = `Institution Name`,
         PI_institution_unit = Unit,
         official_farm_name = `Official Farm Name`, 
         farm_field_name = `FarmField Name`,
         data_year_first = `Data Year 1`,
         data_year_last = `Data Year n`,
         state = State,
         county = County,
         FIPS = `FIPS code`,
         city_nearest = `City (nearest)`,
         latitude = Latitude, 
         longitude =  Longitude,
         site_area = `Site Area`,
         number_of_plots = `Number of Plots/ Subplots`,
         experimental_design = `Experimental Design`,
         drainage_retention_practice = `Drainage Retention Practice`,
         source_of_irrigation_water_1 = `Source of Irrigation Water [1]`,
         source_of_irrigation_water_2 = `Source of Irrigation Water [2]`,
         type_of_water_storage_system = `Type of Water Storage System`,
         water_storage_capacity = `Water Storage Capacity`,
         pond_surface_area = `Pond Surface Area`,
         landscape_slope = `Landscape Slope`,
         drain_depth = `Depth of Tile`,
         drain_spacing = `Tile Spacing`,
         drain_main_diameter = `Tile Main Diameter`,
         drain_submain_diameter = `Tile Sub-Main Diameter`,
         drain_lateral_diameter = `Tile Lateral Diameter`,
         depth_of_restrictive_layer = `Depth to restrictive layer`,
         drain_grade = `Tile Grade`,
         drainage_coefficient = `Drainage Coefficient`,
         drainage_intensity = `Drainage Intensity`,
         kirkham_coefficient = `Kirkham Coefficient`,
         drainage_system_installation_year = `Installation Date of Drainage System`,
         control_structure_installation_year = `Installation Date of Control Structure`,
         buffer_width = `Buffer Width`,
         buffer_slope_parallel = `Buffer Slope_ Parallel`,
         buffer_slope_perp = `Buffer Slope_ Perp`,
         buffer_vegetation_dominant = `Buffer Vegetation_ Dominant`,
         buffer_vegetation_secondary = `Buffer Vegetation_ Secondary`,
         buffer_distribution_pipe_depth = `Buffer_Depth of Distribution Pipe`,
         buffer_distribution_pipe_length = `Buffer_Depth of Distribution Pipe`,
         buffer_distribution_pipe_slope = `Buffer_Slope of Distribution Pipe`,
         # IEM_climate_site = `IEM Climate Site`,
         # NW_lat = `NW Lat`,
         # NW_lon = `NW Lon`,
         # NE_lat = `NE Lat`,
         # NE_lon = `NE Lon`,
         # SW_lat = `SW Lat`,
         # SW_lon = `SW Lon`,
         # SE_lat = `SE Lat`,
         # SE_lon = `SE Lon`,
         # Row_lonlat = `Raw LonLat`,
         soil_series_name_1 = `Soil Series Name (1)`,
         soil_series_description_1 = `Soil Series Description (1)`,
         soil_texture_series_1 = `Soil Texture Series (1)`,
         soil_drainage_class_1 = `Drainage Class (1)`,
         soil_taxonomic_class_1 = `Soil Taxonomic Class (1)`,
         
         soil_series_name_2 = `Soil Series Name (2)`,
         soil_series_description_2 = `Soil Series Description (2)`,
         soil_texture_series_2 = `Soil Texture Series (2)`,
         soil_drainage_class_2 = `Drainage Class (2)`,
         soil_taxonomic_class_2 = `Soil Taxonomic Class (2)`,
         
         soil_series_name_3 = `Soil Series Name (3)`,
         soil_series_description_3 = `Soil Series Description (3)`,
         soil_texture_series_3 = `Soil Texture Series (3)`,
         soil_drainage_class_3 = `Drainage Class (3)`,
         soil_taxonomic_class_3 = `Soil Taxonomic Class (3)`
         ) %>%
  mutate(data_year_first = str_remove(data_year_first, '\\.0$'),
         data_year_last  = str_remove(data_year_last,  '\\.0$'),
         latitude = round(as.numeric(latitude), 2),
         longitude = round(as.numeric(longitude), 2),
         site_area = ifelse(is.na(as.numeric(site_area)), 
                            site_area, 
                            as.character(round(as.numeric(site_area), 1))),
         drain_spacing = ifelse(is.na(as.numeric(drain_spacing)),
                               drain_spacing, 
                               as.character(round(as.numeric(drain_spacing), 1))),
         depth_of_restrictive_layer = str_replace(depth_of_restrictive_layer, "> ", ">"),
         drainage_system_installation_year  = str_remove(drainage_system_installation_year,  '\\.0$'),
         control_structure_installation_year  = str_remove(control_structure_installation_year,  '\\.0$'),
         buffer_width = ifelse(is.na(as.numeric(buffer_width)),
                               buffer_width, 
                               as.character(round(as.numeric(buffer_width), 1)))) %>%
  # standardize soil texture names
  mutate(soil_texture_series_1 = str_to_lower(soil_texture_series_1),
         soil_texture_series_1 = 
           case_when(soil_texture_series_1 == "fine sandy loam" ~ "sandy loam",
                     soil_texture_series_1 == "silty loam" ~ "silt loam",
                     TRUE ~ soil_texture_series_1),
         soil_texture_series_2 = str_to_lower(soil_texture_series_2),
         soil_texture_series_2 = 
           case_when(soil_texture_series_2 == "fine sandy loam" ~ "sandy loam",
                     soil_texture_series_2 == "silty loam" ~ "silt loam",
                     TRUE ~ soil_texture_series_2),
         soil_texture_series_3 = str_to_lower(soil_texture_series_3),
         soil_texture_series_3 = 
           case_when(soil_texture_series_3 == "fine sandy loam" ~ "sandy loam",
                     soil_texture_series_3 == "silty loam" ~ "silt loam",
                     TRUE ~ soil_texture_series_3)) %>%
  # standardize experimental design names
  mutate(experimental_design = ifelse(str_detect(experimental_design, "split"),
                                      str_to_title(experimental_design),
                                      experimental_design)) %>%
  mutate_all(function(x) {str_replace(x, 'TBD', 'n/a')}) -> site_history_GOOD


# ... Plot identifier ----------------------------------------------
plot_keys %>%
  filter(KEEP == 'YES') %>%
  # swap plot ID with Name at FAIRM
  mutate(Plot_ID = ifelse(str_detect(Plot_ID, 'Sump'), Plot_Name, Plot_ID)) %>%
  # remove plot IDs for SB sites
  mutate(Plot_ID = ifelse(DWM == 'saturated buffer' & Site_ID != "WILKIN3", 
                          NA_character_, Plot_ID)) %>%
  # round up numbers
  mutate(
    Plot_Area_Agro = round(as.numeric(Plot_Area_Agro), 2),
    Plot_Area_Drain = round(as.numeric(Plot_Area_Drain), 2),
    Tile_Depth = ifelse(Tile_Depth == "unknown", 
                        Tile_Depth, 
                        round(as.numeric(Tile_Depth), 2)),
    Tile_Spacing = ifelse(Tile_Spacing %in% c("varies", "unknown", "random"), 
                          Tile_Spacing, 
                          as.character(round(as.numeric(Tile_Spacing), 1))),
    Tile_Grade = ifelse(Tile_Grade == "unknown",
                        Tile_Grade, 
                        round(as.numeric(Tile_Grade), 2)),
    Tile_Diameter = ifelse(Tile_Diameter == "unknown",
                           Tile_Diameter,
                           round(as.numeric(Tile_Diameter), 1)),
    Sub_Main_Diameter = round(as.numeric(Sub_Main_Diameter), 1),
    Main_Diameter = ifelse(Main_Diameter == "unknown",
                           Main_Diameter,
                           round(as.numeric(Main_Diameter), 1)),
    Tile_Depth = ifelse(Tile_Depth == "1", "1.00", Tile_Depth)
         ) %>%
  # remove redundant comments
  mutate(Comments = case_when(str_detect(Comments, 'soil types in the wetland') ~ NA_character_,
                              str_detect(Comments, 'for SB Plot_Area_Agro') ~ NA_character_,
                              str_detect(Comments, 'Field 10') ~ str_remove(Comments, ' is an extension of 4, but are farmed separately.\\ It'),
                              str_detect(Comments, 'Field 11') ~ str_remove(Comments, ' is an extension of 5, but are farmed separately\\. It'),
                              str_detect(Comments, 'N treatments') ~ NA_character_,
                              str_detect(Comments, 'plot is not ') ~ NA_character_,
                              TRUE ~ Comments),
         Comments = ifelse(Site_ID == 'BEAR', 'drainage area before 2014 was 10.1 ha', Comments)) %>%
  select(KEY_PLOT, 
         siteid = Site_ID,
         plotid = Plot_ID,
         dwm_treatment = DWM,
         irrigation_type = IRR_Type,
         drainage_area = Plot_Area_Drain,
         plot_area = Plot_Area_Agro,
         drain_depth = Tile_Depth,
         drain_spacing = Tile_Spacing,
         drain_main_diameter = Main_Diameter,
         drain_submain_diameter = Sub_Main_Diameter,
         drain_lateral_diameter = Tile_Diameter,
         drain_grade = Tile_Grade,
         drain_material = Tile_Material,
         comments = Comments,
         comments_dwm = Comments_DWM) -> plot_keys_GOOD


# ... DWM identifier ----------------------------------------------
dwm_keys %>%
  # get rid of site-plots that are not part of TD project
  inner_join(plot_keys_GOOD[1:5], by = 'KEY_PLOT') %>%
  # remove control structures managing sub-zones
  filter(CS_ID != "west_z2") %>%
  gather(year, dwm, starts_with('Y_')) %>%
  mutate(year = parse_number(year)) %>%
  filter(dwm != 'NA') %>% 
  select(KEY_PLOT,
         siteid,
         plotid,
         year,
         dwm_abb = dwm,
         dwm = DWM,
         dwr = irrigation_type) %>%
  mutate(dwm = case_when(dwm_abb == 'UD' ~ 'Non-Drained',
                         dwm_abb == 'CD' ~ 'Controlled Drainage',
                         dwm_abb == 'FD' ~ 'Free Drainage',
                         dwm_abb == 'SB' ~ 'Saturated Buffer',
                         dwm_abb == 'SD' ~ 'Surface Drainage',
                         dwm_abb == 'SH' ~ 'Shallow Drainage',
                         dwm_abb == 'SI' ~ 'Controlled Drainage with Subirrigation',
                         TRUE  ~ 'TBD'),
         dwr = case_when(dwr == 'subsurface' & !str_detect(dwm, 'Subirrigation') ~ 'none',
                         dwr == 'overhead' & !year %in% 2004:2009 ~ 'none',
                         str_detect(dwr, 'drip') ~ 'drip',
                         TRUE ~ dwr),
         dwr = case_when(dwr == 'none' ~ 'Non-Irrigated',
                         TRUE ~ paste(str_to_title(dwr), 'Irrigation'))) %>%
  arrange(siteid, year) -> dwm_keys_GOOD


# ... Methods -----------------------------------------------------------------
methods %>%
  filter(ACTION != 'NO DATA' | is.na(ACTION)) %>%
  select(siteid = SiteID, 
         data_category = Data_Categoty, 
         NEW_CODE = NEW_VAR_CODE, 
         method_description = Description_of_Method) %>%
  arrange(siteid, data_category, NEW_CODE) -> methods_GOOD


# Save Metadata -----------------------------------------------------------

write_rds(site_history_GOOD, 'Standard_Data/meta_site_history.rds', compress = 'xz')

plot_keys_GOOD %>%
  select(-KEY_PLOT) %>%
  write_rds('Standard_Data/meta_plot_ids.rds', compress = 'xz')

dwm_keys_GOOD %>%
  select(-KEY_PLOT) %>%
  write_rds('Standard_Data/meta_plot_treatments_annual.rds', compress = 'xz')

write_rds(methods_GOOD, 'Standard_Data/meta_methods.rds', compress = 'xz')

