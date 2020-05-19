# Initialize functions 
source('00_project_settings.R')



# DOWNLOAD ................................................................

# Site History
DownloadGoogleSheet('TD Site Metadata + History (Master)', FOLDER = 'Metadata')

# Master Key
DownloadGoogleSheet('TD Site Keys', FOLDER = 'Metadata')



# READ ....................................................................

# Site History
read_excel('Input_Data/Metadata/TD Site Metadata + History (Master).xlsx') %>%
  slice(-1) -> site_history


# Site Key
read_excel('Input_Data/Metadata/TD Site Keys.xlsx',
           sheet = 'SITES',
           skip = 1) -> site_keys


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
         lanscape_slope = `Landscape Slope`,
         tile_depth = `Depth of Tile`,
         tile_spacing = `Tile Spacing`,
         tile_main_diamter = `Tile Main Diameter`,
         tile_submain_diamter = `Tile Sub-Main Diameter`,
         tile_lateral_diameter = `Tile Lateral Diameter`,
         depth_of_restrictive_layer = `Depth to restrictive layer`,
         tile_grade = `Tile Grade`,
         drainage_coefficient = `Drainage Coefficient`,
         drainage_intensity = `Drainage Intensity`,
         kirkham_coefficient = `Kirkham Coefficient`,
         drainage_system_installation_date = `Installation Date of Drainage System`,
         control_structure_installation_date = `Installation Date of Control Structure`,
         buffer_width = `Buffer Width`,
         buffer_slope_parallel = `Buffer Slope_ Parallel`,
         buffer_slope_perp = `Buffer Slope_ Perp`,
         buffer_vegetation_dominant = `Buffer Vegetation_ Dominant`,
         buffer_vegetation_secondary = `Buffer Vegetation_ Secondary`,
         buffer_distribution_pipe_depth = `Buffer_Depth of Distribution Pipe`,
         buffer_distribution_pipe_length = `Buffer_Depth of Distribution Pipe`,
         buffer_distribution_pipe_slope = `Buffer_Slope of Distribution Pipe`,
         # NW_lat = `NW Lat`,
         # NW_lon = `NW Lon`,
         # NE_lat = `NE Lat`,
         # NE_lon = `NE Lon`,
         # SW_lat = `SW Lat`,
         # SW_lon = `SW Lon`,
         # SE_lat = `SE Lat`,
         # SE_lon = `SE Lon`,
         # Row_lonlat = `Raw LonLat`,
         `Drainage Class (1)`:`Soil Taxonomic Class (3)`) %>%
  mutate(data_year_first = str_remove(data_year_first, '\\.0$'),
         data_year_last  = str_remove(data_year_last,  '\\.0$'),
         latitude = round(as.numeric(latitude), 2),
         longitude = round(as.numeric(longitude), 2),
         site_area = ifelse(is.na(as.numeric(site_area)), 
                            site_area, 
                            as.character(round(as.numeric(site_area), 1))),
         tile_spacing = ifelse(is.na(as.numeric(tile_spacing)),
                               tile_spacing, 
                               as.character(round(as.numeric(tile_spacing), 1))),
         depth_of_restrictive_layer = str_replace(depth_of_restrictive_layer, "> ", ">"),
         drainage_system_installation_date  = str_remove(drainage_system_installation_date,  '\\.0$'),
         control_structure_installation_date  = str_remove(control_structure_installation_date,  '\\.0$'),
         buffer_width = ifelse(is.na(as.numeric(buffer_width)),
                               buffer_width, 
                               as.character(round(as.numeric(buffer_width), 1)))) %>%
  mutate_all(function(x) {str_replace(x, 'TBD', 'n/a')})-> site_history_GOOD

# Plot identifier
site_keys %>%
  filter(!Site_ID %in% c('BATH_A', 'BATH_R', 'HICKS_S', 'MILLER')) %>%
  mutate(ID = paste(State, County, sep = "_"),
         ID = str_remove_all(ID, " ")) %>%
  group_by(ID) %>%
  mutate(Site_Name = n(),
         PI_Main = 1:n()) %>%
  ungroup() %>%
  mutate(ID = ifelse(Site_Name > 1, paste0(ID, PI_Main), ID),
         KEY = str_replace(KEY_SITE, '\\.', '_')) %>%
  select(siteid = Site_ID, ID, KEY) %>%
  arrange(ID) %>%
  write_csv('Ag_Commons_Data/siteids.csv')


# ... Methods -----------------------------------------------------------------
methods %>%
  filter(ACTION != 'NO DATA' | is.na(ACTION)) %>%
  select(siteid = SiteID, 
         data_categoty = Data_Categoty, 
         NEW_CODE = NEW_VAR_CODE, 
         method_description = Description_of_Method) %>%
  arrange(siteid, data_categoty, NEW_CODE) -> methods_GOOD


# Save Metadata -----------------------------------------------------------

write_rds(site_history_GOOD, 'Standard_Data/meta_site_history.rds', compress = 'xz')
write_rds(methods_GOOD, 'Standard_Data/meta_methods.rds', compress = 'xz')

