# Initialize functions 
source('00_project_settings.R')



# functions for rectifying backend data
backend_date <- function(df, COL = 'date') {
  df %>%
    mutate(DATE = as.numeric(!!sym(COL))) %>%
    mutate(DATE = as.Date(DATE, origin = ymd(18991230))) %>%
    mutate(!!COL := DATE) %>% 
    mutate(sheet = NA, editedby = NA, updated = NA) %>%
    select(-sheet, -editedby, -updated, -DATE)
}

plotid_expansion <- function(df, COL = 'plotid') {
  df %>%
    mutate(PLOT = str_split(!!sym(COL), ", ?+")) %>%
    unnest(PLOT) %>%
    mutate(PLOT = str_remove(PLOT, '\r$')) %>%
    mutate(!!COL := PLOT) %>%
    select(-PLOT)
}


# DOWNLOAD ................................................................
 
DownloadGoogleSheet('Management DataStore (TD backend for mandata)', FOLDER = 'Metadata',
                    ID = "1m797EoMbtAvasqum-pB5ykVMeHsGWwcVvre8MbvE-xM")



# READ ....................................................................

ReadExcelSheets('Input_Data/Metadata/Management DataStore (TD backend for mandata).xlsx') ->
  fm_backend



# Format data -------------------------------------------------------------

#... Planting and Harvesting -----
fm_backend %>%
  pluck(1) %>%
  backend_date() %>%
  plotid_expansion() %>%
  select(action,
         siteid = uniqueid,
         plotid,
         year_calendar = calendaryear,
         year_crop = cropyear,
         date,
         operation,
         cashcrop,
         plant_hybrid = planthybrid,
         plant_maturity = plantmaturity,
         plant_maturity_GDD_F = `plantmaturity_GDD (F)`,
         plant_maturity_GDD_source = plantmaturity_GDD_source,
         plant_rate = plantrate,
         plant_rate_units = plantrateunits,
         comments) %>%
  separate(operation, into = 'operation', sep = '_', extra = 'drop') %>%
  mutate(operation = case_when(operation == 'harvest' ~ 'harvesting',
                               operation == 'plant'   ~ 'planting',
                               TRUE ~ operation)) -> fm_planting


# ... Tillage and Fertilizer Application ----
fm_backend %>%
  pluck(2) %>%
  backend_date() %>%
  plotid_expansion() %>%
  select(action,
         siteid = uniqueid,
         plotid,
         year_calendar = calendaryear,
         year_crop = cropyear,
         date,
         cashcrop,
         operation,
         depth,
         lime_rate = limerate,
         manure_source = manuresource,
         manure_method = manuremethod,
         manure_rate = manurerate,     
         -manurerateunits,               
         fertilizer_form = fertilizerform,
         fertilizer_crop = fertilizercrop,
         fertilizer_application_type = fertilizerapptype,
         fertilizer_formulation = fertilizerformulation,
         fertilizer_rate = productrate,
         nitrogen_percent = nitrogen,
         phosphate_percent = phosphate,
         phosphorus_percent = phosphorus, 
         potash_percent = potash,
         potassium_percent = potassium,   
         sulfur_percent = sulfur,
         zinc_percent = zinc,
         magnesium_percent = magnesium,
         calcium_percent = calcium,
         iron_percent = iron,
         nitrogen_elem,
         phosphorus_elem,
         potassium_elem,
         sulfur_elem,
         zinc_elem,
         magnesium_elem,
         calcium_elem,
         iron_elem,
         stabilizer,
         stabilizer_used = stabilizerused,
         stabilizer_name = stabilizername,
         comments) %>%
  separate(operation, into = c('operation', 'operation_type'), sep = '_', extra = 'drop') %>%
  mutate(operation = case_when(operation == 'sample' ~ 'sampling',
                               operation == 'fertilizer' ~ 'fertilizing',
                               operation == 'manure'     ~ 'fertilizing',
                               operation == 'soiladmend' ~ 'soil amendment',
                               TRUE ~ operation),
         operation_type = case_when(operation == 'sampling' ~ 'soil nitrate',
                                    TRUE ~ operation_type)) -> fm_fertilizing


# ... Residue ----
fm_backend %>%
  pluck(3) %>%
  select(action,
         siteid = uniqueid,
         year_calendar = calendaryear,
         year_crop = cropyear,
         crop,
         notill,
         comments) %>%
  ungroup() -> fm_residue


# ... Pesticides ----
fm_backend %>%
  pluck(4) %>%
  backend_date() %>%
  mutate(operation_type = operation,
         operation = 'pesticide application') %>%
  select(action,
    siteid = uniqueid,
    #plotid,
    year_calendar = calendaryear,
    year_crop = cropyear,
    date,
    crop,
    operation,
    operation_type,
    method,
    timing,
    total_rate = totalrate,
    product1_name = product1, product1_rate = rate1, product1_form = rateunit1,
    product2_name = product2, product2_rate = rate2, product2_form = rateunit2,
    product3_name = product3, product3_rate = rate3, product3_form = rateunit3,
    product4_name = product4, product4_rate = rate4, product4_form = rateunit4,
    everything()) -> fm_pesticide


# ... Irrigation ----
fm_backend %>%
  pluck(5) %>%
  separate(irr_end_date, into = c('irr_end_date', 'irr_end_time'), sep = "T") %>%
  separate(irr_start_date, into = c('irr_start_date', 'irr_start_time'), sep = "T") %>%
  mutate_at(vars(ends_with('time')), function(x) str_remove(x, 'Z')) %>%
  mutate_at(vars(ends_with("_date")), ymd) %>%
  plotid_expansion(COL = 'irr_structure') %>%
  mutate(plotid = ifelse(uniqueid %in% c('DEFI_R', 'VANWERT'), 
                         plotid, 
                         irr_structure)) %>%
  select(action,
         siteid = uniqueid,
         plotid,
         irrigation_structure = irr_structure,
         irrigation_method = irrigationmethod,
         year_calendar = calendaryear,
         date_irrigation_start = irr_start_date,
         time_irrigation_start = irr_start_time,
         date_irrigation_end = irr_end_date,
         time_irrigation_end = irr_end_time,
         irrigation_amount = irrigationamount, 
         comments) -> fm_irrigation



# ... DWM ----
fm_backend %>%
  pluck(6) %>%
  select(-(updated:sheet)) %>%
  separate(outlet_date, into = c('date', 'time'), sep = 'T') %>%
  mutate(date = as.Date(date),
         time = str_sub(time, 1, 5)) %>%
  plotid_expansion(COL = 'box_structure') %>%
  mutate(plotid = ifelse(uniqueid == 'MUDS1', box_structure, plotid)) %>%
  select(action,
         siteid = uniqueid,
         plotid,
         control_structure = box_structure,
         year_calendar = calendaryear,        
         date,
         time,
         outlet_depth = outletdepth,
         outlet_height = outletheight,
         comments) -> fm_dwm



# ... Notes ----
fm_backend %>%
  pluck(7) %>%
  select(1:4) %>%
  mutate(calendaryear = str_remove(calendaryear, "\\.0$")) %>%
  select(action,
         siteid = uniqueid,
         year_calendar = calendaryear,
         notes) -> fm_notes




# Save Metadata -----------------------------------------------------------

# save management data
write_rds(fm_planting, 'Inter_Data/fm_planting.rds', compress = 'xz')
write_rds(fm_fertilizing, 'Inter_Data/fm_fertilizing.rds', compress = 'xz')
write_rds(fm_pesticide, 'Inter_Data/fm_pesticide.rds', compress = 'xz')
write_rds(fm_residue, 'Inter_Data/fm_residue.rds', compress = 'xz')
write_rds(fm_dwm, 'Inter_Data/fm_dwm.rds', compress = 'xz')
write_rds(fm_irrigation, 'Inter_Data/fm_irrigation.rds', compress = 'xz')
write_rds(fm_notes, 'Inter_Data/fm_notes.rds', compress = 'xz')

