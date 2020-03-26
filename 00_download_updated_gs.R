# identify subfolders within Google Folder for TD data
# let googledrive know this is a file ID or URL, as opposed to file name
as_id("0B6ZGw0coobCxfnphMzRKbEhadEVsSXVYSHIwX1AtZ0hJWVJGbjhmR2VTb2VJZzQybVZ1Mk0") %>%
  drive_get() %>%
  # identify the subfolders in that folder
  drive_ls(type = 'folder') %>%
  # remove my personal folder
  filter(!str_detect(name, '^_')) -> ws_folders


# identify the xlsx files in each subfolder
files <- vector(mode = 'list', length = nrow(ws_folders))
for (i in 1:nrow(ws_folders)) {
  files[[i]] <- drive_ls(ws_folders[i, ], type = 'spreadsheet') %>%
    mutate(ws = ws_folders$name[i])
}
gs_files <-
  bind_rows(files) %>%
  select(ws, everything()) %>%
  mutate(last_updated = .[[4]] %>% map_chr(~ .x[['modifiedTime']]))


# identify files downloaded the last
read_csv('Input_Data/log.csv') %>%
  # filter(str_detect(sheet_title, 'Crop')) %>%
  group_by(sheet_title) %>%
  filter(downloaded == max(downloaded)) -> gs_last_downloaded


# identifies Google Sheets that have been updated
gs_files %>% 
  select(-ws, -drive_resource) %>% 
  # fraction of seconds makes comparison of update inaccurate
  mutate(last_updated = str_sub(last_updated, 1, -6)) %>%
  mutate(last_updated = ymd_hms(last_updated)) %>%
  full_join(gs_last_downloaded, by = c('name' = 'sheet_title')) %>%
  filter(last_updated != updated) ->
  gs_files_to_update


# identify files to be downloaded
gs_files_to_update %>%
  mutate(FOLDER = case_when(str_detect(name, 'Crop Yield') ~ 'AGR',
                            str_detect(name, 'Soil') ~ 'SOIL',
                            str_detect(name, 'Weather') ~ 'WEATHER',
                            str_detect(name, 'Tile Flow') ~ 'WATER/TILE_FLOW',
                            str_detect(name, 'WQ') ~ 'WATER/WQ',
                            str_detect(name, 'N Load') ~ 'WATER/LOAD',
                            str_detect(name, 'Water Table') ~ 'WATER/WATER_TABLE',
                            TRUE ~ NA_character_)) %>%
  filter(!is.na(FOLDER)) -> gs_files_to_download

# download files from Google Drive
if (nrow(gs_files_to_download) == 0) {
  print('No files to update')
} else {
  print('Here we go')
  walk2(gs_files_to_download$name, 
        gs_files_to_download$FOLDER, 
        ~ DownloadGoogleSheet(.x, .y))
  }
  