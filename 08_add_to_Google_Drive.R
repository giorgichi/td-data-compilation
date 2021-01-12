# Load the RSQLite Library
library(tidyverse)
library(googledrive)


# Read site ids
siteids <- read_csv('Ag_Commons_Data/siteids.csv')

# Generate names for Google Drive folders
siteids %>%
  mutate(photos = paste(ID, 'photos', sep  = '_'),
         maps = paste(ID, 'maps', sep  = '_')) %>%
  select(photos, maps) %>%
  gather(key, name) %>%
  pull(name) -> google_drive_folder_names


# Creat folders 
walk(.x = google_drive_folder_names,
     .f = ~ drive_mkdir(name = .x,
                        path = as_id('1MA6spcXyu_TeyZYkUSizks9fuQTSLC7m')))


# Read URLs of Google Folders
read_csv('Input_Data/drive.txt') %>%
  gather(Folder, URL, 2:3) -> drive_URLs


walk(.x  = drive_URLs$URL,
    .f =  ~ drive_share(as_id(.x), 
                        role = 'owner', 
                        type = 'user', 
                        transferOwnership = TRUE,
                        emailAddress = 'transformingdrainage@gmail.com'))


