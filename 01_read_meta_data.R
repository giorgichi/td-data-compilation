# Initialize functions 
source('00_project_settings.R')



# DOWNLOAD ................................................................

# Master Key
DownloadGoogleSheet('TD Site Keys', FOLDER = 'Metadata')



# READ ....................................................................

# Read Site Key
read_excel('Input_Data/Metadata/TD Site Keys.xlsx',
           sheet = 'SITES',
           skip = 1) -> meta_key_site



