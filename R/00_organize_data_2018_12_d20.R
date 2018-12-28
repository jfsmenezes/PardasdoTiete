#' ---
#' title: Organization of Cougar and Jaguar GPS movement data
#' author: Bernardo Niebuhr <bernardo_brandaum@yahoo.com.br>
#' date: CENAP-ICMBio/Pro-Carnivoros, Atibaia, SP, Brazil, Dec. 2018
#' 
#' output:
#' pdf_document:
#'   toc: true
#'   toc_depth: 2
#'   number_sections: true
#' ---

#-----------------------------
# Set up 

# Remove old stuff
rm(list = ls())

# Load packages
if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load('tidyverse', 'lubridate', 'sf')

# Directories

# Data folder
datadir <- '/home/bniebuhr/Documents/Atividades_2018/oncas_on_the_move/_Pardas_Tiete/data/'
# Output folder - the same as the data folder, since we're just re-organizing data
outdir <- datadir

#-----------------------------
# Load data

# Read metada and transform all non-numeric columns in strings
meta.data <- read.table('meta_data.csv', header = T, sep = ';') %>% 
  mutate_if(!sapply(., is.numeric), as.character) %>% 
  mutate_at('release.date', as.POSIXct, tz = 'UTC')
head(meta.data)
str(meta.data)

# Read movement data

# Female Cougars - Pardas do Tiete
# F1 - Sucuri and F2 - Poran
mov.data.puma.f1e2 <- read.table('pardas_tiete_f1e2_Sucuri_Pora_2018_02_d20.csv', header = T, sep = '\t', dec = ',') %>% 
  mutate_if(!sapply(., is.numeric), as.character) %>% 
  mutate(timestamp = as.POSIXct(paste(UTC_Date, UTC_Time), format = '%d/%m/%Y %H:%M', tz = 'UTC'))
  
head(mov.data.puma.f1e2)
ncol(mov.data.puma.f1e2)
str(mov.data.puma.f1e2)

# F3 - Luna - collar not retrieved, in safety mode

# All other individuals

# List files
files <- list.files('.', pattern = '.csv$')
# Only files for these individuals
# ch <- c(paste0('tiete_m', 2:5), paste0('legado_', c('f1', 'm1')))
ch <- meta.data$name %>% iconv(to = 'ASCII//TRANSLIT') %>% 
  .[-c(1:2)]

# Filter
# (files2read <- files[lapply(ch, grep, x = files) %>% unlist])
# files[str_detect(files, str_c(ch, collapse = '|'))]
# files %>% .[str_detect(., str_c(ch, collapse = '|'))]
(files2read <- files %>% 
    magrittr::extract(str_detect(., str_c(ch, collapse = '|'))))

# Read data and create a new column for timestamp
ind.data <- list()
for(i in 1:length(files2read)) {
  print(files2read[i])
  ind.data[[i]] <- read.table(files2read[i], header = T, sep = ',', dec = '.') %>% 
    mutate_if(!sapply(., is.numeric), as.character) %>% 
    mutate(timestamp = as.POSIXct(paste(UTC_Date, UTC_Time), format = '%Y-%m-%d %H:%M', tz = 'UTC'))
}
ind.data

# Check
str(ind.data)
lapply(ind.data, function(x) ncol(x))
lapply(ind.data, function(x) colnames(x))
lapply(ind.data, function(x) head(x))

#-----------------------------
# Organize data

# Select columns

# Columns to be kept
# (cols.to.keep <- ind.data[[1]] %>% 
#    select(c(1:6,11:ncol(.))) %>% 
#    colnames)
cols.to.drop <- c('TempDeg', 'Activity', 'CNR', 'Sats', 'TimeOn.s.', 'MinVolt')

# Gather datasets in a single data.frame and order in time
mov.datasets <- ind.data %>% 
  bind_rows() %>% 
  select(-cols.to.drop) %>%
  dplyr::union(
    mov.data.puma.f1e2 %>% 
      select(-cols.to.drop)
  ) %>%
  dplyr::arrange(ID, timestamp)

# Verify
unique(mov.datasets$Name)

# Add columns with information

# Add columns of information, filter by release date, drop not useful columns
colnames(meta.data)
colnames(mov.datasets)

mov.datasets.all <- mov.datasets %>% 
  left_join(meta.data, by = 'ID') %>%
  filter(timestamp >= release.date) %>%
  select(-c(Name_old, name, Sex, release.date, last.date, observation, 
            UTC_Date, UTC_Time, ends_with('.y'))) %>% 
  rename(Tag_ID = Tag_ID.x, name = Name)

head(mov.datasets.all)
ncol(mov.datasets.all)
str(mov.datasets.all)

# Verify by plot

# all
ggplot(data = mov.datasets.all) +
  geom_path(aes(x = Longitude, y = Latitude, color = name)) +
  coord_quickmap()

ggplot(data = mov.datasets.all) +
  geom_path(aes(x = Longitude, y = Latitude)) +
  facet_wrap(~ name, scales = 'free')

# Problems with individual Mineiro
# Remove outliers

outlier.lines <- mov.datasets.all %>% 
  rownames_to_column() %>% 
  filter(name == 'Mineiro', Longitude < -48.5, Latitude > -22) %>% 
  dplyr::pull(rowname) %>%
  as.numeric

mov.datasets.all <- mov.datasets.all %>% 
  filter(!(row_number() %in% outlier.lines))
  
# Check
ggplot(data = mov.datasets.all) +
  geom_path(aes(x = Longitude, y = Latitude)) +
  facet_wrap(~ name, scales = 'free')

#-------------
# Export data

# Separate studies from Tiete and Legado
studies <- unique(mov.datasets.all$study.name)

# Transform data into spatial
shp <- mov.datasets.all %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) # CRS(+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs)
  
# Include coordinates in SIRGAS2000-UTM 22S
shp.complete <- shp %>% 
  st_transform(crs = 31982) %>% # CRS(+proj=utm +zone=22 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs)
  st_coordinates() %>% 
  cbind(shp) %>%
  rename(x_GRS80_utm22S = X, y_GRS80_utm22S = Y)

# Export data
for(i in studies) {
  to.save <- shp.complete %>% filter(study.name == i)
  
  setwd(outdir)
  st_write(to.save, paste0('movement_data_', i, '_all_individuals_',
                           str_replace_all(lubridate::today(), '-', '_'), '.csv'),
           layer_options = 'GEOMETRY=AS_XY', delete_dsn = TRUE)
  
  setwd(paste0(outdir, '/shape'))
  st_write(to.save, paste0('movement_data_', i, '_all_individuals_',
                           str_replace_all(lubridate::today(), '-', '_'), '.shp'),
           delete_dsn = TRUE)
  
  to.save %>% select(ID, name, timestamp) %>%
    st_write(paste0('movement_data_', i, '_all_individuals_',
                           str_replace_all(lubridate::today(), '-', '_'), '.kml'),
           delete_dsn = TRUE)
}
