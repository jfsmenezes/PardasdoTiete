#' ---
#' title: 'Movement analysis of cougars - Projeto Pardas do Tiete'
#' author: 
#' - Bernardo Niebuhr - CENAP/ICMBio
#' date: Atibaia, SP, Brazil, December 2018
#' 
#' output: github_document
#' pdf_document:
#'   toc: true
#'   toc_depth: 2
#'   number_sections: true
#' ---

# --------------- label=load_packages, warning=FALSE, message=FALSE, echo=FALSE

# Load packages
if(!require(install.load)) install.packages('install.load'); library(install.load)

# For rendering the document
install.load::install_load('ezknitr', 'knitr', 'caTools', 'bitops')

# Print options
options(width = 165)
opts_chunk$set(error = FALSE, message = FALSE, warning = FALSE, cache = FALSE, echo = FALSE, results = TRUE)

#' # Introduction
#' 
#' Let's first load and organize the data, and calculate basic statistics.

# --------------- label=setup
# Set up 

# Clean everything before beginning
rm(list = ls())

# Load packages
install.load::install_load('tidyverse')
install.load::install_load('raster', 'sf', 'rgdal')
install.load::install_load('ggmap', 'ggridges', 'scales') # scales::viridis_pal
install.load::install_load('amt', 'adehabitatLT', 'move', 'ctmm', 'recurse', 'moveHMM',
                           'circular')

# Options

# Should we export figures? 
# (or plot all within R, if FALSE)
exportFIG <- TRUE

# Directories

# Code folder
codedir <- '/home/bniebuhr/Documents/Atividades_2018/oncas_on_the_move/_Pardas_Tiete/code/'
# Data folder
datadir <- '/home/bniebuhr/Documents/Atividades_2018/oncas_on_the_move/_Pardas_Tiete/data/'
# Map folder
mapdir <- '/home/bniebuhr/Documents/Atividades_2018/oncas_on_the_move/_Pardas_Tiete/maps/'
# Output folder
outdir <- '/home/bniebuhr/Documents/Atividades_2018/oncas_on_the_move/_Pardas_Tiete/results/'

# --------------- label=load_data
# Load Puma data

# Change to dir
setwd(datadir)

# Load Puma movement data
# read_csv('movement_data_Pardas_Tiete_all_individuals_2018_12_26.csv') %>% print(width = Inf)
mov.data.puma <- read.table('movement_data_Pardas_Tiete_all_individuals_2018_12_26.csv', header = T, sep = ',') %>% 
  mutate_at('timestamp', as.POSIXct, tz = 'UTC') %>% # timestamp as timestamp
  type_convert() #%>% # check that numeric columns are indeed numeric
  #as_tibble()
  # mutate_at('timestamp', lubridate::ymd_hms) %>%   
  # mutate_at('timestamp', as.POSIXct, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')

mov.data.puma
mov.data.puma %>% print(width = Inf)
str(mov.data.puma)

# Add -3 hours manually to correct for time zone, without introducing summer time
# mov.data.puma$timestamp <- mov.data.puma$timestamp - 3*60*60

# Remove duplicated and arrange in chronological order
dupl <- mov.data.puma %>% 
  dplyr::select(name, timestamp) %>% 
  duplicated
sum(dupl)

mov.data <- mov.data.puma[!dupl,] %>% # remove duplicated
  dplyr::arrange(name, timestamp) # order

# --------------- label=load_map
# Load land use map

# Until now, we're only using maps for defining the projection and datum and plotting

# setwd(mapdir)
map.landuse <- raster(paste0(mapdir, '/mapa_projeto/uso_pardas_utm_5m/uso_pardas_utm_100m.tif'))
# raster::plot(map.landuse)

# Study area
# study.area <- sf::st_read(dsn = paste0(mapdir, '/mapa_projeto/'), layer = 'area_estudo_SIRGAS2000_UTM22S')
study.area <- rgdal::readOGR(dsn = paste0(mapdir, '/mapa_projeto/'), layer = 'area_estudo_SIRGAS2000_UTM22S')
# plot(study.area)

# Load Brasil map
# map.br.states <- sf::st_read(dsn = paste0(mapdir, '/br_unidades_da_federacao/'), layer = 'br_estados_SIRGAS2000_UTM22S')
map.br.states <- rgdal::readOGR(dsn = paste0(mapdir, '/br_unidades_da_federacao/'), layer = 'br_estados_SIRGAS2000_UTM22S')
# sp::plot(map.br.states)

map.br.cities <- sf::st_read(dsn = paste0(mapdir, '/br_municipios/'), layer = 'br_municipios_SIRGAS2000_UTM22S')
map.br.cities <- rgdal::readOGR(dsn = paste0(mapdir, '/br_municipios/'), layer = 'br_municipios_SIRGAS2000_UTM22S')
# sp::plot(map.br.cities)

# --------------- label=transform_data_into_movement_data
# Transform data into movement data

# Before transforming, consider only these individuals
inds <- c('Mineiro', 'Zeus', 'Piloto')
mov.data <- mov.data %>% 
  filter(name %in% inds) %>% 
  mutate_at('name', function (x) as.factor(as.character(x)))
  
# adehabitatLT
# Coordinates
coords <- mov.data[,c(3,4)]
colnames(coords) <- c('x', 'y')
  
# Time
head(mov.data$timestamp)

# ltraj object
mov.traj <- as.ltraj(xy = coords, date = mov.data$timestamp, id = mov.data$name, 
                     burst = mov.data$name, infolocs = mov.data)

# General information
mov.traj
head(mov.traj[[1]])
mov.traj.df <- ld(mov.traj)
# plot(mov.traj)

# amt
# use SIRGAS2000, UTM 22S - EPSG 31982
mov.track <- mk_track(mov.data, .x = x_GRS80_utm22S, .y = y_GRS80_utm22S, .t = timestamp, crs = sp::CRS("+init=epsg:31982"),
                      ID, name, species, sex, dispersive.behavior)

# movement basic statistics
mov.track %>% 
  group_by(name) %>% 
  summarise(
    begin = min(t_),
    end = max(t_),
    range = diff(range(t_)),
    n = n()
  )

mov.track %>% filter(name %in% inds) %>%
  nrow

# move
# move object
move.data <- move(x = mov.data$X, y = mov.data$Y,
                  time = mov.data$timestamp,
                  proj = sp::CRS("+init=epsg:4326"),
                  data = mov.data, animal = mov.data$name, sensor = 'GPS')
move.data

# If we want to change projection
# move.data.meters <- spTransform(move.data, CRSobj = CRS(new.projection))

# If we need to re-transform the move object into data.frame
# animals <- as(move.data, "data.frame")

# --------------- label=visualization, fig.width=15, fig.height=10
# Visualization

#' # Where are the individuals and when were they monitored?
#' 

# Plot - all
move.data
move::plot(move.data, pch = 19, xlab = 'Longitude', ylab = 'Latitude')

# Plot - Google maps
# Add later

# Plot without satellite image
g2 <-
  mov.track %>% 
  ggplot(aes(x = x_, y = y_)) + 
  geom_point(size = 0.5) +
  facet_wrap(~ name, scales = "free", ncol = 2) +
  theme(legend.position="none") +
  labs(x = '', y = '')
g2
ggsave(paste0(outdir, 'tracks.png'), dpi = 600)

# Another plot, dynamic
# leaflet::leaflet(move.data) %>%
#   leaflet::addTiles() %>%
#   leaflet::addCircles(move.data$X, move.data$Y)

# --------------- label=plot_landuse, fig.width=7.5, fig.height=7.5, eval=F
# Plot land use map
par(mar = c(3,3,0,0) + 0.1, oma = c(0,0,1,0))
Mybreaks <- 0:8

Mycols <- c('deepskyblue2', 'tan1', 'forestgreen', 'lightyellow2',
            'darkred', '#e5524a', 'lightskyblue4')
showclasses <- c('Água', 'Cana-de-açúcar', 'Floresta', 'Pasto ou campo', 
                 'Plantio florestal', 'Solo exposto', 'Áreas urbanas') 

raster::plot(map.landuse, breaks = Mybreaks, col = Mycols, legend = F)
# sp::plot(map.br.cities, lwd = 0.4, border = 'grey', add = T)
legend("bottomleft", legend = showclasses, fill = Mycols, cex = 0.9, ncol = 1, bty = "n")
north.arrow(800000, 7650000, 5000, lab = 'N')
scalebar(100000, xy = c(720000, 7680000), type = 'bar', divs = 4, label = c(0, 50, '100 km'))

# Plot with land use map
# Crop map to the study area
boxx <- mov.track %>% filter(name %in% inds) %>% 
  bbox %>% raster::buffer(width = 3000)
map.landuse.local <- crop(map.landuse, boxx)
map.br.cities.local <- crop(map.br.cities, boxx)
# map.br.cities.local <- sf::st_crop(map.br.cities, boxx)

# Plot map
if(exportFIG) png(paste0(outdir, 'study_area_locations.png'), width = 19, height = 16, units = 'cm', res = 500)

plot(0,0,type = 'n', axes = F, xlab = '', ylab = '')

par(fig=c(0.35,0.95,0.1,0.95), mar = c(0,0,0,0) + 0.1, new = T)
# sp::plot(boxx)
plot(0,0,type = 'n', axes = T, xlab = '', ylab = '',
     xlim = extent(boxx)[1:2], ylim = extent(boxx)[3:4])
raster::plot(map.landuse.local, col = Mycols, legend = F, add = T)
# par(fig=c(0.3,0.95,0.05,0.95), mar = c(0,0,0,0) + 0.1, new = T)
# sp::plot(map.br.cities.local, lwd = 0.5, add = T, axes = F)
# how to plot sf with no color?

# Cols for points
cols <- c(1:length(unique(mov.track$name)))
# Plot data over the map
par(fig=c(0.35,0.95,0.1,0.95), mar = c(0,0,0,0) + 0.1, new = T)
cols.p <- cols[as.factor(mov.track$name)]
points(mov.track$x_, mov.track$y_, pch = 20, col = cols.p)

legend('bottomright', legend = unique(mov.track$name), col = cols,
       pch = 19, cex = 1.2)
scalebar(20000, xy = c(778000, 7595000), type = 'bar', divs = 4, label = c(0, 10, '20 km'))

# Plot SP and where the study area is located
par(fig=c(0,0.3,0.5,0.95), mar = c(0,0,0,0) + 0.1, new = T)
sp::plot(map.br.states[map.br.states$NM_ESTADO == 'SÃO PAULO',], lwd = 2)
sp::plot(study.area, lwd = 2, add = T)
sp::plot(boxx, lwd = 2, add = T)

# Land use map legend
par(fig=c(0,0.3,0.1,0.5), mar = c(0,0,0,0) + 0.1, new = T)
plot(0, 0, type = 'n', axes = F, xlab = '', ylab = '')
legend("bottomleft", legend = showclasses, fill = Mycols, cex = 1, ncol = 1, bty = "n")
dev.off()

# --------------- label=monitoring_period, out.width='80%', fig.align='center', fig.asp = 0.9

# Monitoring period

if(exportFIG) png(paste0(outdir, 'monitoring_period.png'), width = 16, height = 16, units = 'cm', res = 500)
par(mar = c(5, 7, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

inds <- unique(as.character(mov.traj.df$id))
plot(mov.traj.df$date, rep(0, length(mov.traj.df$date)),
     ylim = c(1, length(inds)), type = 'n', axes = F,
     xlab = '', ylab = '')
abline(h = 1:length(inds), col = 'grey', lwd = 3, lty = 2)

min.x.year <- format(min(mov.traj.df$date), '%Y')
min.x <- as.POSIXct(paste('1/1/', min.x.year, ' 00:00', sep = ''), format = '%m/%d/%Y %H:%M', tz = 'UTC')
max.x.year <- format(max(mov.traj.df$date), '%Y')
max.x <- as.POSIXct(paste('12/12/', max.x.year, ' 00:00', sep = ''), format = '%m/%d/%Y %H:%M', tz = 'UTC')

# abline(v = seq(min(mov.data.diff$timestamp.posix), max(mov.data.diff$timestamp.posix), 'years'),lwd = 3)
abline(v = seq(min.x, max.x, 'years'), lwd = 3, lty = 2)
# axis.POSIXct(1, at = seq(min.x, max.x, 'years'), format = "%m/%Y")
axis.POSIXct(1, at = seq(min.x, max.x, 'months'), format = "%m/%Y")
# axis.POSIXct(3, at = seq(min.x, max.x, 'years'), format = "%m/%Y")
axis(2, at = 1:length(inds), labels = inds, las = 1)
box()
mtext(2, text = 'Indivíduo', line = 4)
# cols <- rainbow(117)
cols <- cols#rainbow(length(inds))
for(i in 1:length(inds)) {
  ind <- mov.traj.df[mov.traj.df$id == inds[i],]
  points(ind$date, rep(i, length(ind$date)), col = cols[i],
         pch = 20)
}
dev.off()

# --------------- label=movement_parameters_all, fig.width=12, fig.height=6, fig.align='center'

par(mfrow = c(1,2), mar = c(2, 2, 2, 1) + 0.1, oma = c(2,2,0,0))

hist(mov.traj.df$dist/1000, main = '',
     breaks = seq(0, max(mov.traj.df$dist/1000, na.rm = T) + 1000, by = 0.2),
     xlab = '', ylab = '', xlim = c(0, 5))
mtext('Velocidade (km/h)', side = 1, outer = T, at = 0.25, line = 0, cex = 1.5)
mtext('Frequência', side = 2, outer = T, line = 0.1, cex = 1.5,
      at = 0.5)

par(mar = c(0, 0, 0, 0) + 0.1)
circular::rose.diag(mov.traj.df$rel.angle[!is.na(mov.traj.df$rel.angle)], bins=18,
                      prop=2.5, shrink = 1, units = 'degrees', control.circle = circle.control(lwd = 0),
                      main = '')
mtext('Ângulo de virada', side = 1, outer = T, line = 0, cex = 1.5, at = 0.75)
dev.off()

# Number of observations with approx 1h between them
dt.1 <- mov.traj.df$dt/3600
sum(dt.1 < 1.01 | dt.1 < 0.99, na.rm = T) # N observations
sum(dt.1 < 1.01 | dt.1 < 0.99, na.rm = T)/length(dt.1) # Proportion

sum(dt.1 < 2 | dt.1 < 0, na.rm = T) # N observations
sum(dt.1 > 2, na.rm = T) # N observations
sum(dt.1 < 2 | dt.1 < 00, na.rm = T)/length(dt.1) # Proportion

# Average distance
mean(mov.traj.df$dist/1000, na.rm = T)

# Percentage observation distance < 2 km
sum(mov.traj.df$dist/1000 > 2, na.rm = T)
sum(mov.traj.df$dist/1000 < 2, na.rm = T)/nrow(mov.traj.df)

# Percentiles
quantile(mov.traj.df$dist/1000, na.rm = T, probs = c(0.5, 0.7, 0.9, 0.95, 0.99))

dd <- mov.traj.df %>% filter(dispersive.behavior == 'dispersive')
quantile(dd$dist/1000, na.rm = T, probs = c(0.5, 0.7, 0.9, 0.95, 0.99))
nd <- mov.traj.df %>% filter(dispersive.behavior != 'dispersive')
quantile(nd$dist/1000, na.rm = T, probs = c(0.5, 0.7, 0.9, 0.95, 0.99))

# Daily distance
mov.traj.df$julian <- round(julian(mov.traj.df$date), 0)

mov.per.day <- mov.traj.df %>% 
  group_by(id, julian) %>%
  summarise(
    daily_distance = sum(dist, na.rm = T),
    mean_angle = mean(rel.angle, na.rm = T)
  ) %>%
  as.data.frame

hist(mov.per.day$daily_distance, main = '')

ggplot(data = mov.per.day, aes(x = daily_distance, fill = id)) +
  geom_density(alpha = 0.4)
ggplot(data = mov.per.day, aes(x = mean_angle, fill = id)) +
  geom_density(alpha = 0.4)

g1 <- ggplot(data = mov.per.day, aes(x = daily_distance/1000, y = id, fill = id)) +
  geom_density_ridges(alpha = 0.5) +
  theme_minimal() +
  labs(x = 'Distância diária (km)', y = '') +
  theme(legend.position = "none") +
  scale_fill_manual(values=c(1,3,2))
g1
ggsave(filename = paste0(outdir, 'dist_diaria.png'), plot = g1, device = 'png', 
       width = 10, height = 10, units = 'cm', dpi = 300)

g2 <- ggplot(data = mov.per.day, aes(x = mean_angle/pi*180, y = id, fill = id)) +
  geom_density_ridges(alpha = 0.5) +
  theme_minimal() +
  labs(x = 'Ângulo médio diário', y = '') +
  theme(legend.position = "none")
g2

par(mfrow = c(2,2))
inds <- unique(mov.traj.df$id)
for(i in 1:length(inds)) {
  dat <- mov.per.day %>% filter(id == inds[i])
  plot(dat$julian, dat$daily_distance/1000, type = 'l', 
       ylab = 'Daily distance (km)', xlab = 'Date', main = inds[i])
}
par(mfrow = c(1,1))

# Percentiles
quantile(mov.per.day$daily_distance/1000, na.rm = T, probs = c(0.5, 0.7, 0.9, 0.95, 0.99))
mean(mov.per.day$daily_distance/1000, na.rm = T)
max(mov.per.day$daily_distance/1000, na.rm = T)
sort(mov.per.day$daily_distance/1000, decreasing = T)[1:5]

mov.per.day %>% group_by(id) %>% 
  summarise(
    mean.dist = mean(daily_distance/1000, na.rm = TRUE),
    median.dist = median(daily_distance/1000, na.rm = TRUE),
    q90.dist = quantile(daily_distance/1000, na.rm = TRUE, probs = 0.9),
    max.dist = max(daily_distance/1000, na.rm = TRUE)
  )

# Distance traveled vs. Time between relocations
# plot(mov.traj.df$dt/3600, mov.traj.df$dist/1000,
#      xlab = 'Time between observations (h)', ylab = "Displacement (km)",
#      xlim = c(0, 50), ylim = c(0, 20))

#' ## 1) Verifying the existence of dispersal

# --------------- label=variograms, fig.width=13, fig.height=10

# Analyzing variograms

# First, resample data with 1h (removing small intervals) and transform data into 
# a move object again
mov.track.rsp <- track_resample(mov.track, rate = hours(1), tolerance = minutes(5)) %>%
  filter_min_n_burst(min_n = 3)

mov.track %>% summarize_sampling_rate(summarize = FALSE) %>% hist
mov.track %>% summarize_sampling_rate(summarize = TRUE)

mov.track.rsp %>% summarize_sampling_rate(summarize = FALSE) %>% hist
mov.track.rsp %>% summarize_sampling_rate(summarize = TRUE) # how to consider only within bursts?

# Intrusao demoniaca, nao esta funcionando!
# Verificar!
# move.data <- mov.track.rsp %>%
#   as.data.frame %>%
#   move(x = .$x_, y = .$y_,
#        time = .$t_,
#        proj = sp::CRS("+init=epsg:31982"),
#        animal = .$name,
#        sensor = 'GPS')

ll <- list()
for(i in levels(mov.track.rsp$name)) {
  ll[[i]] <- move.data <- mov.track.rsp %>%
    as.data.frame %>% 
    filter(name == i) %>% 
    move(x = .$x_, y = .$y_,
         time = .$t_,
         proj = attr(., 'crs_'),
         # proj = sp::CRS("+init=epsg:31982"),
         animal = .$name,
         sensor = 'GPS')
}
ll
move.data <- moveStack(ll)
move.data@idData

# Prepare data in the ctmm format
mov.tel <- as.telemetry(move.data, projection = move.data@proj4string)
str(mov.tel)
head(mov.tel)

# Plot
plot(mov.tel, col = 1:length(mov.tel))

# Run variograms
if(exportFIG) png(paste0(outdir, 'svf.png'), width = 12, height = 12, units = 'cm', res = 300)
lay <- rbind(c(1,2),
             c(3,4))
layout(lay)
par(mar = c(2, 2, 2, 1) + 0.1, oma = c(2,2,0,0))

SVF <- list()
for(i in 1:length(mov.tel)) {
  if(length(mov.tel) == 1) {
    ind <- mov.tel
  } else {
    ind <- mov.tel[[i]]
  }
  SVF[[i]] <- variogram(ind)
  level <- c(0.5,0.95) # 50% and 95% CIs

  main.title <- names(mov.tel[i])
  xlim <- c(0, abs(difftime(ind$timestamp[1], ind$timestamp[length(ind$timestamp)], units = 'days')/30))
  plot(SVF[[i]], fraction = 0.85, level=level, axes = T)
  title(main.title)
}

mtext('Tempo entre observações (meses)', side = 1, outer = T, line = 0.5, cex = 1.2)
mtext(expression(Semivariância (km^2)), side = 2, outer = T, line = -0.5, cex = 1.2)
dev.off()

# Fit models
FITS.all <- list()
AKDE.all <- list()

mov.tel.res <- list(mov.tel[['Zeus']])
for(i in 1:length(mov.tel.res)) {
  print(paste('individual', i, sep = ' = '))
  
  ind <- mov.tel.res[[i]]
  SVF <- variogram(ind)
  
  level <- c(0.5, 0.95) # 50% and 95% CIs
  xlim <- c(0, 12 %#% "hour") # 0-12 hour window
  
  m.ouf <- ctmm.guess(ind, interactive=FALSE) # automated model guess
  FITS <- ctmm.select(ind, m.ouf, verbose=TRUE, level=1)
  FITS.all[[i]] <- FITS
  
  par(mfrow = c(2,2))
  plot(SVF, CTMM = FITS[[1]], xlim=xlim, level=level)
  title(ind@info$identity)
  plot(SVF, CTMM = FITS[[1]], fraction = 0.75, level=level)
  title(names(FITS[1]))
  
  HR <- akde(ind, CTMM = FITS[[1]])
  plot(ind, UD = HR)
  #title(rownames(summary(FITS))[1])
  AKDE.all[[i]] <- HR
  
  # krig <- occurrence(ind, CTMM = FITS[[1]])
  # plot(krig)
}

# Export figure with SVF, model, and homerange
if(exportFIG) png(paste0(outdir, 'variograms_fit.png'), width = 15, height = 15, units = 'cm', res = 600)

par(mfrow = c(2,2), mar = c(2, 2, 2, 1) + 0.1, oma = c(2,2,0,0))
for(i in 1:length(mov.tel.res)) {
  print(paste('individual', i, sep = ' = '))
  
  ind <- mov.tel.res[[i]]
  SVF <- variogram(ind)
  
  level <- c(0.5, 0.95) # 50% and 95% CIs
  xlim <- c(0, 12 %#% "hour") # 0-12 hour window
  
  par(mfrow = c(2,2))
  plot(SVF, CTMM = FITS.all[[i]][[1]], xlim=xlim, level=level)
  title(ind@info$identity)
  plot(SVF, CTMM = FITS.all[[i]][[1]], fraction = 0.75, level=level,
       xaxt = 'n', yaxt = 'n')
  axis(1); axis(2)

  plot(ind, UD = AKDE.all[[i]])
  # title(rownames(summary(FITS.all[[i]]))[1])
  
  # krig <- occurrence(ind, CTMM = FITS[[1]])
  # plot(krig)
}
mtext(expression(Semivariância (km^2)), side = 2, line = 0, outer = T, at = 0.75)
mtext('Tempo entre observações (dias)', side = 1, line = -16.5, outer = T, at = 0.25)
mtext('Tempo entre observações (meses)', side = 1, line = -16.5, outer = T, at = 0.75)

if(exportFIG) dev.off()

# Values
summary(FITS.all[[1]][[1]])
summary(AKDE.all[[1]])

#' Revisitation analysis

# --------------- label=chronological_plot, fig.width=13, fig.height=10

if(exportFIG) png(paste0(outdir, 'chronological.png'), width = 11, height = 11, units = 'cm', res = 600)

lay <- rbind(c(1,2),
             c(3,4))
layout(lay)
par(mar = c(2, 2, 2, 1) + 0.1, oma = c(0,0,0,0))

for(i in move.data@idData$name) {
  print(i)
  mov.ind <- move.data[[i]]

  plot(mov.ind$x_, mov.ind$y_, col = viridis_pal()(nrow(mov.ind)), pch = 20, 
       xlab = "x", ylab = "y", asp = 1, main = i)
  xx <- par('xaxp')[1:2]
  yy <- par('yaxp')[1:2]
  
  if(i == length(mov.traj)) {
    gradient.rect(xleft = min(xx) + 7*diff(xx)/10,
                  xright = min(xx) + 10*diff(xx)/10,
                  ybottom = min(yy) + 9*diff(yy)/10,
                  ytop = min(yy) + 10*diff(yy)/10,
                  col = viridis_pal()(nrow(mov.ind.df)),
                  border = F)
  }
}
dev.off()

# --------------- label=revisitation_analysis, fig.width=13, fig.height=10

# Revisitation analysis
# par(mfrow = c(3,2), mar = c(2, 2, 2, 1) + 0.1, oma = c(1,2,0,0))
if(exportFIG) png(paste0(outdir, 'revisit.png'), width = 11, height = 11, units = 'cm', res = 600)

# average daily distance
avg.dd <- mov.per.day %>% 
  summarise(mean.dist = mean(daily_distance, na.rm = TRUE)) %>% 
  pull(mean.dist)

visits <- list()
radius <- avg.dd # average daily distance

lay <- rbind(c(1,2),
             c(3,4))
layout(lay)
par(mar = c(2, 2, 2, 1) + 0.1, oma = c(2,2,0,0))

for(i in move.data@idData$name) {
  if(length(move.data@idData$name) == 1) {
    mov.ind <- move.data
  } else {
    mov.ind <- move.data[[i]]
  }
  visits[[i]] <- getRecursions(mov.ind, radius = radius)

  plot(visits[[i]], mov.ind, type = 'n', axes = T)
  xx <- par('xaxp')[1:2]
  yy <- par('yaxp')[1:2]
  par(new = T)
  main.title <- i
  plot(visits[[i]], mov.ind,
       pch = 20,
       main = main.title)
}
dev.off()

