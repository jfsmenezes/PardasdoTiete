#------------------------------------------------------------------------
#
# Load and organize data from two female pumas - Tiete
# Exploratory movement analysis
#
# Bernardo Niebuhr - bernardo_brandaum@yahoo.com.br
#
# March 2018
# GNU/GPLv2 license
#------------------------------------------------------------------------

#-----------------------------
# Set up 

# Load packages
if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load('raster', 'rgdal', 'adehabitatLT', 'GISTools', 'zoo', 
  'circular')

# Options

exportFIG <- TRUE

# Directories

# Code folder
codedir <- '/home/leecb/Documentos/Atividades_2018/oncas_on_the_move/oncas_tiete/code'
# Data folder
datadir <- '/home/leecb/Documentos/Atividades_2018/oncas_on_the_move/oncas_tiete/data'
# Map folder
mapdir <- '/home/leecb/Documentos/Atividades_2018/oncas_on_the_move/oncas_tiete/maps'
# Output folder
outdir <- '/home/leecb/Documentos/Atividades_2018/oncas_on_the_move/oncas_tiete/results'

# Load plot google function
setwd(codedir)
source('plot_google_source_code.r')

#-----------------------------
# Load data

# Change to dir
setwd(datadir)

# Load movement data
mov.data <- read.table('jaguar_locations_f1e2_2018_02_d20.csv', header = T,
  sep = '\t', dec = ',')
head(mov.data)

# Load land use map
setwd(mapdir)
map.fbds <- raster('FBDS_map_all_states_compressed.tif')
plot(map.fbds)

# Load Brasil map
map.br <- readOGR('.', layer = 'Brasil_estados_albers_sad69')
plot(map.br)

#-----------------------------
# Organize data

# Replace missing coordinates per 0
# (a position that obviously in not in Brazil)
mov.data$Latitude[is.na(mov.data$Latitude)] <- 0
mov.data$Longitude[is.na(mov.data$Longitude)] <- 0

# Transform data into a SpatialPointsDataFrame object
mov.spdata <- SpatialPointsDataFrame(coords = mov.data[,5:4],
                                     data = mov.data[,c(1:3,6:ncol(mov.data))], 
                                     proj4string = CRS('+proj=longlat +ellps=WGS84 
                                       +datum=WGS84 +no_defs'))

# Reproject to Albers, Projection SAD69
new.projection <- crs(map.fbds)@projargs
mov.spdata.albers <- spTransform(mov.spdata, CRS(new.projection))

#-----------------------------
# Visualization

#------------
# Plot Study area
if(exportFIG) {
  setwd(outdir)
  png('mapa_fbds.png', width = 15, height = 15, units = 'cm', res = 300)
}
par(mar = c(3,3,0,0) + 0.1)
Mybreaks <- 0:6
Mycols <- c('deepskyblue2', 'lightyellow2', 'lightskyblue4', 'forestgreen',
            'darkorange2', 'darkred')

showclasses <- c('Água', 'Áreas antrópicas', 'Áreas edificadas', 'Floresta', 
                 'Áreas naturais não florestais', 'Silvicultura') 
  
plot(map.fbds, breaks = Mybreaks, col = Mycols, legend = F)
plot(map.br, add = T)
legend("bottomright", legend = showclasses, fill = Mycols, cex = 0.9, ncol = 1,
  bty = "n")
north.arrow(2500000, 1800000, 50000, lab = 'N')
scalebar(1000000, xy = c(-50000, 2800000), type = 'bar', divs = 4,
  label = c(0, 500, '1000 km'))
graphics::text(coordinates(mov.spdata.albers)[1,1],
  coordinates(mov.spdata.albers)[1,2], labels = '*', cex = 3)
if(exportFIG) dev.off() 

#------------
# Plot map and locations

# Missing data = maximum x and y values
max.x <- max(mov.spdata.albers@bbox[1,])
max.y <- max(mov.spdata.albers@bbox[2,])

# Removing this data to visualize
mov.spdata.albers.vis <- mov.spdata.albers[coordinates(mov.spdata.albers)[,1] < max.x,]

# Crop map to the study area
boxx <- mov.spdata.albers.vis@bbox
boxx[,1] <- boxx[,1] - 7000
boxx[,2] <- boxx[,2] + 3000
ext <- extent(boxx)
map.fbds.local <- crop(map.fbds, ext)

# Plot map
if(exportFIG) {
  setwd(outdir)
  png('study_area_locations.png', width = 15, height = 15, units = 'cm', res = 300)
}
# plot(mov.spdata.albers.vis@bbox[1,], mov.spdata.albers.vis@bbox[2,], type = 'n',
#      axes = F, xlab = '', ylab = '')
plot(0,0,type = 'n', axes = F, xlab = '', ylab = '')
par(fig=c(0.4,0.95,0.05,0.95), mar = c(0,0,0,0) + 0.1, new = T)
plot(0,0,type = 'n', axes = F, xlab = '', ylab = '',
     xlim = boxx[1,], ylim = boxx[2,])
plot(map.fbds.local, col = Mycols, legend = F, add = T)

par(fig=c(0.4,0.95,0.05,0.95), mar = c(0,0,0,0) + 0.1, new = T)
legend('bottomright', legend = levels(mov.spdata.albers.vis$Name), col = c('yellow', 'red'),
       pch = 19)

scalebar(10000, xy = c(988000, 1180000), type = 'bar', divs = 4, label = c(0, 5, '10 km'))

# Plot data over the map
cols = c(rgb(1, 1, 0, alpha = 0.3), rgb(1, 0, 0, alpha = 0.3))[as.factor(mov.spdata.albers.vis$ID)]
plot(mov.spdata.albers.vis, pch = 20,
     col = cols, add = T)

# Plot SP and where the study area is located
par(fig=c(0,0.4,0.5,0.95), mar = c(0,0,0,0) + 0.1, new = T)
plot(map.br[map.br$UF == 'SP',], lwd = 2)
rect(boxx[1,1], boxx[2,1], boxx[1,2], boxx[2,2], col = 2, lwd = 2)

# Land use map legend
par(fig=c(0,0.4,0.1,0.5), mar = c(0,0,0,0) + 0.1, new = T)
plot(0, 0, type = 'n', axes = F, xlab = '', ylab = '')
legend("bottomleft", legend = showclasses, fill = Mycols, cex = 0.9, ncol = 1, bty = "n")

if(exportFIG) dev.off()

# Plot locations with google maps
if(exportFIG) {
  setwd(outdir)
  png('study_area_locations_google.png', width = 15, height = 15, units = 'cm', res = 300)
}

cols = c('yellow', 'red')[as.factor(mov.spdata.albers.vis$ID)]
plot.google(mov.spdata.albers.vis, cex = 0.6, pcol = cols, lwd = 1)
scalebar(10000, xy = c(980000, 1190000), type = 'bar', divs = 4, label = c(0, 5, '10 km'))

if(exportFIG) dev.off()

#------------
# Proportion of locations vs land use
mov.spdata.albers.vis$land_use <- extract(map.fbds, mov.spdata.albers.vis)
mov.spdata.albers.vis$land_use <- factor(mov.spdata.albers.vis$land_use, 
  levels = 1:4,
  labels = c('water', 'antropic', 'urban', 'forest'))

table(mov.spdata.albers.vis$land_use)
table(mov.spdata.albers.vis$land_use)/nrow(mov.spdata.albers.vis)

#-----------------------------
# Transform data into movement data

# Coordinates
coords <- data.frame(coordinates(mov.spdata.albers))
coords[coords$Longitude == max.x,] <- c(NA, NA)

# Time
time <- paste(mov.spdata.albers$UTC_Date, mov.spdata.albers$UTC_Time)
time.posix <- as.POSIXct(time, "%d/%m/%Y %H:%M:%S", tz = 'GMT')

mov.traj <- as.ltraj(xy = coords, date=time.posix, id=mov.spdata.albers$Name, 
                     burst=mov.spdata.albers$ID, infolocs = mov.spdata.albers@data)

# General information
mov.traj
head(mov.traj[[1]])
mov.traj.df <- ld(mov.traj)

# Range of records, in days
for(i in 1:length(mov.traj)) {
  print(
    paste(
      attr(mov.traj[[i]], 'id'), ': ', 
      max(mov.traj[[i]]$date) - min(mov.traj[[i]]$date), ' days', sep = ''
    )
  )
}

# Regularity of monitoring
if(exportFIG) {
  setwd(outdir)
  png('period_monitoring.png', width = 15, height = 15, units = 'cm', res = 300)
}
plot(mov.traj.df$date, rep(0, nrow(mov.traj.df)), type = 'n', ylim = c(0.5, 2.5), 
  #xlim = as.POSIXct(c('1/1/2015', '1/8/2016'), "%d/%m/%Y", tz = 'GMT'),
  xlab = 'Data', ylab = 'Indivíduo', axes = F, 
  main = 'Período de monitoramento')

axis.POSIXct(1, at = seq(min(mov.traj.df$date), max(mov.traj.df$date), 'months'),
  format = "%m/%Y", tick = T)

axis(2, at = c(1, 2), labels = levels(mov.traj.df$Name), las = 2)
box()
grid(nx = NULL, ny = NA)
points(mov.traj[[1]]$date, rep(1, nrow(mov.traj[[1]])), col = 'yellow')
points(mov.traj[[2]]$date, rep(2, nrow(mov.traj[[2]])), col = 2)

if(exportFIG) dev.off()

# Explore regularity in fix rate
if(exportFIG) {
  setwd(outdir)
  png('sampling_rate.png', width = 15, height = 15, units = 'cm', res = 300)
}

op <- par(mfrow = c(2,1), mar = c(2, 3, 2, 1) + 0.1, oma = c(3, 3, 0, 0))
hist(mov.traj[[1]]$dt/3600, xlab = '', ylab = '', main = attr(mov.traj[[1]], 'id'))
hist(mov.traj[[2]]$dt/3600, xlab = '', ylab = '', main = attr(mov.traj[[1]], 'id'))
mtext('Número de registros', 2, outer = T, cex = 1.5)
mtext('Tempo entre registros (h)', 1, outer = T, line = 1, cex = 1.5)
par(op)

if(exportFIG) dev.off()

# Number of observations with approx 1h between them
dt.1 <- mov.traj.df$dt/3600
sum(dt.1 < 1.01 | dt.1 < 0.99, na.rm = T) # N observations
sum(dt.1 < 1.01 | dt.1 < 0.99, na.rm = T)/length(dt.1) # Proportion

sum(dt.1 < 2 | dt.1 < 0, na.rm = T) # N observations
sum(dt.1 > 2, na.rm = T) # N observations
sum(dt.1 < 2 | dt.1 < 00, na.rm = T)/length(dt.1) # Proportion

# Plot trajectories
op <- par(mfrow = c(1,2), mar = c(4, 3, 3, 1) + 0.1)
plot(mov.traj[1], xlab = '', ylab = '', main = attr(mov.traj[[1]], 'id'))
plot(mov.traj[2], xlab = '', ylab = '', main = attr(mov.traj[[2]], 'id'))
par(op)

#-----------------------------
# Regularize data

#----------
# Function to cut bursts with gaps larger than 12 hours
# the warning message is because there are isolated relocations 
# within bursts that are not enough to make for a trajectory 
# (needs at least 3 relocation points)
foo <- function(dt) {
  return(dt > (60*60*4))
}
mov.traj.2 <- cutltraj(mov.traj, "foo(dt)", nextr = TRUE)

if(exportFIG) {
  setwd(outdir)
  png('break_bursts_4h.png', width = 20, height = 15, units = 'cm', res = 300)
}

par(mfrow = c(2,3), mar = c(3, 2, 2, 1) + 0.1, oma = c(0,3,0,0))
for(i in 1:length(mov.traj)) {
  plot(mov.traj[i], xlab = '', ylab = '', 
    main = paste(
      attr(mov.traj[[i]], 'id'),': trajetória ', attr(mov.traj[[i]], 'burst'), 
      sep = '')
    )
  
  if(i == 1) mtext('Trajetórias originais', side = 2, cex = 1.2, line = 2.5)
}
plot(0, 0, type = 'n', axes = F)

for(i in 1:length(mov.traj.2)) {
  plot(mov.traj.2[i], xlab = '', ylab = '', 
    main = paste(
      attr(mov.traj.2[[i]], 'id'), ': trajetória ', attr(mov.traj.2[[i]], 'burst'), 
                    sep = ''
    )
  )
  if(i == 1) mtext('Trajetórias separadas (12h)', side = 2, cex = 1.2, line = 2.5)
}

if(exportFIG) dev.off()

#----------
# Exercise: filter data each 4 hours and make them regular
plotltr(mov.traj[1:2], "dt/3600")
refda <- strptime("00:00", "%H:%M")
# place missing values in gaps (traj not regular yet)
mov.traj.3 <- setNA(mov.traj, refda, 1, units = "h")
plotltr(mov.traj.3[1:2], "dt/3600")
mov.traj.3[[1]]
#regularizing trajectories for real
mov.traj.3.reg <- sett0(mov.traj.3, refda, 1, units = "h")
plotltr(mov.traj.3.reg[1:2], "dt/3600")

plot(mov.traj)
plot(mov.traj.3.reg)

# Number of observations with approx 1h between them
dt.3 <- ld(mov.traj.3)$dt/3600
sum(dt.3 < 1.01 | dt.3 < 0.99, na.rm = T) # N observations
sum(dt.3 < 1.01 | dt.3 < 0.99, na.rm = T)/length(dt.3) # Proportion

# Plot fix rate before and after regularization
if(exportFIG) {
  setwd(outdir)
  png('regularization_process_1h.png', width = 15, height = 15, units = 'cm',
    res = 300)
}

par(mfrow = c(3,2), mar = c(3, 2, 2, 1) + 0.1, oma = c(3,3,0,0))
for(i in 1:length(mov.traj)) {
  plot(mov.traj[[i]]$date, mov.traj[[i]]$dt/3600, type = 'l', 
       xlab = '', ylab = '', main = attr(mov.traj[[i]], 'id'))
  points(mov.traj[[i]]$date, mov.traj[[i]]$dt/3600, pch = 20, cex = .7)
}

for(i in 1:length(mov.traj.3)) {
  plot(mov.traj.3[[i]]$date, mov.traj.3[[i]]$dt/3600, type = 'l', 
       xlab = '', ylab = '', main = attr(mov.traj.3[[i]], 'id'))
  points(mov.traj.3[[i]]$date, mov.traj.3[[i]]$dt/3600, pch = 20, cex = .7)
}

for(i in 1:length(mov.traj.3.reg)) {
  plot(mov.traj.3.reg[[i]]$date, mov.traj.3.reg[[i]]$dt/3600, type = 'l', 
       xlab = '', ylab = '', main = attr(mov.traj.3.reg[[i]], 'id'))
  points(mov.traj.3.reg[[i]]$date, mov.traj.3.reg[[i]]$dt/3600, pch = 20, cex = .7)
}

mtext('Data', 1, line = 1.5, outer = T, cex = 1.5)
mtext('Tempo entre localizações (h)', 2, line = 1, outer = T, cex = 1.5)

if(exportFIG) dev.off()

#=-------
# Interpolations must be done burst by burst (a loop)
# otherwise whack positions are be generated due to 
# possible missing points between bursts 
# (i.e. end of burst is interpolated to beginning of next) 
mov.traj.3.reg.df <- ld(mov.traj.3.reg) #in order to count bursts (gotta figure out a better way)
head(mov.traj.3.reg.df)
mov.traj.3.reg.fix <- NULL
for (i in 1:length(levels(mov.traj.3.reg.df$burst))) {
  mov.traj.3.fill <- transform(mov.traj.3.reg[[i]], 
                               xfill = na.approx(x, date, na.rm=FALSE),
                               yfill = na.approx(y, date, na.rm=FALSE))
  mov.traj.3.reg.fix <- rbind(mov.traj.3.reg.fix, mov.traj.3.fill)
}

nrow(mov.traj.df)
nrow(mov.traj.3.reg.fix)

# original coordinates
orig.xy <- mov.traj.3.reg.fix[,c(1,2)]
colnames(orig.xy) <- c("orig.x", "orig.y")

#==================================
# NOTE: figure out a more elegant way to incorporate rec function? 
# Recalculating Ltraj
mov.traj.fixed <- as.ltraj(xy = mov.traj.3.reg.fix[,c("xfill","yfill")], 
  date=mov.traj.3.reg.fix$date, 
  id=mov.traj.3.reg.df$id, burst = mov.traj.3.reg.df$burst, typeII = T,
  infolocs = cbind(mov.traj.3.reg.df[,13:ncol(mov.traj.3.reg.df)], orig.xy)
)


plot(mov.traj)
plot(mov.traj.fixed)

mov.traj.fixed.df <- ld(mov.traj.fixed)

if(exportFIG) {
  setwd(outdir)
  png('bursts_regular_1h.png', width = 15, height = 15, units = 'cm', res = 300)
}

par(mfrow = c(2,2), mar = c(3, 2, 2, 1) + 0.1, oma = c(0,3,0,0))
for(i in 1:length(mov.traj)) {
  plot(mov.traj[i], xlab = '', ylab = '', 
       main = paste(attr(mov.traj[[i]], 'id')))
  if(i == 1) mtext('Trajetórias originais', side = 2, cex = 1.2, line = 2.5)
}

for(i in 1:length(mov.traj.fixed)) {
  plot(mov.traj.fixed[i], xlab = '', ylab = '', 
       main = paste(attr(mov.traj.fixed[[i]], 'id')))
  if(i == 1) mtext('Trajetórias regularizadas (1h)', side = 2, cex = 1.2, line = 2.5)
}

if(exportFIG) dev.off()

#----------
# Movement parameters for original data
if(exportFIG) {
  setwd(outdir)
  png('movement_patterns.png', width = 15, height = 15, units = 'cm', res = 300)
}

par(mfrow = c(3,2), mar = c(3, 2, 2, 1) + 0.1, oma = c(1,3,0,0))
for(i in 1:length(mov.traj)) {
  plot(mov.traj[[i]]$date, sqrt(mov.traj[[i]]$R2n)/1000, type = 'l',
       xlab = '', ylab = '', main = attr(mov.traj[[i]], 'id'))
  points(mov.traj[[i]]$date, sqrt(mov.traj[[i]]$R2n)/1000, pch = 20, cex = .7)
}
mtext('Data', side = 1, outer = T, line = -29, cex = 1.2, at = 0.27)
mtext('Data', side = 1, outer = T, line = -29, cex = 1.2, at = 0.77)
mtext('Distância em relação\n ao ponto inicial (km)', side = 2, outer = T, 
  line = 0.2, cex = 0.9, at = 0.85)

for(i in 1:length(mov.traj)) {
  hist(mov.traj[[i]]$dist/1000, main = '', breaks = 20, xlab = '', ylab = '')
}

mtext('Deslocamento (km)', side = 1, outer = T, line = -15, cex = 1.2, at = 0.27)
mtext('Deslocamento (km)', side = 1, outer = T, line = -15, cex = 1.2, at = 0.77)
mtext('Frequência', side = 2, outer = T, line = 0.2, cex = 1.2, 
      at = 0.5)

for(i in 1:length(mov.traj)) {
  circular::rose.diag(mov.traj[[i]]$rel.angle[!is.na(mov.traj[[i]]$rel.angle)],
    bins=18, prop=2.5, shrink = 1, units = 'degrees', 
    control.circle = circle.control(lwd = 0), main = "")
}
# for(i in 1:length(mov.traj)) {
#   hist(mov.traj[[i]]$rel.angle)
# }
mtext('Ângulos de virada', side = 1, outer = T, line = -1, cex = 1.2)

if(exportFIG) dev.off()

#-----------------------------
# Filter data?