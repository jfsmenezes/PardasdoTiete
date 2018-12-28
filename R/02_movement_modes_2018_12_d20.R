#' ---
#' title: 'Movement analysis of cougars - HMM - Projeto Pardas do Tiete'
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
install.load::install_load('amt', 'move', 'moveHMM', 'circular')

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

# --------------- label=transform_data_into_movement_data
# Transform data into movement data

# Before transforming, consider only these individuals
inds <- c('Mineiro', 'Zeus', 'Piloto')
mov.data <- mov.data %>% 
  filter(name %in% inds) %>% 
  mutate_at('name', function (x) as.factor(as.character(x)))

# amt
# use SIRGAS2000, UTM 22S - EPSG 31982
mov.track <- mk_track(mov.data, .x = x_GRS80_utm22S, .y = y_GRS80_utm22S, .t = timestamp, 
                      crs = sp::CRS("+init=epsg:31982"),
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

mov.track %>% 
  nrow

# simple plot
# Plot without satellite image
g2 <-
  mov.track %>% 
  ggplot(aes(x = x_, y = y_)) + 
  geom_point(size = 0.5) +
  facet_wrap(~ name, scales = "free", ncol = 2) +
  theme(legend.position="none") +
  labs(x = '', y = '')
g2

# --------------- label=regularize data, fig.width=13, fig.height=10

# First, resample data with 1h (removing small intervals)
mov.track.rsp <- mov.track %>% 
  track_resample(rate = hours(24), tolerance = hours(1)) %>%
  filter_min_n_burst(min_n = 1)
# This generates problems - it does not recognize different individuals

mov.track.rsp <- mov.track %>% 
  nest(-name) %>% 
  mutate(data.rsp = map(data, track_resample, rate = hours(24), tolerance = hours(12))) %>% 
  unnest(data.rsp)
mov.track.rsp  
unique(mov.track.rsp$burst_) # only one burst per individual
  
# --------------- label=hmm
# Hidden Markov Models

# Assumption - regular data - ok

# how to do that with different bursts?

# We want distances in km, not m
mov.track.rsp <- mov.track.rsp %>% 
  mutate(
    x = x_/1000,
    y = y_/1000,
    ID = name
  )

hmm.data <- mov.track.rsp %>% 
  #filter(ID == 'Mineiro') %>% 
  as.data.frame %>% 
  prepData(type='UTM', coordNames = c('x', 'y'))
head(hmm.data)

# Ploting data and basic movement data statistics
plot(hmm.data)

#--- 
#  Fitting the model

# Let's fit a 2- and a 3-state model with no covariates
# Let's first fit the model for step lengths only

## 2-state model: initial parameters for gamma distribution
mu0 <- c(0.5, 1) # step mean (two parameters: one for each state)
sigma0 <- c(0.5, 1) # step SD
stepPar0 <- c(mu0, sigma0)

## call to fitting function
m1 <- fitHMM(data = hmm.data, nbStates = 2, stepPar0 = stepPar0, 
             angleDist = 'none', formula = ~1)
m1

## 3-state model: initial parameters for gamma distribution
mu1 <- c(0.5, 1, 2) # step mean (two parameters: one for each state)
sigma1 <- c(0.5, 1, 2) # step SD
stepPar1 <- c(mu1, sigma1)

m2 <- fitHMM(data = hmm.data, nbStates = 3, stepPar0 = stepPar1, 
             angleDist = 'none', formula = ~1)
m2

# Compare models
AIC(m1, m2)

# Confidence intervals
CI(m2, alpha = 0.99) # NOT WORKING

# Plot the model
plot(m1)
plot(m2)

m2$mle

#--- 
#  Fitting the model

# Let's fit a 2- and a 3-state model with no covariates
# Now let's first fit the model for step lengths and turning angles

## 2-state model: initial parameters for gamma and von Mises distributions
mu0 <- c(0.5, 4) # step mean (two parameters: one for each state)
sigma0 <- c(0.5, 2) # step SD
stepPar0 <- c(mu0, sigma0)
angleMean0 <- c(pi/2, 0) # angle mean
kappa0 <- c(0.1, 1) # angle concentration
anglePar0 <- c(angleMean0, kappa0)

## call to fitting function
m3 <- fitHMM(data = hmm.data, nbStates = 2, stepPar0 = stepPar0,
            anglePar0 = anglePar0, formula = ~1)
m3

## 3-state model: initial parameters for gamma and von Mises distributions
mu1 <- c(0.5, 1, 2) # step mean (two parameters: one for each state)
sigma1 <- c(0.5, 1, 2) # step SD
stepPar1 <- c(mu1, sigma1)
angleMean1 <- c(0, pi/4,pi/2) # angle mean
kappa1 <- c(0.1, 1, 5) # angle concentration
anglePar1 <- c(angleMean1, kappa1)

m4 <- fitHMM(data = hmm.data, nbStates = 3, stepPar0 = stepPar1, 
             anglePar0 = anglePar1, formula = ~1)
m4

# Compare models
AIC(m3, m4)

# Confidence intervals
CI(m4, alpha = 0.99)

# Plot the model
plot(m3)
plot(m4)

## 4-state model: initial parameters for gamma and von Mises distributions
mu1 <- c(0.5, 1, 2, 4) # step mean (two parameters: one for each state)
sigma1 <- c(0.5, 1, 2, 3) # step SD
stepPar1 <- c(mu1, sigma1)
angleMean1 <- c(0, pi/4, pi/2, 0) # angle mean
kappa1 <- c(0.1, 1, 5, 2) # angle concentration
anglePar1 <- c(angleMean1, kappa1)

m5 <- fitHMM(data = hmm.data, nbStates = 4, stepPar0 = stepPar1, 
             anglePar0 = anglePar1, formula = ~1)
m5
m5$mle
AIC(m3, m4, m5)
plot(m5) # most parsimonious

CI(m5)

#----
# Visualize results for model m2

#--- 
# Plot before analysis - variables

if(exportFIG) png(paste0(outdir, 'HMMprepare.png'), width = 20, height = 13, units = 'cm', res = 400)

# set layout
layout(matrix(c(1,1,1,1,2,3,4,5), 2, 4))

data <- hmm.data %>% filter(name == 'Mineiro')

# plot track
plot(data$x, data$y, asp = 1, pch = 20, cex = 0.6, type = "o", xlab = "x", ylab = "y")

# plot time series and histogram of step lengths
plot(data$step, type = "h", xlab = "Tempo", ylab = "Tamanho do passo (km)")
hist(data$step, main = "", col = "lightgrey", border = 0, xlab = "Tamanho do passo (km)")

# plot time series and histogram of turning angles
plot(data$angle, type = "h", xlab = "Tempo", ylab = "Ângulo de virada (rad)", yaxt = "n")
axis(side = 2, at = c(-pi,-pi/2,0,pi/2,pi), labels = expression(-pi,-pi/2,0,pi/2,pi))
hist(data$angle, breaks = seq(-pi,pi,length = 15), main = "", col = "lightgrey",
     border = 0, xlab = "Ângulo de virada (rad)", xaxt = "n")
axis(side = 1, at = c(-pi,-pi/2,0,pi/2,pi), labels = expression(-pi,-pi/2,0,pi/2,pi))

mtext('Mineiro', side = 3, outer = TRUE, line = -3, cex = 1.5)
dev.off()

#--- 
# After model fitting

pdf(paste0(outdir, 'HMMfit.pdf'))
plot(m2)
dev.off()

### Think about a way of plotting that with flexibility