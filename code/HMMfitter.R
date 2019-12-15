# ---
# title:  Hidden Markov Chain operator for Jaguar data 
# author: Bernardo Niebuhr <bernardo_brandaum@yahoo.com.br>
#         Jorge Menezes    <jorgefernandosaraiva@gmail.com> 
# date: CENAP-ICMBio/Pro-Carnivoros, Atibaia, SP, Brazil, June 2019
# 
# ---

## Intent: This is a short, automated version of the file 02_movement_modes_2018_06_d23.R
## whose purpose is to perform a 2-state Hidden Markov Chain and classify location as dis-
## persive or not. The function feeds on the exploration done by Bernardo but does not 
## contain any of it, for the purpose of generating a routine that can be fed new data
## at ease.


## Input: an Geopackage file, with locations. Each location must contain coordinates in 
## a projected coordinate system along with timestamp and individual identification.

## Output: An RData file with an object mov.track.rsp of class track.xyt and with a column 
## containing "dispersal" or "residencency" behavior categorization.

## Load dependencies
library(sf)
library(amt)
library(fitHMM)
library(lubridate)

## Load location file
fixes.geo <- st_read("./data/locations/pardas_tiete_all_individuals.gpkg",layer=fixes)

## Ensure timestamp is understood as dates
fixes.geo <- fixes.geo %>% mutate_at('timestamp', ymd_hms)


# Create move track object to allow resampling and creating a regular track
mov.track <- mk_track(fixes.geo, .x = "Longitude", .y = "Latitude", .t = timestamp, 
                      crs = st_crs(fixes.geo),
                      ID, name, species, sex)

## Execute resampling per individual
# Since track.xyt objects cannot be used with group_by(), we used nest() and map() to repeat
# process for every jaguar
preparer <-  function(x) { x %>% track_resample(rate = hours(24), tolerance = hours(12)) %>% filter_min_n_burst(min_n = 3)}
mov.track.rsp <- mov.track.rsp %>% 
                 nest(-name) %>%
                 mutate(data.rsp = map(data, preparer) ) %>%
                 unnest(data.rsp) %>% 
                 mutate(hour = t_ %>% format('%H') %>% as.factor)

## Add jaguar name to burst_ variable so we can use bursts can be uniquely identified in the dataset
# (Althought this information is not used in the rest of the script)
mov.track.rsp <- mov.track.rsp %>%
  mutate(brst = paste0(name, burst_))

## Change coordinates for numberical stability(?) in the HMM. 
mov.track.rsp <- mov.track.rsp %>% 
  mutate(
    x = x_/1000,
    y = y_/1000,
    ID = name
  )

## Convert to fitHMM format 
hmm.data <- mov.track.rsp %>% 
  as.data.frame %>% 
  prepData(type='UTM', coordNames = c('x', 'y'))

## Setting initial parameters for gamma and von Mises distributions,
## in a 4-state model on step length and turning angles.
# No covariates are used in this model.
# We are using state 4 as the definition of "dispersal", and considering everyone else
# to be a residence status.
# This may require input to validate that new iterations will have the same state as dispersal

mu1        <- c(0.5, 1, 2, 4)          # step mean (two parameters: one for each state)
sigma1     <- c(0.5, 1, 2, 3)       # step SD
stepPar1   <- c(mu1, sigma1)
angleMean1 <- c(0, pi/4, pi/2, 0) # angle mean
kappa1     <- c(0.1, 1, 5, 2) # angle concentration
anglePar1  <- c(angleMean1, kappa1)

m5 <- fitHMM(data = hmm.data, nbStates = 4, stepPar0 = stepPar1, 
             anglePar0 = anglePar1, formula = ~1)

## Ask user to input state to be considered dispersal
plot(m5)
dispstate <- readline("What states should we consider dispersal? (if there more than one state separate by commas)")
dispstate <- as.numeric( strsplit(dispstate,",")[[1]] )

## create final object with locations and dispersal data
mov.track <- mov.track.rsp %>% 
  mutate(state = as.factor(ifelse(viterbi(m5) %in% dispstate, 'dispersal', 'residency'))) %>%
  mutate(sl_  = amt::step_lengths(.)/1000 )

save(mov.track, file= "movcleaned.RData")



