#' ---
#' title: 'Light code for Puma's step selection function'
#' author: Bernardo Niebuhr - CENAP/ICMBio 
#'         Jorge Menezes    - CENAP/ICMBio
#' ---

# Intent: This code is a summarized version of 03_ssf_pumas.R, without
#         any exploratory graphs and discussion. Its intent is to serve
#         as a quick way to calculate SSF without requiring intervention
#         or decision making. In that way, it can be called by the reserve
#         selection code immediately.

# Input:  1) a 'mov.track' (class "track.xyt") object with animals locations.
#         Must contain a dispersal.behavior column with information if the
#         is dispersing or resident, as determined by a previous ARIMA.
#         2) A series of maps represeting the environmental variables

# Output: a list of models representing the best model for each individual (n=8)
#         step selection function. 


# Questions:
# 1) Should I be concerned with ROC not being weell estimated in conditional regression
# even after using the sample as a reference?
# 2) Is it a problema that my model has variables related to movement (cosine of direction etc)
# considering i won't have those when predicting?
# 3) How do I combine the models for each individuals, especially if they point to different variables.

### Load packages ###
library( lubridate )
library( tidyverse )
library( raster    )
library( amt       )
library( dismo     )


### load acessory functions for data handling.
# include functions is.in.formula, prepare, and runner
source("./code/acessory functions.r")

### Load mov.track (a.k.a. the locations)
load("./movcleaned.RData")



### Load maps ###
## TODO: add map sent by Jefferson (once he does one that is ok.)
map.list <- list.files('./data/rasters/', pattern = 'tif', full.names = T)
maps     <- stack(map.list)


### Separe individuals and prepare each one separately ###
# uses prepare function of acessory functions 
# note this is a nested tibble, with one dataset for each animal 
prob.train = 0.8
prepared <- mov.track %>% 
  nest(-name) %>% slice(3) %>%
  mutate( trk = map(data, prepare, maps , prob.train) )

# list of SSF models for consideration for all individuals.
# Nomeclature first number indicatesd the model, 
# second number denote a variation with time of day and dispersion behavior interacting
# with distance
# Third number indicates a model without dispersion behavior for animals withouth dispersing status
# The last number indicates buffer distance 

  modelslist <- list(
      # m1.500      = case_ ~ prop_forest_500m + log_sl_ + strata(step_id_),
      # m2.500      = case_ ~ prop_forest_500m + log_sl_ + log_dist_water + strata(step_id_)#,
      # m3.500      = case_ ~ prop_forest_500m + log_sl_ + log_dist_water + prop_forest_500m:log_dist_water + strata(step_id_),
      # m4.1.1.500  = case_ ~ prop_forest_500m:tod:disp + log_sl_:tod:disp + strata(step_id_)
      m4.1.1.500  = case_ ~ prop_forest_500m+disp+tod +prop_forest_500m:disp:tod + log_sl_:tod:disp + strata(step_id_)
      # m4.2.1.500  = case_ ~ prop_forest_500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + strata(step_id_),
      # m5.1.1.500  = case_ ~ prop_forest_500m:tod:disp + log_sl_:tod:disp + log_dist_water:tod:disp + strata(step_id_),
      # m5.2.1.500  = case_ ~ prop_forest_500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + log_dist_water:tod:disp + strata(step_id_),
      # m6.1.1.500  = case_ ~ prop_forest_500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_500m + prop_pasture_500m + strata(step_id_),
      # m6.2.1.500  = case_ ~ prop_forest_500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_500m:tod:disp + prop_pasture_500m:tod:disp + strata(step_id_),
      # m7.1.1.500  = case_ ~ prop_forest_500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_500m + prop_pasture_500m + log_dist_cities + log_dist_roads + strata(step_id_),
      # m7.2.1.500  = case_ ~ prop_forest_500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_500m:tod:disp + prop_pasture_500m:tod:disp + log_dist_cities:tod:disp + log_dist_roads:tod:disp + strata(step_id_)

      # m4.2.2.500  = case_ ~ prop_forest_500m:tod + log_sl_:tod + cos(ta_):tod + strata(step_id_),
      # m5.1.2.500  = case_ ~ prop_forest_500m:tod + log_sl_:tod + log_dist_water:tod + strata(step_id_),
      # m5.2.2.500  = case_ ~ prop_forest_500m:tod + log_sl_:tod + cos(ta_):tod + log_dist_water:tod + strata(step_id_),
      # m6.1.2.500  = case_ ~ prop_forest_500m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_500m + prop_pasture_500m + strata(step_id_),
      # m6.2.2.500  = case_ ~ prop_forest_500m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_500m:tod + prop_pasture_500m:tod + strata(step_id_),
      # m7.1.2.500  = case_ ~ prop_forest_500m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_500m + prop_pasture_500m + log_dist_cities + log_dist_roads + strata(step_id_),
      # m7.2.2.500  = case_ ~ prop_forest_500m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_500m:tod + prop_pasture_500m:tod + log_dist_cities:tod + log_dist_roads:tod + strata(step_id_)

      # m1.100      = case_ ~ prop_forest_100m + log_sl_ + strata(step_id_),
      # m2.100      = case_ ~ prop_forest_100m + log_sl_ + log_dist_water + strata(step_id_),
      # m3.100      = case_ ~ prop_forest_100m + log_sl_ + log_dist_water + prop_forest_100m:log_dist_water + strata(step_id_),
      # m4.1.1.100  = case_ ~ prop_forest_100m:tod:disp + log_sl_:tod:disp + strata(step_id_),
      # m4.2.1.100  = case_ ~ prop_forest_100m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + strata(step_id_),
      # m5.1.1.100  = case_ ~ prop_forest_100m:tod:disp + log_sl_:tod:disp + log_dist_water:tod:disp + strata(step_id_),
      # m5.2.1.100  = case_ ~ prop_forest_100m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + log_dist_water:tod:disp + strata(step_id_),
      # m6.1.1.100  = case_ ~ prop_forest_100m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_100m + prop_pasture_100m + strata(step_id_),
      # m6.2.1.100  = case_ ~ prop_forest_100m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_100m:tod:disp + prop_pasture_100m:tod:disp + strata(step_id_),
      # m7.1.1.100  = case_ ~ prop_forest_100m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_100m + prop_pasture_100m + log_dist_cities + log_dist_roads + strata(step_id_),
      # m7.2.1.100  = case_ ~ prop_forest_100m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_100m:tod:disp + prop_pasture_100m:tod:disp + log_dist_cities:tod:disp + log_dist_roads:tod:disp + strata(step_id_)

      # m4.2.2.100  = case_ ~ prop_forest_100m:tod + log_sl_:tod + cos(ta_):tod + strata(step_id_),
      # m5.1.2.100  = case_ ~ prop_forest_100m:tod + log_sl_:tod + log_dist_water:tod + strata(step_id_),
      # m5.2.2.100  = case_ ~ prop_forest_100m:tod + log_sl_:tod + cos(ta_):tod + log_dist_water:tod + strata(step_id_),
      # m6.1.2.100  = case_ ~ prop_forest_100m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_100m + prop_pasture_100m + strata(step_id_),
      # m6.2.2.100  = case_ ~ prop_forest_100m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_100m:tod + prop_pasture_100m:tod + strata(step_id_),
      # m7.1.2.100  = case_ ~ prop_forest_100m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_100m + prop_pasture_100m + log_dist_cities + log_dist_roads + strata(step_id_),
      # m7.2.2.100  = case_ ~ prop_forest_100m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_100m:tod + prop_pasture_100m:tod + log_dist_cities:tod + log_dist_roads:tod + strata(step_id_)

      # m1.2500      = case_ ~ prop_forest_2500m + log_sl_ + strata(step_id_),
      # m2.2500      = case_ ~ prop_forest_2500m + log_sl_ + log_dist_water + strata(step_id_),
      # m3.2500      = case_ ~ prop_forest_2500m + log_sl_ + log_dist_water + prop_forest_2500m:log_dist_water + strata(step_id_),
      # m4.1.1.2500  = case_ ~ prop_forest_2500m:tod:disp + log_sl_:tod:disp + strata(step_id_),
      # m4.2.1.2500  = case_ ~ prop_forest_2500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + strata(step_id_),
      # m5.1.1.2500  = case_ ~ prop_forest_2500m:tod:disp + log_sl_:tod:disp + log_dist_water:tod:disp + strata(step_id_),
      # m5.2.1.2500  = case_ ~ prop_forest_2500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + log_dist_water:tod:disp + strata(step_id_),
      # m6.1.1.2500  = case_ ~ prop_forest_2500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_2500m + prop_pasture_2500m + strata(step_id_),
      # m6.2.1.2500  = case_ ~ prop_forest_2500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_2500m:tod:disp + prop_pasture_2500m:tod:disp + strata(step_id_),
      # m7.1.1.2500  = case_ ~ prop_forest_2500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_2500m + prop_pasture_2500m + log_dist_cities + log_dist_roads + strata(step_id_),
      # m7.2.1.2500  = case_ ~ prop_forest_2500m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_2500m:tod:disp + prop_pasture_2500m:tod:disp + log_dist_cities:tod:disp + log_dist_roads:tod:disp + strata(step_id_)

      # m4.2.2.2500  = case_ ~ prop_forest_2500m:tod + log_sl_:tod + cos(ta_):tod + strata(step_id_),
      # m5.1.2.2500  = case_ ~ prop_forest_2500m:tod + log_sl_:tod + log_dist_water:tod + strata(step_id_),
      # m5.2.2.2500  = case_ ~ prop_forest_2500m:tod + log_sl_:tod + cos(ta_):tod + log_dist_water:tod + strata(step_id_),
      # m6.1.2.2500  = case_ ~ prop_forest_2500m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_2500m + prop_pasture_2500m + strata(step_id_),
      # m6.2.2.2500  = case_ ~ prop_forest_2500m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_2500m:tod + prop_pasture_2500m:tod + strata(step_id_),
      # m7.1.2.2500  = case_ ~ prop_forest_2500m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_2500m + prop_pasture_2500m + log_dist_cities + log_dist_roads + strata(step_id_),
      # m7.2.2.2500  = case_ ~ prop_forest_2500m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_2500m:tod + prop_pasture_2500m:tod + log_dist_cities:tod + log_dist_roads:tod + strata(step_id_)

      # m1.5000      = case_ ~ prop_forest_5000m + log_sl_ + strata(step_id_),
      # m2.5000      = case_ ~ prop_forest_5000m + log_sl_ + log_dist_water + strata(step_id_),
      # m3.5000      = case_ ~ prop_forest_5000m + log_sl_ + log_dist_water + prop_forest_5000m:log_dist_water + strata(step_id_),
      # m4.1.1.5000  = case_ ~ prop_forest_5000m:tod:disp + log_sl_:tod:disp + strata(step_id_),
      # m4.2.1.5000  = case_ ~ prop_forest_5000m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + strata(step_id_),
      # m5.1.1.5000  = case_ ~ prop_forest_5000m:tod:disp + log_sl_:tod:disp + log_dist_water:tod:disp + strata(step_id_),
      # m5.2.1.5000  = case_ ~ prop_forest_5000m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + log_dist_water:tod:disp + strata(step_id_),
      # m6.1.1.5000  = case_ ~ prop_forest_5000m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_5000m + prop_pasture_5000m + strata(step_id_),
      # m6.2.1.5000  = case_ ~ prop_forest_5000m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_5000m:tod:disp + prop_pasture_5000m:tod:disp + strata(step_id_),
      # m7.1.1.5000  = case_ ~ prop_forest_5000m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_5000m + prop_pasture_5000m + log_dist_cities + log_dist_roads + strata(step_id_),
      # m7.2.1.5000  = case_ ~ prop_forest_5000m:tod:disp + log_sl_:tod:disp + cos(ta_):tod:disp + dist_water:tod:disp + prop_sugarcane_5000m:tod:disp + prop_pasture_5000m:tod:disp + log_dist_cities:tod:disp + log_dist_roads:tod:disp + strata(step_id_)

      # m4.2.2.5000  = case_ ~ prop_forest_5000m:tod + log_sl_:tod + cos(ta_):tod + strata(step_id_),
      # m5.1.2.5000  = case_ ~ prop_forest_5000m:tod + log_sl_:tod + log_dist_water:tod + strata(step_id_),
      # m5.2.2.5000  = case_ ~ prop_forest_5000m:tod + log_sl_:tod + cos(ta_):tod + log_dist_water:tod + strata(step_id_),
      # m6.1.2.5000  = case_ ~ prop_forest_5000m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_5000m + prop_pasture_5000m + strata(step_id_),
      # m6.2.2.5000  = case_ ~ prop_forest_5000m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_5000m:tod + prop_pasture_5000m:tod + strata(step_id_),
      # m7.1.2.5000  = case_ ~ prop_forest_5000m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_5000m + prop_pasture_5000m + log_dist_cities + log_dist_roads + strata(step_id_),
      # m7.2.2.5000  = case_ ~ prop_forest_5000m:tod + log_sl_:tod + cos(ta_):tod + dist_water:tod + prop_sugarcane_5000m:tod + prop_pasture_5000m:tod + log_dist_cities:tod + log_dist_roads:tod + strata(step_id_)

  )


  ### Running models ###
  # Running is within the runner function, so we can handle 
  # the edge case of animals without dispersive behavior.
  # In models we have a column fit, in each line is all the models for that individual
  # we flatten that structure using unnest.
  # Now we have one row for every model, with notations on individual and model name.
  fits <- prepared %>% mutate(fit = map(trk, runner, modelslist) )


  models <- fits %>% mutate(m.name = map(fit, names) )  %>% unnest(cols = c("m.name", "fit")) 

  ### Add AIC ###
  # (not comparable between non nested models)
  aicmodel <- models %>% mutate(aics = map_dbl(fit, ~ AIC(.x$model)))

  ### Calculate AUCs ###
  # TODO: fix strata error by calculating predicting probabilities for every step, assuming 
  # all steps are equally likely (following Muller & MacLehose, 2014)
  preds <- map2(aicmodel$fit, aicmodel$trk, ~ cbind(.y[!(.y$train),],prediction = predict(.x$model, newdata=.y[!(.y$train),],type="risk",reference="sample")))
  aucs  <- map_dbl(preds, ~ evaluate(.x$prediction[.x$case_], .x$prediction[!.x$case_])@auc)
  aucmodel <- cbind(aicmodel, aucs)

for(a in 1:length(aicmodel$trk)) {
  test<-aicmodel$trk[[a]]
  print(table(test$disp,test$train) )
}
for(a in 1:length(aicmodel$trk)) {
  print(levels(aicmodel$trk[[a]]$tod))
predict(aicmodel$fit[[a]]$model, aicmodel$trk[[a]][!(aicmodel$trk[[a]]$train),],type="risk",reference="sample" )

}