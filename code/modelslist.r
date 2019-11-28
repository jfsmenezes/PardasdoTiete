# list of SSF models for consideration for all individuals.
# Nomeclature first number indicatesd the model, 
# second number denote a variation with time of day and dispersion behavior interacting
# with distance
# Third number indicates a model without dispersion behavior for animals withouth dispersing status
# The last number indicates buffer distance 
  modelslist <- list(
      m1.500     = case_ ~ prop_forest_500m  + strata(step_id_),
      m2.500      = case_ ~ prop_forest_500m  + log_dist_water + strata(step_id_),
      m3.500      = case_ ~ prop_forest_500m  + log_dist_water + prop_forest_500m:log_dist_water + strata(step_id_),
      m4.1.1.500  = case_ ~ prop_forest_500m + disp + prop_forest_500m:disp + strata(step_id_),
      m5.1.1.500  = case_ ~ prop_forest_500m + disp + prop_forest_500m:disp + log_dist_water + log_dist_water:disp + strata(step_id_),
      m6.1.1.500  = case_ ~ prop_forest_500m + disp + prop_forest_500m:disp + dist_water + dist_water:disp + prop_sugarcane_500m + prop_pasture_500m + strata(step_id_),
      m7.1.1.500  = case_ ~ prop_forest_500m + disp + prop_forest_500m:disp + dist_water + dist_water:disp + prop_sugarcane_500m + prop_pasture_500m + log_dist_cities + log_dist_roads + strata(step_id_),

      m5.1.2.500  = case_ ~ prop_forest_500m  + log_dist_water + strata(step_id_),
      #m6.1.2.500  = case_ ~ prop_forest_500m  + dist_water + prop_sugarcane_500m + prop_pasture_500m + strata(step_id_),
      #m7.1.2.500  = case_ ~ prop_forest_500m  + dist_water + prop_sugarcane_500m + prop_pasture_500m + log_dist_cities + log_dist_roads + strata(step_id_),

      m1.100      = case_ ~ prop_forest_100m  + strata(step_id_),
      m2.100      = case_ ~ prop_forest_100m  + log_dist_water + strata(step_id_),
      m3.100      = case_ ~ prop_forest_100m  + log_dist_water + prop_forest_100m:log_dist_water + strata(step_id_),
      m4.1.1.100  = case_ ~ prop_forest_100m + disp + prop_forest_100m:disp + strata(step_id_),
      m5.1.1.100  = case_ ~ prop_forest_100m + disp + prop_forest_100m:disp + log_dist_water + log_dist_water:disp + strata(step_id_),
      m6.1.1.100  = case_ ~ prop_forest_100m + disp + prop_forest_100m:disp + dist_water + dist_water:disp + prop_sugarcane_100m + prop_pasture_100m + strata(step_id_),
      m7.1.1.100  = case_ ~ prop_forest_100m + disp + prop_forest_100m:disp + dist_water + dist_water:disp + prop_sugarcane_100m + prop_pasture_100m + log_dist_cities + log_dist_roads + strata(step_id_),

      m5.1.2.100  = case_ ~ prop_forest_100m  + log_dist_water + strata(step_id_),
      #m6.1.2.100  = case_ ~ prop_forest_100m  + dist_water + prop_sugarcane_100m + prop_pasture_100m + strata(step_id_),
      #m7.1.2.100  = case_ ~ prop_forest_100m  + dist_water + prop_sugarcane_100m + prop_pasture_100m + log_dist_cities + log_dist_roads + strata(step_id_),

      m1.2500      = case_ ~ prop_forest_2500m  + strata(step_id_),
      m2.2500      = case_ ~ prop_forest_2500m  + log_dist_water + strata(step_id_),
      m3.2500      = case_ ~ prop_forest_2500m  + log_dist_water + prop_forest_2500m:log_dist_water + strata(step_id_),
      m4.1.1.2500  = case_ ~ prop_forest_2500m + disp + prop_forest_2500m:disp + strata(step_id_),
      m5.1.1.2500  = case_ ~ prop_forest_2500m + disp + prop_forest_2500m:disp + log_dist_water + log_dist_water:disp + strata(step_id_),
      m6.1.1.2500  = case_ ~ prop_forest_2500m + disp + prop_forest_2500m:disp + dist_water + dist_water:disp + prop_sugarcane_2500m + prop_pasture_2500m + strata(step_id_),
      m7.1.1.2500  = case_ ~ prop_forest_2500m + disp + prop_forest_2500m:disp + dist_water + dist_water:disp + prop_sugarcane_2500m + prop_pasture_2500m + log_dist_cities + log_dist_roads + strata(step_id_),

      m5.1.2.2500  = case_ ~ prop_forest_2500m  + log_dist_water + strata(step_id_),
      #m6.1.2.2500  = case_ ~ prop_forest_2500m  + dist_water + prop_sugarcane_2500m + prop_pasture_2500m + strata(step_id_),
      #m7.1.2.2500  = case_ ~ prop_forest_2500m  + dist_water + prop_sugarcane_2500m + prop_pasture_2500m + log_dist_cities + log_dist_roads + strata(step_id_),

      m1.5000      = case_ ~ prop_forest_5000m  + strata(step_id_),
      m2.5000      = case_ ~ prop_forest_5000m  + log_dist_water + strata(step_id_),
      m3.5000      = case_ ~ prop_forest_5000m  + log_dist_water + prop_forest_5000m:log_dist_water + strata(step_id_),
      m4.1.1.5000  = case_ ~ prop_forest_5000m + disp + prop_forest_5000m:disp + strata(step_id_),
      m5.1.1.5000  = case_ ~ prop_forest_5000m + disp + prop_forest_5000m:disp + log_dist_water + log_dist_water:disp + strata(step_id_),
      m6.1.1.5000  = case_ ~ prop_forest_5000m + disp + prop_forest_5000m:disp + dist_water + dist_water:disp + prop_sugarcane_5000m + prop_pasture_5000m + strata(step_id_),
      m7.1.1.5000  = case_ ~ prop_forest_5000m + disp + prop_forest_5000m:disp + dist_water + dist_water:disp + prop_sugarcane_5000m + prop_pasture_5000m + log_dist_cities + log_dist_roads + strata(step_id_),

      m5.1.2.5000  = case_ ~ prop_forest_5000m  + log_dist_water + strata(step_id_)
      #m6.1.2.5000  = case_ ~ prop_forest_5000m  + dist_water + prop_sugarcane_5000m + prop_pasture_5000m + strata(step_id_),
      #m7.1.2.5000  = case_ ~ prop_forest_5000m  + dist_water + prop_sugarcane_5000m + prop_pasture_5000m + log_dist_cities + log_dist_roads + strata(step_id_)
  )
