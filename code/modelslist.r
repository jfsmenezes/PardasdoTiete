# list of SSF models for consideration for all individuals.
# Nomeclature first number indicatesd the model, 
# second number denote a variation with time of day and dispersion behavior interacting
# with distance
# Third number indicates a model without dispersion behavior for animals withouth dispersing status
# The last number indicates buffer distance 

  modelslist <- list(
       m1.500      = case_ ~ prop_forest_500m + log_sl_ + strata(step_id_)
      # m2.500      = case_ ~ prop_forest_500m + log_sl_ + log_dist_water + strata(step_id_)#,
      # m3.500      = case_ ~ prop_forest_500m + log_sl_ + log_dist_water + prop_forest_500m:log_dist_water + strata(step_id_),
      # m4.1.1.500  = case_ ~ prop_forest_500m:tod:disp + log_sl_:tod:disp + strata(step_id_)
      # m4.1.1.500  = case_ ~ prop_forest_500m+disp+tod +prop_forest_500m:disp:tod + log_sl_:tod:disp + strata(step_id_)
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

