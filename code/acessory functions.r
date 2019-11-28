#' ---
#' title: 'Acessory functions for light code '
#' author: Jorge Menezes    - CENAP/ICMBio
#' ---

# Intent: This is a aggregation file for all acessory 
#         functions used in "lightssf.r". 

# This file contains no input or outputs. 


# prepare: This function does all movement data preparation for running 
# step selection functions it takes an object of class "track.xyt",
# resamples it, creating bursts of 1 hour ± 10 minutes, eliminates all
# burst with less than 3 points (and hence isolated points), classifies it
# in day or night, add random steps to represent pseudo-absences. 
# This creates object d1.
# From this object, it extract values from a list of stacks, 
# and logaritimize or convert them in factors. 

# TODO: solve dispersal.behavior and disp being equal
prepare <- function(d, maps, prob.train) {
    d1  <- d %>% 
      amt::track_resample(rate = lubridate::hours(1), tolerance = lubridate::minutes(10)) %>%
      filter_min_n_burst2(min_n = 3) %>% 
      amt::steps_by_burst() %>% 
      amt::time_of_day() %>% 
      left_join(
        d %>% 
          dplyr::select(t_, dispersal.behavior),
        by = c("t1_" = "t_") 
      ) %>% 
      amt::random_steps() 

    d2 <- amt::extract_covariates(d1, maps) %>%
          rename(landuse = FBDS_land_use_SP_sirgas_albers_canasat_7_pasture_8) %>% 
          mutate(landuse = ifelse(landuse == 3 | landuse == 6, 2, landuse))    %>%
          mutate(landuse = factor(landuse, 
                                  levels = c(1,2,4,5,7,8), 
                                  labels = c('water', 'antropic', 'forest', 'natural','sugarcane', 'pasture')
                                  )
                ) %>%
          mutate(log_sl_         = log(sl_),
                 log_dist_water  = log(dist_water+1.1),
                 log_dist_cities = log(dist_cities+1.1),
                 log_dist_roads  = log(dist_roads+1.1),
                 disp            = as.factor(dispersal.behavior),
                 tod             = as.factor(tod_end_)
          ) %>%
          select(-FBDS_land_use_SP_sirgas_albers_canasat_7, - dispersal.behavior, -tod_end_)


    burst_in_sp <- as.data.frame(d2) %>% 
                   group_by(burst_) %>% 
                   summarize( valid = all(!is.na(landuse))) %>% 
                   filter(valid==TRUE) %>% 
                   pull(1)
    
    d3 <- filter( d2, burst_ %in% burst_in_sp)
    d4 <- train_selector(d3, prop=prob.train)
    print("preparation complete!")
    return(d4)
  }

# runner: function that takes a list of formulas o run ssf on, and separate the ones
# models without dispersion behavior if there is the animal does not have this behavior
runner <-  function(d, modelslist) {
    if(length(unique(d$disp))<2) {
        modelslist <- modelslist[!sapply(modelslist, is.in.formula, "disp")]
        }
    d <- d[d$train,]
    runs <- map( modelslist, ~ fit_ssf(d, .x, model=T, iter.max=50) )
    names(runs) <- names(modelslist)
    print("run complete")
    return(runs)
}

# is.in.formula: function to detect if a certain character string is within a formula.
is.in.formula <- function(formula,text) any(grepl(text,as.character(formula)))

# filter_min_by_burst2: For some reason steps_by_burst gives an error of corrupted
# grouped df. In stack overflow a user says package amt has fixed this bug
# but it is only available in Linux. So I created my own function.
filter_min_n_burst2 <- function(track, min_n = 3) {
  bursts <- unique(track$burst_)
  bursts_length <- table(track$burst_)
  removable <- bursts[ bursts_length < min_n]
  remaining <- track[ !(track$burst_ %in% removable), ]
  return(remaining)
}


# train_selector: selects a random subset of lines from a data.frame,
# respecting that the subset has all levels of all factors.
# A proportion can be specificied 
train_selector <- function(data,prop=0.8) {
  classes  <- lapply(data, class)
  isfactor <- colnames(data)[ sapply(classes, function(x) "factor" %in% x) ]

  split.factor <- split(data, interaction(data[isfactor]) )
  sampled <- lapply(  split.factor , function(x) pull(sample_n(x,1),1) )
  
  selected <- do.call(c, sampled)

  bneed     <- round(length(unique(data$burst_))*prop,0) - length(selected)

  random  <- sample(data$burst_[!(data$burst_ %in% selected)], bneed )
  complete <- c(selected, complete)

  data$train <- data$burst_ %in% complete

  return(data)
}



sample_each <- function(data,prop=0.8) {
  classes  <- lapply(data, class)
  isfactor <- sapply(classes, function(x) "factor" %in% x)
  levels   <- lapply(data[isfactor], levels)
  levels   <- levels[order(sapply(levels, length),decreasing=T)]
  
  selected <- data.frame()
  
  for(a in 1:length(levels)) {
    for(b in 1:length(levels[[a]])) {

      temp <- data[ data[names(levels)[a]] == levels[[a]][b],  ]
      selected <- rbind(selected, temp[sample(1:nrow(temp),1),] )

    }}
  selected  <- unique(selected)
  remaining <- anti_join(data, selected)
  nneed     <- round(nrow(data)*0.8,0) - nrow(selected)
  random    <- remaining[ sample(1:nrow(remaining), nneed), ]
  complete  <- rbind(selected, random)

  complete <- data %>% nest_join(complete) %>% 
                       mutate(train = map(complete,nrow)>0) %>%
                       select(-complete)

  return(complete)
  }

specialpredict <- function(model, newdata) {
  formula <- update(model$formula, NULL ~ . - strata(step_id_) - 1)
  organized <- model.matrix(formula, data= newdata)
  organizedcols <-  colnames( organized)
  if( ("dispdispersal" %in% organizedcols) && ("dispresidency" %in% organizedcols)) {
    organized<- organized[,-match("dispdispersal", organizedcols)]
  }
  stopifnot( names(coef(model)) == colnames(organized) ) 
  linear.pred <- c( organized %*% coef(model) )
  quality <- exp(linear.pred)/(1+exp(linear.pred))
  return(quality)
}