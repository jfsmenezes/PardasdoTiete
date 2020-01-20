# ---
# title: ARIMA function for identifying dispersal movements
# author: Jorge Menezes    <jorgefernandosaraiva@gmail.com> 
#         
# date: CENAP-ICMBio/Pro-Carnivoros, Brazil, January 2020
# 
# ---

 ## Intent: This is a short, even simpler, automated version of the file 02_arima_fit_simple.R
 ##  whose purpose is to received a gpkg filepath, read and classify animal location in two different models
 ##  dispersal or residency. The model assumes 5 potential trajectories (from being always resident, always
 ##  dispersing, dispersing -> resident, resident -> dispersal and resident -> dispersal -> resident). 
 ##  To discern between each, we constructed an ARIMA with different components of AR and I. and compared the fit
 ##  by comparing their loglikelihood. This process is repeated on all individuals and information is combined and 
 ##  stored in a track.xyt object, the input for the SSF code.
 
 ## Input: the filepath for a gpkg file with all locations. It assumes there is a timestamp column 
 ## with the time the locations where taken in POSIXct format. It also assumes there is a Name column 
 ## identifying each animal.

 ## Output: A rds file containing an object of class rds with animals locations and a column disp with information 
 ## of dispersal





ARIMAfitter<- function(infile,outfile, crs=NULL) {

#For debug:
#library(sf)
#infile <- "./experiment 003/data derived/pardas_tiete_all_individuals.gpkg"
if(is.null(crs)) {
    crs <- '+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs'
}



# TODO: investigate why we only have three individuals
data <- st_read(infile)
inds <- as.character(unique(data$Name))


# Prepare the subdataset for ARIMA. This dataset only has a centroid of locations per day
# for each individual. Coordinates are also transferred to X and Y fields, and a columns 
# of behavior is added assuming "dispersal" as the default behavior.
dailypos <- data %>% 
            group_by(Name, as.Date(timestamp) ) %>% 
            summarize(timestamp = first(timestamp), geom = st_centroid(st_union(geom))) %>% 
            mutate(dayrank = rank(timestamp) ) %>%
            ungroup() %>%
            mutate(X = st_coordinates(.)[,1], Y= st_coordinates(.)[,2]) %>%
            mutate(disp = "dispersal") %>%
            select(Name, timestamp, dayrank, disp, X, Y, geom) 

models <- c('resident', 'dispersing', 'departure', 'settling', 'depart-settle')

holder <- vector("list", length= length(inds))
for(i in 1:length(inds)) {

    # Separate locations for one individual
    dailypos.ind <- dailypos[dailypos$Name == inds[i],]
    
    # Find most likely arrival dates (assuming animal is settling) and departure dates (assuming it is leaving)
    dep.day  <- with(dailypos.ind, findDepartureDate(dayrank, X, Y, method = 'ML'))
    setl.day <- with(dailypos.ind, findDepartureDate(dayrank, X, Y, method = 'ML'))


    # Using these estimates as best guess, run models for all behaviors and extract their loglikelihood.
    ll <- array(NA, dim = 5)

    ll[1] <- tryCatch(with(dailypos.ind, ll.finddate(Time = dayrank, X = X, Y = Y, cp = NA                  , event = "resident"     , method.arima = 'ML')), error=function(e){cat("ERROR :",conditionMessage(e), "\n"); return(NA)})
    ll[2] <- tryCatch(with(dailypos.ind, ll.finddate(Time = dayrank, X = X, Y = Y, cp = NA                  , event = "dispersing"   , method.arima = 'ML')), error=function(e){cat("ERROR :",conditionMessage(e), "\n"); return(NA)})
    ll[3] <- tryCatch(with(dailypos.ind, ll.finddate(Time = dayrank, X = X, Y = Y, cp = dep.day             , event = "departure"    , method.arima = 'ML')), error=function(e){cat("ERROR :",conditionMessage(e), "\n"); return(NA)})
    ll[4] <- tryCatch(with(dailypos.ind, ll.finddate(Time = dayrank, X = X, Y = Y, cp = setl.day            , event = "settling"     , method.arima = 'ML')), error=function(e){cat("ERROR :",conditionMessage(e), "\n"); return(NA)})
    if(dep.day != setl.day) {
        ll[5] <- tryCatch(with(dailypos.ind, ll.finddate(Time = dayrank, X = X, Y = Y, cp = c(dep.day, setl.day), event = "depart-settle", method.arima = 'ML')), error=function(e){cat("ERROR :",conditionMessage(e), "\n"); return(NA)})
    } # avoid errors when models on departure and settlement agree on the same date.



    # Chose the model with greatest loglikelihood.
    mod.opt <- models[which(ll == max(ll, na.rm = T))]
  
    # based on that model update dispersal column accordingly
    if(mod.opt == "resident")       dailypos.ind$disp  <- "residency"
    if(mod.opt == "departure")      dailypos.ind$disp  <- ifelse(dailypos.ind$dayrank < setl.day, "dispersal","residency")
    if(mod.opt == "settling")       dailypos.ind$disp  <- ifelse(dailypos.ind$dayrank < dep.day,  "residency","dispersal")
    if(mod.opt == "depart-settle")  dailypos.ind$disp  <- ifelse( (dailypos.ind$dayrank < dep.day) | (dailypos.ind$dayrank > setl.day),  "residency","dispersal")

    holder[[i]] <- dailypos.ind
  }
  
# Merge all individuals in the same dataset again 
dailypos <- do.call(rbind, holder) 

# pass information to dataset with all locations using a left join
data$day <- as.Date(data$timestamp)
dailypos$day <- as.Date(dailypos$timestamp)
data <- left_join(data, as.data.frame(dailypos[,c("Name","day","disp")]), by = c("Name","day"))
data <- as.data.frame(data)
data <- data[,c("Name","timestamp","Longitude","Latitude","disp")]
mov.track <- make_track(data, Longitude, Latitude, timestamp, Name, disp,crs = CRS(crs))
saveRDS(mov.track,outfile)
return(outfile)

}


ll.finddate <- function(cp, Time, X, Y, 
                        # cp = NULL, cp2 = NULL, 
                        event = c("settling","departing","depart-settle","resident","dispersing")[1], 
                        method.arima = c("CSS-ML", "ML", "CSS"),
                        ...){
  
  if(event == "settling"){ order1 <- c(1,1,0); order2 <- c(1,0,0); k = 5 } else
    if(event == "departure") { order1 <- c(1,0,0); order2 <- c(1,1,0); k = 5} else 
      if(event == "depart-settle") { order1 <- c(1,0,0); order2 <- c(1,1,0); order3 <- c(1,0,0); k = 8} else
        if(event == "resident") { order1 <- c(1,0,0); k = 2 } else
          if(event == "dispersing") { order1 <- c(1,1,0); k = 2} else
            stop("Event must be one of `resident`, `dispersing`, `settling`, `departure`, or `depart-settle`.")
  
  if(event == "settling" | event == "departure") {
    fits <- list(
      X.fit1 = arima(X[Time < cp[1]], order = order1, method = method.arima, ...),
      X.fit2 = arima(X[Time >= cp[1]], order = order2, method = method.arima,  ...),
      Y.fit1 = arima(Y[Time < cp[1]], order = order1, method = method.arima,  ...),
      Y.fit2 = arima(Y[Time >= cp[1]], order = order2, method = method.arima,  ...))
  } else {
    if(event == "depart-settle") {
      fits <- list(
        X.fit1 = arima(X[Time < cp[1]], order = order1, method = method.arima,  ...),
        X.fit2 = arima(X[Time >= cp[1] & Time < cp[2]], order = order2, method = method.arima,  ...),
        X.fit3 = arima(X[Time >= cp[2]], order = order3, method = method.arima,  ...),
        Y.fit1 = arima(Y[Time < cp[1]], order = order1, method = method.arima,  ...),
        Y.fit2 = arima(Y[Time >= cp[1] & Time < cp[2]], order = order2, method = method.arima,  ...),
        Y.fit3 = arima(Y[Time >= cp[2]], order = order3, method = method.arima,  ...))
    } else {
      if(event == 'resident' | event == 'dispersing') {
        fits <- list(
          X.fit1 = arima(X, order = order1, method = method.arima,  ...),
          Y.fit1 = arima(Y, order = order1, method = method.arima,  ...))
      }
    }
  }
  
  # ll <- ll = sum(sapply(fits, function(fit) fit$loglik))
  # AIC <- -2*ll + 2*k
  # 
  # return(list(ll = ll, k = k, AIC = AIC))
  
  return(ll = sum(sapply(fits, function(fit) fit$loglik)))
}

findSettlingDate <- function(Time, X, Y, ...) 
  optimize(ll.finddate, Time= Time, X = X, Y = Y, maximum = TRUE, 
           lower = min(Time), upper = max(Time), event = "settling", ...)$maximum

findDepartureDate <- function(Time, X, Y, ...)
  optimize(ll.finddate, Time= Time, X = X, Y = Y, maximum = TRUE, 
           lower = min(Time), upper = max(Time), event = "departure", ...)$maximum
