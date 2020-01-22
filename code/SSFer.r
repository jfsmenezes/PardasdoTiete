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
# 3) How do I combine the models for each individuals, especially if they point to different variables.

ssfer <- function(data,tempdir, outfile) {
  


### load acessory functions for data handling.
# include functions is.in.formula, prepare, and runner
source("./code/acessory functions.r")


# read the list of proposed models for the ssf
modelslist <- scan("./code/modelslist.r", what = "character", comment.char="#", sep="\n")
n.modelslist <- str_extract(modelslist, ".+?(?=\\=)") %>% str_trim()
modelslist <- lapply(modelslist, as.formula)
names(modelslist) <-  n.modelslist

### Load mov.track (a.k.a. the locations)
mov.track <- readRDS(data)
mapstack  <- stack(list.files(tempdir,pattern="tif$",full.names=T))

### Load maps ###
## TODO: add map sent by Jefferson (once he does one that is ok.)
#map.list <- list.files('./data/rasters/', pattern = 'tif', full.names = T)
#maps     <- stack(map.list)


### Separe individuals and prepare each one separately ###
# uses prepare function of acessory functions 
# note this is a nested tibble, with one dataset for each animal 
prob.train = 0.8
storage <- mov.track %>% 
  nest(-Name) %>% 
  mutate( trk = map(data, prepare, mapstack , prob.train) )


### Running models ###
# Running is within the runner function, so we can handle 
# the edge case of animals without dispersive behavior.
# In models we have a column fit, in each line is all the models for that individual
# we flatten that structure using unnest.
# Now we have one row for every model, with notations on individual and model name.
storage <- storage %>% mutate(fit = map(trk, runner, modelslist) )


storage <- storage %>% mutate(m.name = map(fit, names) )  %>% unnest(cols = c("m.name", "fit")) 

### Add AIC ###
# (not comparable between non nested models)

storage <- storage %>% filter(sapply(fit,function(x) class(x)[1])=="fit_clogit") %>% mutate(aics = map_dbl(fit, ~ AIC(.x$model)))

### Calculate AUCs ###
# TODO: fix strata error by calculating predicting probabilities for every step, assuming 
# all steps are equally likely (following Muller & MacLehose, 2014)
preds <- storage %>% mutate( aucs = map2_dbl(fit, trk, auccalculator) )

bestmodels <- preds %>% group_by(Name) %>% arrange(desc(aucs)) %>% slice(1) %>%
              ungroup() %>% select(-data, -trk)
saveRDS(bestmodels, file=outfile )

}
plot(mapstack$landuse,ext=extent(469723,470000,-1140213,-1131000))
windows()
storage <- storage[,-3]
 test<-predict(mapstack, storage$fit[[1]]$model, ext=extent(469723,470000,-1140213,-1131000),const=data.frame(step_id_=46),type="risk",reference="sample")
plot(test)
