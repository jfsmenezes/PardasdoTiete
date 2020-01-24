#### Main script for analysis desiging reserves for Cougars in the 
#### Tiete region 

## Intent: This script act as the main function caller for this project
## This structure allow for easily chaging dataset and store different
## versions of the analysis in different functions
## To do so, it does many calls to directions and checking for the existence
## of files 

## Input: none directly, although it carries the assumptions from all its subfunctions
## Ouput: Currently, just the predicted map of Jaguar quality for the study area
## eventually, a set of reserves desings.


# Load dependencies
options(java.parameters = "-Xmx1g" )
library(raster)
library(gdalUtils)
library(dismo)
library(RQGIS3)
library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(parallel)
library(amt)
library(stringi)

# TODO: check for convergence issues in ARIMA fitter (LAPACK matrix is exactly singular)


source("./code/data importer.r")
source("./code/envpreparator (function).r")
source("./code/ARIMAfitter.r")
source("./code/SSFer.r")
source("./code/predictor.r")
source("./code/acessory functions.r")

## TODO: Add log_dist_cities to envpreparator


experiment.folder <- "./experiment 003"
res<-30


## add which values to calculate
produce.gpkg        <- TRUE
produce.studystack  <- TRUE
produce.arima       <- TRUE
produce.models      <- TRUE
produce.predictions <- TRUE


if(produce.gpkg) { 
    data.importer(derivdir   = paste0(experiment.folder,"/data derived"),
                  rawdir     = paste0("./raw/data 17.12.19"), 
                  tempdir    = paste0(experiment.folder,"/maps derived/observedstack"),
                  res = res,
                  qgis.folder = "C:/Program Files/QGIS 3.4"
                  )
}
if(produce.studystack ) {
    envpreparator( buffergeo = st_read("./raw/maps/area_estudo/area_estudo_SIRGAS2000_UTM22S.shp"),
               tempdir   =   paste0(experiment.folder, "/maps derived/studyarea"),
               finalrds  = "experiment003map.rds",
               res=res,
               overwrite.gb = TRUE,
               qgis.folder  = "C:/Program Files/QGIS 3.4"
)
}
if(produce.arima) {
    ARIMAfitter(infile  = paste0(experiment.folder, "/data derived/pardas_tiete_all_individuals.gpkg"),
               outfile = paste0(experiment.folder, "/data derived/mov.track.rds") 
    )
}
if(produce.models) {
    ssfer(data = paste0(experiment.folder, "/data derived/mov.track.rds"),
          tempdir = paste0(experiment.folder, "/maps derived/observedstack"),
          outfile = paste0(experiment.folder, "/data derived/bestmodels.rds") )
}


if(produce.predictions) {
    predictor(models = paste0(experiment.folder, "/data derived/bestmodels.rds"),
              tempdir = paste0(experiment.folder, "/maps derived/studyarea"),
              outfolder = paste0(experiment.folder, "/maps derived/qualitypredictions"),
              qgis.folder  = "C:/Program Files/QGIS 3.4",
              overwrite = TRUE

}

plot(test$x_[test$Name=="Piloto"],test$y_[test$Name=="Piloto"],col=factor(test$disp[test$Name=="Piloto"]),)