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
#library(moveHMM) # conflicts with RQGIS for some unknown reason. However it will soon be replaced by the ARIMA
# so I'm not very concerned
# Also conflicts with openxlsx



source("./code/data importer.r")
source("./code/envpreparator (function).r")
source("./code/ARIMAfitter.r")
source("./code/SSFer.r")
source("./code/predictor.r")
source("./code/acessory functions.r")

## TODO: Add log_dist_cities to envpreparator


experiment.folder <- "./experiment 003"
res<-30


## First check for the presence of files in the current experiment folder.
hasprediction <- file.exists(paste0(experiment.folder,"/maps derived/qualitypredictions/qualityexpmean.sdat"))
hasstudystack <- list.files(paste0(experiment.folder,"/maps derived/studyarea/"), pattern="RData") > 0
hasmodels     <- file.exists(paste0(experiment.folder, "/data derived/bestmodels.RData"))
hasHMMdata    <- file.exists(paste0(experiment.folder, "/data derived/movcleaned.RData"))
hasgpkg       <- file.exists(paste0(experiment.folder, "/data derived/pardas_tiete_all_individuals.gpkg"))

if(!hasgpkg) { 
    data.importer(derivdir   = paste0(experiment.folder,"/data derived"),
                  rawdir     = paste0("./raw/data 17.12.19"), 
                  tempdir    = paste0(experiment.folder,"/maps derived/observedstack"),
                  res = res,
                  qgis.folder = "C:/Program Files/QGIS 3.4"
                  )
}

if(!hasHMMdata) {
    ARIMAfitter(infile  = paste0(experiment.folder, "/data derived/pardas_tiete_all_individuals.gpkg"),
               outfile = paste0(experiment.folder, "/data derived/mov.track.rds") 
    )
}
if(!hasmodels) {
    ssfer(data = paste0(experiment.folder, "/data derived/mov.track.rds"),
          tempdir = paste0(experiment.folder, "/maps derived/observedstack"),
          outfolder = paste0(experiment.folder, "/data derived") )
}

if(!hasstudystack) {
    envpreparator( buffergeo = st_read("./raw/maps/area_estudo/area_estudo_SIRGAS2000_UTM22S.shp"),
               tempdir   =   paste0(experiment.folder, "/maps derived/studyarea"),
               finalrds  = "experiment003map.rds",
               res=res,
               overwrite.gb = TRUE,
               qgis.folder  = "C:/Program Files/QGIS 3.4"
)
}
if(!hasprediction) {
    predictor(datafolder = paste0(experiment.folder, "/data derived"),
    mapfolder = paste0(experiment.folder, "/maps derived/studyarea"),
    outfolder = paste0(experiment.folder, "/maps derived/qualitypredictions")

}