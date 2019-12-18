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

library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(parallel)
library(amt)
library(moveHMM)
library(dismo)
library(raster) 
library(RSAGA)
library(gdalUtils)



source("./code/data importer.r")
source("./code/envpreparator (function).r")
source("./code/HMMfitter.r")
source("./code/lightssf.r")
source("./code/predictor.r")
source("./code/acessory functions.r")




experiment.folder <- "./experiment 001"
res<-5000


## First check for the presence of files in the current experiment folder.
hasprediction <- file.exists(paste0(experiment.folder,"/maps derived/qualitypredictions/qualityexpmean.sdat"))
hasstudystack <- list.files(paste0(experiment.folder,"/maps derived/studyarea/"), pattern="RData") > 0
hasmodels     <- file.exists(paste0(experiment.folder, "/data derived/bestmodels.RData"))
hasHMMdata    <- file.exists(paste0(experiment.folder, "/data derived/movcleaned.RData"))
hasgpkg       <- file.exists(paste0(experiment.folder, "/data derived/pardas_tiete_all_individuals.gpkg"))

if(!hasgpkg) { 
    data.importer(rawfolder = "./experiment 001/data derived/modifiedlocs", 
                  tempdir = paste0(experiment.folder,"/maps derived/observedstack"),
                  finalfolder = paste0(experiment.folder,"/maps derived/observedstack"),
                  gpkgfolder = paste0(experiment.folder, "/data derived"),
                  res = res 
                  )
}

if(!hasHMMdata) {
    HMMfitter(infile = paste0(experiment.folder, "/data derived/pardas_tiete_all_individuals.gpkg"),
              outfolder = paste0(experiment.folder, "/data derived") 
    )
}
if(!hasmodels) {
    ssfer(datafolder = paste0(experiment.folder, "/data derived"),
          mapfolder = paste0(experiment.folder, "/maps derived/observedstack"),
          outfolder = paste0(experiment.folder, "/data derived") )
}

if(!hasstudystack) {
    envpreparator( buffergeo = st_read("./raw/maps/area_estudo/area_estudo_SIRGAS2000_UTM22S.shp"),
               tempdir   =   paste0(experiment.folder, "/maps derived/studyarea"),
               finalfolder  = paste0(experiment.folder, "/maps derived/studyarea"),
               finalrdata= "lowres.RData",
               res=5000,
               overwrite.gb = TRUE
)
}
if(!hasprediction) {
    predictor(datafolder = paste0(experiment.folder, "/data derived"),
    mapfolder = paste0(experiment.folder, "/maps derived/studyarea"),
    outfolder = paste0(experiment.folder, "/maps derived/qualitypredictions")

}