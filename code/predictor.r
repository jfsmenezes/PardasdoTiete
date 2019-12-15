### Script for tunring a map of environtal variables from
### the study area in a prediction map

predictor <- function(datafolder, mapsfolder,outfolder) {

library(raster)
library(RSAGA)
library(parallel)
# set SAGA to use all but one available cores
#env <- rsaga.env(cores = detectCores() - 1)
env <- rsaga.env()
source("./code/acessory functions.r")


# Load best model object with best models for each animal
load(paste0(datafolder,"/bestmodels.RData"))

# Load mapstack with pointers to each file
load(list.files(mapsfolder,pattern="RData",full.names=T)[1])

for(a in 1:nrow(bestmodels)) {
    coefs <- coef(bestmodels$fit[[a]]$model)


    # Check if there is any coefficient who does not have predicted data
    notinteractions <- names(coefs)[!grepl(":", names(coefs))]
    stopifnot( all( notinteractions %in% names(mapstack) ) )

    # Grab all relevant variables in the same order as the coefficient
    used.stack <- mapstack[[notinteractions]]
    used.stack <- stack(used.stack) #enforces the object is a Rasterstack to avoid different handle in case we get a Raster layer from previous line (happens when there is only one variable in the model)
    
    predict(used.stack, bestmodels$fit[[a]]$model, fun=specialpredict,
            filename=paste0(outfolder,"/qualitylinear",a,".sdat") )

    
    # Finish with expected number of events
    final <- rsaga.grid.calculus( paste0(outfolder,"/qualitylinear",a) , 
                                 out.grid = paste0(outfolder,"/qualityexp",a), 
                                 formula = "(2.7182818^a)/(1+2.7182818^a)", 
                                 intern=FALSE 
                                 )
}

# generate a final mean of all predict maps (i.e. across all individuals).
qualitymaps <- stack(paste0(mapsfolder,"/qualityexp",1:nrow(bestmodels),".sdat"))
meanquality <- overlay(qualitymaps,fun=mean,filename = paste0(outfolder,"/qualityexpmean.sdat") )
crs(meanquality) <- crs(mapstack)

}








