### Script for tunring a map of environtal variables from
### the study area in a prediction map

library(raster)
library(RSAGA)
library(parallel)
# set SAGA to use all but one available cores
#env <- rsaga.env(cores = detectCores() - 1)
env <- rsaga.env()
source("./code/acessory functions.r")

# calculate study area predictions
envpreparator( buffergeo = st_read("./maps/area_estudo/area_estudo_SIRGAS2000_UTM22S.shp"),
               tempdir   = "./maps/low-resolution",
               finalfolder  = "./maps/low-resolution",
               finalrdata= "lowres.RData",
               res=5000,
               overwrite.gb = FALSE
)




# Load best model object with best models for each animal
load("./data/derived/bestmodels.RData")

# Load mapstack with pointers to each file
load("./maps/low-resolution/lowres.RData")

for(a in 1:nrow(bestmodels)) {
    coefs <- coef(bestmodels$fit[[a]]$model)


    # Check if there is any coefficient who does not have predicted data
    notinteractions <- names(coefs)[!grepl(":", names(coefs))]
    stopifnot( all( notinteractions %in% names(mapstack) ) )

    # Grab all relevant variables in the same order as the coefficient
    used.stack <- mapstack[[notinteractions]]
    used.stack <- stack(used.stack) #enforces the object is a Rasterstack to avoid different handle in case we get a Raster layer from previous line (happens when there is only one variable in the model)
    
    predict(used.stack, bestmodels$fit[[a]]$model, fun=specialpredict,
            filename=paste0("./maps/low-resolution/qualitylinear",a,".sdat") )

    
    # Finish with expected number of events
    final <- rsaga.grid.calculus( paste0("./maps/low-resolution/qualitylinear",a) , 
                                 out.grid = paste0("./maps/low-resolution/qualityexp",a), 
                                 formula = "(2.7182818^a)/(1+2.7182818^a)", 
                                 intern=FALSE 
                                 )
}

# generate a final mean of all predict maps (i.e. across all individuals).
qualitymaps <- stack(paste0("./maps/low-resolution/qualityexp",1:nrow(bestmodels),".sdat"))
meanquality <- overlay(qualitymaps,fun=mean,filename ="./maps/low-resolution/qualityexpmean.sdat" )
crs(meanquality) <- crs(mapstack)










