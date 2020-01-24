### Script for tunring a map of environtal variables from
### the study area in a prediction map

predictor <- function(models, tempdir, outfolder, qgis.folder, overwrite = FALSE) {

# set qgis environment
set_env(qgis.folder)

# Load best model object with best models for each animal
bestmodels <- readRDS(models)

# Load mapstack with pointers to each file
mapstack  <- stack(list.files(tempdir,pattern="tif$",full.names=T))



for(a in 1:nrow(bestmodels)) {
     #For debug:
     ext=extent(469723,470000,-1140213,-1131000)

     # get the first step_id_ for each individual from their models (it does not matter which I take)
     stepid <- bestmodels$fit[[1]]$model$model[1,"strata(step_id_)"] %>% as.character %>% sub("step_id_=","",.) %>% as.numeric
    
     # run the predictions
     # P.S: with na.rm=T predict will eliminate rows whenever there is a cell with NA or NaN, even if this cell is in a layer that is not part of the model.
     preds <-predict(object = mapstack, 
             model  = bestmodels$fit[[2]]$model,
             const  = data.frame(step_id_=stepid),
             type   = "risk",
             reference = "sample", 
             ext = ext,
             na.rm = FALSE, 
             progress = "text",
             factors = list(landuse =c(2,4,5,7,8)),
             filename = paste0(outfolder,"/qualitylinear_",bestmodels$Name[a],".tif")
             )
    
    # Take the exponent of the linear predictor
    run_qgis("grass7:r.mapcalc", maps = normalizePath(paste0(outfolder,"/qualitylinear_",bestmodels$Name[a],".tif")), 
                      expression = paste0("qualityexp_",bestmodels$Name[a],"=qualitylinear_",bestmodels$Name[a],"/(1+qualitylinear_",bestmodels$Name[a],")"),
                      output_dir = outfolder
                       )
    
}
  # Store filename
  predholder <- list.files(outfolder,pattern="qualityexp_",full.names=T)
  
  # Take average of all animals quality predictions
  expr <- paste0("meanquality=","(",paste(paste0("qualityexp_",bestmodels$Name),collapse="+"),")","/", length(bestmodels$Name))
  avgquality <- run_qgis("grass7:mapcalc", maps = normalizePath(predholder), expression = expr, output_dir = outfolder )

}





