### Exploration with species distribution modelling
## Intent: Our current models based on jaguars SSF are proving to behave very poorly, with AUC ~ 0.5
## I am starting to think this is a result of most animals are engaged in a dispersing movements.
## In this behavior the animal is focused in a long-term goal (reaching a good known location far away).
## Since the SSF mechanism is based on finding the next location based on the previous one, it behaves 
## as a short-sighted process, unable to preferences on scales larger than a step. 
## This view is supported by cos(turning angle) have such a big importance in Bernardo's models.
## This indicates that animal presence is determined mainly by whether or not they are keeping the same track.
## In other words animals are working towards a goal that is beyond the current step.
## In other words is like trying to find the optimal value using a greedy optimization algorithm  
## Using an RSF might be a solution to that, but this is also limited to the total movement radius of the species.
## That made me wonder if a classical SDM would not be an better solution than SSF/RSF. The code below attempt to have AUC
## over existing data.


options(java.parameters = "-Xmx1g" )
data = paste0(experiment.folder, "/data derived/mov.track.rds")
tempdir = paste0(experiment.folder, "/maps derived/observedstack")
outfile = paste0(experiment.folder, "/data derived/bestmodels.rds")


## Read values
mov.track <- readRDS(data)
mapstack  <- stack(list.files(tempdir,pattern="tif$",full.names=T))

#Convert locations to spatial Points
presences <- SpatialPoints(mov.track[,1:2],proj4string=attr(mov.track,"crs_"))

## Since there are a lot of NA space in the raster stack I feel I should make sure random
## pseudo-absences fall in a valid area. For that I will use random points with a mask

# For debug (mask)
#ext=extent(469723,470000,-1170000,-1131000)
#croppedstack <- crop(mapstack,ext)
mask <- subs(mapstack$landuse,data.frame(0,NA),subsWithNA=FALSE)

absences <- randomPoints(mask, 10000,p= presences)


# Run maxent code
model <- maxent(mas, presences, absences)

