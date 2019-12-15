#' ---
#' title: 'Environmental variable calculat'
#' author: Jorge Menezes    - CENAP/ICMBio
#' ---

# Intent: This code creates the environment maps required by lightssf.r to run 
#         the stepselection functions. it assumes data from the FBDS, Canasat, Pastagens.org 
#         and roads have been download in Maps folder. It replaces much of the calculation done by Bernardo on
#         GRASS. I am recreating this data mostly because I forgot to download the maps from the 
#         CENAP before I left.


# Input:  The presence of the map files in the map folder within this project.
# Output: A raster brick with the environmental varibles calculated. See
#         trimestal report for definition

library(sf)
library(dplyr)
library(raster)
library(gdalUtils)
library(RSAGA)
library(parallel)

Sys.setenv(GDAL_DATA="C:\\Program Files\\QGIS 3.4\\share\\gdal")
env <- rsaga.env(cores= detectCores()-1 )

### Base raster layer ###
baseproj   <- "+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" 


studyarea  <- st_read("./maps/area_estudo/area_estudo_SIRGAS2000_UTM22S.shp")
studyarea  <- st_transform( studyarea, baseproj)
bbox       <- st_bbox(studyarea)


### Creating land use database ###

# FBDS is a series of small maps each for each municipality. I am afraid it might be too much to hold
# in memory with a single file as a single object. My solution all forest features in a geopackage
# since I can update this format by adding one feature at a time. After that I can use gdal utils to
# rasterize this features at 30 m, also without having put the vector data on R.

adder <- function(muni.use, baseproj) { 
    muni.shp <- st_read(muni.use, quiet = T)
    muni.shp <- st_transform(muni.shp, crs = baseproj)
    muni.shp <- muni.shp[ c(st_intersects(muni.shp, studyarea, sparse=F)), c("geometry", "CLASSE_USO") ]
    if(nrow(muni.shp)>0) {    
        st_write(muni.shp, dsn="./maps/FBDS/SP/forestmap.gpkg",update=T, quiet=T)
    }

}

muni.folders <- list.dirs("./maps/FBDS/SP",recursive = F)
muni.use <- lapply(paste0(muni.folders, "/USO"), list.files, pattern="shp$", full.names = T)
muni.use <- sapply(muni.use, "[[", 1) # the municipality of cubatão has two shapes. This take one for everyone.

pb <- txtProgressBar()
for(a in 1:length(muni.use)) {
    adder(muni.use[a], baseproj)
    setTxtProgressBar(pb, a/length(muni.use))
}

# I now execute the same code for river files, whose info is not contained in the FDBS use map

adder.water <- function(muni.use, baseproj) { 
    muni.shp <- st_read(muni.use, quiet = T)
    if(nrow(muni.shp)==0) {return(NULL)} else{
    
        muni.shp <- st_transform(muni.shp, crs = baseproj)
        muni.shp <- cbind(muni.shp[,"geometry"], CLASSE_USO = "água")   
        muni.shp <- muni.shp[ c(st_intersects(muni.shp, studyarea, sparse=F)), c("geometry", "CLASSE_USO") ]
        if(nrow(muni.shp)>0) {
            st_write(muni.shp, dsn="./maps/FBDS/SP/forestmap.gpkg",update=T, quiet=T)
        }
    }
}

muni.folders <- list.dirs("./maps/FBDS/SP",recursive = F)
muni.water <- lapply(paste0(muni.folders, "/HIDROGRAFIA"), list.files, pattern="shp$", full.names = T)
muni.water <- unlist(muni.water)
muni.water <- muni.water[!grepl("MASSAS", muni.water)] # remove water bodies because they are included in land use maps

pb <- txtProgressBar()
for(a in 1:length(muni.water)) {
    adder.water(muni.water[a], baseproj)
    setTxtProgressBar(pb, a/length(muni.water))
}

# A similar principle is used for the canasat map
cana <- st_read(".\\maps\\canasat\\Cana2013_WGS_SP.shp") %>% 
        st_transform(baseproj) %>% 
        filter(. ,c(st_intersects(. , studyarea, sparse=F))) %>%
        mutate(CLASSE_USO = "cana-de-açucar") %>%
        dplyr::select(geometry, CLASSE_USO) %>%
        st_write(dsn="./maps/FBDS/SP/forestmap.gpkg",update=T)

# Next, we add pasture
pasto <- st_read(".\\maps\\Pasture\\pasture_2018.shp") %>% 
        st_transform(baseproj) %>% 
        filter(. ,c(st_intersects(. , studyarea, sparse=F))) %>%
        mutate(CLASSE_USO = "pastagem") %>%
        dplyr::select(geometry, CLASSE_USO) %>%
        st_write(dsn="./maps/FBDS/SP/forestmap.gpkg",update=T)


# Finally adding road
road <- st_read(".\\maps\\Roads\\gROADS-v1-americas.shp") %>% 
        st_transform(baseproj) %>%
        filter(. ,c(st_intersects(. , studyarea, sparse=F))) %>%
        mutate(CLASSE_USO = "estradas") %>%
        dplyr::select(geometry, CLASSE_USO) %>%
        st_write(dsn="./maps/FBDS/SP/forestmap.gpkg",update=T)


# Create a land use map using gdalrasterize commands specific for each land use 
  # This assume a implicit order in which the FBDS is overwritten by information from
  # canasat or from the pasture dataset. I opt for this system because these two maps 
  # are more precise. 

gdalUtils::gdal_rasterize( src = "./maps/FBDS/SP/forestmap.gpkg", 
                           dst_filename = "landuse_studyarea.tif",
                           where = "CLASSE_USO='água'", 
                           burn = 1, 
                           init =0,
                           tr= c(5,5), 
                           te = bbox 
                           )
gdalUtils::gdal_rasterize( src = "./maps/FBDS/SP/forestmap.gpkg", 
                           dst_filename = "landuse_studyarea.tif",
                           where = "CLASSE_USO IN ('área antropizada', 'área edificada', 'silvicultura')",
                           burn=2
                           )
gdalUtils::gdal_rasterize( src = "./maps/FBDS/SP/forestmap.gpkg", 
                           dst_filename = "landuse_studyarea.tif",
                           where = "CLASSE_USO='formação florestal'",
                           burn=4
                           )
gdalUtils::gdal_rasterize( src = "./maps/FBDS/SP/forestmap.gpkg", 
                           dst_filename = "landuse_studyarea.tif",
                           where = "CLASSE_USO='formação não florestal'",
                           burn=5
                           )
gdalUtils::gdal_rasterize( src = "./maps/FBDS/SP/forestmap.gpkg", 
                           dst_filename = "landuse_studyarea.tif",
                           where = "CLASSE_USO='cana-de-açucar'",
                           burn=7
                           )   
gdalUtils::gdal_rasterize( src = "./maps/FBDS/SP/forestmap.gpkg", 
                           dst_filename = "landuse_studyarea.tif",
                           where = "CLASSE_USO='pastagem'",
                           burn=8
                           )                           

# Create a distance to road and distance to road and water by creating a landuse map with those 
# characteristics and then using gdal_proximity to extract distances
gdalUtils::gdal_rasterize( src = "./maps/FBDS/SP/forestmap.gpkg", 
                           dst_filename = "./maps/water.tif",
                           where = "CLASSE_USO='água'", 
                           burn = 1, 
                           init =0,
                           tr= c(5,5), 
                           te = bbox 
                           )
shell("py3_env && gdal_proximity ./maps/water.tif ./maps/waterprox.tif -distunits GEO")

gdalUtils::gdal_rasterize( src = "./maps/FBDS/SP/forestmap.gpkg", 
                           dst_filename = "./maps/estradas.tif",
                           where = "CLASSE_USO='estradas'", 
                           burn = 1, 
                           init =0,
                           tr= c(5,5), 
                           te = bbox 
                           )
shell("py3_env && gdal_proximity ./maps/estradas.tif ./maps/roadprox.tif -distunits GEO")

## Decrease resolution to run focal analysis

system("C:\\OSGeo4W64\\bin\\gdalwarp.exe ./maps/landuse_studyarea.tif ./maps/landuse_studyarea30m.tif -tr 30 30 -r \"near\" ")

### Run proportion calculations in SAGA.####

 # Focal statistics in the raster package are very slow so I opted to use
 # a C-based GIS software. GRASS was proving to be a hell to configure, 
 # likewise for GDAL. GDAL is also very limited. In the end SAGA proved 
 # to be the best outcome.
binaryzer <- function(landuse, class, filename,env) {
  rsaga.import.gdal(landuse,env = env)
  landuse.saga <- sub("tif$","sgrd",landuse)
  rsaga.geoprocessor("grid_tools",15,
                     list(INPUT = landuse.saga, RESULT = filename, METHOD=0, OLD= class, NEW =1, OTHERS = 0, OTHEROPT=""),
                     env=env,display.command = T,check.parameters = F)
}

binaryzer("./maps/landuse_studyarea30m.tif" ,class = 4, filename = "./maps/onlyforest", env=env)
binaryzer("./maps/landuse_studyarea30m.tif" ,class = 7, filename = "./maps/onlysugar",  env=env)
binaryzer("./maps/landuse_studyarea30m.tif" ,class = 8, filename = "./maps/onlypasture",env=env)

# Execute a low-pass filter (a.k.a. a mean of the values). This is matematically equivalent 
# to calculate the proportions of 1-cells in the radius. In more biological terms this is equivalent
# calculte the proportion of sugarcane, of forest and of cattle on the region.
# Radius is given in number of pixels, so I simply calculated np = m/res, with res being the resolution
# in m. 

rsaga.filter.simple("./maps/onlyforest.sgrd", "./maps/onlyforest100m.sgrd",  method = "smooth", radius = 100/30, env=env)
rsaga.filter.simple("./maps/onlyforest.sgrd", "./maps/onlyforest500m.sgrd",  method = "smooth", radius = 500/30, env=env)
rsaga.filter.simple("./maps/onlyforest.sgrd", "./maps/onlyforest2500m.sgrd", method = "smooth", radius = 2500/30, env=env)
rsaga.filter.simple("./maps/onlyforest.sgrd", "./maps/onlyforest5000m.sgrd", method = "smooth", radius = 5000/30, env=env)



rsaga.filter.simple("./maps/onlysugar.sgrd", "./maps/onlysugar100m.sgrd",  method = "smooth", radius = 100/30, env=env)
rsaga.filter.simple("./maps/onlysugar.sgrd", "./maps/onlysugar500m.sgrd",  method = "smooth", radius = 500/30, env=env)
rsaga.filter.simple("./maps/onlysugar.sgrd", "./maps/onlysugar2500m.sgrd", method = "smooth", radius = 2500/30, env=env)
rsaga.filter.simple("./maps/onlysugar.sgrd", "./maps/onlysugar5000m.sgrd", method = "smooth", radius = 5000/30, env=env)



rsaga.filter.simple("./maps/onlycattle.sgrd", "./maps/onlycattle100m.sgrd",  method = "smooth", radius = 100/30, env=env)
rsaga.filter.simple("./maps/onlycattle.sgrd", "./maps/onlycattle500m.sgrd",  method = "smooth", radius = 500/30, env=env)
rsaga.filter.simple("./maps/onlycattle.sgrd", "./maps/onlycattle2500m.sgrd", method = "smooth", radius = 2500/30, env=env)
rsaga.filter.simple("./maps/onlycattle.sgrd", "./maps/onlycattle5000m.sgrd", method = "smooth", radius = 5000/30, env=env)


### Reproject previous maps to 30m ###
system("C:\\OSGeo4W64\\bin\\gdalwarp.exe ./maps/waterprox.tif ./maps/waterprox30m.tif -tr 30 30 -r \"bilinear\" ")
system("C:\\OSGeo4W64\\bin\\gdalwarp.exe ./maps/roadprox.tif ./maps/roadprox30m.tif -tr 30 30 -r \"bilinear\" ")

### Build final map stack and save on pointer (R object in RData)
predmaps <- stack(
    "./maps/landuse_studyarea30m.tif",
    "./maps/waterprox30m.tif",
    "./maps/roadprox30m.tif",
    "./maps/onlyforest100m.sgrd",
    "./maps/onlyforest500m.sgrd",
    "./maps/onlyforest2500m.sgrd",
    "./maps/onlyforest5000m.sgrd",
    "./maps/onlysugar100m.sgrd", 
    "./maps/onlysugar500m.sgrd", 
    "./maps/onlysugar2500m.sgrd",
    "./maps/onlysugar5000m.sgrd",
    "./maps/onlycattle100m.sgrd", 
    "./maps/onlycattle500m.sgrd", 
    "./maps/onlycattle2500m.sgrd",
    "./maps/onlycattle5000m.sgrd"
     )
save(predmaps, filename = paste0(finalfolder, "./maps.RData")