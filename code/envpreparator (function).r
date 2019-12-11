#' ---
#' title: 'Environmental variable calculat'
#' author: Jorge Menezes    - CENAP/ICMBio
#' ---

# Intent: This code creates the environment maps required by lightssf.r to run 
#         the stepselection functions. it assumes data from the FBDS, Canasat, Pastagens.org 
#         and roads have been download in Maps folder. It replaces much of the calculation done by Bernardo on
#         GRASS. I am recreating this data mostly because I forgot to download the maps from the 
#         CENAP before I left. I created it has a function to allow calculating it for study area or for
#         a buffer around existing points.


# Input:  The presence of the map files in the map folder within this project.
# Output: A raster brick with the environmental varibles calculated. See
#         trimestal report for definition


envpreparator <- function(buffergeo, tempdir="./maps", finalfolder="./maps", res=30 ) {

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


studyarea  <- buffergeo
studyarea  <- st_transform( studyarea, baseproj)
bbox       <- st_bbox(studyarea)

### Creating land use database ###

# FBDS is a series of small maps each for each municipality. I am afraid it might be too much to hold
# in memory with a single file as a single object. My solution all forest features in a geopackage
# since I can update this format by adding one feature at a time. After that I can use gdal utils to
# rasterize this features at 30 m, also without having put the vector data on R.
landusebase <- paste0(tempdir,"/forestmap.gpkg")



muni.folders <- list.dirs("./maps/FBDS/SP",recursive = F)
muni.use <- lapply(paste0(muni.folders, "/USO"), list.files, pattern="shp$", full.names = T)
muni.use <- sapply(muni.use, "[[", 1) # the municipality of cubatão has two shapes. This take one for everyone.

pb <- txtProgressBar()
for(a in 1:length(muni.use)) {
    adder(muni.use[a], baseproj, geofile= landusebase)
    setTxtProgressBar(pb, a/length(muni.use))
}

# I now execute the same code for river files, whose info is not contained in the FDBS use map

muni.folders <- list.dirs("./maps/FBDS/SP",recursive = F)
muni.water <- lapply(paste0(muni.folders, "/HIDROGRAFIA"), list.files, pattern="shp$", full.names = T)
muni.water <- unlist(muni.water)
muni.water <- muni.water[!grepl("MASSAS", muni.water)] # remove water bodies because they are included in land use maps

pb <- txtProgressBar()
for(a in 1:length(muni.water)) {
    adder.water(muni.water[a], baseproj, geofile=landusebase)
    setTxtProgressBar(pb, a/length(muni.water))
}

# A similar principle is used for the canasat map
cana <- st_read(".\\maps\\canasat\\Cana2013_WGS_SP.shp") %>% 
        st_transform(baseproj) %>% 
        filter(. ,c(st_intersects(. , studyarea, sparse=F))) %>%
        mutate(CLASSE_USO = "cana-de-açucar") %>%
        dplyr::select(geometry, CLASSE_USO) %>%
        st_write(dsn= landusebase,update=T)

# Next, we add pasture
pasto <- st_read(".\\maps\\Pasture\\pasture_2018.shp") %>% 
        st_transform(baseproj) %>% 
        filter(. ,c(st_intersects(. , studyarea, sparse=F))) %>%
        mutate(CLASSE_USO = "pastagem") %>%
        dplyr::select(geometry, CLASSE_USO) %>%
        st_write(dsn=landusebase,update=T)


# Finally adding road
road <- st_read(".\\maps\\Roads\\gROADS-v1-americas.shp") %>% 
        st_transform(baseproj) %>%
        filter(. ,c(st_intersects(. , studyarea, sparse=F))) %>%
        mutate(CLASSE_USO = "estradas") %>%
        dplyr::select(geometry, CLASSE_USO) %>%
        st_write(dsn=landusebase,update=T)


# Create a land use map using gdalrasterize commands specific for each land use 
  # This assume a implicit order in which the FBDS is overwritten by information from
  # canasat or from the pasture dataset. I opt for this system because these two maps 
  # are more precise. 
landuseraster <- paste0(tempdir, "/landuse_studyarea.tif")



gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO='água'", 
                           burn = 1, 
                           init =0,
                           tr= rep(res,2), 
                           te = bbox 
                           )
gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO IN ('área antropizada', 'área edificada', 'silvicultura')",
                           burn=2
                           )
gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO='formação florestal'",
                           burn=4
                           )
gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO='formação não florestal'",
                           burn=5
                           )
gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO='cana-de-açucar'",
                           burn=7
                           )   
gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO='pastagem'",
                           burn=8
                           )       

# Create a distance to road and distance to road and water by creating a landuse map with those 
# characteristics and then using gdal_proximity to extract distances
watermap <- paste0(tempdir,"/water.tif")
roadmap <-  paste0(tempdir,"/estradas.tif")
waterproxmap <- paste0(tempdir,"/waterprox.tif")
roadproxmap <-  paste0(tempdir,"/estradasprox.tif")

gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = watermap,
                           where = "CLASSE_USO='água'", 
                           burn = 1, 
                           init =0,
                           tr= rep(res,2), 
                           te = bbox 
                           )
command <- paste("py3_env && gdal_proximity", watermap, waterproxmap,"-distunits GEO")
shell(command)

gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = roadmap,
                           where = "CLASSE_USO='estradas'", 
                           burn = 1, 
                           init =0,
                           tr= rep(res,2), 
                           te = bbox 
                           )

command <- paste("py3_env && gdal_proximity", roadmap, roadproxmap,"-distunits GEO")
shell(command)

### Run proportion calculations in SAGA.####

 # Focal statistics in the raster package are very slow so I opted to use
 # a C-based GIS software. GRASS was proving to be a hell to configure, 
 # likewise for GDAL. GDAL is also very limited. In the end SAGA proved 
 # to be the best outcome.

onlyforest  <- paste0(tempdir,"./maps/onlyforest", )
onlysugar   <- paste0(tempdir,"./maps/onlysugar",  )
onlypasture <- paste0(tempdir,"./maps/onlypasture" )



binaryzer(landuseraster ,class = 4, filename = onlyforest  ,env=env)
binaryzer(landuseraster ,class = 7, filename = onlysugar   ,env=env)
binaryzer(landuseraster ,class = 8, filename = onlypasture ,env=env)

# Execute a low-pass filter (a.k.a. a mean of the values). This is matematically equivalent 
# to calculate the proportions of 1-cells in the radius. In more biological terms this is equivalent
# calculte the proportion of sugarcane, of forest and of cattle on the region.
# Radius is given in number of pixels, so I simply calculated np = m/res, with res being the resolution
# in m. 

onlyforest100m   <-   paste0(finalfolder, "/onlyforest100m.sgrd")
onlyforest500m   <-   paste0(finalfolder, "/onlyforest500m.sgrd")
onlyforest2500m  <-   paste0(finalfolder, "/onlyforest2500m.sgrd")
onlyforest5000m  <-   paste0(finalfolder, "/onlyforest5000m.sgrd")
onlysugar100m    <-   paste0(finalfolder, "/onlysugar100m.sgrd")  
onlysugar500m    <-   paste0(finalfolder, "/onlysugar500m.sgrd")  
onlysugar2500m   <-   paste0(finalfolder, "/onlysugar2500m.sgrd") 
onlysugar5000m   <-   paste0(finalfolder, "/onlysugar5000m.sgrd") 
onlycattle100m   <-   paste0(finalfolder, "/onlycattle100m.sgrd")
onlycattle500m   <-   paste0(finalfolder, "/onlycattle500m.sgrd")
onlycattle2500m  <-   paste0(finalfolder, "/onlycattle2500m.sgrd")
onlycattle5000m  <-   paste0(finalfolder, "/onlycattle5000m.sgrd")



rsaga.filter.simple(onlyforest, onlyforest100m  , method = "smooth", radius = round(100/res) , env=env)
rsaga.filter.simple(onlyforest, onlyforest500m  , method = "smooth", radius = round(500/res) , env=env)
rsaga.filter.simple(onlyforest, onlyforest2500m , method = "smooth", radius = round(2500/res), env=env)
rsaga.filter.simple(onlyforest, onlyforest5000m , method = "smooth", radius = round(5000/res), env=env)



rsaga.filter.simple(onlysugar, onlysugar100m  , method = "smooth", radius = round(100/res) , env=env)
rsaga.filter.simple(onlysugar, onlysugar500m  , method = "smooth", radius = round(500/res) , env=env)
rsaga.filter.simple(onlysugar, onlysugar2500m , method = "smooth", radius = round(2500/res), env=env)
rsaga.filter.simple(onlysugar, onlysugar5000m , method = "smooth", radius = round(5000/res), env=env)

 

rsaga.filter.simple(onlycattle, onlycattle100m  , method = "smooth", radius = round(100/res) , env=env)
rsaga.filter.simple(onlycattle, onlycattle500m  , method = "smooth", radius = round(500/res) , env=env)
rsaga.filter.simple(onlycattle, onlycattle2500m , method = "smooth", radius = round(2500/res), env=env)
rsaga.filter.simple(onlycattle, onlycattle5000m , method = "smooth", radius = round(5000/res), env=env)


mapstack <- stack(
    landuseraster,
    waterproxmap,
    roadproxmap,
    onlyforest100m ,
    onlyforest500m ,
    onlyforest2500m,
    onlyforest5000m,
    onlysugar100m  ,
    onlysugar500m  ,
    onlysugar2500m ,
    onlysugar5000m ,
    onlycattle100m ,
    onlycattle500m ,
    onlycattle2500m,
    onlycattle5000m
)
save(mapstack, file = paste0(finalfolder, "maps.RData"))



}

adder <- function(muni.use, baseproj, geofile = "./maps/FBDS/SP/forestmap.gpkg") { 
    muni.shp <- st_read(muni.use, quiet = T)
    muni.shp <- st_transform(muni.shp, crs = baseproj)
    muni.shp <- muni.shp[ c(st_intersects(muni.shp, studyarea, sparse=F)), c("geometry", "CLASSE_USO") ]
    if(nrow(muni.shp)>0) {    
        st_write(muni.shp, dsn=geofile ,update=T, quiet=T)
    }

}
adder.water <- function(muni.use, baseproj, geofile = "./maps/FBDS/SP/forestmap.gpkg") { 
    muni.shp <- st_read(muni.use, quiet = T)
    if(nrow(muni.shp)==0) {return(NULL)} else{
    
        muni.shp <- st_transform(muni.shp, crs = baseproj)
        muni.shp <- cbind(muni.shp[,"geometry"], CLASSE_USO = "água")   
        muni.shp <- muni.shp[ c(st_intersects(muni.shp, studyarea, sparse=F)), c("geometry", "CLASSE_USO") ]
        if(nrow(muni.shp)>0) {
            st_write(muni.shp, dsn=geofile,update=T, quiet=T)
        }
    }
}
binaryzer <- function(landuse, class, filename,env) {
  rsaga.import.gdal(landuse,env = env)
  landuse.saga <- sub("tif$","sgrd",landuse)
  rsaga.geoprocessor("grid_tools",15,
                     list(INPUT = landuse.saga, RESULT = filename, METHOD=0, OLD= class, NEW =1, OTHERS = 0, OTHEROPT=""),
                     env=env,display.command = T,check.parameters = F)
}
