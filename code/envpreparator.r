#' ---
#' title: 'Environmental variable calculat'
#' author: Jorge Menezes    - CENAP/ICMBio
#' ---

# Intent: This code creates the environment maps required by lightssf.r to run 
#         the stepselection functions. it assumes data from the FBDS, Canasat, Pastagens.org 
#         and roads have been download. It replaces much of the calculation done by Bernardo on
#         GRASS. I am recreating this data mostly because I forgot to download the maps from the 
#         CENAP before I left.


# Input:  The presence of the map files in the map folder within this project.
# Output: A raster brick with the environmental varibles calculated. See
#         trimestal report for definition

library(sf)
library(dplyr)
library(raster)
library(gdalUtils)
Sys.setenv(GDAL_DATA="C:\\Program Files\\QGIS 3.4\\share\\gdal")
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
        select(geometry, CLASSE_USO) %>%
        st_write(dsn="./maps/FBDS/SP/forestmap.gpkg",update=T)

# Next, we add pasture
pasto <- st_read(".\\maps\\Pasture\\pasture_2018.shp") %>% 
        st_transform(baseproj) %>% 
        filter(. ,c(st_intersects(. , studyarea, sparse=F))) %>%
        mutate(CLASSE_USO = "pastagem") %>%
        select(geometry, CLASSE_USO) %>%
        st_write(dsn="./maps/FBDS/SP/forestmap.gpkg",update=T)


# Finally adding road
road <- st_read(".\\maps\\Roads\\gROADS-v1-americas.shp") %>% 
        st_transform(baseproj) %>%
        filter(. ,c(st_intersects(. , studyarea, sparse=F))) %>%
        mutate(CLASSE_USO = "estradas") %>%
        dplyr::select(geometry, CLASSE_USO) %>%
        st_write(dsn="./maps/FBDS/SP/forestmap.gpkg",update=T)


# TODO: Make gdalrasterize commands specific for each land use 
# TODO: Find what is the best way to calculate proportion of forest (grass?, postgis?)

forestmap <- gdalUtils::gdal_rasterize(src_datasource = "./maps/FBDS/SP/forestmap.gpkg", 
                                       dst_filename = "./maps/FBDS/SP/forestmap.tif", burn = 1, init =0,
                                       output_Raster = T, tr= c(5,5), te = bbox)
                                       )

w200   <- focalWeight(forestmap, 200, "circle")
w1000  <- focalWeight(forestmap, 1000, "circle")
w5000  <- focalWeight(forestmap, 5000, "circle")
w10000 <- focalWeight(forestmap, 10000, "circle")


forestprop200   <- focal(forestmap, w200,   filename = "./maps/FBDS/SP/forestprop200.tif"  )
forestprop1000  <- focal(forestmap, w1000,  filename = "./maps/FBDS/SP/forestprop1000.tif" )
forestprop5000  <- focal(forestmap, w5000,  filename = "./maps/FBDS/SP/forestprop5000.tif" )
forestprop10000 <- focal(forestmap, w10000, filename = "./maps/FBDS/SP/forestprop10000.tif")

gdalUtils::gdalwarp("./maps/FBDS/SP/forestprop200.tif"  , "./maps/FBDS/SP/forestprop200r.tif"  , r="average", tr= c(30,30) )
gdalUtils::gdalwarp("./maps/FBDS/SP/forestprop1000.tif" , "./maps/FBDS/SP/forestprop1000r.tif" , r="average", tr= c(30,30) )
gdalUtils::gdalwarp("./maps/FBDS/SP/forestprop5000.tif" , "./maps/FBDS/SP/forestprop5000r.tif" , r="average", tr= c(30,30) )
gdalUtils::gdalwarp("./maps/FBDS/SP/forestprop10000.tif", "./maps/FBDS/SP/forestprop10000r.tif", r="average", tr= c(30,30) )






### FBDS 'distance to water' handle ###

# We have the same problem we had with proportion of forest. It is not possible to handle all the municipalities file in memory.
# However, this time rasterizing might not be a good strategy, since it will imply we will treat rivers as having being 30 m wide.
# So instead I one distance for each municipality and then take the mean. This way I can still process the vector in memory.

muni.folders <- list.dirs("./maps/FBDS/SP",recursive = F)
muni.water <-  lapply(paste0(muni.folders, "/HIDROGRAFIA"), list.files, pattern="shp$", full.names = T)
muni.water <-  unlist(muni.water)
muni.water <-  muni.water[!grepl("NASCENTES",muni.water)][1:2] # removes water springs, since they are already part of the river shapefile. check that

# Create the first raster, using the extent of the study area, already projected to albers. 
# The original file also has to be projected.
projected <- tempfile()
gdalUtils::ogr2ogr(muni.water[1], projected, spat = bbox, t_srs = baseproj) 
watermap <- gdalUtils::gdal_rasterize( src_datasource = projected, 
                                       dst_filename = "./maps/FBDS/SP/watermap.tif", burn = 1, init =0,
                                       output_Raster = T, tr= c(5,5), te = bbox
                                       )

for( a in 2:length(muni.water)) {
    gdalUtils::gdal_rasterize(src_datasource = muni.water[a], dst_filename = "./maps/FBDS/SP/watermap.tif", burn = 1)
 }

watermap  <- raster("./maps/FBDS/SP/watermap.tif")
waterdist <- distance(waterdist, filename = "./maps/FBDS/SP/waterdist.tif" )

  


### FBDS 'distance to cities' handle ###

muni.folders <- list.dirs("./maps/FBDS/SP",recursive = F)
muni.city <-  lapply(paste0(muni.folders, "/USO"), list.files, pattern="shp$", full.names = T)
muni.city <-  unlist(muni.city)

projected <- tempfile(fileext=".gpkg")
gdalUtils::ogr2ogr(muni.city[1], projected, spat = bbox, t_srs = baseproj, where ="CLASSE_USO IN ('área antropizada', 'área edificada') ")
citymap <- gdalUtils::gdal_rasterize( src_datasource = projected, 
                                       dst_filename = "./maps/FBDS/SP/citymap.tif", burn = 1, init =0,
                                       output_Raster = T, tr= c(5,5), te = bbox
                                       )
for( a in 2:length(muni.city)) {
    gdalUtils::gdal_rasterize(src_datasource = muni.city[a], dst_filename = "./maps/FBDS/SP/watermap.tif", burn = 1)
 }

citymap  <- raster("./maps/FBDS/SP/citymap.tif")
cityist <- distance(citydist, filename = "./maps/FBDS/SP/citydist.tif" )

