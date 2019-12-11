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



# TODO: Find what is the best way to calculate proportion of forest (grass?, postgis?)

landuse <- raster("landuse_studyarea.tif")

w100  <- focalWeight(landuse,  100, "circle")
w500  <- focalWeight(landuse,  500, "circle")
w2500 <- focalWeight(landuse, 2500, "circle")
w5000 <- focalWeight(landuse, 5000, "circle")


forestprop100  <- focal(landuse==1, fun=mean, w100,  filename = "./maps/FBDS/SP/forestprop100.tif"  )
forestprop500  <- focal(landuse==1, fun=mean, w500,  filename = "./maps/FBDS/SP/forestprop500.tif"  )
forestprop2500 <- focal(landuse==1, fun=mean, w2500, filename = "./maps/FBDS/SP/forestprop2500.tif" )
forestprop5000 <- focal(landuse==1, fun=mean, w5000, filename = "./maps/FBDS/SP/forestprop5000.tif" )

sugarprop100  <- focal(landuse==7, fun=mean, w100,   filename = "./maps/FBDS/SP/sugarprop100.tif"  )
sugarprop500  <- focal(landuse==7, fun=mean, w500,   filename = "./maps/FBDS/SP/sugarprop500.tif"  )
sugarprop2500 <- focal(landuse==7, fun=mean, w2500,  filename = "./maps/FBDS/SP/sugarprop2500.tif" )
sugarprop5000 <- focal(landuse==7, fun=mean, w5000,  filename = "./maps/FBDS/SP/sugarprop5000.tif" )

cattleprop100  <- focal(landuse==8, fun=mean, w100,  filename = "./maps/FBDS/SP/cattleprop100.tif" )
cattleprop500  <- focal(landuse==8, fun=mean, w500,  filename = "./maps/FBDS/SP/cattleprop500.tif" )
cattleprop2500 <- focal(landuse==8, fun=mean, w2500, filename = "./maps/FBDS/SP/cattleprop2500.tif")
cattleprop5000 <- focal(landuse==8, fun=mean, w5000, filename = "./maps/FBDS/SP/cattleprop5000.tif")


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

