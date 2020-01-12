#' ---
#' 
#' title: 'Environmental variable calculator'
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


envpreparator <- function(buffergeo, tempdir="./maps", finalrds, res=30, overwrite.gb = TRUE, qgis.folder) {

#debug: 
# qgis.folder <- "C:/Program Files/QGIS 3.4"
# tempdir <- "./test/maps derived"
# finalfolder <- "./test/maps derived" 
# overwrite.gb <- FALSE
# res=2500
# finalrds = "observedstack.rds"

set_env(qgis.folder)
Sys.setenv(GDAL_DATA = paste0(qgis.folder, "\\share\\gdal"))
Sys.setenv(PROJ_LIB  = paste0(qgis.folder, "\\share\\proj"))
### Base raster layer ###
baseproj   <- "+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" 

#debug:
# buffergeo <- st_read("D:/Trabalho/pardas do tiete/PardasdoTiete/test/data derived/pardas_tiete_all_individuals.gpkg")
# buffergeo <- st_transform(buffergeo, crs=baseproj) %>% st_buffer(20000) %>% st_union()

studyarea  <- buffergeo
studyarea  <- st_transform( studyarea, baseproj)
bbox       <- st_bbox(studyarea)

### Creating land use database ###

# FBDS is a series of small maps each for each municipality. I am afraid it might be too much to hold
# in memory with a single file as a single object. My solution all forest features in a geopackage
# since I can update this format by adding one feature at a time. After that I can use gdal utils to
# rasterize this features at 30 m, also without having put the vector data on R.



landusebase <- paste0(tempdir,"/forestmap.gpkg")

if(file.exists(landusebase) && overwrite.gb | !file.exists(landusebase)) {

    muni.folders <- list.dirs("./raw/maps/FBDS/SP",recursive = F)
    muni.use <- lapply(paste0(muni.folders, "/USO"), list.files, pattern="shp$", full.names = T)
    muni.use <- sapply(muni.use, "[[", 1) # the municipality of cubatão has two shapes. This take one for everyone.

    pb <- txtProgressBar()
    for(a in 1:length(muni.use)) {
        adder(muni.use[a],  studyarea, baseproj, geofile= landusebase)
        setTxtProgressBar(pb, a/length(muni.use))
    }

    # I now execute the same code for river files, whose info is not contained in the FDBS use map

    muni.folders <- list.dirs("./raw/maps/FBDS/SP",recursive = F)
    muni.water <- lapply(paste0(muni.folders, "/HIDROGRAFIA"), list.files, pattern="shp$", full.names = T)
    muni.water <- unlist(muni.water)
    muni.water <- muni.water[!grepl("MASSAS", muni.water)] # remove water bodies because they are included in land use maps

    pb <- txtProgressBar()
    for(a in 1:length(muni.water)) {
        adder.water(muni.water[a], studyarea, baseproj, geofile=landusebase)
        setTxtProgressBar(pb, a/length(muni.water))
    }



# A similar principle is used for the canasat map
cana <- st_read(".\\raw\\maps\\canasat\\Cana2013_WGS_SP.shp") %>% 
        st_transform(baseproj) %>% 
        filter(. ,c(st_intersects(. , studyarea, sparse=F))) %>%
        mutate(CLASSE_USO = "canadeacucar") %>%
        dplyr::select(geometry, CLASSE_USO) %>%
        st_write(dsn= landusebase,update=T)

# Next, we add pasture
pasto <- st_read(".\\raw\\maps\\Pasture\\pasture_2018.shp") %>% 
        st_transform(baseproj) %>% 
        filter(. ,c(st_intersects(. , studyarea, sparse=F))) %>%
        mutate(CLASSE_USO = "pastagem") %>%
        dplyr::select(geometry, CLASSE_USO) %>%
        st_write(dsn=landusebase,update=T)


# Finally adding road
road <- st_read(".\\raw\\maps\\Roads\\gROADS-v1-americas.shp") %>% 
        st_transform(baseproj) %>%
        filter(. ,as.logical(rowSums(st_intersects(. , studyarea, sparse=F)))) %>%
        mutate(CLASSE_USO = "estradas") %>%
        dplyr::select(geometry, CLASSE_USO) %>%
        st_write(dsn=landusebase,update=T)

}
# Create a land use map using gdalrasterize commands specific for each land use 
  # This assume a implicit order in which the FBDS is overwritten by information from
  # canasat or from the pasture dataset. I opt for this system because these two maps 
  # are more precise. 
landuseraster <- paste0(tempdir, "/landuse_studyarea2.tif")



gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO='agua'", 
                           burn = 1, 
                           init = 0,
                           tr= rep(res,2), 
                           te = bbox,    
                           )
gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO IN ('area antropizada', 'area edificada', 'silvicultura')",
                           burn=2
                           )
gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO='formacao florestal'",
                           burn=4
                           )
gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO='formacao nao florestal'",
                           burn=5
                           )
gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO='canadeacucar'",
                           burn=7
                           )   
gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = landuseraster,
                           where = "CLASSE_USO='pastagem'",
                           burn=8
                           )     

# Create a distance to road and distance to road and water by creating a landuse map with those 
# characteristics and then using gdal_proximity to extract distances
watermap     <- paste0(tempdir,"/water.tif")
roadmap      <-  paste0(tempdir,"/estradas.tif")
waterproxmap <- paste0(tempdir,"/waterproxmap.tif")
roadproxmap  <-  paste0(tempdir,"/estradasproxmap.tif")

gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = watermap,
                           where = "CLASSE_USO='água'", 
                           burn = 1, 
                           init =0,
                           tr= rep(res,2), 
                           te = bbox 
                           )
run_qgis(alg = "gdal:proximity", INPUT = watermap, BAND=1, DATA_TYPE=5, UNITS = 0, OUTPUT = waterproxmap, load_output = FALSE )

gdalUtils::gdal_rasterize( src = landusebase, 
                           dst_filename = roadmap,
                           where = "CLASSE_USO='estradas'", 
                           burn = 1, 
                           init =0,
                           tr= rep(res,2), 
                           te = bbox 
                           )
run_qgis(alg = "gdal:proximity", INPUT = roadmap, BAND=1, DATA_TYPE=5, UNITS = 0, OUTPUT = roadproxmap, load_output = FALSE )


# Applies Log to these maps using GRASS
log_dist_water <- paste0(tempdir, "/log_dist_water.tif")
log_dist_roads <-  paste0(tempdir,"/log_dist_roads.tif")

run_qgis(alg = "grass7:r.mapcalc", maps = normalizePath(waterproxmap), expression = "log_dist_water=log(waterproxmap)", output_dir = tempdir())
run_qgis(alg = "grass7:r.mapcalc", maps = normalizePath(roadproxmap), expression = "log_dist_roads=log(estradasproxmap)", output_dir = tempdir())

file.copy( from = paste0(tempdir(),"\\log_dist_water.tif"), to = log_dist_water)
file.copy( from = paste0(tempdir(),"\\log_dist_roads.tif"), to = log_dist_roads)

### Run proportion calculations in GRASS using r.neighbors.####


type.name = c("forest","sugar", "pasture")
type.number = c(4,7,8)
sizes = c(100,500,2500,5000)

for( a in 1:length(type.name)) {
    for(b in 1:length(sizes)) {
        run_qgis(alg = "grass7:r.mapcalc", maps = normalizePath(landuseraster) , expression = paste0("binary=landuse_studyarea==",type.number[a]), output_dir = tempdir())
        
        if(ceiling(sizes[b]/res) == 1 ) {
            file.copy( from = paste0(tempdir(),"\\binary.tif"), 
                       to = paste0(tempdir, "/prop_",type.name[a],"_",sizes[b],"m.tif")
                       )

        } else{

            params = get_args_man("grass7:r.mfilter")
            params["input"] = paste0(tempdir(),"/binary.tif")
            params["filter"] = filter.maker(sizes[b],res, paste0(tempdir(), "/filter.txt" ))
            params["output"] = normalizePath(paste0(tempdir,"/prop_",type.name[a],"_",sizes[b],"m.tif"))

            run_qgis(alg = "grass7:r.mfilter", params=params)           
        }
    }
}

# Adding a dispresidency map on the end, in case we want to consider
# different effects
init(raster(landuseraster), fun=function(x) rep(1,x), filename = paste0(tempdir,"/dispresidency.tif"))

list.files(tempdir, pattern = "tif$",full.names=T)

mapstack <- stack( list.files(tempdir, pattern = "tif$",full.names=T))
mapstack <- addLayer(mapstack, mapstack[["dispresidency"]])

names(mapstack) = c(
    "dispdispersal"        ,
    "roads"                ,
    "dist_roads"           ,
    "landuse"              ,
    "log_dist_roads"       ,
    "log_dist_water"       ,
    "prop_forest_100m"     ,
    "prop_forest_2500m"    ,
    "prop_forest_5000m"    ,
    "prop_forest_500m"     ,
    "prop_pasture_100m"    ,
    "prop_pasture_2500m"   ,
    "prop_pasture_5000m"   ,
    "prop_pasture_500m"    ,
    "prop_sugarcane_100m"  ,
    "prop_sugarcane_2500m" ,
    "prop_sugarcane_5000m" ,
    "prop_sugarcane_500m"  ,
    "water"                ,
    "dist_water"           ,
    "dispresidency"   
)


# Save the pointers to the raster in a object for future reading 
saveRDS(mapstack, file = paste0(tempdir, "/",finalrds))
return(paste0(tempdir, "/",finalrds))

}

adder <- function(muni.use, studyarea, baseproj, geofile = "./maps/FBDS/SP/forestmap.gpkg") { 
    muni.shp <- st_read(muni.use, quiet = T)
    muni.shp <- st_transform(muni.shp, crs = baseproj)
    muni.shp <- muni.shp[ c(st_intersects(muni.shp, studyarea, sparse=F)), c("geometry", "CLASSE_USO") ]
    
    if(nrow(muni.shp)>0) {
        muni.shp$CLASSE_USO <- stri_trans_general(as.character(muni.shp$CLASSE_USO), "latin-ascii")    
        st_write(muni.shp, dsn=geofile ,update=T, quiet=T)
    }

}

adder.water <- function(muni.use, studyarea, baseproj, geofile = "./maps/FBDS/SP/forestmap.gpkg") { 
    muni.shp <- st_read(muni.use, quiet = T)
    if(nrow(muni.shp)==0) {return(NULL)} else{
    
        muni.shp <- st_transform(muni.shp, crs = baseproj)
        muni.shp <- cbind(muni.shp[,"geometry"], CLASSE_USO = "água")   
        muni.shp <- muni.shp[ c(st_intersects(muni.shp, studyarea, sparse=F)), c("geometry", "CLASSE_USO") ]
        if(nrow(muni.shp)>0) {
            muni.shp$CLASSE_USO <- stri_trans_general( as.character(muni.shp$CLASSE_USO) , "latin-ascii")
            st_write(muni.shp, dsn=geofile,update=T, quiet=T)
        }
    }
}

filter.maker <-  function(radius,res,filename) {
    diameter <- radius * 2
    diameter.px <- ceiling(diameter/res)
    if( (diameter.px %% 2)==0) diameter.px = diameter.px + 1
    write(paste("MATRIX", diameter.px),file=filename)
    write.table(matrix(1,diameter.px, diameter.px),file=filename,append=T,row.names=F,col.names=F)
    write("DIVISOR 0", file=filename,append=T)
    write("TYPE P", file=filename,append=T)
    return(filename)
}
