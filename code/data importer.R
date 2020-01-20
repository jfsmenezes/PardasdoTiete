# ---
# title: Organization of Cougar and Jaguar GPS movement data
# author: Bernardo Niebuhr <bernardo_brandaum@yahoo.com.br>
#         Jorge Menezes    <jorgefernandosaraiva@gmail.com> 
# date: CENAP-ICMBio/Pro-Carnivoros, Atibaia, SP, Brazil, June 2019
# 
# ---

## Simplified Importer tool:
 ## Intent: This is a short, automated version of the file 00_organize_data_2018_06_d23.R
 ## whose purpose is to read the several csv files that have animal locations
 ## and  return a single csv files with standartized columns.        
 ## It also performs small data wrangling operations: formatting timestamps to POSIXct format,
 ## eliminating fixes before the capture of the individual, and order locations by time.
 
 ## Input: a directory with several csv files, along with a metadata.csv a column Tag_ID 
 ## with the ID of each animal, a release.date column and a name column. Other information on the
 ## may be added but it is not necessary.

 ## Output: A Geopackage with data of all animals, along with a ID and timestamp columns.


data.importer <-  function(derivdir, rawdir, tempdir, res, qgis.folder, crs =NULL) {
    ### Loading dependencies
    if(is.null(crs)) {
        crs <- '+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs'
    }

    meta.data <- read.csv2(paste0(rawdir,"/meta_data.csv"),stringsAsFactors = F)

    ### Load  xlsx file with all locations.
    # Convert timestamp from excel serial code to R's POSIXct
    # Also rename columns to more readable format
    # Finally changes the timezone to UTC, because read_xlsx assumes data are following the local timezone
    fixes <- read_xlsx(paste0(derivdir,"/Pardas_do_Tiete_todos_individuos.xlsx"))
    colnames(fixes) <- c("Name","timestamp","Latitude","Longitude")
    tz(fixes$timestamp) <- "UTC"
    

    ### Use a left join with meta.data to find releasedates and eliminate animals from it.
    # Also arrange by animal and then in cronological order, and eliminate duplicate rows.
    fixes <- fixes %>% 
            left_join(meta.data[,c("ID","release.date.utc","Name")], by="Name") %>%
            mutate(release.date.utc = as.POSIXct(strptime(release.date.utc,format="%d/%m/%Y %H:%M"))) %>%
            filter( timestamp >= release.date.utc, Latitude > -40) %>%
            arrange(ID, timestamp) %>%
            distinct() %>%
            dplyr::select( - release.date.utc)


    ### creating spatial object and converting it to Albers equal area
    fixes.geo <- st_as_sf(fixes, coords = c("Longitude","Latitude"), crs = 4326) %>%
                st_transform(crs=crs) %>%
                mutate(Longitude  = st_coordinates(.)[,1], Latitude = st_coordinates(.)[,2]) 
                
    st_write(fixes.geo, dsn=paste0(derivdir,"/pardas_tiete_all_individuals.gpkg"))

    ### creates the maps for extracting ssf values             
    st_buffer(fixes.geo, 20000) %>% 
    st_union() %>% 
    envpreparator(finalrds = "observedstack.rds", 
                  tempdir= tempdir, res= res,
                  qgis.folder = qgis.folder
                  )
    print("Generated gpkg with jaguar data!")
}

























