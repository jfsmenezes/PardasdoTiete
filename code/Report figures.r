# Script to generate pictures for December/2019 trimestral report #


# Load dependencies
library(sf)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(dplyr)

locs <- st_read("./experiment 001/dataderived/pardas_tiete_all_individuals.gpkg")
metadata<- read.csv2("./experiment 001/dataderived/modifiedlocs/meta_data.csv")
## Figure 1: Period of collar activity from all the animals
## (data from December/2019)
ggplot(locs,aes(x=timestamp, y=Name,col=Name)) + geom_point() +theme_bw() +xlab("tempo")+ylab("Nome do Animal")

# Table 1: Duration, sex, beggining of sample, end of sample, and  sample size for each animal

table1 <- data.frame(ID = unique(locs$ID), 
           Nome = as.character( metadata$Name[match(unique(locs$ID), metadata$ID)] ) ,
           Sexo =  as.character( metadata$sex[match(unique(locs$ID), metadata$ID)] ),
           begin= format(as.POSIXct(tapply(locs$timestamp, locs$ID, min), origin = "1970-01-01 00:00.00 UTC" ),"%d/%m/%Y"),
           end  = format(as.POSIXct(tapply(locs$timestamp, locs$ID, max), origin = "1970-01-01 00:00.00 UTC" ),"%d/%m/%Y"),
           duration = round( difftime(
                             as.POSIXct(tapply(locs$timestamp, locs$ID, max),origin = "1970-01-01 00:00.00 UTC"), 
                             as.POSIXct(tapply(locs$timestamp, locs$ID, min),origin = "1970-01-01 00:00.00 UTC"),
                             units="days")
                             ,0),
            n=as.numeric(table(locs$ID))
)
write.xlsx(table1,file="./presentations/Relatorio trimestral 2019_12/table1.xlsx")

## Figure 2: Plotting animal locations on top of a map of the São paulo and of the study area
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
boundaries<- readRDS("./maps/limites politicos/base gdam nivel 2.rds")
boundaries<- boundaries[boundaries$NAME_1=="São Paulo",]
boundsp <- st_union(boundaries)
studyarea <- st_read("./maps/area_estudo/area_estudo_SIRGAS2000_UTM22S.shp") %>% st_transform(crs=4326)
plot(boundaries$geometry,border="gray50",lty=1)
plot(boundsp,lwd=2,add=T)
plot(studyarea$geometry, lwd=2,lty=2,add=T)
plot(locs["Name"] %>% st_transform(crs=4326),pch=16,pal=gg_color_hue(6),add=T)
legend("topright",legend=sort(unique(locs$Name)),fill=gg_color_hue(6))

## Figure 3: Individualized plot of animal trajectories.
ggplot(locs,aes(x=Longitude,y=Latitude)) +geom_point()+ facet_wrap(~Name,scales="free") +theme_bw() +scale_x_continuous(labels=NULL) +scale_y_continuous(labels=NULL)

## Figure 4: Plot of average distance from initial points per day, for each individual
first.captures <- locs %>% group_by(Name) %>% arrange(timestamp) %>% slice(1) %>% arrange(ID)
distances <- st_distance(locs, first.captures)
distances <- as.numeric(distances[cbind( 1:nrow(locs), match(locs$ID,unique(locs$ID)))])
locs.dist <- cbind(locs,distances)

ggplot(locs.dist,aes(x=timestamp, y=distances))+geom_line()+facet_wrap(~Name,scale="free")+theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Table 2: Daily distance walked
dayinds <- st_coordinates(locs) %>% as.data.frame %>% split( list(locs$Name,date(locs$timestamp)))
dayinds <- dayinds[sapply(dayinds,nrow)>0]

distanceday <- numeric(length=length(dayinds))
for( a in 1:length(dayinds)) {
    dists<- dist(dayinds[[a]])
    dists<- as.matrix(dists)
    dists<- dists[-nrow(dists),-1]
    dists.cum <- sum(diag(dists))
    distanceday[a] <- dists.cum
}
indsofday <- strsplit( names(dayinds), "\\.")
indsofday <- sapply(indsofday,"[[",1)
summ <- tapply(distanceday,indsofday, summary)
summ <- do.call(rbind, summ)
summ <- as.data.frame(summ)
summ <- cbind(ID = metadata$ID[match(rownames(summ),metadata$Name)], Nome = rownames(summ), summ[,-4] )
summ <- summ[order(summ$ID),]
write.xlsx(summ,file="./presentations/Relatorio trimestral 2019_12/table2.xlsx")




# Figure 11: Prediction map based on data from September/2019
library(raster)
predmap <- raster("C:\\Users\\jorge\\Documents\\AssociadoCNAP\\Projeto Pardas do Tiete\\maps\\low-resolution\\qualityexpmean.sdat")
plot(predmap)


