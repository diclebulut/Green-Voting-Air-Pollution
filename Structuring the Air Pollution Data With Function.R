#Libraries
library(plm)
library(rgdal)
library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(geojsonio)
library(terra)
#Libraries end

#LOADING THE SHAPEFILE FOR ALL OPERATIONS
# load the GeoJSON file
constJson <- geojson_read("westminster-parliamentary-constituencies.geojson")
#Read the Json file
d = read_sf("westminster-parliamentary-constituencies.geojson")
#Write it as shape file
st_write(d, "json_shapefile.shp")
#SHAPEFILE SAVED IN THE WORKING DIRECTORY

#function
consMatch <- function(pollutant_year) {
  pollutant_year <- pollutant_year[-c(1, 2, 3, 4, 5), ]
  pollutant_year <- pollutant_year[pollutant_year$X.2 != "MISSING", ]
  
  
  colnames(pollutant_year) <- c("grid", "easting", "northing", "pollutant")
  pollutant_year$easting <- as.numeric(pollutant_year$easting)
  pollutant_year$northing<- as.numeric(pollutant_year$northing)
  
  
  #Load the shapefile from the pre-made .shp document
  constJson <- readOGR("json_shapefile.shp")
  
  #Turning UTM to lat long
  
  coordinates_sp<- SpatialPointsDataFrame(coords = pollutant_year[, c("easting", "northing")], data = pollutant_year)
  proj4string(coordinates_sp) <- CRS("+init=epsg:27700")
  coordinates_sp_coor <- spTransform(coordinates_sp, CRS("+init=epsg:4326"))
  
  
  proj4string(coordinates_sp_coor) <- CRS("+proj=longlat +datum=WGS84")
  constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_coor)))
  pollutant_year <- as.data.frame(coordinates_sp_coor)
  colnames(pollutant_year) <- c("grid", "easting", "northing", "pollutant", "lon", "lat")
  pollutant_year <- pollutant_year[, -c(1,2,3)]
  
  coordinates(pollutant_year) <- c("lon", "lat")
  proj4string(pollutant_year) <- CRS("+proj=longlat +datum=WGS84")
  constJson <- spTransform(constJson, CRS(proj4string(pollutant_year)))
  
  #Match grids and constituencies
  matched <- over(pollutant_year, constJson)
  
  #Putting the data frame together
  matched$pollutant <- pollutant_year$pollutant
  matched$obs_lat <- pollutant_year$lat
  matched$obs_lon <- pollutant_year$lon
  matched_clean <- na.omit(matched)
  unique(matched_clean$pcn20nm)
  
  
  #AVERAGES
  matched_clean$pollutant <- as.numeric(matched_clean$pollutant)
  avg_pollutant_year <- matched_clean %>% 
    group_by(pcn20cd) %>% 
    summarize(avg_value = mean(pollutant))


write.csv(avg_pollutant_year, file = "test/avg.csv", row.names = FALSE)
}


no2_2010 <- read.csv("mapno22010.csv")
no2_2015 <- read.csv("mapno22015.csv")
no2_2017 <- read.csv("mapno22017.csv")
no2_2019 <- read.csv("mapno22019.csv")


pm10_2010 <- read.csv("mappm102010g.csv")
pm10_2015 <- read.csv("mappm102015g.csv")
pm10_2017 <- read.csv("mappm102017g.csv")
pm10_2019 <- read.csv("mappm102019g.csv")


pm2.5_2010 <- read.csv("mappm252010g.csv")
pm2.5_2015 <- read.csv("mappm252015g.csv")
pm2.5_2017 <- read.csv("mappm252017g.csv")
pm2.5_2019 <- read.csv("mappm252019g.csv")


so2_2010 <- read.csv("mapso210ann.csv")
so2_2015 <- read.csv("mapso22015.csv")
so2_2017 <- read.csv("mapso22017.csv")
so2_2019 <- read.csv("mapso22019.csv")


bz_2010 <- read.csv("mapbz2010.csv")
bz_2015 <- read.csv("mapbz2015.csv")
bz_2017 <- read.csv("mapbz2017.csv")
bz_2019 <- read.csv("mapbz2019.csv")



consMatch(no2_2010) 
consMatch(no2_2015) 
consMatch(no2_2017) 
consMatch(no2_2019) 

consMatch(pm10_2010) 
consMatch(pm10_2015) 
consMatch(pm10_2017) 
consMatch(pm10_2019) 

consMatch(pm2.5_2010) 
consMatch(pm2.5_2015) 
consMatch(pm2.5_2017) 
consMatch(pm2.5_2019) 

consMatch(so2_2010) 
consMatch(so2_2015) 
consMatch(so2_2017) 
consMatch(so2_2019) 

consMatch(bz_2010) 
consMatch(bz_2015) 
consMatch(bz_2017) 
consMatch(bz_2019) 



