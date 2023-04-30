#Version 4.5 
#File creation code for no2, pm10, pm2.5, so2 and benzene (bz)
#Taken from version 2.0 and 3.0 (also 4.0)


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



#NO2 2010
no2_2010 <- read.csv("mapno22010.csv")
no2_2010 <- no2_2010[-c(1, 2, 3, 4, 5), ]
no2_2010 <- no2_2010[no2_2010$X.2 != "MISSING", ]


colnames(no2_2010) <- c("grid", "easting", "northing", "no2")
no2_2010$easting <- as.numeric(no2_2010$easting)
no2_2010$northing<- as.numeric(no2_2010$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_no2_2010 <- SpatialPointsDataFrame(coords = no2_2010[, c("easting", "northing")], data = no2_2010)
proj4string(coordinates_sp_no2_2010) <- CRS("+init=epsg:27700")
coordinates_sp_no2_2010_coor <- spTransform(coordinates_sp_no2_2010, CRS("+init=epsg:4326"))
coordinates_sp_no2_2010_coor

proj4string(coordinates_sp_no2_2010_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_no2_2010_coor)))
no2_2010 <- as.data.frame(coordinates_sp_no2_2010_coor)
colnames(no2_2010) <- c("grid", "easting", "northing", "no2", "lon", "lat")
no2_2010 <- no2_2010[, -c(1,2,3)]

coordinates(no2_2010) <- c("lon", "lat")
proj4string(no2_2010) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(no2_2010)))

#Match grids and constituencies
matched_no2_2010 <- over(no2_2010, constJson)

#Putting the data frame together
matched_no2_2010$no2 <- no2_2010$no2
matched_no2_2010$obs_lat <- no2_2010$lat
matched_no2_2010$obs_lon <- no2_2010$lon
matched_no2_2010_clean <- na.omit(matched_no2_2010)
unique(matched_no2_2010_clean$pcn20nm)


#AVERAGES
matched_no2_2010_clean$no2 <- as.numeric(matched_no2_2010_clean$no2)
avg_no2_2010 <- matched_no2_2010_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(no2))
write.csv(avg_no2_2010, file = "avg_no2_2010.csv", row.names = FALSE)


#NO2 2010 END

#NO2 2015

no2_2015 <- read.csv("mapno22015.csv")
no2_2015 <- no2_2015[-c(1, 2, 3, 4, 5), ]
no2_2015 <- no2_2015[no2_2015$X.2 != "MISSING", ]


colnames(no2_2015) <- c("grid", "easting", "northing", "no2")
no2_2015$easting <- as.numeric(no2_2015$easting)
no2_2015$northing<- as.numeric(no2_2015$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_no2_2015 <- SpatialPointsDataFrame(coords = no2_2015[, c("easting", "northing")], data = no2_2015)
proj4string(coordinates_sp_no2_2015) <- CRS("+init=epsg:27700")
coordinates_sp_no2_2015_coor <- spTransform(coordinates_sp_no2_2015, CRS("+init=epsg:4326"))
coordinates_sp_no2_2015_coor

proj4string(coordinates_sp_no2_2015_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_no2_2015_coor)))
no2_2015 <- as.data.frame(coordinates_sp_no2_2015_coor)
colnames(no2_2015) <- c("grid", "easting", "northing", "no2", "lon", "lat")
no2_2015 <- no2_2015[, -c(1,2,3)]

coordinates(no2_2015) <- c("lon", "lat")
proj4string(no2_2015) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(no2_2015)))

#Match grids and constituencies
matched_no2_2015 <- over(no2_2015, constJson)

#Putting the data frame together
matched_no2_2015$no2 <- no2_2015$no2
matched_no2_2015$obs_lat <- no2_2015$lat
matched_no2_2015$obs_lon <- no2_2015$lon
matched_no2_2015_clean <- na.omit(matched_no2_2015)
unique(matched_no2_2015_clean$pcn20nm)


#AVERAGES
matched_no2_2015_clean$no2 <- as.numeric(matched_no2_2015_clean$no2)
avg_no2_2015 <- matched_no2_2015_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(no2))

write.csv(avg_no2_2015, file = "avg_no2_2015.csv", row.names = FALSE)

#NO2 2015 END

#NO2 2017

no2_2017 <- read.csv("mapno22017.csv")
no2_2017 <- no2_2017[-c(1, 2, 3, 4, 5), ]
no2_2017 <- no2_2017[no2_2017$X.2 != "MISSING", ]


colnames(no2_2017) <- c("grid", "easting", "northing", "no2")
no2_2017$easting <- as.numeric(no2_2017$easting)
no2_2017$northing<- as.numeric(no2_2017$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_no2_2017 <- SpatialPointsDataFrame(coords = no2_2017[, c("easting", "northing")], data = no2_2017)
proj4string(coordinates_sp_no2_2017) <- CRS("+init=epsg:27700")
coordinates_sp_no2_2017_coor <- spTransform(coordinates_sp_no2_2017, CRS("+init=epsg:4326"))
coordinates_sp_no2_2017_coor

proj4string(coordinates_sp_no2_2017_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_no2_2017_coor)))
no2_2017 <- as.data.frame(coordinates_sp_no2_2017_coor)
colnames(no2_2017) <- c("grid", "easting", "northing", "no2", "lon", "lat")
no2_2017 <- no2_2017[, -c(1,2,3)]

coordinates(no2_2017) <- c("lon", "lat")
proj4string(no2_2017) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(no2_2017)))

#Match grids and constituencies
matched_no2_2017 <- over(no2_2017, constJson)

#Putting the data frame together
matched_no2_2017$no2 <- no2_2017$no2
matched_no2_2017$obs_lat <- no2_2017$lat
matched_no2_2017$obs_lon <- no2_2017$lon
matched_no2_2017_clean <- na.omit(matched_no2_2017)
unique(matched_no2_2017_clean$pcn20nm)


#AVERAGES
matched_no2_2017_clean$no2 <- as.numeric(matched_no2_2017_clean$no2)
avg_no2_2017 <- matched_no2_2017_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(no2))

write.csv(avg_no2_2017, file = "avg_no2_2017.csv", row.names = FALSE)

#NO2 2017 END

#NO2 2019

no2_2019 <- read.csv("mapno22019.csv")
no2_2019 <- no2_2019[-c(1, 2, 3, 4, 5), ]
no2_2019 <- no2_2019[no2_2019$X.2 != "MISSING", ]


colnames(no2_2019) <- c("grid", "easting", "northing", "no2")
no2_2019$easting <- as.numeric(no2_2019$easting)
no2_2019$northing<- as.numeric(no2_2019$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_no2_2019 <- SpatialPointsDataFrame(coords = no2_2019[, c("easting", "northing")], data = no2_2019)
proj4string(coordinates_sp_no2_2019) <- CRS("+init=epsg:27700")
coordinates_sp_no2_2019_coor <- spTransform(coordinates_sp_no2_2019, CRS("+init=epsg:4326"))
coordinates_sp_no2_2019_coor

proj4string(coordinates_sp_no2_2019_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_no2_2019_coor)))
no2_2019 <- as.data.frame(coordinates_sp_no2_2019_coor)
colnames(no2_2019) <- c("grid", "easting", "northing", "no2", "lon", "lat")
no2_2019 <- no2_2019[, -c(1,2,3)]

coordinates(no2_2019) <- c("lon", "lat")
proj4string(no2_2019) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(no2_2019)))

#Match grids and constituencies
matched_no2_2019 <- over(no2_2019, constJson)

#Putting the data frame together
matched_no2_2019$no2 <- no2_2019$no2
matched_no2_2019$obs_lat <- no2_2019$lat
matched_no2_2019$obs_lon <- no2_2019$lon
matched_no2_2019_clean <- na.omit(matched_no2_2019)
unique(matched_no2_2019_clean$pcn20nm)


#AVERAGES
matched_no2_2019_clean$no2 <- as.numeric(matched_no2_2019_clean$no2)
avg_no2_2019 <- matched_no2_2019_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(no2))

write.csv(avg_no2_2019, file = "avg_no2_2019.csv", row.names = FALSE)

#NO2 2019 END


#PM10 2010

pm10_2010 <- read.csv("mappm102010g.csv")
pm10_2010 <- pm10_2010[-c(1, 2, 3, 4, 5), ]
pm10_2010 <- pm10_2010[pm10_2010$X.2 != "MISSING", ]


colnames(pm10_2010) <- c("grid", "easting", "northing", "pm10")
pm10_2010$easting <- as.numeric(pm10_2010$easting)
pm10_2010$northing<- as.numeric(pm10_2010$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_pm10_2010 <- SpatialPointsDataFrame(coords = pm10_2010[, c("easting", "northing")], data = pm10_2010)
proj4string(coordinates_sp_pm10_2010) <- CRS("+init=epsg:27700")
coordinates_sp_pm10_2010_coor <- spTransform(coordinates_sp_pm10_2010, CRS("+init=epsg:4326"))
coordinates_sp_pm10_2010_coor

proj4string(coordinates_sp_pm10_2010_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_pm10_2010_coor)))
pm10_2010 <- as.data.frame(coordinates_sp_pm10_2010_coor)
colnames(pm10_2010) <- c("grid", "easting", "northing", "pm10", "lon", "lat")
pm10_2010 <- pm10_2010[, -c(1,2,3)]

coordinates(pm10_2010) <- c("lon", "lat")
proj4string(pm10_2010) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(pm10_2010)))

#Match grids and constituencies
matched_pm10_2010 <- over(pm10_2010, constJson)

#Putting the data frame together
matched_pm10_2010$pm10 <- pm10_2010$pm10
matched_pm10_2010$obs_lat <- pm10_2010$lat
matched_pm10_2010$obs_lon <- pm10_2010$lon
matched_pm10_2010_clean <- na.omit(matched_pm10_2010)
unique(matched_pm10_2010_clean$pcn20nm)


#AVERAGES
matched_pm10_2010_clean$pm10 <- as.numeric(matched_pm10_2010_clean$pm10)
avg_pm10_2010 <- matched_pm10_2010_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(pm10))

write.csv(avg_pm10_2010, file = "avg_pm10_2010.csv", row.names = FALSE)

#PM10 2010 END

#PM10 2015

pm10_2015 <- read.csv("mappm102015g.csv")
pm10_2015 <- pm10_2015[-c(1, 2, 3, 4, 5), ]
pm10_2015 <- pm10_2015[pm10_2015$X.2 != "MISSING", ]


colnames(pm10_2015) <- c("grid", "easting", "northing", "pm10")
pm10_2015$easting <- as.numeric(pm10_2015$easting)
pm10_2015$northing<- as.numeric(pm10_2015$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_pm10_2015 <- SpatialPointsDataFrame(coords = pm10_2015[, c("easting", "northing")], data = pm10_2015)
proj4string(coordinates_sp_pm10_2015) <- CRS("+init=epsg:27700")
coordinates_sp_pm10_2015_coor <- spTransform(coordinates_sp_pm10_2015, CRS("+init=epsg:4326"))
coordinates_sp_pm10_2015_coor

proj4string(coordinates_sp_pm10_2015_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_pm10_2015_coor)))
pm10_2015 <- as.data.frame(coordinates_sp_pm10_2015_coor)
colnames(pm10_2015) <- c("grid", "easting", "northing", "pm10", "lon", "lat")
pm10_2015 <- pm10_2015[, -c(1,2,3)]

coordinates(pm10_2015) <- c("lon", "lat")
proj4string(pm10_2015) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(pm10_2015)))

#Match grids and constituencies
matched_pm10_2015 <- over(pm10_2015, constJson)

#Putting the data frame together
matched_pm10_2015$pm10 <- pm10_2015$pm10
matched_pm10_2015$obs_lat <- pm10_2015$lat
matched_pm10_2015$obs_lon <- pm10_2015$lon
matched_pm10_2015_clean <- na.omit(matched_pm10_2015)
unique(matched_pm10_2015_clean$pcn20nm)


#AVERAGES
matched_pm10_2015_clean$pm10 <- as.numeric(matched_pm10_2015_clean$pm10)
avg_pm10_2015 <- matched_pm10_2015_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(pm10))

write.csv(avg_pm10_2015, file = "avg_pm10_2015.csv", row.names = FALSE)

#PM10 2015 END

#PM10 2017

pm10_2017 <- read.csv("mappm102017g.csv")
pm10_2017 <- pm10_2017[-c(1, 2, 3, 4, 5), ]
pm10_2017 <- pm10_2017[pm10_2017$X.2 != "MISSING", ]


colnames(pm10_2017) <- c("grid", "easting", "northing", "pm10")
pm10_2017$easting <- as.numeric(pm10_2017$easting)
pm10_2017$northing<- as.numeric(pm10_2017$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_pm10_2017 <- SpatialPointsDataFrame(coords = pm10_2017[, c("easting", "northing")], data = pm10_2017)
proj4string(coordinates_sp_pm10_2017) <- CRS("+init=epsg:27700")
coordinates_sp_pm10_2017_coor <- spTransform(coordinates_sp_pm10_2017, CRS("+init=epsg:4326"))
coordinates_sp_pm10_2017_coor

proj4string(coordinates_sp_pm10_2017_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_pm10_2017_coor)))
pm10_2017 <- as.data.frame(coordinates_sp_pm10_2017_coor)
colnames(pm10_2017) <- c("grid", "easting", "northing", "pm10", "lon", "lat")
pm10_2017 <- pm10_2017[, -c(1,2,3)]

coordinates(pm10_2017) <- c("lon", "lat")
proj4string(pm10_2017) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(pm10_2017)))

#Match grids and constituencies
matched_pm10_2017 <- over(pm10_2017, constJson)

#Putting the data frame together
matched_pm10_2017$pm10 <- pm10_2017$pm10
matched_pm10_2017$obs_lat <- pm10_2017$lat
matched_pm10_2017$obs_lon <- pm10_2017$lon
matched_pm10_2017_clean <- na.omit(matched_pm10_2017)
unique(matched_pm10_2017_clean$pcn20nm)


#AVERAGES
matched_pm10_2017_clean$pm10 <- as.numeric(matched_pm10_2017_clean$pm10)
avg_pm10_2017 <- matched_pm10_2017_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(pm10))

write.csv(avg_pm10_2017, file = "avg_pm10_2017.csv", row.names = FALSE)

#PM10 2017 END

#PM10 2019

pm10_2019 <- read.csv("mappm102019g.csv")
pm10_2019 <- pm10_2019[-c(1, 2, 3, 4, 5), ]
pm10_2019 <- pm10_2019[pm10_2019$X.2 != "MISSING", ]


colnames(pm10_2019) <- c("grid", "easting", "northing", "pm10")
pm10_2019$easting <- as.numeric(pm10_2019$easting)
pm10_2019$northing<- as.numeric(pm10_2019$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_pm10_2019 <- SpatialPointsDataFrame(coords = pm10_2019[, c("easting", "northing")], data = pm10_2019)
proj4string(coordinates_sp_pm10_2019) <- CRS("+init=epsg:27700")
coordinates_sp_pm10_2019_coor <- spTransform(coordinates_sp_pm10_2019, CRS("+init=epsg:4326"))
coordinates_sp_pm10_2019_coor

proj4string(coordinates_sp_pm10_2019_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_pm10_2019_coor)))
pm10_2019 <- as.data.frame(coordinates_sp_pm10_2019_coor)
colnames(pm10_2019) <- c("grid", "easting", "northing", "pm10", "lon", "lat")
pm10_2019 <- pm10_2019[, -c(1,2,3)]

coordinates(pm10_2019) <- c("lon", "lat")
proj4string(pm10_2019) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(pm10_2019)))

#Match grids and constituencies
matched_pm10_2019 <- over(pm10_2019, constJson)

#Putting the data frame together
matched_pm10_2019$pm10 <- pm10_2019$pm10
matched_pm10_2019$obs_lat <- pm10_2019$lat
matched_pm10_2019$obs_lon <- pm10_2019$lon
matched_pm10_2019_clean <- na.omit(matched_pm10_2019)
unique(matched_pm10_2019_clean$pcn20nm)


#AVERAGES
matched_pm10_2019_clean$pm10 <- as.numeric(matched_pm10_2019_clean$pm10)
avg_pm10_2019 <- matched_pm10_2019_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(pm10))

write.csv(avg_pm10_2019, file = "avg_pm10_2019.csv", row.names = FALSE)

#PM10 2019 END


#PM2.5 2010

pm2.5_2010 <- read.csv("Air pollution/mappm252010g.csv")
pm2.5_2010 <- pm2.5_2010[-c(1, 2, 3, 4, 5), ]
pm2.5_2010 <- pm2.5_2010[pm2.5_2010$X.2 != "MISSING", ]


colnames(pm2.5_2010) <- c("grid", "easting", "northing", "pm2.5")
pm2.5_2010$easting <- as.numeric(pm2.5_2010$easting)
pm2.5_2010$northing<- as.numeric(pm2.5_2010$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("Air pollution/json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_pm2.5_2010 <- SpatialPointsDataFrame(coords = pm2.5_2010[, c("easting", "northing")], data = pm2.5_2010)
proj4string(coordinates_sp_pm2.5_2010) <- CRS("+init=epsg:27700")
coordinates_sp_pm2.5_2010_coor <- spTransform(coordinates_sp_pm2.5_2010, CRS("+init=epsg:4326"))
coordinates_sp_pm2.5_2010_coor

proj4string(coordinates_sp_pm2.5_2010_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_pm2.5_2010_coor)))
pm2.5_2010 <- as.data.frame(coordinates_sp_pm2.5_2010_coor)
colnames(pm2.5_2010) <- c("grid", "easting", "northing", "pm2.5", "lon", "lat")
pm2.5_2010 <- pm2.5_2010[, -c(1,2,3)]

coordinates(pm2.5_2010) <- c("lon", "lat")
proj4string(pm2.5_2010) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(pm2.5_2010)))

#Match grids and constituencies
matched_pm2.5_2010 <- over(pm2.5_2010, constJson)

#Putting the data frame together
matched_pm2.5_2010$pm2.5 <- pm2.5_2010$pm2.5
matched_pm2.5_2010$obs_lat <- pm2.5_2010$lat
matched_pm2.5_2010$obs_lon <- pm2.5_2010$lon
matched_pm2.5_2010_clean <- na.omit(matched_pm2.5_2010)
unique(matched_pm2.5_2010_clean$pcn20nm)


#AVERAGES
matched_pm2.5_2010_clean$pm2.5 <- as.numeric(matched_pm2.5_2010_clean$pm2.5)
avg_pm2.5_2010 <- matched_pm2.5_2010_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(pm2.5))

write.csv(avg_pm2.5_2010, file = "avg_pm2.5_2010.csv", row.names = FALSE)

#PM2.5 2010 END

#pm2.5 2015

pm2.5_2015 <- read.csv("Air pollution/mappm252015g.csv")
pm2.5_2015 <- pm2.5_2015[-c(1, 2, 3, 4, 5), ]
pm2.5_2015 <- pm2.5_2015[pm2.5_2015$X.2 != "MISSING", ]


colnames(pm2.5_2015) <- c("grid", "easting", "northing", "pm2.5")
pm2.5_2015$easting <- as.numeric(pm2.5_2015$easting)
pm2.5_2015$northing<- as.numeric(pm2.5_2015$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("Air pollution/json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_pm2.5_2015 <- SpatialPointsDataFrame(coords = pm2.5_2015[, c("easting", "northing")], data = pm2.5_2015)
proj4string(coordinates_sp_pm2.5_2015) <- CRS("+init=epsg:27700")
coordinates_sp_pm2.5_2015_coor <- spTransform(coordinates_sp_pm2.5_2015, CRS("+init=epsg:4326"))
coordinates_sp_pm2.5_2015_coor

proj4string(coordinates_sp_pm2.5_2015_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_pm2.5_2015_coor)))
pm2.5_2015 <- as.data.frame(coordinates_sp_pm2.5_2015_coor)
colnames(pm2.5_2015) <- c("grid", "easting", "northing", "pm2.5", "lon", "lat")
pm2.5_2015 <- pm2.5_2015[, -c(1,2,3)]

coordinates(pm2.5_2015) <- c("lon", "lat")
proj4string(pm2.5_2015) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(pm2.5_2015)))

#Match grids and constituencies
matched_pm2.5_2015 <- over(pm2.5_2015, constJson)

#Putting the data frame together
matched_pm2.5_2015$pm2.5 <- pm2.5_2015$pm2.5
matched_pm2.5_2015$obs_lat <- pm2.5_2015$lat
matched_pm2.5_2015$obs_lon <- pm2.5_2015$lon
matched_pm2.5_2015_clean <- na.omit(matched_pm2.5_2015)
unique(matched_pm2.5_2015_clean$pcn20nm)


#AVERAGES
matched_pm2.5_2015_clean$pm2.5 <- as.numeric(matched_pm2.5_2015_clean$pm2.5)
avg_pm2.5_2015 <- matched_pm2.5_2015_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(pm2.5))

write.csv(avg_pm2.5_2015, file = "avg_pm2.5_2015.csv", row.names = FALSE)

#pm2.5 2015 END

#pm2.5 2017

pm2.5_2017 <- read.csv("Air pollution/mappm252017g.csv")
pm2.5_2017 <- pm2.5_2017[-c(1, 2, 3, 4, 5), ]
pm2.5_2017 <- pm2.5_2017[pm2.5_2017$X.2 != "MISSING", ]


colnames(pm2.5_2017) <- c("grid", "easting", "northing", "pm2.5")
pm2.5_2017$easting <- as.numeric(pm2.5_2017$easting)
pm2.5_2017$northing<- as.numeric(pm2.5_2017$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("Air pollution/json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_pm2.5_2017 <- SpatialPointsDataFrame(coords = pm2.5_2017[, c("easting", "northing")], data = pm2.5_2017)
proj4string(coordinates_sp_pm2.5_2017) <- CRS("+init=epsg:27700")
coordinates_sp_pm2.5_2017_coor <- spTransform(coordinates_sp_pm2.5_2017, CRS("+init=epsg:4326"))
coordinates_sp_pm2.5_2017_coor

proj4string(coordinates_sp_pm2.5_2017_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_pm2.5_2017_coor)))
pm2.5_2017 <- as.data.frame(coordinates_sp_pm2.5_2017_coor)
colnames(pm2.5_2017) <- c("grid", "easting", "northing", "pm2.5", "lon", "lat")
pm2.5_2017 <- pm2.5_2017[, -c(1,2,3)]

coordinates(pm2.5_2017) <- c("lon", "lat")
proj4string(pm2.5_2017) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(pm2.5_2017)))

#Match grids and constituencies
matched_pm2.5_2017 <- over(pm2.5_2017, constJson)

#Putting the data frame together
matched_pm2.5_2017$pm2.5 <- pm2.5_2017$pm2.5
matched_pm2.5_2017$obs_lat <- pm2.5_2017$lat
matched_pm2.5_2017$obs_lon <- pm2.5_2017$lon
matched_pm2.5_2017_clean <- na.omit(matched_pm2.5_2017)
unique(matched_pm2.5_2017_clean$pcn20nm)


#AVERAGES
matched_pm2.5_2017_clean$pm2.5 <- as.numeric(matched_pm2.5_2017_clean$pm2.5)
avg_pm2.5_2017 <- matched_pm2.5_2017_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(pm2.5))

write.csv(avg_pm2.5_2017, file = "avg_pm2.5_2017.csv", row.names = FALSE)

#pm2.5 2017 END

#pm2.5 2019

pm2.5_2019 <- read.csv("Air pollution/mappm252019g.csv")
pm2.5_2019 <- pm2.5_2019[-c(1, 2, 3, 4, 5), ]
pm2.5_2019 <- pm2.5_2019[pm2.5_2019$X.2 != "MISSING", ]


colnames(pm2.5_2019) <- c("grid", "easting", "northing", "pm2.5")
pm2.5_2019$easting <- as.numeric(pm2.5_2019$easting)
pm2.5_2019$northing<- as.numeric(pm2.5_2019$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("Air pollution/json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_pm2.5_2019 <- SpatialPointsDataFrame(coords = pm2.5_2019[, c("easting", "northing")], data = pm2.5_2019)
proj4string(coordinates_sp_pm2.5_2019) <- CRS("+init=epsg:27700")
coordinates_sp_pm2.5_2019_coor <- spTransform(coordinates_sp_pm2.5_2019, CRS("+init=epsg:4326"))
coordinates_sp_pm2.5_2019_coor

proj4string(coordinates_sp_pm2.5_2019_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_pm2.5_2019_coor)))
pm2.5_2019 <- as.data.frame(coordinates_sp_pm2.5_2019_coor)
colnames(pm2.5_2019) <- c("grid", "easting", "northing", "pm2.5", "lon", "lat")
pm2.5_2019 <- pm2.5_2019[, -c(1,2,3)]

coordinates(pm2.5_2019) <- c("lon", "lat")
proj4string(pm2.5_2019) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(pm2.5_2019)))

#Match grids and constituencies
matched_pm2.5_2019 <- over(pm2.5_2019, constJson)

#Putting the data frame together
matched_pm2.5_2019$pm2.5 <- pm2.5_2019$pm2.5
matched_pm2.5_2019$obs_lat <- pm2.5_2019$lat
matched_pm2.5_2019$obs_lon <- pm2.5_2019$lon
matched_pm2.5_2019_clean <- na.omit(matched_pm2.5_2019)
unique(matched_pm2.5_2019_clean$pcn20nm)


#AVERAGES
matched_pm2.5_2019_clean$pm2.5 <- as.numeric(matched_pm2.5_2019_clean$pm2.5)
avg_pm2.5_2019 <- matched_pm2.5_2019_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(pm2.5))

write.csv(avg_pm2.5_2019, file = "avg_pm2.5_2019.csv", row.names = FALSE)

#pm2.5 2019 END

#SO2 2010

so2_2010 <- read.csv("mapso210ann.csv")
so2_2010 <- so2_2010[-c(1, 2, 3, 4, 5), ]
so2_2010 <- so2_2010[so2_2010$X.2 != "MISSING", ]


colnames(so2_2010) <- c("grid", "easting", "northing", "so2")
so2_2010$easting <- as.numeric(so2_2010$easting)
so2_2010$northing<- as.numeric(so2_2010$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_so2_2010 <- SpatialPointsDataFrame(coords = so2_2010[, c("easting", "northing")], data = so2_2010)
proj4string(coordinates_sp_so2_2010) <- CRS("+init=epsg:27700")
coordinates_sp_so2_2010_coor <- spTransform(coordinates_sp_so2_2010, CRS("+init=epsg:4326"))
coordinates_sp_so2_2010_coor

proj4string(coordinates_sp_so2_2010_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_so2_2010_coor)))
so2_2010 <- as.data.frame(coordinates_sp_so2_2010_coor)
colnames(so2_2010) <- c("grid", "easting", "northing", "so2", "lon", "lat")
so2_2010 <- so2_2010[, -c(1,2,3)]

coordinates(so2_2010) <- c("lon", "lat")
proj4string(so2_2010) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(so2_2010)))

#Match grids and constituencies
matched_so2_2010 <- over(so2_2010, constJson)

#Putting the data frame together
matched_so2_2010$so2 <- so2_2010$so2
matched_so2_2010$obs_lat <- so2_2010$lat
matched_so2_2010$obs_lon <- so2_2010$lon
matched_so2_2010_clean <- na.omit(matched_so2_2010)
unique(matched_so2_2010_clean$pcn20nm)


#AVERAGES
matched_so2_2010_clean$so2 <- as.numeric(matched_so2_2010_clean$so2)
avg_so2_2010 <- matched_so2_2010_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(so2))

write.csv(avg_so2_2010, file = "avg_so2_2010.csv", row.names = FALSE)

#so2 2010 END

#so2 2015

so2_2015 <- read.csv("mapso22015.csv")
so2_2015 <- so2_2015[-c(1, 2, 3, 4, 5), ]
so2_2015 <- so2_2015[so2_2015$X.2 != "MISSING", ]


colnames(so2_2015) <- c("grid", "easting", "northing", "so2")
so2_2015$easting <- as.numeric(so2_2015$easting)
so2_2015$northing<- as.numeric(so2_2015$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_so2_2015 <- SpatialPointsDataFrame(coords = so2_2015[, c("easting", "northing")], data = so2_2015)
proj4string(coordinates_sp_so2_2015) <- CRS("+init=epsg:27700")
coordinates_sp_so2_2015_coor <- spTransform(coordinates_sp_so2_2015, CRS("+init=epsg:4326"))
coordinates_sp_so2_2015_coor

proj4string(coordinates_sp_so2_2015_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_so2_2015_coor)))
so2_2015 <- as.data.frame(coordinates_sp_so2_2015_coor)
colnames(so2_2015) <- c("grid", "easting", "northing", "so2", "lon", "lat")
so2_2015 <- so2_2015[, -c(1,2,3)]

coordinates(so2_2015) <- c("lon", "lat")
proj4string(so2_2015) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(so2_2015)))

#Match grids and constituencies
matched_so2_2015 <- over(so2_2015, constJson)

#Putting the data frame together
matched_so2_2015$so2 <- so2_2015$so2
matched_so2_2015$obs_lat <- so2_2015$lat
matched_so2_2015$obs_lon <- so2_2015$lon
matched_so2_2015_clean <- na.omit(matched_so2_2015)
unique(matched_so2_2015_clean$pcn20nm)


#AVERAGES
matched_so2_2015_clean$so2 <- as.numeric(matched_so2_2015_clean$so2)
avg_so2_2015 <- matched_so2_2015_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(so2))

write.csv(avg_so2_2015, file = "avg_so2_2015.csv", row.names = FALSE)

#so2 2015 END

#so2 2017

so2_2017 <- read.csv("mapso22017.csv")
so2_2017 <- so2_2017[-c(1, 2, 3, 4, 5), ]
so2_2017 <- so2_2017[so2_2017$X.2 != "MISSING", ]


colnames(so2_2017) <- c("grid", "easting", "northing", "so2")
so2_2017$easting <- as.numeric(so2_2017$easting)
so2_2017$northing<- as.numeric(so2_2017$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_so2_2017 <- SpatialPointsDataFrame(coords = so2_2017[, c("easting", "northing")], data = so2_2017)
proj4string(coordinates_sp_so2_2017) <- CRS("+init=epsg:27700")
coordinates_sp_so2_2017_coor <- spTransform(coordinates_sp_so2_2017, CRS("+init=epsg:4326"))
coordinates_sp_so2_2017_coor

proj4string(coordinates_sp_so2_2017_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_so2_2017_coor)))
so2_2017 <- as.data.frame(coordinates_sp_so2_2017_coor)
colnames(so2_2017) <- c("grid", "easting", "northing", "so2", "lon", "lat")
so2_2017 <- so2_2017[, -c(1,2,3)]

coordinates(so2_2017) <- c("lon", "lat")
proj4string(so2_2017) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(so2_2017)))

#Match grids and constituencies
matched_so2_2017 <- over(so2_2017, constJson)

#Putting the data frame together
matched_so2_2017$so2 <- so2_2017$so2
matched_so2_2017$obs_lat <- so2_2017$lat
matched_so2_2017$obs_lon <- so2_2017$lon
matched_so2_2017_clean <- na.omit(matched_so2_2017)
unique(matched_so2_2017_clean$pcn20nm)


#AVERAGES
matched_so2_2017_clean$so2 <- as.numeric(matched_so2_2017_clean$so2)
avg_so2_2017 <- matched_so2_2017_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(so2))

write.csv(avg_so2_2017, file = "avg_so2_2017.csv", row.names = FALSE)

#so2 2017 END

#so2 2019

so2_2019 <- read.csv("mapso22019.csv")
so2_2019 <- so2_2019[-c(1, 2, 3, 4, 5), ]
so2_2019 <- so2_2019[so2_2019$X.2 != "MISSING", ]


colnames(so2_2019) <- c("grid", "easting", "northing", "so2")
so2_2019$easting <- as.numeric(so2_2019$easting)
so2_2019$northing<- as.numeric(so2_2019$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_so2_2019 <- SpatialPointsDataFrame(coords = so2_2019[, c("easting", "northing")], data = so2_2019)
proj4string(coordinates_sp_so2_2019) <- CRS("+init=epsg:27700")
coordinates_sp_so2_2019_coor <- spTransform(coordinates_sp_so2_2019, CRS("+init=epsg:4326"))
coordinates_sp_so2_2019_coor

proj4string(coordinates_sp_so2_2019_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_so2_2019_coor)))
so2_2019 <- as.data.frame(coordinates_sp_so2_2019_coor)
colnames(so2_2019) <- c("grid", "easting", "northing", "so2", "lon", "lat")
so2_2019 <- so2_2019[, -c(1,2,3)]

coordinates(so2_2019) <- c("lon", "lat")
proj4string(so2_2019) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(so2_2019)))

#Match grids and constituencies
matched_so2_2019 <- over(so2_2019, constJson)

#Putting the data frame together
matched_so2_2019$so2 <- so2_2019$so2
matched_so2_2019$obs_lat <- so2_2019$lat
matched_so2_2019$obs_lon <- so2_2019$lon
matched_so2_2019_clean <- na.omit(matched_so2_2019)
unique(matched_so2_2019_clean$pcn20nm)


#AVERAGES
matched_so2_2019_clean$so2 <- as.numeric(matched_so2_2019_clean$so2)
avg_so2_2019 <- matched_so2_2019_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(so2))

write.csv(avg_so2_2019, file = "avg_so2_2019.csv", row.names = FALSE)

#so2 2019 END


#bz 2010

bz_2010 <- read.csv("mapbz2010.csv")
bz_2010 <- bz_2010[-c(1, 2, 3, 4, 5), ]
bz_2010 <- bz_2010[bz_2010$X.2 != "MISSING", ]


colnames(bz_2010) <- c("grid", "easting", "northing", "bz")
bz_2010$easting <- as.numeric(bz_2010$easting)
bz_2010$northing<- as.numeric(bz_2010$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_bz_2010 <- SpatialPointsDataFrame(coords = bz_2010[, c("easting", "northing")], data = bz_2010)
proj4string(coordinates_sp_bz_2010) <- CRS("+init=epsg:27700")
coordinates_sp_bz_2010_coor <- spTransform(coordinates_sp_bz_2010, CRS("+init=epsg:4326"))
coordinates_sp_bz_2010_coor

proj4string(coordinates_sp_bz_2010_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_bz_2010_coor)))
bz_2010 <- as.data.frame(coordinates_sp_bz_2010_coor)
colnames(bz_2010) <- c("grid", "easting", "northing", "bz", "lon", "lat")
bz_2010 <- bz_2010[, -c(1,2,3)]

coordinates(bz_2010) <- c("lon", "lat")
proj4string(bz_2010) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(bz_2010)))

#Match grids and constituencies
matched_bz_2010 <- over(bz_2010, constJson)

#Putting the data frame together
matched_bz_2010$bz <- bz_2010$bz
matched_bz_2010$obs_lat <- bz_2010$lat
matched_bz_2010$obs_lon <- bz_2010$lon
matched_bz_2010_clean <- na.omit(matched_bz_2010)
unique(matched_bz_2010_clean$pcn20nm)


#AVERAGES
matched_bz_2010_clean$bz <- as.numeric(matched_bz_2010_clean$bz)
avg_bz_2010 <- matched_bz_2010_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(bz))

write.csv(avg_bz_2010, file = "avg_bz_2010.csv", row.names = FALSE)

#bz 2010 END

#bz 2015

bz_2015 <- read.csv("mapbz2015.csv")
bz_2015 <- bz_2015[-c(1, 2, 3, 4, 5), ]
bz_2015 <- bz_2015[bz_2015$X.2 != "MISSING", ]


colnames(bz_2015) <- c("grid", "easting", "northing", "bz")
bz_2015$easting <- as.numeric(bz_2015$easting)
bz_2015$northing<- as.numeric(bz_2015$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_bz_2015 <- SpatialPointsDataFrame(coords = bz_2015[, c("easting", "northing")], data = bz_2015)
proj4string(coordinates_sp_bz_2015) <- CRS("+init=epsg:27700")
coordinates_sp_bz_2015_coor <- spTransform(coordinates_sp_bz_2015, CRS("+init=epsg:4326"))
coordinates_sp_bz_2015_coor

proj4string(coordinates_sp_bz_2015_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_bz_2015_coor)))
bz_2015 <- as.data.frame(coordinates_sp_bz_2015_coor)
colnames(bz_2015) <- c("grid", "easting", "northing", "bz", "lon", "lat")
bz_2015 <- bz_2015[, -c(1,2,3)]

coordinates(bz_2015) <- c("lon", "lat")
proj4string(bz_2015) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(bz_2015)))

#Match grids and constituencies
matched_bz_2015 <- over(bz_2015, constJson)

#Putting the data frame together
matched_bz_2015$bz <- bz_2015$bz
matched_bz_2015$obs_lat <- bz_2015$lat
matched_bz_2015$obs_lon <- bz_2015$lon
matched_bz_2015_clean <- na.omit(matched_bz_2015)
unique(matched_bz_2015_clean$pcn20nm)


#AVERAGES
matched_bz_2015_clean$bz <- as.numeric(matched_bz_2015_clean$bz)
avg_bz_2015 <- matched_bz_2015_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(bz))

write.csv(avg_bz_2015, file = "avg_bz_2015.csv", row.names = FALSE)

#bz 2015 END

#bz 2017

bz_2017 <- read.csv("mapbz2017.csv")
bz_2017 <- bz_2017[-c(1, 2, 3, 4, 5), ]
bz_2017 <- bz_2017[bz_2017$X.2 != "MISSING", ]


colnames(bz_2017) <- c("grid", "easting", "northing", "bz")
bz_2017$easting <- as.numeric(bz_2017$easting)
bz_2017$northing<- as.numeric(bz_2017$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_bz_2017 <- SpatialPointsDataFrame(coords = bz_2017[, c("easting", "northing")], data = bz_2017)
proj4string(coordinates_sp_bz_2017) <- CRS("+init=epsg:27700")
coordinates_sp_bz_2017_coor <- spTransform(coordinates_sp_bz_2017, CRS("+init=epsg:4326"))
coordinates_sp_bz_2017_coor

proj4string(coordinates_sp_bz_2017_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_bz_2017_coor)))
bz_2017 <- as.data.frame(coordinates_sp_bz_2017_coor)
colnames(bz_2017) <- c("grid", "easting", "northing", "bz", "lon", "lat")
bz_2017 <- bz_2017[, -c(1,2,3)]

coordinates(bz_2017) <- c("lon", "lat")
proj4string(bz_2017) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(bz_2017)))

#Match grids and constituencies
matched_bz_2017 <- over(bz_2017, constJson)

#Putting the data frame together
matched_bz_2017$bz <- bz_2017$bz
matched_bz_2017$obs_lat <- bz_2017$lat
matched_bz_2017$obs_lon <- bz_2017$lon
matched_bz_2017_clean <- na.omit(matched_bz_2017)
unique(matched_bz_2017_clean$pcn20nm)


#AVERAGES
matched_bz_2017_clean$bz <- as.numeric(matched_bz_2017_clean$bz)
avg_bz_2017 <- matched_bz_2017_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(bz))

write.csv(avg_bz_2017, file = "avg_bz_2017.csv", row.names = FALSE)

#bz 2017 END

#bz 2019

bz_2019 <- read.csv("mapbz2019.csv")
bz_2019 <- bz_2019[-c(1, 2, 3, 4, 5), ]
bz_2019 <- bz_2019[bz_2019$X.2 != "MISSING", ]


colnames(bz_2019) <- c("grid", "easting", "northing", "bz")
bz_2019$easting <- as.numeric(bz_2019$easting)
bz_2019$northing<- as.numeric(bz_2019$northing)


#Load the shapefile from the pre-made .shp document
constJson <- readOGR("json_shapefile.shp")

#Turning UTM to lat long

coordinates_sp_bz_2019 <- SpatialPointsDataFrame(coords = bz_2019[, c("easting", "northing")], data = bz_2019)
proj4string(coordinates_sp_bz_2019) <- CRS("+init=epsg:27700")
coordinates_sp_bz_2019_coor <- spTransform(coordinates_sp_bz_2019, CRS("+init=epsg:4326"))
coordinates_sp_bz_2019_coor

proj4string(coordinates_sp_bz_2019_coor) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(coordinates_sp_bz_2019_coor)))
bz_2019 <- as.data.frame(coordinates_sp_bz_2019_coor)
colnames(bz_2019) <- c("grid", "easting", "northing", "bz", "lon", "lat")
bz_2019 <- bz_2019[, -c(1,2,3)]

coordinates(bz_2019) <- c("lon", "lat")
proj4string(bz_2019) <- CRS("+proj=longlat +datum=WGS84")
constJson <- spTransform(constJson, CRS(proj4string(bz_2019)))

#Match grids and constituencies
matched_bz_2019 <- over(bz_2019, constJson)

#Putting the data frame together
matched_bz_2019$bz <- bz_2019$bz
matched_bz_2019$obs_lat <- bz_2019$lat
matched_bz_2019$obs_lon <- bz_2019$lon
matched_bz_2019_clean <- na.omit(matched_bz_2019)
unique(matched_bz_2019_clean$pcn20nm)


#AVERAGES
matched_bz_2019_clean$bz <- as.numeric(matched_bz_2019_clean$bz)
avg_bz_2019 <- matched_bz_2019_clean %>% 
  group_by(pcn20cd) %>% 
  summarize(avg_value = mean(bz))

write.csv(avg_bz_2019, file = "avg_bz_2019.csv", row.names = FALSE)

#bz 2019 END

