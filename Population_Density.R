library(sf)
library(sp)
library(terra)
library(dplyr)
library(rgdal)
library(rgeos)
library(maptools)

constJson <- readOGR("Raw Air Pollution Data/json_shapefile.shp")

constJson <- spTransform(constJson, CRS("+init=EPSG:27700"))

constituency_area <- gArea(constJson, byid = TRUE) / 1e6
constituency_areas <- data.frame(constituency = constJson@data$pcn20cd, area_sq_km = constituency_area)
print(constituency_areas)
colnames(constituency_areas) <- c("id", "area")

pop2010 <- read.csv("Raw Population Data/pop2010.csv")
pop2015 <- read.csv("Raw Population Data/pop2015.csv")
pop2017 <- read.csv("Raw Population Data/pop2017.csv")
pop2019 <- read.csv("Raw Population Data/pop2019.csv")

pop2010 <- pop2010[, -c(3, 4, 5)]
pop2015 <- pop2015[, -c(3, 4, 5)]
pop2017 <- pop2017[, -c(3, 4, 5)]
pop2019 <- pop2019[, -c(3, 4, 5)]

density_2010 <- merge(constituency_areas, pop2010, by = "id")
density_2015 <- merge(constituency_areas, pop2015, by = "id")
density_2017 <- merge(constituency_areas, pop2017, by = "id")
density_2019 <- merge(constituency_areas, pop2019, by = "id")

colnames(density_2015) <- c("id", "area", "pop")



density_2010$density <- density_2010$pop / density_2010$area
density_2015$density <- density_2015$pop / density_2015$area
density_2017$density <- density_2017$pop / density_2017$area
density_2019$density <- density_2019$pop / density_2019$area


density_2010 <- density_2010[, -c(2, 3)]
density_2015 <- density_2015[, -c(2, 3)]
density_2017 <- density_2017[, -c(2, 3)]
density_2019 <- density_2019[, -c(2, 3)]


write.csv(density_2010, file = "Structured Data/density_2010", row.names = FALSE)
write.csv(density_2015, file = "Structured Data/density_2015", row.names = FALSE)
write.csv(density_2017, file = "Structured Data/density_2017", row.names = FALSE)
write.csv(density_2019, file = "Structured Data/density_2019", row.names = FALSE)
