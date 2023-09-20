# for van

library(sf)
library(tidyverse)

bc_roads <- st_read("data/raw/BCrte.shp")

van_border <- st_read("data/raw/city_boundary.shp")

van_border <- st_polygonize(van_border)

bc_roads <- st_transform(bc_roads, st_crs(van_border))

van_roads <- st_intersection(van_border, bc_roads)


van_roads$length <- st_length(van_roads)

van_roads$short <- van_roads$length < units::set_units(150,m)

short_roads <- van_roads[van_roads$short == TRUE,]

short_road_midpoints <- st_centroid(short_roads)

lat <- 0
lng <- 0

for (i in 1:nrow(short_road_midpoints)){
  lng[i] <- round(short_road_midpoints$geometry[[i]][1], 4)
  lat[i] <- round(short_road_midpoints$geometry[[i]][2], 4)
}

df_short <- data.frame(long = lng, lat = lat)

df_sf <- st_as_sf(df_short, coords = c("lng", "lat"), 
                  crs = 4326, agr = "constant")

ggplot() + geom_sf(data = short_roads) + geom_sf(data = short_road_midpoints)
