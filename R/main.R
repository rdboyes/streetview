# main exectutable file

library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)

roads <- st_read(here::here("data/raw/road-segments.shp"))

# roads.lines <- st_crop(roads.lines, xmin = -76.65, xmax = -76.4, ymax = 44.275, ymin = 44.2)

roads.lines <- st_cast(roads, "LINESTRING")

road.seg <- st_simplify(roads.lines, dTolerance = .001)
road.seg.no.int <- st_segmentize(road.seg, dfMaxLength = units::set_units(150, m))

road.points2 <- st_cast(road.seg.no.int, "POINT")
road.points <- st_cast(road.seg, "POINT")

all_road_points <- rbind(road.points, road.points2)

all_road_points_df <- as.data.frame(all_road_points) %>% select(-geometry)

lat <- 0
lng <- 0

for (i in 1:nrow(all_road_points)){
  lng[i] <- round(all_road_points$geometry[[i]][1], 4)
  lat[i] <- round(all_road_points$geometry[[i]][2], 4)
}

all_road_points_df$lng <- lng
all_road_points_df$lat <- lat

all_road_points_df$dup <- duplicated(all_road_points_df) | duplicated(all_road_points_df, fromLast = TRUE)

df_sf <- st_as_sf(all_road_points_df[!all_road_points_df$dup,], coords = c("lng", "lat"), 
                  crs = 4326, agr = "constant")

df_sf <- st_crop(df_sf, xmin = -76.65, xmax = -76.4, ymax = 44.275, ymin = 44.2)

lat <- 0
lng <- 0

for (i in 1:nrow(df_sf)){
  lng[i] <- round(df_sf$geometry[[i]][1], 4)
  lat[i] <- round(df_sf$geometry[[i]][2], 4)
}

df <- data.frame(long = lng, lat = lat)


source("D:/apikey.R")

library(googleway)
library(magick)


for(i in 1:2154){
  names <- list()
  names_c <- list()
  names[[1]] <- paste0("D:/Projects/kingston_streetview_test/data/images/",i,"N.png")
  names[[2]] <- paste0("D:/Projects/kingston_streetview_test/data/images/",i,"S.png")
  names[[3]] <- paste0("D:/Projects/kingston_streetview_test/data/images/",i,"E.png")
  names[[4]] <- paste0("D:/Projects/kingston_streetview_test/data/images/",i,"W.png")
  names_c[[1]] <- paste0("D:/Projects/kingston_streetview_test/data/images/",i,"N_c.png")
  names_c[[2]] <- paste0("D:/Projects/kingston_streetview_test/data/images/",i,"S_c.png")
  names_c[[3]] <- paste0("D:/Projects/kingston_streetview_test/data/images/",i,"E_c.png")
  names_c[[4]] <- paste0("D:/Projects/kingston_streetview_test/data/images/",i,"W_c.png")
  for(j in 1:4){
    png(names[[j]], width = 2449, height = 1425)  
    google_streetview(c(df$lat[i], df$long[i]),
                      key = my_key,
                      heading = (90*(j-1)),
                      fov = 120, 
                      size = c(2449,1425),
                      signature = "WRs2upbwHaP_de8QUn5jbO25Bt4=")
    dev.off()
    image <- image_read(names[[j]])
    image_write(image_crop(image,"2049x1025+200+200"), path = names_c[[j]])
  }
}

# also needed is a function to include short roads (under the segmentation length) 

roads_in_test_area$length <- st_length(roads_in_test_area)

roads_in_test_area$short <- roads_in_test_area$length < units::set_units(150,m)

short_roads <- roads_in_test_area[roads_in_test_area$short == TRUE,]

short_road_midpoints <- st_centroid(short_roads)

lat <- 0
lng <- 0

for (i in 1:nrow(short_road_midpoints)){
  lng[i] <- round(short_road_midpoints$geometry[[i]][1], 4)
  lat[i] <- round(short_road_midpoints$geometry[[i]][2], 4)
}

df_short <- data.frame(long = lng, lat = lat)

for(i in 176:2116){
  names <- list()
  names_c <- list()
  names[[1]] <- paste0("D:/Projects/kingston_streetview_test/data/images/s",i,"N.png")
  names[[2]] <- paste0("D:/Projects/kingston_streetview_test/data/images/s",i,"S.png")
  names[[3]] <- paste0("D:/Projects/kingston_streetview_test/data/images/s",i,"E.png")
  names[[4]] <- paste0("D:/Projects/kingston_streetview_test/data/images/s",i,"W.png")
  names_c[[1]] <- paste0("D:/Projects/kingston_streetview_test/data/images/s",i,"N_c.png")
  names_c[[2]] <- paste0("D:/Projects/kingston_streetview_test/data/images/s",i,"S_c.png")
  names_c[[3]] <- paste0("D:/Projects/kingston_streetview_test/data/images/s",i,"E_c.png")
  names_c[[4]] <- paste0("D:/Projects/kingston_streetview_test/data/images/s",i,"W_c.png")
  for(j in 1:4){
    png(names[[j]], width = 2449, height = 1425)  
    google_streetview(c(df_short$lat[i], df_short$long[i]),
                      key = my_key,
                      heading = (90*(j-1)),
                      fov = 120, 
                      size = c(2449,1425),
                      signature = "WRs2upbwHaP_de8QUn5jbO25Bt4=")
    dev.off()
    image <- image_read(names[[j]])
    image_write(image_crop(image,"2049x1025+200+200"), path = names_c[[j]])
  }
}

# run grid_attempt.rmd

load(here::here("data/processed/output_table_grid_kng.R"))

out_table_big <- output_table

load(here::here("data/processed/output_table_grid_kng_small.R"))

out_table_small <- output_table


library(caret)
library(AppliedPredictiveModeling)

for(i in 1:2154){
  out_table_big[[i]]$id <- i
}

for(i in 1:2116){
  out_table_small[[i]]$id <- i + 2154
}

Pixel_freq1 <- do.call(rbind.data.frame, out_table_big)
pix_small <- do.call(rbind.data.frame, out_table_small)

Pixel_freq <- rbind(Pixel_freq1, pix_small)

Pixel_freq <- Pixel_freq %>% group_by(id, Var1) %>% summarize(Mean_Freq = mean(Freq, na.rm = T))

pixel_freq2 <- Pixel_freq %>% spread(Var1, Mean_Freq, fill = 0) 

pixel_freq2 <- pixel_freq2 %>% rename(road = `0`, 
                                      sidewalk = `1`,
                                      building = `2`, 
                                      wall = `3`, 
                                      fence = `4`,
                                      pole = `5`,
                                      traffic_light = `6`,
                                      traffic_sign = `7`, 
                                      vegetation = `8`,
                                      terrain = `9`,
                                      sky = `10`,
                                      person = `11`,
                                      rider = `12`,
                                      car = `13`,
                                      truck = `14`,
                                      bus = `15`,
                                      train = `16`,
                                      motorcycle = `17`,
                                      bicycle = `18`)

A2 = t(t(pixel_freq2)/apply(pixel_freq2,2,max))

scaled_pixel_frequency <- as.data.frame(A2)

scaled_pixel_frequency %<>% select(-id) %>% mutate(id = 1:4270) 

short_road_midpoints2 <- select(short_road_midpoints, -bicycle) 
short_road_midpoints2 <- select(short_road_midpoints2, -sidewalk) 
short_road_midpoints2 <- select(short_road_midpoints2, -length) 
short_road_midpoints2 <- select(short_road_midpoints2, -short) 

df_sf2 <- select(df_sf, -dup)

combine_points <- rbind(df_sf2, short_road_midpoints2)

points_on_land <- combine_points

points_on_land$road          <- scaled_pixel_frequency$road          
points_on_land$sidewalk      <- scaled_pixel_frequency$sidewalk     
points_on_land$building      <- scaled_pixel_frequency$building     
points_on_land$wall          <- scaled_pixel_frequency$wall         
points_on_land$fence         <- scaled_pixel_frequency$fence        
points_on_land$pole          <- scaled_pixel_frequency$pole         
points_on_land$traffic_light <- scaled_pixel_frequency$traffic_light
points_on_land$traffic_sign  <- scaled_pixel_frequency$traffic_sign 
points_on_land$vegetation    <- scaled_pixel_frequency$vegetation   
points_on_land$terrain       <- scaled_pixel_frequency$terrain      
points_on_land$sky           <- scaled_pixel_frequency$sky          
points_on_land$person        <- scaled_pixel_frequency$person       
points_on_land$rider         <- scaled_pixel_frequency$rider        
points_on_land$car           <- scaled_pixel_frequency$car          
points_on_land$truck         <- scaled_pixel_frequency$truck        
points_on_land$bus           <- scaled_pixel_frequency$bus          
points_on_land$train         <- scaled_pixel_frequency$train        
points_on_land$motorcycle    <- scaled_pixel_frequency$motorcycle   
points_on_land$bicycle       <- scaled_pixel_frequency$bicycle    

save(points_on_land, file = "points_on_land_kng_all.rda")

ggplot(points_on_land) + geom_sf(aes(color = log(points_on_land$bicycle))) +
  scale_color_gradient()

roads_in_test_area <- st_crop(roads, xmin = -76.65, xmax = -76.4, ymax = 44.275, ymin = 44.2)

ggplot() + geom_sf(data = roads_in_test_area) +
  geom_sf(data = points_on_land, aes(color = log(points_on_land$person))) +
  scale_color_gradient(low = "grey", high = "blue") 

library(summarytools)

#### bicycle variable ####

av_bicycle <- points_on_land %>% group_by(road_elemen) %>% summarize(mean_bicycle = mean(log(bicycle)))

av_bicycle$mean_bicycle[is.infinite(av_bicycle$mean_bicycle)] <- NA

av_bicycle$mean_bicycle <- av_bicycle$mean_bicycle - min(av_bicycle$mean_bicycle, na.rm = T)
av_bicycle$mean_bicycle <- av_bicycle$mean_bicycle/max(av_bicycle$mean_bicycle, na.rm = T)

hist(av_bicycle$mean_bicycle)

temp <- as.data.frame(roads_in_test_area$road_elemen)

colnames(temp)<-"road_elemen"

roads_bike <- left_join(temp, select(as.data.frame(av_bicycle), -geometry), by = "road_elemen")

roads_in_test_area$bicycle <- roads_bike$mean_bicycle

ggplot() + geom_sf(data = roads_in_test_area, aes(color = bicycle)) + 
  scale_color_gradient(low = "red", high = "blue") 

#### sidewalks variable ####

av_sidewalk <- points_on_land %>% group_by(road_elemen) %>% summarize(mean_sidewalk= mean(log(sidewalk)))

av_sidewalk$mean_sidewalk[is.infinite(av_sidewalk$mean_sidewalk)] <- NA

av_sidewalk$mean_sidewalk <- av_sidewalk$mean_sidewalk - min(av_sidewalk$mean_sidewalk, na.rm = T)
av_sidewalk$mean_sidewalk <- av_sidewalk$mean_sidewalk/max(av_sidewalk$mean_sidewalk, na.rm = T)

hist(av_sidewalk$mean_sidewalk)

temp <- as.data.frame(roads_in_test_area$road_elemen)

colnames(temp)<-"road_elemen"

roads_sidewalk <- left_join(temp, select(as.data.frame(av_sidewalk), -geometry), by = "road_elemen")

roads_in_test_area$sidewalk <- roads_sidewalk$mean_sidewalk

calculated <- ggplot() + geom_sf(data = filter(roads_in_test_area, !is.na(sidewalk)), aes(color = sidewalk)) + 
  scale_color_gradient(low = "white", high = "blue") 

actual_sidewalks <- st_read(here::here("/data/raw/sidewalk-surface.shp"))

actual_sidewalks_in_test <- st_crop(actual_sidewalks, xmin = -76.65, xmax = -76.4, ymax = 44.275, ymin = 44.2)

actual <- ggplot() + geom_sf(data = actual_sidewalks_in_test)

multiplot(actual, calculated, cols = 1)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

