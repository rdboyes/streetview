
ggplot(points_on_land) + geom_sf(aes(color = log(points_on_land$bicycle))) +
  scale_color_gradient()

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
