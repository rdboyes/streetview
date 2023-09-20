# sidewalks correlation coefficients

library(sf)

load(here::here("data/points_on_land_kng_all.rda"))

kingston_roads <- st_read(here::here("data/rte/ONrte.shp")) 

kingston_roads <- kingston_roads %>% 
  st_transform(st_crs(points_on_land)) %>% 
  st_crop(st_bbox(points_on_land))

kingston_roads_utm <- st_transform(kingston_roads, "+proj=utm +zone=18 +units=m +datum=WGS84")

road_buffers <- st_buffer(kingston_roads_utm, 25)

sidewalks <- st_read(here::here("data/kingston_sidewalks/sidewalk-surface.shp"))

sidewalks_utm <- sidewalks %>% 
  st_transform(st_crs(points_on_land)) %>% 
  st_crop(st_bbox(points_on_land)) %>% 
  st_transform(st_crs(road_buffers)) %>% 
  filter(material == "CONCRETE")

points_with_sidewalk <- filter(points_on_land, sidewalk > 0.01) %>% 
  st_transform(st_crs(kingston_roads_utm))

sv_sidewalk_yn <- lengths(st_contains(road_buffers, points_with_sidewalk)) > 0
gt_sidewalk_yn <- lengths(st_intersects(road_buffers, sidewalks_utm)) > 0

install.packages('tidymodels')

conf <- tibble::tibble(gt = as.factor(gt_sidewalk_yn), sv = as.factor(sv_sidewalk_yn))

save(conf, file = here::here("data/sidewalk_conf.rda"))

library(tidymodels)

summary(conf_mat(conf, truth = gt, estimate = sv))

class <- as.factor(case_when(sv_sidewalk_yn & gt_sidewalk_yn ~ "True Positive",
                   sv_sidewalk_yn & !gt_sidewalk_yn ~ "False Positive",
                   !sv_sidewalk_yn & gt_sidewalk_yn ~ "False Negative",
                   !sv_sidewalk_yn & !gt_sidewalk_yn ~ "True Negative"))

kingston_roads_utm$class <- class

sidewalks_confusion <- ggplot() + 
  geom_sf(data = kingston_roads_utm, aes(color = class)) + theme_minimal() +
  scale_color_brewer(palette = "Set1")

save(sidewalks_confusion, file = here::here("data/sidewalks_confusion_plot.rda"))
