# process the output of the neural net

load(here::here('data/xception_van_e.rda'))
load(here::here('data/xception_van_n.rda'))
load(here::here('data/xception_van_w.rda'))
load(here::here('data/xception_van_s.rda'))
load(here::here('data/streetview_points.rda'))

library(sf)

locations <- st_as_sf(streetview_points, coords = c('lng', 'lat'), crs = 4326)

mobilenet <- purrr::map(output_table_e, ~pivot_wider(., names_from = matrixE, values_from = Freq))
mobilenet_w <- purrr::map(output_table_w, ~pivot_wider(., names_from = matrixW, values_from = Freq))
mobilenet_s <- purrr::map(output_table_s, ~pivot_wider(., names_from = matrixS, values_from = Freq))
mobilenet_n <- purrr::map(output_table_n, ~pivot_wider(., names_from = matrixN, values_from = Freq))

mobilenet_df_e <- do.call(plyr::rbind.fill, mobilenet)
mobilenet_df_w <- do.call(plyr::rbind.fill, mobilenet_w)
mobilenet_df_s <- do.call(plyr::rbind.fill, mobilenet_s)
mobilenet_df_n <- do.call(plyr::rbind.fill, mobilenet_n)


mobilenet_df_e %>% rename(road = `0`, 
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
                    bicycle = `18`) -> mobilenet_final_e

mobilenet_df_w %>% rename(road = `0`, 
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
                          bicycle = `18`) -> mobilenet_final_w

mobilenet_df_s %>% rename(road = `0`, 
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
                          bicycle = `18`) -> mobilenet_final_s

mobilenet_df_n %>% rename(road = `0`, 
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
                          bicycle = `18`) -> mobilenet_final_n


mobilenet_final_e[is.na(mobilenet_final_e)] <- 0
mobilenet_final_w[is.na(mobilenet_final_w)] <- 0
mobilenet_final_n[is.na(mobilenet_final_n)] <- 0
mobilenet_final_s[is.na(mobilenet_final_s)] <- 0

locations$road <- mobilenet_final_e$road + 
  mobilenet_final_w$road + 
  mobilenet_final_n$road + 
  mobilenet_final_s$road

locations$sidewalk <- mobilenet_final_e$sidewalk + 
  mobilenet_final_w$sidewalk + 
  mobilenet_final_n$sidewalk + 
  mobilenet_final_s$sidewalk

locations$traffic_light <- mobilenet_final_e$traffic_light + 
  mobilenet_final_w$traffic_light + 
  mobilenet_final_n$traffic_light + 
  mobilenet_final_s$traffic_light

locations$vegetation <- mobilenet_final_e$vegetation + 
  mobilenet_final_w$vegetation + 
  mobilenet_final_n$vegetation + 
  mobilenet_final_s$vegetation

locations$person <- mobilenet_final_e$person + 
  mobilenet_final_w$person + 
  mobilenet_final_n$person + 
  mobilenet_final_s$person

locations$truck <- mobilenet_final_e$truck + 
  mobilenet_final_w$truck + 
  mobilenet_final_n$truck + 
  mobilenet_final_s$truck

locations$car <- mobilenet_final_e$car + 
  mobilenet_final_w$car + 
  mobilenet_final_n$car + 
  mobilenet_final_s$car

save(locations, file = here::here('data/locations_xc.rda'))



locations_utm <- st_transform(locations, "+proj=utm +zone=10 +units=m +datum=WGS84")
buffers_50 <- st_buffer(locations_utm, 50)


trees <- st_read(here::here('data/street-trees.geojson'))
trees <- st_crop(trees, st_bbox(locations_utm))

trees_utm <- st_transform(trees, "+proj=utm +zone=10 +units=m +datum=WGS84")

no_short_trees <- trees_utm %>% filter(height_range_id >= 3)

buffers_50$tree_count <- lengths(st_intersects(buffers_50, trees_utm))
buffers_50$tall_tree_count <- lengths(st_intersects(buffers_50, no_short_trees))

save(buffers_50, file = here::here('data/buffers_van_50.rda'))

man_500 <- st_read(here::here("data/man-buffers/500m.shp"))

van_500 <- st_transform(man_500, st_crs(locations_utm))
van_500 <- st_crop(van_500, st_bbox(locations_utm))
van_500$num_trees <- lengths(st_intersects(van_500, trees_utm))

sv_in_manhattan <- st_intersects(van_500, locations_utm)

av_tree <- list()

for(i in 1:11986){
  if(length(sv_in_manhattan[[i]]) == 0){
    av_tree[[i]] <- 0
  }else{
    av_tree[[i]] <- mean(locations_utm$vegetation[sv_in_manhattan[[i]]], na.rm = T) 
  }
}

av_tree_df <- do.call(rbind.data.frame, av_tree)

van_500$SV_average_vegetation <- av_tree_df[,1]

ggplot(data = van_500, aes(y = SV_average_vegetation, x = num_trees)) + geom_point() + geom_smooth(method = "lm")

save(van_500, file = here::here("data/buffers_postal.rda"))


int<- st_read(here::here('data/street-intersections/street-intersections.shp'))
st_crop(traffic, locations) -> crop_traffic

#directional-traffic-count-locations.shp
