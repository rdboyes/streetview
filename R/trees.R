# kingston trees exploration

library(sf)
library(tidyverse)

st_read("data/kingston-trees/trees-municipal.shp") %>% 
  st_crop(c(xmin = -76.6, xmax = -76.475, ymin = 44.20, ymax = 44.27)) -> kingston_trees

load(here::here("data/points_on_land_kng_all.rda"))

kingston_roads <- st_read(here::here("data/rte/ONrte.shp")) 

kingston_roads <- kingston_roads %>% 
  st_transform(st_crs(points_on_land)) %>% 
  st_crop(st_bbox(points_on_land))

kingston_roads_utm <- kingston_roads %>% 
  st_crop(c(xmin = -76.6, xmax = -76.475, ymin = 44.20, ymax = 44.27)) %>% 
  st_transform("+proj=utm +zone=18 +units=m +datum=WGS84")

library(hkdatasets)
library(sf)
library(dplyr)
library(mapview)
library(tmap)

area_honeycomb_grid = st_make_grid(kingston_trees, 
                                   cellsize = c(.005, .005), 
                                   what = "polygons", 
                                   square = FALSE)

honeycomb_grid_sf = st_sf(area_honeycomb_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_honeycomb_grid)))

honeycomb_grid_sf$n_colli = lengths(st_intersects(honeycomb_grid_sf, kingston_trees))
honeycomb_count = filter(honeycomb_grid_sf, n_colli > 0)

st_intersects(honeycomb_grid_sf, points_on_land) |> 
  map_dbl(~sum(points_on_land$vegetation[.], na.rm = TRUE)) -> average_veg

average_veg[is.na(average_veg)] <- 0
  
honeycomb_grid_sf$average_veg <- average_veg

ggplot() + 
  geom_sf(aes(fill = n_colli), data = honeycomb_grid_sf, colour = "white") + 
  geom_sf(data = kingston_roads_utm, alpha = 0.8) +
  scale_fill_distiller(name = "Count of Trees", direction = 1) + 
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barwidth = 20, label.position = "bottom")) -> gt_tree_plot

ggplot() + 
  geom_sf(aes(fill = average_veg), data = honeycomb_grid_sf, colour = "white") + 
  geom_sf(data = kingston_roads_utm, alpha = 0.8) +
  scale_fill_distiller(name = "Sum Relative Vegetation", direction = 1) + 
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barwidth = 20, label.position = "bottom")) -> sv_tree_plot

library(patchwork)

kingston_tree_comparison_figure <- gt_tree_plot + sv_tree_plot

ggsave(kingston_tree_comparison_figure, file = "fig/kingston_tree_comparison_figure.png", dpi = 600)

#### Traffic 

st_intersects(honeycomb_grid_sf, points_on_land) |> 
  map_dbl(~mean(points_on_land$person[.], na.rm = TRUE)) -> av_person

st_intersects(honeycomb_grid_sf, points_on_land) |> 
  map_dbl(~mean(points_on_land$bicycle[.], na.rm = TRUE)) -> av_bicycle

st_intersects(honeycomb_grid_sf, points_on_land) |> 
  map_dbl(~mean(points_on_land$car[.], na.rm = TRUE)) -> av_car

st_intersects(honeycomb_grid_sf, points_on_land) |> 
  map_dbl(~mean(points_on_land$road[.], na.rm = TRUE)) -> av_road

av_road[is.na(av_road)] <- 0
av_car[is.na(av_car)] <- 0
av_bicycle[is.na(av_bicycle)] <- 0
av_person[is.na(av_person)] <- 0

honeycomb_grid_sf$av_road <- av_road
honeycomb_grid_sf$av_car <- av_car
honeycomb_grid_sf$av_bicycle <- av_bicycle
honeycomb_grid_sf$av_person <- log(av_person)

ggplot() + 
  geom_sf(aes(fill = av_person, color = I(ifelse(is.na(av_person), "#ffffff00", "#ffffff77"))), 
          data = honeycomb_grid_sf) + 
  geom_sf(data = kingston_roads_utm, alpha = 0.8) +
  scale_fill_distiller(
    na.value = "#ffffff00", 
    aesthetics = "fill",
    name = "Log Mean Relative Pixels", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barwidth = 20, label.position = "bottom")) -> sv_person

load("data/kingston_intersections.rda")

st_crs(points_on_land)

### Pedestrian

data_sf_all |> 
  st_set_crs(st_crs(points_on_land)) |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  mutate(quartile = ntile(foot_traffic_total, 4)) |> 
  ggplot() + 
    geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  geom_sf(aes(fill = quartile), colour = "black", shape = 21) +
  scale_colour_distiller(
    na.value = "#ffffff00", 
    aesthetics = "fill",
    name = "Quartile Foot Traffic", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") -> gt_traffic_person

points_on_land |> 
  mutate(quartile = ntile(person, 4)) |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  ggplot() + 
  geom_sf(aes(colour = quartile)) +
  geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  scale_colour_distiller(
    na.value = "#ffffff00", 
    aesthetics ="colour",
    name = "Quartile Person Pixel Count", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") -> sv_traffic_person


kingston_person_traffic <- sv_traffic_person + gt_traffic_person

ggsave(kingston_person_traffic, file = "fig/kingston_person_traffic.png", dpi = 600)

### bicycle

data_sf_all |> 
  st_set_crs(st_crs(points_on_land)) |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  mutate(quartile = ntile(bicycle_traffic_total, 4)) |> 
  ggplot() + 
  geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  geom_sf(aes(fill = quartile), colour = "black", shape = 21) +
  scale_colour_distiller(
    na.value = "#ffffff00", 
    aesthetics = "fill",
    name = "Quartile Bicycle Traffic", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") -> gt_traffic_bicycle

points_on_land |> 
  mutate(quartile = ntile(bicycle, 4)) |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  ggplot() + 
  geom_sf(aes(colour = quartile)) +
  geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  scale_colour_distiller(
    na.value = "#ffffff00", 
    aesthetics = "colour",
    name = "Quartile Bicycle Pixel Count", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") -> sv_traffic_bicycle


kingston_bicycle_traffic <- sv_traffic_bicycle + gt_traffic_bicycle

ggsave(kingston_bicycle_traffic, file = "fig/kingston_bicycle_traffic.png", dpi = 600)

### cars

data_sf_all |> 
  st_set_crs(st_crs(points_on_land)) |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  mutate(quartile = ntile(car_traffic_total, 4)) |> 
  ggplot() + 
  geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  geom_sf(aes(fill = quartile), colour = "black", shape = 21) +
  scale_colour_distiller(
    na.value = "#ffffff00", 
    aesthetics = "fill",
    name = "Quartile Vehicle Traffic", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") -> gt_traffic_vehicle

points_on_land |> 
  mutate(quartile = ntile(car, 4)) |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  ggplot() + 
  geom_sf(aes(colour = quartile)) +
  geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  scale_colour_distiller(
    na.value = "#ffffff00", 
    aesthetics = "colour",
    name = "Quartile Vehicle Pixel Count", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") -> sv_traffic_car

points_on_land |> 
  mutate(quartile = ntile(road, 4)) |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  ggplot() + 
  geom_sf(aes(colour = quartile)) +
  geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  scale_colour_distiller(
    na.value = "#ffffff00", 
    aesthetics = "colour",
    name = "Quartile Road Pixel Count", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") -> sv_traffic_road


kingston_vehicle_traffic <- sv_traffic_road + sv_traffic_car + gt_traffic_vehicle

ggsave(kingston_vehicle_traffic, file = "fig/kingston_vehicle_traffic.png", dpi = 600)

### Traffic Lights

data_sf_all |> 
  st_set_crs(st_crs(points_on_land)) |> 
  filter(intersection_type == "Traffic Signal") |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  ggplot() + 
  geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  geom_sf(fill = "red", colour = "black", shape = 21) +
  scale_colour_distiller(
    na.value = "#ffffff00", 
    aesthetics = "fill",
    name = "Quartile Vehicle Traffic", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") + 
  labs(caption = "Traffic Light Locations")-> gt_traffic_lights

points_on_land |> 
  mutate(quartile = ntile(traffic_light, 10)) |> 
  filter(quartile == 10) |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  ggplot() + 
  geom_sf(shape = 21, fill = "orange", colour = "black") +
  geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  scale_colour_manual(
    name = "Top Decile Pixel Count"
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(caption = "Top Decile Pixel Count (Traffic Lights)") -> sv_traffic_light

kingston_traffic_lights <- sv_traffic_light + gt_traffic_lights

ggsave(kingston_traffic_lights, file = "fig/kingston_traffic_lights.png", dpi = 600)

### Traffic Signs

data_sf_all |> 
  st_set_crs(st_crs(points_on_land)) |> 
  filter(!intersection_type == "Traffic Signal") |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  ggplot() + 
  geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  geom_sf(fill = "red", colour = "black", shape = 21) +
  scale_colour_distiller(
    na.value = "#ffffff00", 
    aesthetics = "fill",
    name = "Quartile Vehicle Traffic", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") + 
  labs(caption = "Traffic Signed Intersections") -> gt_traffic_signs

points_on_land |> 
  mutate(quartile = ntile(traffic_sign, 10)) |> 
  filter(quartile == 10) |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  ggplot() + 
  geom_sf(shape = 21, fill = "orange", colour = "black") +
  geom_sf(data = kingston_roads_utm, alpha = 0.5) +
  scale_colour_manual(
    name = "Top Decile Pixel Count"
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(caption = "Top Decile Pixel Count (Traffic Signs)") -> sv_traffic_signs

kingston_traffic_signs <- sv_traffic_signs + gt_traffic_signs 

ggsave(kingston_traffic_signs, file = "fig/kingston_traffic_signs.png", dpi = 600)  

load(here::here("data/sidewalks_confusion_plot.rda"))

library(scales)

### Sidewalks

load(file = "data/sc_4326.rda")

sidewalks_confusion_4326 <- ggplot() + 
  geom_sf(data = filter(sc_4326, !is.na(SUFTYPE)), aes(color = class), key_glyph = "rect") + 
  scale_color_manual(values = c("True Positive" = "#3471eb",
                                "True Negative" = "blue",
                                "False Positive" = "red",
                                "False Negative" = "orange"),
                     name = "Accuracy") +
  theme(legend.position = "bottom") + 
  xlim(-76.62, -76.478) +
  ylim(44.21, 44.265) +
  theme_classic()

ggsave(sidewalks_confusion_4326, file = "fig/sidewalks_confusion.png", dpi = 600) 

# sc_4326 %>% 
#   st_crop(kng_bbox) %>% 
#   st_drop_geometry() %>% 
#   group_by(class) %>% 
#   summarize(Meters = sum(RDLEN_M, na.rm = T))

tibble(Predicted = c("Yes", "No"), Yes = c("356.0", "76.1"), No = c("39.5", "41.2")) %>% 
  gt::gt() %>% 
  gt::tab_spanner("Ground Truth (km)", c("Yes", "No")) -> confusion_matrix_sidewalks



sidewalk_measure_in_buffer <- st_join(st_transform(sc_4326, st_crs(kng_manhattan_buffers)),
                                      kng_manhattan_buffers, join = st_within)


sidewalk_buffer_accuracy <- sidewalk_measure_in_buffer %>% 
  st_drop_geometry() %>% 
  group_by(Name, class) %>% 
  summarize(value = sum(RDLEN_M, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "class", values_from = "value") %>% 
  tidyr::replace_na(list("True Positive" = 0,
                         "True Negative" = 0,
                         "False Positive" = 0,
                         "False Negative" = 0)) %>% 
  mutate(sv = (`True Positive` + `False Positive`),
         gt = (`True Positive` + `False Negative`)) %>% 
  select(Name, sv, gt)

sidewalk_accuracy_plot <- ggplot(data = sidewalk_buffer_accuracy) + 
  geom_point(aes(x = sv, y = gt), alpha = 0.1) +
  scale_x_log10() + 
  scale_y_log10() + 
  theme_classic() + 
  labs(x = "Predicted Sidewalk Length (m)",
       y = "Actual Sidewalk Length (m)")

points_on_land |> 
  st_crop(st_bbox(st_transform(kingston_roads_utm, st_crs(points_on_land)))) |> 
  ggplot() + 
  geom_sf() + 
  geom_sf(data = kingston_roads_utm) + 
  theme_classic() -> kingston_points

ggsave(kingston_points, file = "fig/figure1.png", dpi = 600)
