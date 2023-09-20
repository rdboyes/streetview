ggplot(data = kng_int_utm) + geom_sf() + geom_sf(data = kingston_roads_utm)

all_int <- st_intersection(kingston_roads_utm)

near_known_intersections <- st_buffer(kng_int_utm, 100)

near_all <- st_union(near_known_intersections)

all_int <- all_int %>% filter(st_geometry_type(.) == "POINT")

st_within(all_int, near_all) %>% as.data.frame() -> is_in

all_int %>% 
  mutate(id = 1:nrow(all_int)) %>% 
  filter(!id %in% is_in$row.id) -> no_signs

ggplot() + 
  geom_sf(data = no_signs) +
  geom_sf(data = near_all)

save(no_signs, file = "no_signs.rda")
