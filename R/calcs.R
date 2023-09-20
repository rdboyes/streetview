library(sf)
library(tidyverse)

load("van_int_df.rda")
van_int <- st_read("E:\\Documents\\GitHub\\streetview-paper\\data\\intersection-traffic-movement-counts\\intersection-traffic-movement-counts.shp")

get_coordinate <- function(url){
  str_extract(url, "#.*") %>% 
    str_replace("#", "0")
}

van_int2 <- van_int %>% 
  mutate(coordinate = get_coordinate(url))

van_int_df %>% 
  filter(stringr::str_detect(Time, "Max")) %>% 
  group_by(Coordinate) %>% 
  summarise(across(Peds:Total, function(x){sum(x, na.rm = TRUE)})) -> sum_df

left_join(van_int2, sum_df, by = c("coordinate" = "Coordinate")) -> geo_int

ggplot(data = geo_int) +
  geom_sf(aes(size = Bikes))

