# hex grid tree plots

library(tidyverse)
library(sf)

load(here::here('data/locations_xc.rda')) # vancouver streetview images
locations_utm <- st_transform(locations, "+proj=utm +zone=10 +units=m +datum=WGS84")

trees <- st_read(here::here('data/street-trees.geojson')) # vancouver trees
trees_utm <- st_transform(trees, "+proj=utm +zone=10 +units=m +datum=WGS84")
trees_utm <- st_crop(trees_utm, st_bbox(locations_utm))

area <- st_bbox(locations) 
area[4] <- 49.295

area %>% st_as_sfc() %>% st_transform(st_crs(locations_utm)) -> area

if (!file.exists(here::here("data/hex-grid-500.rds"))) {
  
  st_make_grid(
    area,
    n = c(60, 60),
    crs = st_crs(area),
    what = "polygons",
    square = FALSE
  ) -> grid
  
  grid <- st_sf(index = 1:length(lengths(grid)), grid)
  
  saveRDS(grid, here::here("data/hex-grid-500.rds"))
  
}

grid <- readRDS(here::here("data/hex-grid-500.rds"))

hexbin <- st_join(trees_utm, grid, join = st_intersects)

load("van_rte.rda")

van_roads <- st_crop(st_transform(van_rte, st_crs(tree_count)), st_bbox(tree_count))

grid %>%
  left_join(
    count(hexbin, index) %>%
      as_tibble() %>%
      select(index, ct=n)
  ) -> tree_count

ggplot() + 
  geom_sf(
    data = tree_count, size = 0.125,
    aes(fill = ct, color = I(ifelse(is.na(ct), "#ffffff00", "#ffffff77")))
  ) +
  geom_sf(data = van_roads, alpha = 0.2) + 
  scale_fill_distiller(
    na.value = "#ffffff00", 
    aesthetics = "fill",
    name = "# Trees", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barwidth = 20, label.position = "bottom"))  -> gt_trees_plot_van

hexsv <- st_join(locations_utm, grid, join = st_intersects)

hexsv %>%
  mutate(rel_vegetation = vegetation/max(vegetation, na.rm = TRUE)) |> 
  group_by(index) %>% 
  summarize(sv_tree = sum(rel_vegetation, na.rm = T)) %>% 
  as_tibble() %>% 
  left_join(grid, .) -> sv_count

sv_count
  
ggplot() +
  geom_sf(
    data = sv_count, size = 0.125,
    aes(fill = sv_tree, color = I(ifelse(is.na(sv_tree), "#ffffff00", "#ffffff77")))
  ) +
  geom_sf(data = van_roads, alpha = 0.2) + 
  scale_fill_distiller(
    na.value = "#ffffff00", 
    aesthetics = "fill",
    name = "Sum Relative Pixels", direction = 1
  ) + 
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(barwidth = 20, label.position = "bottom")) -> sv_trees_plot_van

sv_trees_plot_van
# 
# normalized_sv_tree <- (sv_count$sv_tree - mean(sv_count$sv_tree, na.rm = T))/sd(sv_count$sv_tree, na.rm = T)
# normalized_true_count <- (tree_count$ct - mean(tree_count$ct, na.rm = T))/sd(tree_count$ct, na.rm = T)
# 
# normalized_difference <- normalized_sv_tree - normalized_true_count
# 
# sv_count$normalized_diff <- normalized_difference
# 
# ggplot() + 
#   geom_sf(
#     data = filter(sv_count, !is.na(normalized_diff)), size = 0.125,
#     aes(fill = normalized_diff, color = I(ifelse(is.na(normalized_diff), "#ffffff00", "#ffffff77")))
#   ) +
#   scale_fill_distiller(palette = "RdBu") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   labs(fill = "Difference between normalized streetview and normalized count.") -> sv_diff_plot
# 
# ggsave(sv_diff_plot, file = "sv_diff.png")


library(patchwork)

tree_hex_map <- gt_trees_plot_van + sv_trees_plot_van

ggsave(tree_hex_map, file = "fig/tree_hex_map_van.png", dpi = 600)
