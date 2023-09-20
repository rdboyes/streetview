library(sf)
library(tidyverse)
library(rvest)
library(rlist)
library(httr)

van_int <- st_read("E:\\Documents\\GitHub\\streetview-paper\\data\\intersection-traffic-movement-counts\\intersection-traffic-movement-counts.shp")

get_recent <- function(coordinate = "0020670"){
  for(year in 2011:1989){
    url <- paste0("https://app.vancouver.ca/vanmapmtc_net/diagrams/reports/",
                  year,
                  "/",
                  coordinate,
                  ".htm")
    
    content <- try({read_html(url)})
    
    if(!inherits(content, "try-error")){
      most_recent_year <- year
      break
    }
  }
  
  result <- content %>% html_table(fill = TRUE) %>% 
    list.clean(function(x){nrow(x) != 2}) %>% 
    list.clean(function(x){!"Total" %in% colnames(x)}) %>% 
    purrr::map(.f = function(x){
      x %>% 
        select(any_of(c("North", "South", "East", "West", "Peds", "Bikes", "Total"))) %>% 
        pivot_longer(1, names_to = "Direction", values_to = "Time")
    }) %>% 
    bind_rows()
  
  result$Coordinate <- coordinate
  result$Year <- most_recent_year
  
  return(result)
}

get_coordinate <- function(url){
  str_extract(url, "#.*") %>% 
  str_replace("#", "0")
}

van_int2 <- van_int %>% 
  mutate(coordinate = get_coordinate(url))

van_intersections_traffic <- list()

for(i in 1:nrow(van_int2)){
  van_intersections_traffic[[i]] <- try({get_recent(van_int2$coordinate[i])})
  print(i)
  Sys.sleep(3)
}

save(van_intersections_traffic, file = "van_intersections_traffic.rda")

list.clean(van_intersections_traffic, function(x){!inherits(x, "data.frame")}) %>% bind_rows() -> van_int_df

save(van_int_df, file = "van_int_df.rda")

van_int_df %>% 
  filter(stringr::str_detect(Time, "Max")) %>% 
  group_by(Coordinate) %>% 
  summarise(across(Peds:Total, function(x){sum(x, na.rm = TRUE)})) -> sum_df

van_int_df_counts <- left_join(van_int2, sum_df, by = c("coordinate" = "Coordinate"))

save(van_int_df_counts, file = "van_int_df_counts.rda")
