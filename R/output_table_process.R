load("D:/Projects/cityscapes-nn/xception_van_e.rda")
load("D:/Projects/cityscapes-nn/xception_van_s.rda")
load("D:/Projects/cityscapes-nn/xception_van_n.rda")
load("D:/Projects/cityscapes-nn/xception_van_w.rda")

library(tidyverse)
library(caret)
library(AppliedPredictiveModeling)

for(i in 1:14858){
  output_table_e[[i]]$id <- i
  output_table_s[[i]]$id <- i
  output_table_w[[i]]$id <- i
  output_table_n[[i]]$id <- i
}

Pixel_freq_e <- do.call(rbind.data.frame, output_table_e) %>% rename(type = matrixE)
Pixel_freq_w <- do.call(rbind.data.frame, output_table_w) %>% rename(type = matrixW)
Pixel_freq_n <- do.call(rbind.data.frame, output_table_n) %>% rename(type = matrixN)
Pixel_freq_s <- do.call(rbind.data.frame, output_table_s) %>% rename(type = matrixS)

Pixel_freq <- bind_rows(Pixel_freq_e,
                        Pixel_freq_w,
                        Pixel_freq_n,
                        Pixel_freq_s)



Pixel_freq <- Pixel_freq %>% group_by(id, type) %>% summarize(Mean_Freq = sum(Freq, na.rm = T))

pixel_freq2 <- Pixel_freq %>% spread(type, Mean_Freq, fill = 0) 

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

scaled_pixel_frequency %<>% select(-id) %>% mutate(id = 1:14858) 




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

save(points_on_land, file = "points_on_land.R")