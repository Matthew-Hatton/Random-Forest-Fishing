rm(list = ls()) #reset
library(ggplot2)
library(rnaturalearth)
library(tidyverse)
library(sf)
library(furrr)

##### Initialise #####
source("./packages/nowtpipe.R")
plan(multisession)
basemap <- ne_countries(scale = "large", country = "Greenland",returnclass = "sf") %>% 
  st_transform(crs = 3035) #import basemap
Domain <- readRDS("./fishing/Future Prediction/Objects/Buffered Domain.rds") %>% #load domain polygon
  st_transform(crs = 3035)#transform domain crs
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255) #transparent lines in background of plots
add_month_column <- function(df) {
  df$month <- as.numeric(substr(df$date, 6, 7))
  return(df)
}

### Global Fishing Watch ### 
GFW_sf <- list.files( "./fishing/Global Fishing Watch/finished data", pattern = "\\GFW_201[2-9]\\.csv$", full.names = TRUE) %>% 
  future_map(.,.f = read.csv) #read all GFW files

for (i in seq_along(GFW_sf)) {
  sf_object <- st_as_sf(GFW_sf[[i]], coords = c("cell_ll_lon", "cell_ll_lat"), crs = 4326) %>%
    st_transform(crs = 3035)
  sf_object$month <- as.numeric(substr(sf_object$date, 6, 7)) #adds months
  sf_object$year <- as.numeric(substr(sf_object$date, 1, 4)) #adds months
  sf_object <- st_intersection(sf_object,Domain) %>% 
    group_by(year,month, geometry) %>%
    summarize(avg_fishing_hours = mean(fishing_hours, na.rm = TRUE)) %>% #calc means
    filter(avg_fishing_hours > 0) #filter out 0s
  coords <- st_coordinates(sf_object) #get coords
  sf_object <- cbind(sf_object, latitude = coords[, "Y"], longitude = coords[, "X"]) %>% 
    st_drop_geometry() #create new lat lon cols and drop geometry
  write.csv(sf_object,paste0("./fishing/Future Prediction/Objects/cropped GFW/GFW_mean",i,".csv"),row.names = FALSE) #save each year (just in case)
  if (i == 1) {
    GFW_mean <- sf_object
  } else {
    GFW_mean <- rbind(GFW_mean, sf_object)
  }
  print(paste0(i,"/",length(GFW_sf)))
}
write.csv(GFW_mean,"./fishing/Future Prediction/Objects/cropped GFW/GFW_mean_all_years.csv") #save here in case of crash - Ronnie likes to turn PC off when doing large spatial operations