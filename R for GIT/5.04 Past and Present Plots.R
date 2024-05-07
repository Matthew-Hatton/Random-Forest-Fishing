#overlay

rm(list = ls()) #reset

library(dismo)
library(rJava)
library(tidyverse)
library(rnaturalearth)
library(sf)
habitat_model <- readRDS("./fishing/Future Prediction/Future Parametrisation/Objects/model_nemoice.RDS")#load model
NM_future <- read.csv("./fishing/Future Prediction/Future Parametrisation/Objects/FutureForModel.csv") %>% 
  st_as_sf(coords = c('longitude','latitude'),crs = 4326) %>% 
  st_transform(crs = 3035)#load data
coords <- st_coordinates(NM_future) #get coords
NM_future <- cbind(NM_future, latitude = coords[, "Y"], longitude = coords[, "X"]) %>% 
  st_drop_geometry() #transform crs

months <- seq(1,12)
basemap <- ne_countries(scale = "large", country = "Greenland",returnclass = "sf") %>% 
  st_transform(crs = 3035) #import basemap

NM <- read.csv("./fishing/Future Prediction/Objects/2.02 NM_fishing.csv",header = TRUE) %>% 
  subset(select = -c(Zonal,Meridional)) #load data

NM_coords <- data.frame(fishing = NM$fishing)
NM_coords$fishing <- ifelse(NM_coords$fishing > 0, 1, 0) #conver to binary
NM_coords_vector <- as.vector(NM_coords[,1])
NM_predictors <- NM %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                               Bathymetry,Ice_conc)) #define predictors (will eventually go off information value)

for (i in months) {
  #future
  NM_2096 <- filter(NM_future,Year == "2099" & Month == i)
  #colnames(NM_2096)[3] <- "ice"
  NM_new <- NM_2096 %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                          Bathymetry,Ice_conc))
  tst <- predict(object = habitat_model,x = NM_new)  
  
  NM_2096$fishing_model <- tst
  binary_predictions <- ifelse(tst > 0.4, 1, 0)
  NM_2096$fishing_model_binary <- binary_predictions
  
  
  #PAST
  NM_2016 <- filter(NM,Year == "2019" & Month == i)
  NM_new <- NM_2016 %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                          Bathymetry,Ice_conc))
  tst <- predict(object = habitat_model,x = NM_new)  
  
  NM_2016$fishing_model <- tst
  
  
  binary_predictions_2016 <- ifelse(tst > 0.4, 1, 0)
  NM_2016$fishing_model_binary <- binary_predictions_2016
  NM_2016_filter <- filter(NM_2016,fishing_model_binary == 1)
  
  
  
  ggplot() +
    geom_point(data = NM_2096,aes(x = longitude,y = latitude,color = fishing_model_binary)) +
    geom_point(data = NM_2016_filter,aes(x = longitude,y = latitude),color = "red",alpha = 0.4) +
    scale_color_viridis_c(begin = 0,end = 1,limits = c(0,1)) +
    geom_sf(data = basemap) +
    #lims(color = c(0,1)) +
    ggtitle(paste0("Month ",i)) +
    labs(color = "Probability of Fishing",x = "Longitude",y = "Latitude") +
    theme(legend.position = "none") +
    NULL
  ggsave(paste0("./fishing/Future Prediction/Figures/BOTH YEARS/2019/fishing_probability_binary_BOTH_NEMO",i,".png"),width = 33.867,height = 19.05,units = "cm",bg = 'white')
}
