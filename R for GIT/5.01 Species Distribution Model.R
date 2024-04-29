rm(list = ls()) #reset

library(dismo)
library(rJava)
library(tidyverse)
library(rnaturalearth)
library(sf)
NM <- read.csv("./fishing/Future Prediction/Objects/2.05 NM_fishing_sediment_ports_ridge.csv",header = TRUE) %>% 
  subset(select = -c(Zonal,Meridional)) #load data
basemap <- ne_countries(scale = "large", country = "Greenland",returnclass = "sf") %>% 
  st_transform(crs = 3035) #import basemap

#start with just one year
NM_2019 <- filter(NM,Year == "2019")
NM_2018 <- filter(NM,Year == "2018")

NM_coords <- data.frame(fishing = NM_2019$fishing)
NM_coords$fishing <- ifelse(NM_coords$fishing > 0, 1, 0) #conver to binary
NM_coords_vector <- as.vector(NM_coords[,1])
NM_predictors <- NM_2019 %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                               Bathymetry,ice,distance_to_ridge)) #define predictors (will eventually go off information value)

habitat_model <- maxent(x = NM_predictors,p = NM_coords_vector)

months <- seq(1,12)

for (month in months) {
  print(month)
  NM_2017 <- filter(NM,Year == "2017" & Month == paste0(month))
  NM_new <- NM_2017 %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                          Bathymetry,ice,distance_to_ridge))
  tst <- predict(object = habitat_model,x = NM_new)  
  
  NM_2017$fishing_model <- tst
  
  
  binary_predictions <- ifelse(tst > 0.5, 1, 0)
  NM_2017$fishing_model_binary <- binary_predictions
  ggplot() +
    geom_point(data = NM_2017,aes(x = longitude,y = latitude,color = fishing_model)) +
    scale_color_viridis_c(begin = 0,end = 1,limits = c(0,1)) +
    geom_sf(data = basemap) +
    ggtitle(paste0("Month ",month)) +
    #lims(color = c(0,1)) +
    labs(color = "Probability of Fishing",x = "Longitude",y = "Latitude") +
    NULL
  ggsave(paste0("./fishing/Future Prediction/Figures/probability/fishing_probability_2017_",month,".png"),width = 33.867,height = 19.05,units = "cm",bg = 'white')
}

