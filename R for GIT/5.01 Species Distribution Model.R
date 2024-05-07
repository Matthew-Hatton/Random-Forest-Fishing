rm(list = ls()) #reset

library(dismo)
library(rJava)
library(tidyverse)
library(rnaturalearth)
library(sf)
# NM <- read.csv("./fishing/Future Prediction/Objects/2.05 NM_fishing_sediment_ports_ridge.csv",header = TRUE) %>% 
#   subset(select = -c(Zonal,Meridional)) #load data
NM <- read.csv("./fishing/Future Prediction/Objects/2.02 NM_fishing.csv",header = TRUE) %>% 
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
                                               Bathymetry,Ice_conc)) #define predictors (will eventually go off information value)

habitat_model <- maxent(x = NM_predictors,p = NM_coords_vector)
saveRDS(habitat_model,"./fishing/Future Prediction/Future Parametrisation/Objects/model_nemoice.RDS") #save model
