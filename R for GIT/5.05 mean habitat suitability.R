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
year <- seq(2012,2019)
year_future <- seq(2092,2099)

present_suitability <- vector(length = 96)
for (y in seq_along(year)) {
  for (i in months) {
    #PAST
    NM_2016 <- filter(NM,Year == year[y] & Month == i)
    NM_new <- NM_2016 %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                            Bathymetry,Ice_conc))
    tst <- predict(object = habitat_model,x = NM_new)  
    
    NM_2016$fishing_model <- tst
    
    
    binary_predictions_2016 <- ifelse(tst > 0.5, 1, 0)
    NM_2016$fishing_model_binary <- binary_predictions_2016
    present_suitability[(y - 1) * length(months) + i] <- (sum(NM_2016$fishing_model_binary == 1)/nrow(NM_2016)) * 100
  }
}

future_suitability <- vector(length = 96)
for (y in seq_along(year_future)) {
  for (i in months) {
    #future
    NM_2096 <- filter(NM_future,Year == year_future[y] & Month == i)
    #colnames(NM_2096)[3] <- "ice"
    NM_new <- NM_2096 %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                            Bathymetry,Ice_conc))
    tst <- predict(object = habitat_model,x = NM_new)  
    
    NM_2096$fishing_model <- tst
    binary_predictions <- ifelse(tst > 0.5, 1, 0)
    NM_2096$fishing_model_binary <- binary_predictions
    #calculate mean habitat suitability (1-suitable,0-not suitable)
    future_suitability[(y - 1) * length(months) + i] <- (sum(NM_2096$fishing_model_binary == 1)/nrow(NM_2096)) * 100
    
  }
}
print(paste0("Present Fishing Suitability: ", round(mean(present_suitability),digits = 2),"%"))
print(paste0("Future Fishing Suitability: ", round(mean(future_suitability),digits = 2),"%"))

#this is for all years
present_df <- data.frame(percentage = present_suitability,
                        month = rep(seq(1,12),every = 12))

present_averages <- present_df %>%
  group_by(month) %>%
  summarize(average_percentage = mean(percentage))
present_averages$Time <- "Present"

future_df <- data.frame(percentage = future_suitability,
                        month = rep(seq(1,12),every = 12))

future_averages <- future_df %>%
  group_by(month) %>%
  summarize(average_percentage = mean(percentage))
future_averages$Time <- "Future"

final_average <- rbind(present_averages,future_averages)

#plot to show changing mean habitat suitability
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ggplot() +
  geom_rect(aes(xmin = 1, xmax = 3, ymin = 0, ymax = 50), fill = "lightblue", color = "NA", alpha = 0.3) +
  geom_rect(aes(xmin = 3, xmax = 6, ymin = 0, ymax = 50), fill = "green", color = "NA", alpha = 0.3) +
  geom_rect(aes(xmin = 6, xmax = 9, ymin = 0, ymax = 50), fill = "red", color = "NA", alpha = 0.3) +
  geom_rect(aes(xmin = 9, xmax = 11, ymin = 0, ymax = 50), fill = "orange", color = "NA", alpha = 0.3) +
  geom_rect(aes(xmin = 11, xmax = 12, ymin = 0, ymax = 50), fill = "lightblue", color = "NA", alpha = 0.3) +
  geom_line(data = final_average, aes(x = month, y = average_percentage, color = Time)) +
  geom_point(data = final_average, aes(x = month, y = average_percentage, color = Time)) +
  labs(x = "Month", y = "Monthly Average Habitat Suitability (%)") +
  scale_x_continuous(breaks = seq(1, 12), labels = month_names) +
  #scale_y_continuous(expand = c(0,0)) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold")) +
  NULL 


ggsave("./fishing/Future Prediction/Figures/mean fishing suitability.png",width = 33.867,height = 19.05,units = "cm")
