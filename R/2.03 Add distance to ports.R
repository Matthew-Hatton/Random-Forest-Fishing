rm(list = ls()) #reset
library(tidyverse)
library(sf)
library(geosphere)

NM <- read.csv("./fishing/Future Prediction/Objects/2.XX NM_fishing_sediment.csv") %>% 
  st_as_sf(coords = c("longitude","latitude"),crs = 3035)#load data with correct crs

ports <- data.frame(Port = c("Nuuk","Sisimiut","Qasigiannguit"),
                    Longitude = c(-51.719538,-53.679031,-51.181939),
                    Latitude = c(64.166343,66.940810,68.819677)) %>% 
  st_as_sf(coords = c("Longitude","Latitude"),crs = 4326) %>% 
  st_transform(crs = 3035) #add points for each port (from the web)


#actually only need to do this on one day, so the value will repeat
single <- filter(NM,Year == "2012" &
                   Month == "1")

dist_matrix <- matrix(NA, nrow = nrow(single), ncol = nrow(ports)) #initialise matrix to store values
for (i in 1:nrow(single)) {
  #calculate distance from point to port
  for (j in 1:nrow(ports)) {
    dist_matrix[i, j] <- st_distance(single[i,], ports[j,])
    print(paste0("finished, ",i," ",j))
  }
}

#extract distance columns (they're in m, need km)
distance_to_N <- dist_matrix[, 1]/1000
distance_to_S <- dist_matrix[, 2]/1000
distance_to_Q <- dist_matrix[, 3]/1000

distance <- data.frame(distance_to_N = rep(distance_to_N,times = dim(NM)[1]/length(distance_to_N)),
                        distance_to_S = rep(distance_to_S,times = dim(NM)[1]/length(distance_to_S)),
                        distance_to_Q = rep(distance_to_Q,times = dim(NM)[1]/length(distance_to_Q))) #repeat values for every month/year
NM <- cbind(NM,distance)

#swap back to lat lon (for save)
coords <- st_coordinates(NM) #get coords
NM_fishing <- cbind(NM, latitude = coords[, "Y"], longitude = coords[, "X"]) %>% 
  st_drop_geometry() #create new lat lon cols and drop geometry
NM_fishing <- subset(NM_fishing,select = -c(x,y)) #chop out x,y

write.csv(NM_fishing,"./fishing/Future Prediction/Objects/2.03 NM_fishing_ports.csv",
          row.names = TRUE)
