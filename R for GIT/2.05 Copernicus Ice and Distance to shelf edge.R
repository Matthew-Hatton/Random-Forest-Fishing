rm(list = ls()) #reset
packages <- c("spatialEco","sf","sp","tidyverse","raster","furrr","RANN","rnaturalearth") #list packages
lapply(packages, require, character.only = TRUE) #load packages

Ice <- read.csv("./fishing/Future Prediction/Objects/Copernicus monthly ice.csv") %>% 
  st_as_sf(coords = c("longitude","latitude"),crs = 3035) #load in ice
Ice_list <- split(Ice,list(Ice$Year,Ice$Month)) # split to year month

NM <- read.csv("./fishing/Future Prediction/Objects/2.03 NM_fishing_ports_sediment.csv") %>% 
  subset(select = -c(X)) %>% 
  st_as_sf(coords = c("longitude","latitude"),crs = 3035) #load NM
NM <- split(NM,list(NM$Year,NM$Month)) #split to year month

for (i in seq_along(NM)) {
  ice <- Ice_list[[i]]
  nm <- NM[[i]] #access NM file
  distances <- st_distance(ice,nm) %>% 
    as.data.frame() #each point is a row. each column is the distance to a NEMO point. calculate distances
  min_distances_indices <- apply(distances, 1, which.min)  #we need to find the location of the minimum distance
  nm$ice_presence <- 0  #initialise new column
  for (j in seq_along(min_distances_indices)){
    nm$ice_presence[min_distances_indices[j]] <- ice$ice[j] #add all closest points
  }
  if (i == 1) { #append
    NM_fishing <- nm
  } else {
    NM_fishing <- rbind(NM_fishing, nm)
  }
  print(paste0(i,"/",length(NM)))
} #adds copernicus ice to NM dataframe
#write.csv(NM_fishing,"./fishing/Future Prediction/Objects/NM_fishing_sediment_ports_ice.csv") DON'T UNCOMMENT
NM_fishing <- read.csv("./fishing/Future Prediction/Objects/2.03 NM_fishing_ports_sediment.csv") 

NM_2012 <- filter(NM_fishing,Year == "2012" & Month == "1") %>% 
  st_as_sf(coords = c("longitude","latitude"),crs = 3035) %>% 
  subset(select = c(geometry)) #we only need to do this for one set of points
bath_path <- "./bathymetry/RAW/gebco/GEBCO_2023_sub_ice_topo.nc" #where is the bathymetry data stored?

ras <- raster::raster(bath_path) #raster it

extent <- extent(-65,-40,60,80) #define cropping window (smaller = faster)
crop_ras <- crop(ras,extent) #crop to zone
crop_ras[crop_ras >= 0] <- 0 #we don't want land data
plot(crop_ras) #check crop

crs(crop_ras) <- CRS('+init=EPSG:4326') #set crs as original
hotspot <- rasterToContour(crop_ras,levels = c(-600)) %>% #convert to polygon
  st_as_sf() %>% #... then to sf object
  st_transform(crs = 3035) %>% #... and transform to our crs
  transmute(Depths = factor(level,levels = c(-600))) %>% st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% 
  st_cast("POINT") %>% #create point geometries
  subset(select = c(geometry)) #keep just the geometry

coords <- st_coordinates(hotspot) #get coords
hotspot <- cbind(hotspot, latitude = coords[, "Y"], longitude = coords[, "X"]) %>% 
  st_drop_geometry() %>% 
  filter(latitude > 6379638.474797536 & longitude < 2300000 & longitude > 2020000) %>% #where is it located?
  st_as_sf(coords = c("longitude","latitude"),crs = 3035) #get fishing hotspot

ggplot() +
  geom_sf(data = NM_2012) +
  geom_sf(data = hotspot,color = "red") #check we located the correct spot

distances <- as.data.frame(st_distance(NM_2012,hotspot))#get distances

min_distances_indices <- apply(distances, 1, which.min)  #so we need to find the location of the minimum distance

NM_2012$distance_to_hotspot <- 0  #init new col to store distances

for (j in seq_along(min_distances_indices)) {
  NM_2012$distance_to_hotspot[j] <- distances[j,][min_distances_indices[j]] #assign distances
}
NM_store <- NM_2012 %>% st_drop_geometry() #drop sf 

NM_rep <- unlist(rep(NM_store$distance_to_hotspot,times = 96)) #repeat distance for every month of every year (considering NEMO points don't move)

NM$distance_to_ridge <- NM_rep #bind to the master DF


write.csv(NM,"./fishing/Future Prediction/Objects/2.05 NM_fishing_sediment_ports_ridge.csv",
          row.names = FALSE) #save
