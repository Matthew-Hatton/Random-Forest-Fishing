## nearest neighbour on points
rm(list = ls()) #reset
library(furrr)
GFW_mean <- read.csv("./fishing/Future Prediction/Objects/cropped GFW/GFW_mean_all_years.csv") %>% 
  st_as_sf(coords = c("longitude","latitude"),crs = 3035) #read in yearly GFW means
GFW_list <- split(GFW_mean,list(GFW_mean$year,GFW_mean$month)) #split by year and month
NM <- list.files("./fishing/Future Prediction/NEMO output", pattern = "\\.209[2-9]\\.rds$", full.names = TRUE) %>% 
  future_map(.,.f = readRDS) #read in files - each element is its own month

for (i in seq_along(NM)) {
  gfw <- GFW_list[[i]]
  nm <- NM[[i]] #access NM file
  nm <- st_as_sf(nm,coords = c("longitude","latitude"),crs = 4326) %>% 
    st_transform(crs = 3035) #convert to desired crs
  distances <- st_distance(gfw,nm) %>% 
    as.data.frame() #each point is a row. each column is the distance to a NEMO point. calculate distances
  min_distances_indices <- apply(distances, 1, which.min)  #we need to find the location of the minimum distance
  nm$fishing_hours <- 0  #initialise new column
  for (j in seq_along(min_distances_indices)){
    nm$fishing[min_distances_indices[j]] <- sum(gfw$fishing_hours[j]+nm$fishing[min_distances_indices[j]]) #add all closest points
  }
  if (i == 1) { #append
    NM_fishing <- nm
  } else {
    NM_fishing <- rbind(NM_fishing, nm)
  }
  print(paste0(i,"/",length(NM)))
}

NM_fishing <- filter(NM_fishing,slab_layer == "S") #take just the shallow layer
coords <- st_coordinates(NM_fishing) #get coords
NM_fishing <- cbind(NM_fishing, latitude = coords[, "Y"], longitude = coords[, "X"]) %>% 
  st_drop_geometry() #create new lat lon cols and drop geometry

write.csv(NM_fishing,"./fishing/Future Prediction/Objects/2.02 NM_fishing.csv",
          row.names = FALSE) #better to save without geometry column

