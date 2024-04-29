#aggregate copernicus ice data into the form we need
rm(list = ls()) #reset
packages <- c("sf","sp","tidyverse","raster","furrr","RANN","rnaturalearth") #which packages do we need?
lapply(packages, require, character.only = TRUE) #load packages
sf_use_s2(FALSE) #turn off spherical geometry


Domain <- readRDS("./fishing/Future Prediction/Objects/Buffered Domain.rds") %>% #load domain polygon
  st_transform(crs = 3035) #transform domain crs

NM_fishing <- read.csv("./fishing/Future Prediction/Objects/NM_fishing_ports_sediment.csv") #load NM df
cop_path <- "./fishing/Global Fishing Watch/RAW/allregioncop.nc" #where is the ncdf ice data?

copernicus_raster <- brick(cop_path) #bricks together all rasters
extent <- extent(0,360,55,90) #define cropping window (smaller = faster)
crop_raster <- crop(copernicus_raster,extent) #crop to zone
plot(crop_raster) #check crop
ice <- crop_raster[[145:240]] #crop to correct year (Jan 2012-Dec 2019)

names(ice[[1]]) <- "ice" #alter layer name of current layer
tst_sf <- rasterToPolygons(ice[[1]]) %>% #convert to polygon
  st_as_sf() %>% #... then to sf object
  group_by("ice") %>% 
  summarise(ice = mean("ice")) %>% #calculate a mean sea-ice
  st_transform(crs = 3035) %>%
  st_make_valid() #fix geometry

crop_df <- rasterToPoints(crop_raster) %>% as.data.frame() #convert to points
selected_cols <- c(1,2, grep("^X(201[2-9]).", colnames(crop_df))) #take appropriate columns
crop_df_selected <- crop_df[, selected_cols]
crop_df_selected <- crop_df_selected %>% 
  filter(x > 295 & x < 315 & y < 75 & y > 58)#rough crop to make spatial operations faster

raster_sf <- st_as_sf(crop_df_selected,coords = c("x","y"),crs = 4326) %>% 
  st_transform(crs = 3035) #convert to sf

NM <- list.files("./fishing/Future Prediction/NEMO output", pattern = "\\.201[2-9]\\.rds$", full.names = TRUE) %>% 
  future_map(.,.f = readRDS) #read in files - each element is its own month
column_list <- list() #initialise storage

for (i in 1:ncol(raster_sf)) {
  column_data <- data.frame(ice = raster_sf[i]) #extract column and store
  st_geometry(column_data) <- raster_sf$geometry #set the geometry
  column_list[[i]] <- column_data #assign to column
}
ice_list <- column_list[-length(column_list)] #drop last col

ggplot() +
  geom_sf(data = Domain) +
  geom_sf(data = ice_list[[1]],size = 0.1,color = "red",alpha = 1) #check that it overlaps buffered domain

monthly_ice <- list()
#need to now intersect this with domain
for (i in seq_along(ice_list)){
  ice_crop <- st_intersection(Domain,ice_list[[i]]) #find the intersection
  monthly_ice[[i]] <- ice_crop #assign ice levels
  print(paste0(i,"/",length(ice_list))) #progress...
}

monthly_ice <- lapply(monthly_ice, function(df) setNames(df, c("Shore", "Area","Elevation","ice","geometry"))) #change names of cols in each list element
monthly_ice_df <- do.call(rbind, monthly_ice) %>% 
  subset(select = c(ice,geometry)) #bind new ice

#add in year month columns
num_rows <- nrow(monthly_ice_df)
num_months <- 12
num_years <- 8
total_entries <- num_months * num_years * 1233

months_per_year <- rep(1:12, each = 1233)
months <- rep(months_per_year, times = num_years)

years <- rep(2012:2019, each = num_months * 1233)

repeat_times <- ceiling(num_rows / total_entries)
months <- rep(months, times = repeat_times)[1:num_rows]
years <- rep(years, times = repeat_times)[1:num_rows]

monthly_ice_df$Month <- months
monthly_ice_df$Year <- years

coords <- st_coordinates(monthly_ice_df) #get coords
monthly_ice_df <- cbind(monthly_ice_df, latitude = coords[, "Y"], longitude = coords[, "X"]) %>% 
  st_drop_geometry() #create new lat lon cols and drop geometry

write.csv(monthly_ice_df,"./fishing/Future Prediction/Objects/Copernicus monthly ice.csv",
          row.names = FALSE) #saving with latlon cols is better