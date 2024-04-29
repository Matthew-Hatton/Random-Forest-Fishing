#### Set up ####
rm(list=ls()) #reset

packages <- c("MiMeMo.tools", "furrr", "ncdf4") #packages used
lapply(packages, library, character.only = TRUE) #so load them
source("./@_Region file.R") #take region file

plan(multisession) #parallel processing is faster

all_files <- list.files("I:\\Science\\MS\\Shared\\CAO\\nemo\\ALLARC",
                        recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%
  mutate(Path  = substr(.,1,42),
         File = substr(.,43,60),  #extract grid_ files
         Year = substr(., 50, 53),  #extract year as two digits
         date = substr(., 56, 57),  #extract date as two digits
         Month = substr(., 54, 55),
         Type = substr(.,43,49)) %>%  #extract month
  filter(!File %in% c("ptrc_T_20000625.nc", "ptrc_T_20470130.nc")) %>%      #drop corrupted files
  filter(Type != "grid_W_") %>%
  filter(Type != "_meter.") %>% 
  filter(Type != "drg.nc") %>%
  filter(Type != "inates.") %>% #drop vertical water
  dplyr::select(Path,File,date,Year,Month,Type) #drops unnecessary

all_files$Path <- gsub("/", "\\", all_files$Path, fixed = TRUE) #change file format

domains <- readRDS("./fishing/Future Prediction/Objects/Buffered Domain.rds") %>% 
  subset(select = -c(Elevation,area)) #read in domain



sf_use_s2(FALSE) #switch off spherical geometry (makes next bit work)

#takes hot minute to run
crop <- readRDS("./fishing/Future Prediction/Objects/Buffered Domain.rds") %>% #loads domain rds, takes every point and draw circle 5000m away
  st_buffer(dist = 5000) %>% #boundary needs to be bigger to sample flows at domain boundary so add a bit on
  summarise() %>%  #combine polgyons to avoid double sampling
  mutate(Shore = "Buffer")

Bathymetry <- readRDS("Objects\\NA_grid.rds") %>%                          #import NEMO bath
  st_drop_geometry() %>%                                                    #drop sf geometry
  dplyr::select(-c("x", "y"), latitude = Latitude, longitude = Longitude)          #clean the column

#### Build summary scheme ####
scheme <- scheme_strathE2E(get_spatial(paste0(all_files$Path[1], all_files$File[1]), grid_W = F),
                           Bathymetry, 40, 600, crop) %>% 
  dplyr::select(x, y, layer, group, weight, slab_layer, longitude, latitude, Bathymetry) %>%   #get scheme to summarise SE2E
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% #convert to sf
  st_join(st_transform(domains, crs = 4326)) %>%                            #attach to model zone
  st_drop_geometry()                                                        #drop sf formatting

start <- scheme_to_start()                                                  #get ncdf4 vectors
count <- scheme_to_count()                                                  #amount of data to import
scheme <- scheme_reframe(scheme)


ice_scheme <- filter(scheme, layer == 1) %>%                                #ice data stored as matrix - needs new
  arrange(group) %>% 
  transmute(n = xyindex_to_nindex(x, y, count[1]))

scheme_result <- arrange(scheme, group) %>%                                 #create a meta-data object to attach to the summaries
  select(x, y, slab_layer, longitude, latitude, Shore, Bathymetry) %>% 
  distinct() %>% 
  mutate(slab_layer = if_else(slab_layer == 1, "S", "D"),
         weights = case_when(slab_layer == "S" & Bathymetry >= 40 ~ 40,     #weights for zonal averages
                             slab_layer == "S" & Bathymetry < 40 ~ Bathymetry,
                             slab_layer == "D" & Bathymetry >= 600 ~ 560,
                             slab_layer == "D" & Bathymetry < 600 ~ (Bathymetry - 40)))

tictoc::tic()
all_files %>% 
  split(., f = list(.$Month, .$Year)) %>%                                   #specify timestep
  #.[1:12] %>%                                                              #test just first 12
  future_map(NEMO_MEDUSA, analysis = "slabR", summary = scheme_result,
             scheme = scheme, ice_scheme = ice_scheme$n, start = start,  
             count = count, out_dir = "./fishing/Future Prediction/NEMO output", .progress = T)    #perform the extraction and save an object for each month (in parallel)
tictoc::toc()