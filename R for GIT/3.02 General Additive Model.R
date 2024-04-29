library(mgcv)
library(patchwork)

NM_shallow <- read.csv("./fishing/Future Prediction/Objects/NEMO data with fishing SHALLOW.csv") %>% 
  subset(select = -c(X)) %>% 
  filter(slab_layer == "S")
gam_model <- gam(fishing_hours ~ti(DIN) +
                   ti(Detritus) + ti(distance_to_N) + ti(distance_to_Q) +
                   ti(distance_to_S) + ti(Name),
                 data = NM_shallow)
summary.gam(gam_model)
