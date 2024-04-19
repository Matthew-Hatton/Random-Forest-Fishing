rm(list=ls()) #reset
library(sf)
library(ggplot2)
library(rnaturalearth)
sf_use_s2(FALSE)
domain <- readRDS("Domains.rds") #load in StrathE2EPolar domain

offshore <- domain[2,] #filter to just offshore zone

buff_dom <- st_buffer(offshore,dist = 70000) %>%  #buffer domain
  st_cast("MULTIPOLYGON")
  
domain[2,] <- buff_dom #replace offshore with buffered zone

domain <- st_make_valid(domain) #fix domain

diffPoly <- st_difference(domain[2,], st_union(domain[1,])) #cut out

domain[2,] <- diffPoly #take just the offshore

saveRDS(domain,"./fishing/Future Prediction/Objects/Buffered Domain.rds") #save domain
