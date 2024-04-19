rm(list=ls()) #reset
library(tidyverse)
library(randomForest)
library(ranger)
set.seed(710) #forreproducibility

# NM1 <- read.csv("./fishing/Future Prediction/Objects/NM_fishing_sediment_ports.csv",header = TRUE) %>% 
#   subset(select = c(fishing,ice,Temperature,Salinity,Bathymetry,DIN,Detritus,Sediment))
NM2 <- read.csv("./fishing/Future Prediction/Objects/NM_fishing_sediment_ports_ridge.csv",header = TRUE) %>% 
  subset(select = -c(latitude,longitude,Zonal,Meridional,Year))
# NM3 <- read.csv("./fishing/Future Prediction/Objects/NM_fishing_sediment_ports.csv",header = TRUE) %>% 
#   subset(select = -c(X,distance_to_S,distance_to_Q))

# model1 <- ranger(fishing ~ ., data = NM1,num.trees = 500)

model2 <- ranger(fishing ~ ., data = NM2,num.trees = 500,importance = "permutation")

# model3 <- ranger(fishing ~.,data = NM3,num.trees = 500)


v <- as.vector(model2$variable.importance)
w <- as.vector(names(model2$variable.importance))
DF<-cbind(w,v)
DF<-as.data.frame(DF)
DF$v <- as.numeric(DF$v)
DF$w[10] <- "Ice Concentration"
mean_value <- mean(DF$v)
std_dev <- sd(DF$v)
variable_importance_zscore <- (DF$v - mean_value) / std_dev
min_value <- min(DF$v)
max_value <- max(DF$v)
variable_importance_min_max <- (DF$v - min_value) / (max_value - min_value)

DF$v <- variable_importance_min_max


ggplot(DF, aes(x=reorder(w,v), y=v,fill=v))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill = F) +
  NULL
