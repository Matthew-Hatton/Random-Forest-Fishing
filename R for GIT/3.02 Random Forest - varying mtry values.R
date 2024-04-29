rm(list=ls()) #reset
library(ranger)
NM <- read.csv("./fishing/Future Prediction/Objects/2.05 NM_fishing_sediment_ports_ridge.csv",header = TRUE) %>% 
  subset(select = -c(latitude,longitude,Zonal,Meridional,Year)) #load data
errors <- data.frame(mtry = c(1,2,3,4,5),
                     error = c(0,0,0,0,0))
for (mtry in c(1,2,3,4,5)) {
  model2 <- ranger(fishing ~ ., data = NM,num.trees = 1000,importance = "permutation",
                   mtry = mtry)
  predictions <- predict(model2, data = NM)$predictions
  NM$pred <- predictions
  errors$error[mtry] <- abs(mean(NM$fishing-NM$pred))
  
}

ggplot() +
  geom_line(data = errors,aes(x = mtry,y = error)) +
  geom_point(data = errors,aes(x = mtry,y = error),size = 2,color = "red")
ggplot() +
  geom_point(data = NM,aes(x = Ice_Thickness,y = fishing))
