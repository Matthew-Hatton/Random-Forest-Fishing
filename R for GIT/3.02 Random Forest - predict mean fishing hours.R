rm(list=ls()) #reset
library(dplyr)
library(zoo)
NM <- read.csv("./fishing/Future Prediction/Objects/2.05 NM_fishing_sediment_ports_ridge.csv",header = TRUE) %>% 
  subset(select = -c(latitude,longitude,Zonal,Meridional)) #load data
NM_years <- NM$Year
NM <- NM %>% subset(select = -c(Year))
model2 <- ranger(fishing ~ ., data = NM,num.trees = 1000,importance = "permutation")

predictions <- predict(model2, data = NM)$predictions
NM$pred <- predictions
#add year back in
NM$Year <- NM_years

monthly_sums <- NM %>%
  group_by(Year, Month) %>%
  summarise(actual = mean(fishing)) %>% 
  subset(select = c(actual))
monthly_sums$month <- seq(1,96)
monthly_sums$type <- "Actual"
monthly_sums$normal <- monthly_sums$actual/max(monthly_sums$actual)

monthly_sums_pred <- NM %>%
  group_by(Year, Month) %>%
  summarise(preds = mean(pred)) %>% 
  subset(select = c(preds))
monthly_sums_pred$month <- seq(1,96)
monthly_sums_pred$type <- "Predicted"
monthly_sums_pred$normal <- monthly_sums_pred$preds/max(monthly_sums_pred$preds)

monthly_ice <- NM %>%
  group_by(Year, Month) %>%
  summarise(ice = mean(Ice_Thickness)) %>% 
  subset(select = c(ice))
monthly_ice$month <- seq(1,96)

ggplot() +
  geom_line(data = monthly_sums,aes(x = month,y = normal,color = type),alpha = 0.9) +
  geom_line(data = monthly_sums_pred,aes(x = month,y = normal,color = type),alpha = 0.9) +
  geom_line(data = monthly_ice,aes(x = month,y = ice),alpha = 0.4,color = "darkgreen") +
  labs(x = "Month",y="Mean Fishing Hours") +
  geom_vline(xintercept = as.numeric(seq(12, 96, by = 12)), linetype = "dotted", color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylim(0,max(monthly_sums$normal,monthly_sums_pred$normal)) +
  labs(color = "Actual/Predicted")

ggplot() +
  #geom_point(data = NM,aes(x = Ice_Thickness,y = pred),size = 0.2) +
  xlim(0,1) +
  geom_smooth(data = NM,aes(x = Ice_Thickness,y = pred))
