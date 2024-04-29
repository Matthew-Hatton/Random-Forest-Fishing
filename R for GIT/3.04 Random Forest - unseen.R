rm(list=ls()) #reset
library(dplyr)
NM <- read.csv("./fishing/Future Prediction/Objects/2.05 NM_fishing_sediment_ports_ridge.csv",header = TRUE) %>% 
  subset(select = -c(latitude,longitude,Zonal,Meridional)) #load data
NM_years <- NM$Year #save years


#let's train the model on 2015 - 2019
NM_train <- filter(NM,Year %in% c(2015,2016,2017,2018,2019))

model2 <- ranger(fishing ~ ., data = NM_train,num.trees = 1000,importance = "permutation") #train on data

NM_test <- filter(NM,Year %in% c(2012,2013,2014))


#does the model perform better on the train data than the test data?
predictions_tst <- predict(model2, data = NM_test)$predictions
NM_test$pred <- predictions_tst

predictions_train <- predict(model2, data = NM_train)$predictions
NM_train$pred <- predictions_train

NM_train_test <- rbind(NM_test,NM_train)

NM_train_test$Year <- NM_years

monthly_sums <- NM_train_test %>%
  group_by(Year, Month) %>%
  summarise(actual = mean(fishing)) %>% 
  subset(select = c(actual))
monthly_sums$month <- seq(1,96)
monthly_sums$type <- "Actual"
monthly_sums$normal <- monthly_sums$actual/max(monthly_sums$actual)

monthly_sums_pred <- NM_train_test %>%
  group_by(Year, Month) %>%
  summarise(preds = mean(pred)) %>% 
  subset(select = c(preds))
monthly_sums_pred$month <- seq(1,96)
monthly_sums_pred$type <- "Predicted"
monthly_sums_pred$normal <- monthly_sums_pred$preds/max(monthly_sums_pred$preds)
