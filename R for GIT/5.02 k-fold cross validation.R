rm(list = ls()) #reset

library(dismo)
library(rJava)
library(tidyverse)
library(sf)
NM <- read.csv("./fishing/Future Prediction/Objects/2.05 NM_fishing_sediment_ports_ridge.csv",header = TRUE) %>% 
  subset(select = -c(Zonal,Meridional)) #load data
cutoff <- seq(0.1,1,0.1)
years <- seq(2012,2019)
NM$fishing <- ifelse(NM$fishing > 0, 1, 0) #convert to binary
partition <- split(NM,NM$Year)
#empty lists for storing results
result_list <- list()
percentage_test_list <- list()
percentage_train_list <- list()

for (i in seq_along(partition)) {
  print(paste0(i,"/",length(seq_along(partition))))
  test <- partition[[i]] #choose test set
  train <- do.call(rbind, partition[-i]) #choose training set
  
  #prep data for model
  NM_coords <- data.frame(fishing = train$fishing) #get fishing data for training
  NM_coords_vector <- as.vector(NM_coords[,1]) #...as a vector
  NM_predictors <- train %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                            Bathymetry,ice,distance_to_ridge)) #define predictors
  #run model
  habitat_model <- maxent(x = NM_predictors,p = NM_coords_vector) #run model
  result_list[[i]] <- habitat_model@results #save results *AUC/permutation importance*
  
  #test model on unseen data
  test_fishing <- test$fishing #store fishing for check
  test_validation <- test %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                          Bathymetry,ice,distance_to_ridge)) #pull validation set
  prediction <- predict(object = habitat_model,x = test_validation) #predict fishing - probability of fishing
  percentage_matching_test <- data.frame(year = rep(years[i],length(cutoff)),
                                                    cutoffs = cutoff,
                                                    percentage = rep(1,length(cutoff)))
  #calculate percentage for test
  for (j in seq_along(cutoff)) {
    rounded_prediction <- ifelse(prediction > cutoff[j], 1, 0) #convert to binary#round prediction based on cutoff
    validation_df <- data.frame(actual = test_fishing,
                                prediction = rounded_prediction) #creates actual v prediction
    matching_entries <- sum(validation_df$actual == validation_df$prediction) #how many of these entries match?
    total_entries <- nrow(validation_df)
    percentage_matching <- (matching_entries / total_entries) * 100 #and what is that as a percentage?
    percentage_matching_test$percentage[j] <- percentage_matching
  }
  percentage_test_list[[i]] <- percentage_matching_test
  
  #test model on training data
  train_fishing <- train$fishing #store fishing for check
  train_validation <- train %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                                Bathymetry,ice,distance_to_ridge)) #pull validation set
  prediction <- predict(object = habitat_model,x = train_validation) #predict fishing - probability of fishing
  percentage_matching_train <- data.frame(year = rep(years[i],length(cutoff)),
                                         cutoffs = cutoff,
                                         percentage = rep(1,length(cutoff)))
  #calculate percentage for test
  for (j in seq_along(cutoff)) {
    rounded_prediction <- ifelse(prediction > cutoff[j], 1, 0) #round prediction based on cutoff
    validation_df <- data.frame(actual = train_fishing,
                                prediction = rounded_prediction) #creates actual v prediction
    matching_entries <- sum(validation_df$actual == validation_df$prediction) #how many of these entries match?
    total_entries <- nrow(validation_df)
    percentage_matching <- (matching_entries / total_entries) * 100 #and what is that as a percentage?
    percentage_matching_train$percentage[j] <- percentage_matching
  }
  percentage_train_list[[i]] <- percentage_matching_test
}
saveRDS(result_list,"./fishing/Future Prediction/Objects/k-fold model results.RDS")
saveRDS(percentage_test_list,"./fishing/Future Prediction/Objects/k-fold percentage matching test data.RDS")
saveRDS(percentage_train_list,"./fishing/Future Prediction/Objects/k-fold percentage matching train data.RDS")
