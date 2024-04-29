#analyse data
rm(list = ls())


results_lst <- readRDS("./fishing/Future Prediction/Objects/k-fold model results.RDS")
percentage_train <- readRDS("./fishing/Future Prediction/Objects/k-fold percentage matching train data.RDS")
percentage_test <- readRDS("./fishing/Future Prediction/Objects/k-fold percentage matching test data.RDS")
