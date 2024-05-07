#analyse data
rm(list = ls())
library(ggplot2)

#load data
years <- seq(2012,2019)
results_lst_NEMO <- readRDS("./fishing/Future Prediction/Objects/k-fold model results NEMO-ice.RDS")
percentage_train_NEMO <- readRDS("./fishing/Future Prediction/Objects/k-fold percentage matching train data NEMO-ice.RDS")
percentage_test_NEMO <- readRDS("./fishing/Future Prediction/Objects/k-fold percentage matching test data NEMO-ice.RDS")

results_lst <- readRDS("./fishing/Future Prediction/Objects/k-fold model results.RDS")
percentage_train <- readRDS("./fishing/Future Prediction/Objects/k-fold percentage matching train data.RDS")
percentage_test <- readRDS("./fishing/Future Prediction/Objects/k-fold percentage matching test data.RDS")

#bind together all train/test percentages - NEMO ice
percentage_train_df_NEMO <- do.call(rbind,percentage_train_NEMO)
percentage_train_df_NEMO$traintest <- "Train"
percentage_test_df_NEMO <- do.call(rbind,percentage_test_NEMO)
percentage_test_df_NEMO$traintest <- "Test"
percentage_test_train_NEMO <- rbind(percentage_train_df_NEMO,percentage_test_df_NEMO)
percentage_test_train_NEMO$cutoffs <- abs(1-percentage_test_train_NEMO$cutoffs)

#bind together all train/test percentages
percentage_train_df <- do.call(rbind,percentage_train)
percentage_train_df$traintest <- "Train"
percentage_test_df <- do.call(rbind,percentage_test)
percentage_test_df$traintest <- "Test"
percentage_test_train <- rbind(percentage_train_df,percentage_test_df)
percentage_test_train$cutoffs <- abs(1-percentage_test_train$cutoffs)

percentage_test_NEMO <- filter(percentage_test_df_NEMO,cutoffs == 0.5)
percentage_test <- filter(percentage_test_df,cutoffs == 0.5)
percentage_test_NEMO$datatype <- "NEMO"
percentage_test$datatype <- "Copernicus"
percentage_test_final <- rbind(percentage_test_NEMO,percentage_test)

#check whether NEMO vs Copernicus ice actually makes a difference
ggplot(data = percentage_test_final,aes(x = year,y = percentage,fill = datatype)) +
  geom_bar(position = "dodge",stat = "identity") +
  scale_y_continuous(limits = c(0,100),expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     breaks = unique(percentage_test_final$year)) +
  labs(x = "K-Fold Year",y = "Percentage Correct (Threshold of 0.5)",fill = "Ice Concentration Source") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  NULL
#ggsave("./fishing/Future Prediction/Figures/NEMOvsCop.png",width = 33.867,height = 19.05,units = "cm")

ggplot(data = percentage_test_train,aes(x = cutoffs,y = percentage,color = traintest)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year,ncol = 4) +
  labs(x = "Probability of Fishing Threshold",y = "Percentage Correct",color = "Data Type") +
  ylim(c(0,100)) +
  scale_x_continuous(limits = c(0,1),breaks = seq(0,1,0.2)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold")) +
  NULL
#ggsave("./fishing/Future Prediction/Figures/Train_V_Test_Perc.png",width = 33.867,height = 19.05,units = "cm")

k_fold_cutoff_train <- filter(percentage_train_df,cutoffs == as.factor(0.7))
k_fold_cutoff_test <- filter(percentage_test_df,cutoffs == as.factor(0.7))
final <- data.frame(k_fold_year = seq(2012,2019),
                    AUC = 1,
                    Perc_of_training = 1,
                    Perc_of_test = 1)
for (i in seq_along(years)) {
  final$AUC[i] <- results_lst[[i]]['Training.AUC',]
  final$Perc_of_training[i] <- k_fold_cutoff_train$percentage[i]
  final$Perc_of_test[i] <- k_fold_cutoff_test$percentage[i]
} #extract AUC values for each model

final$differences <- (final$Perc_of_test-final$Perc_of_training)

#differences between test and train percentages
ggplot(data = final,aes(x = k_fold_year,y = differences)) +
  geom_point() +
  geom_line() +
  ylim(c(-2,2)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  labs(x = "K-Fold Year",y = "Absolute difference between test and train (percentage)")
