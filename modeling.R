library(caret)
library(randomForest)
library(pROC)

trip_data_all_features_with_label <- readRDS("data/processed/trip_data_all_features_with_label")
#trip_data_all_features_with_label <- trip_data_all_features_with_label[1:1000,]

required_columns <- names(trip_data_all_features_with_label)[ - grep("_combn_",names(trip_data_all_features_with_label))]

trip_data_feature_subset <- trip_data_all_features_with_label[,required_columns]

train_data <- as.data.frame(trip_data_feature_subset[,-1])
x_train <- train_data[,-ncol(train_data)]
y_train  <- train_data$label


control <- trainControl(method='repeatedcv', 
                        number=5, 
                        repeats=3)

set.seed(123)
rf_model <- caret::train(x = x_train,y = y_train,method = "rf",trainControl = control)
rf_model
saveRDS(rf_model,"output/rf_model")


predictions <- as.data.frame(predict(rf_model,x_train, type = "prob"))
head(predictions)
predictions$predict <- names(predictions)[1:2][apply(predictions[,1:2], 1, which.max)]
predictions$observed <- y_train

plot(roc(predictions))





