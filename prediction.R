library(caret)

preprocessed_test_data <- readRDS("output/preprocessed_test_data")

required_columns <- names(preprocessed_test_data)[ - grep("_combn_",names(preprocessed_test_data))]
preprocessed_test_data <- preprocessed_test_data[,required_columns]

rf_model <- readRDS("output/rf_model")

predictions <- as.data.frame(predict(rf_model,preprocessed_test_data, type = "prob"))
head(predictions)

predictions$predict <- names(predictions)[1:2][apply(predictions[,1:2], 1, which.max)]

# Replace ______ with the  true values of predictions.
predictions$predict <- as.factor(predictions$predict)
predictions$observed <- _______________________
confusionMatrix(predictions$predict,predictions$observed)
plot(roc(as.numeric(predictions$predict),as.numeric(predictions$observed)))
plot(roc(predictions))
