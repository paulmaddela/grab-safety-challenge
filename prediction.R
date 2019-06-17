library(caret)

preprocessed_test_data <- readRDS("output/preprocessed_test_data")

rf_model <- readRDS("output/rf_model")

predictions <- as.data.frame(predict(rf_model,preprocessed_test_data, type = "prob"))
head(predictions)

predictions$predict <- names(predictions)[1:2][apply(predictions[,1:2], 1, which.max)]

# Replace ______ with the  true values of predictions.
predictions$observed <- _______________________
plot(roc(predictions))
