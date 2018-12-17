context("Checking nonet_ensemble Classification")

# Setup
Bank_Note <- data.frame(banknote_authentication)
dataframe <- Bank_Note
dataframe$class <- as.factor(ifelse(dataframe$class >= 1, 'Yes', 'No'))
dataframe <- data.frame(dataframe)

index <- createDataPartition(dataframe$class, p=0.75, list=FALSE)
trainSet <- dataframe[ index,]
testSet <- dataframe[-index,]

#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
  method = "repeatedcv",
  repeats = 3,
  verbose = FALSE)

outcomeName <- 'class'
predictors <- c("variance", "skewness", "curtosis", "entropy")

banknote_rf <- train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
banknote_nnet <- train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')


predictions_rf <- predict.train(object=banknote_rf,testSet[,predictors],type="prob")
predictions_nnet <- predict.train(object=banknote_nnet,testSet[,predictors],type="prob")

predictions_rf_raw <- predict.train(object=banknote_rf,testSet[,predictors],type="raw")
predictions_nnet_raw <- predict.train(object=banknote_nnet,testSet[,predictors],type="raw")

Stack_object <- list(predictions_rf$Yes, predictions_nnet$Yes)

names(Stack_object) <- c("model_rf", "model_nnet")

prediction_nonet_raw <- nonet_ensemble(Stack_object, "model_nnet")

prediction_nonet <- as.factor(ifelse(prediction_nonet_raw >= "0.5", "Yes", "No"))

confusionMatrix(prediction_nonet, testSet[,outcomeName])
confusionMatrix(predictions_rf_raw,testSet[,outcomeName])
confusionMatrix(predictions_nnet_raw,testSet[,outcomeName])

# Test
test_that("predictions_rf$Yes return numeric vector", {
  expect_is(predictions_rf$Yes, "numeric")
})


test_that("predictions_nnet$Yes return numeric vector", {
  expect_is(predictions_nnet$Yes, "numeric")
})


test_that("prediction_nonet_raw return numeric vector", {
  expect_is(prediction_nonet_raw, "numeric")
})
