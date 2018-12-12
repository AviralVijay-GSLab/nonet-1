context("Checking nonet_ensemble Clustering")

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
predictors <- c("variance", "skewness")

set.seed(30)
#kMeans_train <- kmeans(trainSet[,predictors], centers=2)
gmm_first <- GMM(trainSet[,predictors], 2, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10, em_iter = 10, verbose = F) 


predict_clustering_first <- predict_GMM(testSet[,predictors], gmm_first$centroids, gmm_first$covariance_matrices, gmm_first$weights)

predict_cluster_first_class <- as.factor(ifelse(predict_clustering_first$cluster_proba[, 1] >= "0.5", "0", "1"))

# Second Linear Regression Model
index <- createDataPartition(dataframe$class, p=0.75, list=FALSE)
trainSet <- dataframe[ index,]
testSet <- dataframe[-index,]

#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
  method = "repeatedcv",
  repeats = 3,
  verbose = FALSE)

outcomeName <- 'class'
predictors <- c("curtosis", "entropy")

gmm_second <- GMM(trainSet[,predictors], 2, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10, em_iter = 10, verbose = F) 

predict_clustering_Second <- predict_GMM(testSet[,predictors], gmm_second$centroids, gmm_second$covariance_matrices, gmm_second$weights)

predict_cluster_Second_class <- as.factor(ifelse(predict_clustering_Second$cluster_proba[, 1] >= "0.5", "0", "1"))



Stack_object <- list(predict_clustering_first$cluster_proba[, 1], predict_clustering_Second$cluster_proba[, 1])

names(Stack_object) <- c("Cluster_first", "Cluster_second")

prediction_nonet_raw <- nonet_ensemble(Stack_object, "Cluster_second")

prediction_nonet <- as.factor(ifelse(prediction_nonet_raw >= "0.5", "0", "1"))

confusionMatrix(prediction_nonet, testSet[,outcomeName])
confusionMatrix(predict_cluster_first_class,testSet[,outcomeName])
confusionMatrix(predict_cluster_Second_class,testSet[,outcomeName])

# Test
test_that("predict_cluster_first_class return numeric vector", {
  purrr::map_lgl(predict_cluster_first_class, is.numeric) %>%
    all() %>%
    expect_true()
})


test_that("predict_cluster_Second_class return numeric vector", {
  purrr::map_lgl(predict_cluster_Second_class, is.numeric) %>%
    all() %>%
    expect_true()
})


test_that("prediction_nonet_raw return numeric vector", {
  purrr::map_lgl(prediction_nonet_raw, is.numeric) %>%
    all() %>%
    expect_true()
})
