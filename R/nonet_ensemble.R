#' Ensemble Prediction without using training labels
#'
#' @param best_modelname Best model name is one which performance better while evaluating using any evaluation matrix like confusion matrix.
#' @param object prediction_list object, as from `tune_models`
#'
#' @return A list of ensembled predictions. You can evaluate the performance of ensembled prediction using the evaulation matrix as Confusion matrix or AUROC.
#' @export
#' @import rlist
#' @import ClusterR
#' @import tidyverse
#' @import caret
#' @examples
#' # nonet functionality can be explained via below example
#' # Setup
#' trainSet <- na.omit(banknote_authentication)[1:1029, ]
#' testSet <- na.omit(banknote_authentication)[1029:1371, ]
#' 
#' trainSet$class <- as.factor(ifelse(trainSet$class >= 1, 'Yes', 'No'))
#' testSet$class <- as.factor(ifelse(testSet$class >= 1, 'Yes', 'No'))
#' 
#' trainSet <- data.frame(trainSet)
#' testSet <- data.frame(testSet)
#' 
#' #Feature selection 
#' control <- rfeControl(functions = rfFuncs,
#'   method = "repeatedcv",
#'   repeats = 3,
#'   verbose = FALSE)
#' 
#' outcomeName <- 'class'
#' predictors <- c("variance", "skewness", "curtosis", "entropy")
#' 
#' banknote_rf <- train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
#' banknote_glm <- train(trainSet[,predictors],trainSet[,outcomeName],method='glm')
#' banknote_nnet <- train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
#' 
#' 
#' predictions_rf <- predict.train(object=Titanic_rf,testSet[,predictors],type="prob")
#' predictions_glm <- predict.train(object=Titanic_glm,testSet[,predictors],type="prob")
#' predictions_nnet <- predict.train(object=Titanic_nnet,testSet[,predictors],type="prob")
#' 
#' predictions_rf_raw <- predict.train(object=Titanic_rf,testSet[,predictors],type="raw")
#' predictions_glm_raw <-  predict.train(object=Titanic_glm,testSet[,predictors],type="raw")
#' predictions_nnet_raw <- predict.train(object=Titanic_nnet,testSet[,predictors],type="raw")
#' 
#' Stack_object <- list(predictions_rf$Yes, predictions_glm$Yes, predictions_nnet$Yes)
#' 
#' names(Stack_object) <- c("model_rf", "model_glm", "model_nnet")
#' 
#' # Prediction using nonet_ensemble function
#' prediction_nonet <- nonet_ensemble(Stack_object, "model_nnet")
#' # Converting probabilities into classes
#' prediction_nonet <- as.factor(ifelse(prediction_nonet >= "0.5", "Yes", "No"))
#' 
#' # Results
#' nonet_eval <- confusionMatrix(prediction_nonet, testSet[,outcomeName])
#' nonet_eval
#' confusionMatrix(predictions_glm_raw,testSet[,outcomeName])
#' confusionMatrix(predictions_rf_raw,testSet[,outcomeName])
#' confusionMatrix(predictions_nnet_raw,testSet[,outcomeName])
#' plot(nonet_eval)

nonet_ensemble <- function(object, best_modelname) {
  mod <- best_modelname
  prediction_list <- object
  if (is.null(prediction_list)) {
    stop("Please provide the object. It should be a list of predictions from different models.")
  }
  else {
   if (is.null(best_modelname)) {
    stop("Please provide the best model name")
  }
  else{
   target <- function(x) {
    if (x == mod) {
      x <- "response_variable"
    }
    else{(x != mod)
      x <- x
    }
  }
  names(prediction_list) <- lapply(names(prediction_list), target)
  preds_data_frame <- data.frame(prediction_list)
  weight_modeling <- glm(response_variable ~ ., data = preds_data_frame)
  weight_model_coefficient <- summary(weight_modeling)$coefficient
  weight_model <- data.frame(weight_model_coefficient)
  weight_model$variables <- row.names(weight_model)
  weight_model <- weight_model[c("variables", "Estimate")][-1, ]
  weight_model$Estimate <- abs(weight_model$Estimate)
  weight_model$weight <- weight_model$Estimate / sum(weight_model$Estimate)
  Preds_new <- list.remove(prediction_list, 'response_variable')
  Weighted_model <- Map("*", Preds_new, weight_model$Estimate)
  Weighted_model_updated <- list.append(Weighted_model, prediction_list$response_variable)
  add <- function(x) Reduce("+", x)
  preds_outcome <- add(Weighted_model_updated)
  return(preds_outcome)
  }
 }
}
