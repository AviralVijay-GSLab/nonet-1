#' Make predictions using the best-performing model
#'
#' @param best_modelname
#' @param object prediction_list object, as from `tune_models`
#'
#' @return A list of ensembled predictions. You can evaluate the performance of ensembled prediction using the evaulation matrix as Confusion matrix or AUROC.
#' @examples
#' # Tune models using only the first 40 rows to keep computation fast
#'
#' models <- machine_learn(pima_diabetes[1:40, ], patient_id,
#'                         outcome = diabetes, tune = FALSE)
#'
#' # Make prediction on the next 10 rows. This uses the best-performing model from
#' # tuning cross validation, and it also prepares the new data in the same way as
#' # the training data was prepared.
#'
#' predictions <- predict(models, newdata = pima_diabetes[41:50, ])
#' predictions
#' evaluate(predictions)
#' plot(predictions)

nonet_data_cleaning <- function(object, best_modelname) {
  mod <- best_modelname
  prediction_list <- object
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
  Preds_new <- list.remove(prediction_list, 'response_variable')
  Weighted_model <- Map("*", Preds_new, weight_model$Estimate)
  Weighted_model_updated <- list.append(Weighted_model, prediction_list$target_variable)
  add <- function(x) Reduce("+", x)
  preds_outcome <- add(Weighted_model_updated)
  return(preds_outcome)
}
