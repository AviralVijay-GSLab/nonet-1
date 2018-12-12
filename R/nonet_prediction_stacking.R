#' Create a list of predictions
#'
#' @param ... arguments supplied to the functions
#' @return A list of ensembled predictions. This list can be used to provide as an input argument for nonet_ensemble function
#' @export
#' @import rlist
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

nonet_prediction_stacking <- function (...){
  dots <- list(...)
  if (is.null(dots)) {
    stop("Please provide the predictions from different models")
  }
  else {
    prediction_stacking <- dots
    if (is.na(prediction_stacking)) {
      stop("Predictions have missing values")
    }
    else {
      return(prediction_stacking)
    }
  }
}

