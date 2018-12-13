#' Plot the predictions or results of nonet_ensemble
#'
#' @param x 
#' @param y 
#' @param data 
#' @param nonet_color 
#' @param nonet_size 
#' @param nonet_alpha 
#' @param nonet_bins 
#' @param plot_type 
#'
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

nonet_plot <- function (x, y, dataframe, plot_type, nonet_color, nonet_size, nonet_alpha, nonet_bins) {
  
  if (is.null(dataframe)) {
    stop("Please provide the not null values in the dataframe")
  }
  else{
    if (is.na.(dataframe)) {
      stop("Please provide the not na values in the dataframe")
    }
    else{
      if (plot_type == "hist") {
        ggplot(Stack_object_df, aes(Stack_object_df$model_glm)) + geom_histogram(bins = nonet_bins)
      }
      else{
        if (plot_type == "default") {
          ggplot(data = nonet_eval_df, aes(x = Prediction, y = Reference)) +
            geom_point(aes(
              color = factor(Freq),
              size = 10,
              alpha = .2
            ))
          geom_point()
        }
        else{
          if (plot_type == "point") {
            ggplot(data = nonet_eval_df, aes(x = Prediction, y = Reference)) +
              geom_point(aes(
                color = factor(Freq),
                size = 10,
                alpha = .2
              ))
            geom_point()
          }
          else{
            if (plot_type == "boxplot") {
              ggplot(data = nonet_eval_df, aes(x = Prediction, y = Reference)) +
                geom_point(aes(
                  color = factor(Freq),
                  size = 10,
                  alpha = .2
                ))
              geom_boxplot()
            }
            else{
              if (plot_type == "density") {
                ggplot(data = nonet_eval_df, aes(x = Prediction, y = Reference)) +
                  geom_point(aes(
                    color = factor(Freq),
                    size = 10,
                    alpha = .2
                  ))
                geom_density()
              }
            }
          }
        }
      }
    }
  }
}

