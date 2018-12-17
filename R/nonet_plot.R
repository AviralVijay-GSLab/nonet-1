#' Plot the predictions or results of nonet_ensemble
#'
#' @param x x axis variable name or histogram entity name
#' @param y y axis variable name
#' @param nonet_size size of plot need to feed in ggplot
#' @param nonet_alpha value of alpha for ggplot
#' @param nonet_bins number of bins for histogram
#' @param plot_type type of plot, if not provided it takes "NULL"
#' @param dataframe dataframe which is used for plotting purpose.
#'
#' @return plotted for the plot results provided as input.
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
#' banknote_nnet <- train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
#' 
#' 
#' predictions_rf <- predict.train(object=Titanic_rf,testSet[,predictors],type="prob")
#' predictions_nnet <- predict.train(object=Titanic_nnet,testSet[,predictors],type="prob")
#' 
#' predictions_rf_raw <- predict.train(object=Titanic_rf,testSet[,predictors],type="raw")
#' predictions_nnet_raw <- predict.train(object=Titanic_nnet,testSet[,predictors],type="raw")
#' 
#' Stack_object <- list(predictions_rf$Yes, predictions_nnet$Yes)
#' 
#' names(Stack_object) <- c("model_rf", "model_nnet")
#' 
#' # Prediction using nonet_ensemble function
#' prediction_nonet <- nonet_ensemble(Stack_object, "model_nnet")
#' # Converting probabilities into classes
#' prediction_nonet <- as.factor(ifelse(prediction_nonet >= "0.5", "Yes", "No"))
#' 
#' # Results
#' nonet_eval <- confusionMatrix(prediction_nonet, testSet[,outcomeName])
#' nonet_eval_rf <- confusionMatrix(predictions_rf_raw,testSet[,outcomeName])
#' nonet_eval_nnet <- confusionMatrix(predictions_nnet_raw,testSet[,outcomeName])
#' nonet_eval_df <- data.frame(nonet_eval$table)
#' nonet_eval_rf_df <- data.frame(nonet_eval_rf$table)
#' nonet_eval_nnet_df <- data.frame(nonet_eval_nnet_df$table)
#' nonet_plot(Prediction, Reference, nonet_eval_df, plot_type = "point")
#' nonet_plot(Prediction, Reference, nonet_eval_rf_df, plot_type = "boxplot")
#' nonet_plot(Prediction, Reference, nonet_eval_nnet_df, plot_type = "density")

nonet_plot <- function (x, y, dataframe, plot_type = NULL , nonet_size = 20, nonet_alpha = .3, nonet_bins = 25) {
  
  if (is.null(dataframe)) {
    stop("Please provide the not null values in the dataframe")
  }
  else{
    if (anyNA(dataframe)) {
      stop("Please provide the not na values in the dataframe")
    }
    else{
      if (plot_type == "hist") {
        plotted <- ggplot(dataframe, aes(dataframe$x)) + geom_histogram(bins = nonet_bins)
        
      }
      else{
        if (plot_type == "NULL") {
           plotted <- ggplot(data = dataframe, aes(x = dataframe$x, y = dataframe$y)) +
            geom_point(size = nonet_size, alpha = nonet_alpha) +
            geom_point()
        }
        else{
          if (plot_type == "point") {
            plotted <- ggplot(data = dataframe, aes(x = x, y = y)) +
              geom_point(size = nonet_size, alpha = nonet_alpha) +
              geom_point()
          }
          else{
            if (plot_type == "boxplot") {
              plotted <- ggplot(data = dataframe, aes(x = x, y = y)) +
                geom_point(size = nonet_size, alpha = nonet_alpha) +
                geom_boxplot()
            }
            else{
              if (plot_type == "density") {
                plotted <- ggplot(data = dataframe, aes(x = x, y = y)) +
                  geom_point(size = nonet_size, alpha = nonet_alpha) +
                 geom_density()
              }
            }
          }
        }
      }
      return(plotted)
    }
  }
}

