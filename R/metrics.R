#' Get F Beta Score
#' @param predictions
#' @param reality
#' @param model a model name.
#' @param positive a positive value example.
#' @param beta a beta value.
#' @param show print score in console.
#' @return f beta score value.
#' @export
#' @importFrom MLmetrics FBeta_Score
fbeta_score <- function(prediction, reality, model="", positive=1, beta=1, show=TRUE) {
  score <- FBeta_Score(
    y_true   = reality,
    y_pred   = prediction,
    positive = positive, 
    beta     = beta
  )
  
  if(show) {
    print(paste(model, ' F', beta, 'Score: ', score, sep=''))
  } else {
    score 
  }
}

#' Get ROC
#' @param predictions
#' @param reality
#' @return ROC
#' @export
#' @importFrom ROCit rocit
roc <- function(predictions, reality) {
  rocit(as.numeric(predictions), as.numeric(reality))
}


#' Get ROC AUC
#' @param predictions
#' @param reality
#' @return AUC
#' @export
#' @importFrom pROC roc
auc <- function(predictions, reality) {
  r <- roc(predictions, reality)
  r$AUC
}

#' Plot ROC
#' @param predictions
#' @param reality
#' @export
plot_roc <- function(predictions, reality) {
  plot(roc(predictions, reality))
}

#' Get true negatives count.
#' @param predictions
#' @param reality
#' @return true negatives count.
#' @export
tn <- function(predictions, reality) table(predictions, reality)[1, 1]

#' Get false negatives count.
#' @param predictions
#' @param reality
#' @return false negatives count.
#' @export
fn <- function(predictions, reality) table(predictions, reality)[1, 2]

#' Get true positives count.
#' @param predictions
#' @param reality
#' @return true positives count.
#' @export
tp <- function(predictions, reality) table(predictions, reality)[2, 2]

#' Get false positives count.
#' @param predictions
#' @param reality
#' @return false positives count.
#' @export
fp <- function(predictions, reality) table(predictions, reality)[2, 1]


#' Plot confusion matrix
#' @param predictions
#' @param reality
#' @export
#' @importFrom cvms confusion_matrix plot_confusion_matrix
plot_cm <- function(predictions, reality) {
  cm <- confusion_matrix(targets=reality, prediction=predictions)
  plot_confusion_matrix(cm)
}

#' Get confusion matrix
#' @param predictions
#' @param reality
#' @return a confusion matrix
#' @export
c_m <- function(predictions, reality) {
  table(predictions, reality, dnn = c("Reality", "Prediction"))
}
