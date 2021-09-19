#' Get features importance from a dataframe for a give target column.
#' @param df a dataframe
#' @param target a target column
#' @export
#' @importFrom randomForest randomForest
features_importance <- function(df, target) {
  features <- df %>% 
    dplyr::select_if(is.numeric)
  
  target <- df %>% 
    dplyr::select(target) %>%
    mutate_if(is.character, as.factor) %>%
    pull(target)
  
  randomForest(x = features, y = target, importance=TRUE)
}

#' Plot a fearures importance result.
#' @param result a fearures importance result
#' @param title plot title.
#' @export
#' @importFrom randomForest varImpPlot
plot_features_importance <- function(result, title ='Features importance') {
  varImpPlot(result, main=title, bg="skyblue", cex=1, pch=22)
}

#' Returns top K more important features.
#' @param result a fearures importance result
#' @param top number of features to get from top
#' @return a features name list
#' @export
#' @importFrom dplyr arrange select pull top_n desc
#' @importFrom randomForest varImpPlot
top_acc_features <- function(result, top=10) {
  index_as_column(as.data.frame(importance(result))) %>% 
    arrange(desc(MeanDecreaseAccuracy)) %>%
    top_n(top) %>%
    dplyr::select(index) %>%
    pull()
}
