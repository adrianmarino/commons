#' Get features importance from a dataframe for a give target column.
#' @param df a dataframe
#' @param target a target column
#' @export
#' @importFrom randomForest randomForest
features_importance <- function(df, target_column) {
  feat <- features(df, target_column) %>%
    mutate_if(is.character, as.factor)

  tar  <- target(df, target_column) %>%
    mutate_if(is.character, as.factor) %>%
    pull(target_column)

  randomForest(x = feat, y = tar, importance=TRUE)
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
#' @importFrom randomForest varImpPlot importance
top_acc_features <- function(result, top=10, order_metric='IncNodePurity') {
  index_as_column(as.data.frame(importance(result))) %>% 
    arrange(desc(!!sym(order_metric))) %>%
    top_n(top) %>%
    select(index_col) %>%
    pull()
}
