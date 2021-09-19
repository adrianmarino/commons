#' Returns datafram e features.
#' @param df a data frame.
#' @param target_col target column.
#' @return a features data frame.
#' @export
#' @importFrom dplyr select
features <- function(df, target_col) {
  if(target_col %in% colnames(df)) {
    df %>% dplyr::select(-target_col)    
  } else {
    df
  }
}

#' Returns target columna valies as an array.
#' @param df a data frame.
#' @param target_col target column.
#' @return target variable values.
#' @export
#' @importFrom dplyr select pull
label <- function(df, target_col) {
  df %>% 
    dplyr::select(target_col) %>% 
    dplyr::pull()
}

#' Add index as another regular data frame column
#' @param df a data frame.
#' @return a data frame.
#' @export
index_as_column <- function(df) {
  df <- cbind(index = rownames(df), df)
  rownames(df) <- 1:nrow(df)
  df
}
