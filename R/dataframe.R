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

#' Returns target column values as array.
#' @param df a data frame.
#' @param target_col target column.
#' @return target variable values.
#' @export
#' @importFrom dplyr select pull
target_array <- function(df, target_col) {
  df %>% 
    dplyr::select(target_col) %>% 
    dplyr::pull()
}

#' Returns a data frame with the target column.
#' @param df a data frame.
#' @param target_col target column.
#' @return target variable values.
#' @export
#' @importFrom dplyr select
target <- function(df, target_col) df %>% dplyr::select(target_col)

#' Add index as another regular data frame column
#' @param df a data frame.
#' @return a data frame.
#' @export
index_as_column <- function(df) {
  df <- cbind(index_col = rownames(df), df)
  rownames(df) <- 1:nrow(df)
  df
}

#' Get numneric column names.
#' @param df a data frame.
#' @return a list of columns.
#' @export
#' @importFrom dplyr select_if
num_vars <- function(df) df %>% select_if(is.numeric) %>% colnames()

#' Get categorical column names.
#' @param df a data frame.
#' @return a list of columns.
#' @export
#' @importFrom dplyr select_if
cat_vars <- function(df) df %>% select(-num_vars(df)) %>% colnames()

