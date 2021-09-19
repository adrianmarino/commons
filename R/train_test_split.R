#' Makes a train test split. It Also makes a suffle by default.
#' @param df dataframe to split.
#' @param train_size split percent. Is percent of rows into train set after 
#' split the input dataframe. Speficy it as a percent  between 0 and 1. i.e. .7 is 70%.  
#' @param shuffle Applies a random order befoe split the dataframe. TRUE by default.
#' @examples  
#'     train_set, test_set %<-% train_test_split(df, .7)
#' @export
train_test_split <- function(df, train_size=.7, shuffle=TRUE) {
  if(shuffle) {
    train_ind <- sample(seq_len(nrow(df)), size = (nrow(df) * train_size))
    train_set <- df[train_ind, ]
    test_set  <- df[-train_ind, ]
  } else {
    train_set <- df[1:abs(nrow(df)*train_size), ]
    test_set  <- df[abs(nrow(df)*train_size):nrow(df), ]
  }
  
  print(paste('Train set size:', nrow(train_set)))
  print(paste('Test set size:', nrow(test_set)))
  
  list(train_set, test_set)
}
