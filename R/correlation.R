#' Get a list of high correlated features.
#' @param df a dataframe.
#' @param cutoff All with a correlation upter to cutoff.
#' @return a list of high correlated columns under input data frame.
#' @export
#' @importFrom caret findCorrelation
find_high_correlated_columns <- function(df, cutoff=0.8) {
  columns <- findCorrelation(cor(df), cutoff=cutoff, verbose = T)
  names(df)[columns]
}

