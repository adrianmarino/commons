#' Load a csv file ito a data.frame
#' @param csv file path.
#' @return a data.frame.
#' @export
#' @importFrom utils read.csv
load_df_from_csv <- function(path) {
  as.data.frame(read.csv(file = path))
}
