#' Perform a PCA rebust analisys un er a input data frame.
#' @param df a input dataframe.
#' @param scale scale variables before apply PCA analisys.
#' @param robust select PCA method (S-estimator, MCD, or MVE).
#' @param nsamp The number of subsamples that the robust estimator should use. This defaults to 10 times the number of rows in the matrix.
#' @return a pca analisys result.
#' @export
#' @importFrom mdqc prcomp.robust
pca <- function(df, scale = TRUE, robust="MVE", nsamp=10) {
  prcomp.robust(df, scale = scale, robust=robust, nsamp=nsamp)
}


#' Plot a PCA analisys into a biplot.
#' @param pca_result      PCA result to plot.
#' @param title           a title.
#' @param colours         groups colors.
#' @param labels          groups labels.
#' @param scale           covariance biplot (scale = 1), form biplot (scale = 0). When scale = 1, the inner product between the variables approximates the covariance and the distance between the points approximates the Mahalanobis distance.
#' @param obs.scale       scale factor to apply to observations
#' @param var.scale       scale factor to apply to variables
#' @param groups          optional factor variable indicating the groups that the observations belong to. If provided the points will be colored according to groups
#' @param ellipse         draw a normal data ellipse for each group?
#' @param labels          optional vector of labels for the observations
#' @param alpha           alpha transparency value for the points (0 = transparent, 1 = opaque)
#' @param varname.adjust  adjustment factor the placement of the variable names, >= 1 means farther from the arrow
#' @param varname.size    size of the text for variable names
#' @export
#' @importFrom ggbiplot ggbiplot
plot_pca <- function(
  pca_result, 
  groups=NULL, 
  alpha=0.08, 
  title='', 
  ellipse = TRUE, 
  colours=c("green", "red"),
  labels=c("No", "Yes"),
  obs.scale = 1,
  var.scale = 1,
  varname.adjust = 1.5,
  varname.size = 3
) {
  ggbiplot(
    obs.scale=obs.scale,
    var.scale=var.scale,
    varname.adjust=varname.adjust,
    varname.size=varname.size,
    pca_result,
    alpha=alpha,
    groups=groups,
    ellipse=ellipse
  ) +
    scale_color_manual(
      name=title, 
      values=colours,
      labels=labels
    ) +
    theme(legend.direction ="horizontal", legend.position = "top")
}

