#' Plot observations in the coordinate plane Fi-Fj.
#' 
#' This function plots the observations in the coordinate plane Fi-Fj.
#' 
#' @param scores a matrix obtained from \code{\link{factanal}} funcion with the
#' the argument \code{scores="regression"}, see example below.
#' @param fx a number to indicate the factor to plot in x axis.
#' @param fy a number to indicate the factor to plot in y axis.
#' @param lchar the number of first characters to identify points, by default is 3.
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' 
#' @examples
#' v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
#' v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
#' v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
#' v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
#' v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
#' v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
#' m1 <- cbind(v1,v2,v3,v4,v5,v6)
#' scores <- factanal(m1, factors=3, method="mle",
#'                    scores="regression")$scores
#' 
#' plot_obsfa(scores, fx=1, fy=2, col='blue', pch=20)
#' plot_obsfa(scores, fx=1, fy=3, col='red', pch=19)
#' @return \code{plot_obsfa} function returns a plot with the observations in the coordinate plane Fi-Fj.
#' 
#' @details The number in \code{fx} and \code{fy} must be less or equal
#' number of factors in \code{factanal} function.
#' 
#' @importFrom graphics plot text abline
#' @export
plot_obsfa <- function(scores, fx=1, fy=2, lchar=3, ...) {
  stopifnot(class(scores) == 'matrix')
  if (is.null(rownames(scores))) {
    n <- dim(scores)[1]
    rownames(scores) <- 1:n
    lchar <- nchar(n)
  }
  plot(x=scores[, fx], y=scores[, fy],
       xlab=bquote(.('Factor') ~ .(fx)),
       ylab=bquote(.('Factor') ~ .(fy)), ...)
  Names <- substr(rownames(scores), start = 1, stop = lchar)
  text(scores[, c(fx, fy)], labels = Names, adj = c(0, 0))
  abline(h = 0, col = "gray60", lty = "longdash")
  abline(v = 0, col = "gray60", lty = "longdash")
}
