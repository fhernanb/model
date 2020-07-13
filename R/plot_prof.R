#' Profile plot
#' 
#' This function gives the profile plot for contingency table objects created by table or xtabs or matrix.
#' 
#' @param x a table, xtabs or matrix object.
#' @param Row a logical value to indicate if profile plot is for Rows o Columns.
#' @param cex.names Expansion factor for axis names (bar labels).
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis, by default is Frequency.
#' @param cex a character expansion factor for legend.
#' @param ... arguments to be passed to methods, such as graphical parameters (see par).
#' 
#' @example examples/examples_plot_prof.R
#' @return \code{plot_prof} function returns a profile plot. 
#' 
#' @importFrom graphics barplot legend par
#' @importFrom grDevices hcl
#' @export
plot_prof <- function(x, Row=TRUE, cex.names=0.5, xlab='',
                      ylab='Frequency', cex=1, ...) {
  stopifnot(class(x) %in% c('table', 'xtabs', 'matrix'))
  if (is.null(colnames(x)) & is.null(rownames(x))) 
    print('The table does not have names')
  type <- ifelse(Row, 1, 2)
  fij <- prop.table(x, type)
  if (Row) {
    Names <- colnames(x)
    fij <- t(fij)
  } else {
    Names <- rownames(x)
  }
  par(oma=c(0, 0, 0, 5))
  seq.col <- seq(from=10, to=300, length.out=dim(x)[-type])
  barplot(fij, beside=T, las=1, ylim=c(0, 1.2*max(fij)),
          xlab=xlab, cex.names=cex.names,
          col=hcl(h=seq.col), ylab=ylab)
  legend(par('usr')[2], par('usr')[4], xpd=NA, cex=cex,
         legend=Names, bty='n',
         fill=hcl(h=seq.col))
}