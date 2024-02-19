#' Summary table for a \code{lmerMod} object.
#'
#' This function creates the summary table with p values for a \code{lmerMod} object.
#'
#' @param mod A `lmerMod` object.
#' @example examples/examples_summary_lmer.R
#' @return \code{summary_lmer} function returns a matrix with p values in last column.
#'
#' @importFrom stats pnorm printCoefmat
#' @export
#'
summary_lmer <- function(mod) {
  if (! inherits(mod, "lmerMod")) 
    stop("The model is not a lmerMod object")
  coefi <- summary(mod)$coefficients
  pvalues <- 2*pnorm(q=abs(coefi[, 3]), lower.tail=FALSE)
  res <- cbind(coefi, p.value=round(pvalues, 4))
  colnames(res) <- c("Estimate", "Std.Err", "t value", "Pr(>t)")
  printCoefmat(res)
}
