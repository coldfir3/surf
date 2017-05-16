#' bla bla
#'
#' bla bla
#'
#' @export
#' @param x array for input data
#' @param lower numeric value for lower quantile bound
#' @param upper numeric value for upper quantile bound
rm.outliers <- function(x, lower = 0.01, upper = 0.99){
  q <- stats::quantile(x, c(lower, upper), na.rm = TRUE)
  x[x > q[2]] <- NA
  x[x < q[1]] <- NA
  return(x)
}
