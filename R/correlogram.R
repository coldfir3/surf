#' bla bla
#'
#' bla bla
#'
#' @export
#' @param im cimg object to compute a correlogram
#' @param samples numeric corresponding to the number of sample pixels to be drawn
#' @param size numeric value for the size of the cross-shaped stencil (\code{size} x \code{size})
#' @param pos numeric data.frame for the pixel position for where to sample, it must be inside the image considering a border marging of \code{size/2}. Also will overide the \code{samples} parameter
#' @param stencil numeric data.frame that can override the default stencil
#'
#' @return a numeric data.frame for the correlogram data
correlogram <- function(im, samples = 5000, size = 11, pos =  NULL, stencil = NULL){

  if (is.null(stencil)){
    if(! (size %% 2))
      stop('error, size must be odd')
    low <- -(size - 1)/2
    upp <- (size - 1)/2
    stencil <- unique(data.frame(
      dx=c(seq(low,upp,1),rep(0,size)),
      dy=c(rep(0,size),seq(low,upp,1))))
  }

  if(is.null(pos)){
    pos.x <- round(stats::runif(samples, 1 + max(stencil$dx), imager::width(im) + min(stencil$dx))) #Use more locations
    pos.y <- round(stats::runif(samples, 1 + max(stencil$dy), imager::height(im) + min(stencil$dy))) #Use more locations
    pos <- cbind(pos.x,pos.y)
  }

  mat <- t(apply(pos, 1, function(v) imager::get.stencil(im, stencil, x = v[1],y = v[2])))
  Dx <- as.matrix(stats::dist(stencil$dx))
  Dy <- as.matrix(stats::dist(stencil$dy))
  df <- data.frame(dist.x = c(Dx),dist.y = c(Dy), cor = c(stats::cor(mat)))

  return(df)
}
