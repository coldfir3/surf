
#' Fill the gaps using nearest neighbors
#'
#' This function fill the NA values of a cimg object by averaging neighbors
#'
#' @export
#' @param im an cimg object with NAs
#' @param cubic if TRUE, use cubic interpolation. If FALSE, use linear (default FALSE)
#' @param extrapolate allow extrapolation (to values outside the image)
#' @param npass number of passes of the algorithm (usefull to fill larger holes)
#' @return a \code{\link[imager]{cimg}} object.
interp <- function (im, cubic = FALSE, extrapolate = FALSE, npass = 3){

  if (extrapolate)
    stop('extrapolation not yet implemented')
  if (cubic)
    stencil <- expand.grid(dx=seq(-2,2,1),dy=seq(-2,2,1))
  else
    stencil <- expand.grid(dx=seq(-1,1,1),dy=seq(-1,1,1))

  w <- imager::width(im)
  h <- imager::height(im)

  for (i in 1:npass){
    nas <- as.array(which(is.na(im), arr.ind = TRUE))
    if (!prod(dim(nas))) break
    nas <- nas[which(nas[,1] != 1 & nas[,1] != w & nas[,2] != 1 & nas[,2] != h),]
    if (!prod(dim(nas))) break

    im[nas] <- apply(nas, 1, function(loc)
        mean(imager::get.stencil(im, stencil, x = loc[1], y = loc[2]), na.rm = TRUE))
  }

  im[which(is.na(im))] <- 0
  return(im)
}

