#' Level a surface
#'
#' Level the surface by assigning the horizontal reference based on a tilted
#' plane regression
#'
#' @export
#' @param surf a \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}}
#'   object.
#' @param method one of two options: \code{'rot'} or \code{'sub'}.
#' @return a corrected \code{\link[imager]{cimg}} or a
#'   \code{\link[imager]{imlist}} object.
#' @examples
#' file <- system.file("extdata", "ground.txt", package = "surf")
#' surf <- read.surf(file)
#' par(mfrow = c(1,2))
#' plot(surf, asp = 1)
#' plot(level(surf), asp = 1)
#'
#' file <- system.file("extdata", "ground.zip", package = "surf")
#' surf <- read.zip(file)
#' plot(surf, asp = 1)
#' plot(level(surf), asp = 1)
#' plot(imager::imlist(c(surf,level(surf))), asp = 1)
level <- function(surf, method = 'rot'){
  UseMethod("level", surf)
}

#' @rdname level
#' @method level cimg
#' @export
level.cimg <- function(surf, method = 'rot'){
  df <- as.data.frame(surf)
  model <- stats::lm(value ~ x + y, data = df)
  if(method == 'sub')
    surf <- surf - stats::fitted(model)
  else if(method == 'rot'){
    alpha <- 100/cos(atan(model$coefficients[2]))
    beta <- 100/cos(atan(model$coefficients[3]))
    surf <- surf - stats::fitted(model)
    surf <- imager::resize(surf, -alpha, -beta)
  }
  else
    stop('error, method should be "rot" or "sub".')
  return(surf)
}

#' @rdname level
#' @method level imlist
#' @export
level.imlist <- function(surf, method = 'rot'){
  surf <- lapply(surf, level, method)
  surf <- imager::imlist(surf)
  return(surf)
}
