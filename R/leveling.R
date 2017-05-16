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
#' surf <- ground[[1]]
#' par(mfrow = c(1,2))
#' plot(surf)
#' plot(level(surf))
#'
#' file <- system.file("extdata", "ground.zip", package = "surf")
#' surf <- read.zip(file)
#' plot(surf, layout = "row")
#' plot(level(surf), layout = "row")
level <- function(surf, method = 'rot'){
  UseMethod("level", surf)
}

#' @rdname level
#' @method level cimg
#' @export
level.cimg <- function(surf, method = 'sub'){
  df <- as.data.frame(surf)
  model <- stats::lm(value ~ x + y, data = df)
  if(method == 'coef')
    return(model$coef)
  else if(method == 'sub')
    surf <- surf - stats::fitted(model)
  else if(method == 'rot'){
    alpha <- cos(atan(model$coefficients[2]))
    beta <- cos(atan(model$coefficients[3]))
    surf <- surf - stats::fitted(model)
    map <- function(x, y) list(x = x * alpha, y = y * beta)
    surf <- imager::imwarp(surf, map = map, direction="backward")
  }
  else
    stop('error, method should be "rot" or "sub".')
  return(surf)
}

#' @rdname level
#' @method level imlist
#' @export
level.imlist <- function(surf, method = 'sub'){
  surf <- lapply(surf, level, method)
  if(method != 'coef')
    surf <- imager::as.imlist(surf)
  return(surf)
}
