#' Title
#'
#' description
#'
#' @export
#' @param surf a \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}} object.
#' @return a corrected \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}} object.
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
level <- function(surf){
  UseMethod("level", surf)
}

#' @rdname level
#' @method level cimg
#' @export
level.cimg <- function(surf){
  df <- as.data.frame(surf)
  model <- stats::lm(value ~ x + y, data = df)
  surf <- surf - stats::fitted(model)
  return(surf)
}

#' @rdname level
#' @method level imlist
#' @export
level.imlist <- function(surf){
  surf <- lapply(surf, level)
  surf <- imager::imlist(surf)
  return(surf)
}
