methods::setOldClass("ecdf")

#' A S4 class object for the Abbott-Firestone data
#'
#' @slot dist distribuition function for the Abbott object
#' @slot Sa Arithmetic average of absolute values
#' @slot Sq Root mean squared
#' @slot Sv Maximum valley depth
#' @slot Sp Maximum peak height
#' @slot St Maximum Height of the surface
#' @slot Ssk Skewness
#' @slot Sku kurtosis
#'
setClass("abbott", representation(
  dist = "ecdf",
  Sa = "numeric",
  Sq = "numeric",
  Sv = "numeric",
  Sp = "numeric",
  St = "numeric",
  Ssk = "numeric",
  Sku = "numeric")
)

#' Abbott-Firestone data constructor
#'
#' Description
#'
#' @param surf a \code{cimg} object
#' @return A S4 object of class \code{\link{abbott-class}}
#'
#' @export
#' @examples
#' file <- system.file("extdata", "ground.txt", package = "surf")
#' surf <- read.surf(file)
#' surf <- level(surf)
#' abb <- abbott(surf)
#' print(abb)
#' plot(abb)
abbott <- function(surf){

  model <- methods::new('abbott')

  surf <- as.data.frame(surf)$value

  model@dist <- stats::ecdf(surf)
  model@Sa <- mean(abs(surf))
  model@Sq <- sqrt(mean(surf^2))
  model@Sv <- min(surf)
  model@Sp <- max(surf)

  return(model)
}

#' @describeIn abbott Custom print for \code{abbott} objects
#' @param x An \code{abbott} object.
#' @export
setMethod("print", signature = "abbott", definition = function(x) base::print(x@Sa))

#' @describeIn abbott Custom show for \code{abbott} objects
#' @param object An \code{abbott} object.
#' @export
setMethod("show", signature = "abbott", definition = function(object) base::print(object@Sa))

#' @describeIn abbott Custom show for \code{abbott} objects
#' @param ynew Optional probability points where to calculate heights.
#' @param y unnused argument of \code{plot} generic function.
#' @param ... Optional arguments to be passed to \code{plot} function
#' @export
setMethod("plot", signature = "abbott", definition = function(x, y, ..., ynew = NULL){

  if(is.null(ynew))
    ynew <- seq(0,1,0.002)
  x <- x@dist(ynew)
  plot(x, ynew, type = 'l', xlab = "heights", ylab = 'probability', ...)

})
