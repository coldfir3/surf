#' Custom mean for \code{imlist} objects
#'
#' @param x \code{imlist} object
#' @param ... Not used on the current implementation.
#' @return mean surface of all surfaces inside \code{imlist} object
#' @export
mean.imlist <- function(x, ...){

  dim <- dim(x[[1]])

  data <- lapply(x, as.vector)
  data <- do.call(rbind, data)
  data <- apply(data, 2, mean)

  return(imager::as.cimg(data, x = dim[1], y = dim[2], z = dim[3], cc = dim[4]))
}

#' Custom sd for \code{imlist} objects
#'
#' @param x \code{imlist} object
#' @return standard deviation surface of all surfaces inside \code{imlist} object
#' @export
sd.imlist <- function(x){ #promote to generic

  dim <- dim(x[[1]])

  data <- lapply(x, as.vector)
  data <- do.call(rbind, data)
  data <- apply(data, 2, stats::sd)

  return(imager::as.cimg(data, x = dim[1], y = dim[2], z = dim[3], cc = dim[4]))
}

#' transforms \code{imlist} objects into observation dataframes
#'
#' Each \code{cimg} object inside \code{surf} is vectorized and alocated in an
#' individual line of a resultind data.frame
#'
#' @param surf \code{imlist} object
#' @return \code{data.frame} of observations
#' @export
as.observations <- function(surf){

  data <- lapply(surf, as.vector)
  data <- do.call(rbind, data)
  data <- as.data.frame(data)

  return(data)
}
