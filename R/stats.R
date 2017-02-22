#' Custom mean for \code{imlist} objects
#'
#' @param x \code{imlist} object
#' @param ... Not used on the current implementation.
#' @return mean surface of all surfaces inside \code{imlist} object
#' @export
mean.imlist <- function(x, ...){

  res <- 0
  for (s in x)
    res <- res + s
  res <- res/length(x)

  return(res)
}

#' Custom sd for \code{imlist} objects
#'
#' @param surf \code{imlist} object
#' @return standard deviation surface of all surfaces inside \code{imlist} object
#' @export
sd.imlist <- function(surf){ #promote to generic

  mu <- mean(surf)

  res <- 0
  for (s in surf)
    res <- (s - mu)^2
  res <- res/(length(surf) - 1)
  res <- res^0.5

    return(res)
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
