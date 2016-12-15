#' Title
#'
#' description
#'
#' @export
#' @param surf a \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}} object.
#' @param formula an optional object of class formula to pass to the lm function.
#' @param form an optional parameter that overides the information given on formula. Options are: "eliptical", "paraboloid". #enable partial match
#' @return a corrected \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}} object.
#' @examples
#' file <- system.file("extdata", "form.txt", package = "surf")
#' surf <- read.surf(file)
#' par(mfrow = c(1,2))
#' plot(surf, asp = 1)
#' plot(form(surf, form = 'paraboloid'), asp = 1)
#'
#' plot(surf, asp = 1)
#' plot(form(surf, value ~ stats::poly(x, 2) + stats::poly(y, 2)), asp = 1)
#'
#' file <- system.file("extdata", "form.zip", package = "surf")
#' surf <- read.zip(file)
#' plot(imager::imlist(c(surf,form(surf))), asp = 1)
form <- function(surf, form = 'paraboloid'){
  UseMethod("form", surf)
}

#' @rdname form
#' @method form cimg
#' @export
form.cimg <- function(surf, form = 'paraboloid'){

  df <- as.data.frame(surf)

  if (form == 'paraboloid')
    model <- stats::lm(value ~ stats::poly(x, 2) + stats::poly(y, 2), data = df)
  else if (form == 'eliptical')
    model <- stats::lm(value ~ stats::poly(x, 2) + stats::poly(y, 2), data = df) #ARRUMAR
  else if (class(form) == "formula")
    model <- stats::lm(form, data = df)
  else
    stop("error message ARRUMAR")
  surf <- surf - stats::fitted(model)
  return(surf)
}

#' @rdname form
#' @method form imlist
#' @export
form.imlist <- function(surf, form = 'paraboloid'){ # arrumar
  surf <- lapply(surf, form.cimg, form)
  surf <- imager::imlist(surf)
  return(surf)
}
