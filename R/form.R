#' Title
#'
#' description
#'
#' @export
#' @param surf a \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}} object.
#' @param formula an optional object of class formula to pass to the lm function.
<<<<<<< HEAD
#' @param form an optional parameter that overides the information given on formula. Options are: "eliptical", "paraboloid". #enable partial match
=======
#' @param form an optional parameter that overides the information given on formula. Options are: "eliptical", "paraboloid".
>>>>>>> e7f0212ba28c40bf218f147c65f66099e2e870a5
#' @return a corrected \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}} object.
#' @examples
#' file <- system.file("extdata", "form.txt", package = "surf")
#' surf <- read.surf(file)
#' par(mfrow = c(1,2))
#' plot(surf, asp = 1)
<<<<<<< HEAD
#' plot(form(surf, form = 'paraboloid'), asp = 1)
#'
#' plot(surf, asp = 1)
#' plot(form(surf, value ~ stats::poly(x, 2) + stats::poly(y, 2)), asp = 1)
#'
#' file <- system.file("extdata", "form.zip", package = "surf")
#' surf <- read.zip(file)
#' plot(imager::imlist(c(surf,form(surf))), asp = 1)
form <- function(surf, form = 'paraboloid'){
=======
#' plot(form(surf), asp = 1)
#'
# file <- system.file("extdata", "form.zip", package = "surf")
# surf <- read.zip(file)
# plot(imager::imlist(c(surf,form(surf))), asp = 1)
form <- function(surf, form = 'paraboloid', formula = NULL){
>>>>>>> e7f0212ba28c40bf218f147c65f66099e2e870a5
  UseMethod("form", surf)
}

#' @rdname form
#' @method form cimg
#' @export
<<<<<<< HEAD
form.cimg <- function(surf, form = 'paraboloid'){
=======
form.cimg <- function(surf, form = 'paraboloid', formula = NULL){
>>>>>>> e7f0212ba28c40bf218f147c65f66099e2e870a5

  df <- as.data.frame(surf)

  if (form == 'paraboloid')
    model <- stats::lm(value ~ stats::poly(x, 2) + stats::poly(y, 2), data = df)
<<<<<<< HEAD
  else if (form == 'eliptical')
    model <- stats::lm(value ~ stats::poly(x, 2) + stats::poly(y, 2), data = df) #ARRUMAR
  else if (class(form) == "formula")
    model <- stats::lm(form, data = df)
  else
    stop("error message ARRUMAR")
=======
  if (form == 'eliptical')
    model <- stats::lm(value ~ stats::poly(x, 2) + stats::poly(y, 2), data = df) #ARRUMAR
  if (!is.null(formula))
    model <- stats::lm(value ~ formula, data = df) #arrumar
>>>>>>> e7f0212ba28c40bf218f147c65f66099e2e870a5
  surf <- surf - stats::fitted(model)
  return(surf)
}

#' @rdname form
#' @method form imlist
#' @export
<<<<<<< HEAD
form.imlist <- function(surf, form = 'paraboloid'){ # arrumar
  surf <- lapply(surf, form.cimg, form)
=======
form.imlist <- function(surf, form = 'paraboloid', formula = NULL){ # arrumar
  surf <- lapply(surf, form)
>>>>>>> e7f0212ba28c40bf218f147c65f66099e2e870a5
  surf <- imager::imlist(surf)
  return(surf)
}
