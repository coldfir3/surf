#' Title
#'
#' description
#'
#' @export
#' @param surf a \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}} object.
#' @param form an optional parameter that overides the information given on formula. Options are: "ellipsoid", "paraboloid". #enable partial match
#' @return a corrected \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}} object.
#' @examples
#' file <- system.file("extdata", "form.txt", package = "surf")
#' surf <- read.surf(file)
#' par(mfrow = c(1,3))
#' plot(surf, asp = 1)
#' plot(form(surf, form = 'paraboloid'), asp = 1)
#' plot(form(surf, form = 'ellipsoid'), asp = 1)
#'
#' file <- system.file("extdata", "form.zip", package = "surf")
#' surf <- read.zip(file)
#' plot(form(surf, form = 'paraboloid'))
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
  else if (form == 'ellipsoid')
#    model <- stats::nls(1 ~ A*x^2 + B*y^2 + C*value^2 + D*x*y + E*x*value + F*y*value + G*x + H*y + I*value,
#                        data = df,
#                        start=list(A = 0, B = 0, C = 0, D = 0, E = 0, F = 0, G = 0, H = 0, I = 1))
    model <- stats::nls(value ~ ((x-x0)^2/a^2 + (y-y0)^2/b^2)^0.5 + c, data = df,
                        start=list(a = 1, b = 1, x0 = 0, y0 = 0, c = mean(surf)))
  else if (class(form) == "formula")
    model <- stats::lm(form, data = df)
  else
    stop("form not allowed, check the help for more information.")

  surf <- surf - stats::fitted(model)
  return(surf)
}

#' @rdname form
#' @method form imlist
#' @export

form.imlist <- function(surf, form = 'paraboloid'){
  surf <- lapply(surf, form.cimg, form)
  surf <- imager::imlist(surf)
  return(surf)
}
