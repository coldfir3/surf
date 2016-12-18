#' Rigid body transformation of a surface
#'
#' This function performs a rigid body transformation of a surface file
#'
#' @export
#' @param surf \code{cimg} object
#' @param a numerical indicating the angle of rotation (in degrees)
#' @param u,v numerical indicating the displacement in the x and y direction (in
#'   pixels)
#' @param order character indicating the order of the transformation: "RT"
#'   indicates rotate then translate and "TR" indicate translate than rotate.
#'   Logical 1 or 0 are also accepted for optimization porpuses.
#' @return a transformed \code{cimg} object
#' @examples
#' par(mfrow = c(2,2), mar = c(2,2,1,1))
#' plot(ground1, asp = 1)
#' plot(transform(ground1, 5, 10, -10), asp = 1)
#' plot(transform(transform(ground1, 5, 10, -10), -5, -10, 10, "TR"))
#' plot(error(transform(transform(ground1, 5, 10, -10), -5, -10, 10, "TR"), ground1))
#' mean(error(transform(transform(ground1, 5, 10, -10), -5, -10, 10, "TR"), ground1))
transform <- function(surf, a, u, v, order = "RT"){

  a <- a * pi/180

  sa <- sin(a)
  ca <- cos(a)
  if(order == 1 | order == "RT")
    map <- function(x, y){
      xl <- x
      yl <- y
      x <-  ca * xl + sa * yl - ca * u - sa * v
      y <- -sa * xl + ca * yl + sa * u - ca * v
      return(list(x = x, y = y))
    }
  else if(order ==0 | order == "TR")
    map <- function(x, y){
      xl <- x
      yl <- y
      x <-  ca * xl + sa * yl - u
      y <- -sa * xl + ca * yl - v
      return(list(x = x, y = y))
    }
  else
    stop("'oder' must be onde of: 'RT', 'TR', 1, 0. ")

  surf <- imager::imwarp(surf, map = map, direction="backward", interpolation = "cubic")
  return(surf)
}
