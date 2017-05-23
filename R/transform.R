#' This function performs a rigid body transformation of a surface file
#'
#' @export
#' @param im \code{cimg} object
#' @param theta numerical indicating the angle of rotation (in degrees)
#' @param u,v numerical indicating the displacement in the x and y direction (in
#'   pixels)
#' @param method method for interpolation (one of "taylor" or "imwarp")
#' @return a transformed \code{cimg} object
#' @examples
#' surf <- ground[[1]]
#' par(mfrow = c(1,2), mar = c(2,2,1,1))
#' plot(surf)
#' plot(transform(surf, theta = 45))
#' plot(transform(surf, 0.5, 0))
#' plot(transform(surf, 0.5, method = 'taylor'))
#' plot(transform(surf, theta = 1))
#' plot(transform(surf, theta = 1, method = 'taylor'))
transform <- function(im, u = 0, v = 0, theta = 0, method = "imwarp"){

  theta <- theta * pi/180

  if(method == "imwarp"){
    if (theta == 0)
      map <- function(x, y){
        x <- x + u
        y <- y + v
        return(list(x = x, y = y))
      }
    else{
      xC <- (ncol(im)+1)/2
      yC <- (nrow(im)+1)/2
      sn <- sin(theta)
      cs <- cos(theta)
      map <- function(x, y){
        xl <- x - xC
        yl <- y - xC
        x <-  cs * xl + sn * yl - u + xC
        y <- -sn * xl + cs * yl - v + xC
        return(list(x = x, y = y))
      }
    }
    im <- imager::imwarp(im, map = map, direction="backward", interpolation = "cubic")
  }
  else if(method == "taylor"){
    g <- imgradient(im, "xy")
    if (theta == 0){
      h <- imhessian(im)
      im <- im + u*g$x + v*g$y + 1/2 * (u^2*h$xx + 2*u*v*h$xy + v^2*h$yy)
    }
    else{
      xC <- (ncol(im)+1)/2
      yC <- (nrow(im)+1)/2
      x <- cimg(array(  replicate(nrow(im), 1:ncol(im)  - xC), dim(im)))
      y <- cimg(array(t(replicate(ncol(im), 1:nrow(im)) - yC), dim(im)))
      im <- im - (u - y*theta + x*theta^2/2) * g$x - (v + x*theta + y*theta^2/2) * g$y
    }
  }
  else
    stop('Undefined method')

  return(im)
}
