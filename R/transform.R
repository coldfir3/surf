#' This function performs a rigid body transformation of a surface file
#'
#' @export
#' @param surf \code{cimg} object
#' @param theta numerical indicating the angle of rotation (in degrees)
#' @param u,v numerical indicating the displacement in the x and y direction (in
#'   pixels)
#' @param method method for interpolation
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
transform <- function(surf, u = 0, v = 0, theta = 0, method = "imwarp"){

  theta <- theta * pi/180

  xC <- (ncol(surf)+1)/2
  yC <- (nrow(surf)+1)/2

  sn <- sin(theta)
  cs <- cos(theta)

  if(method == "imwarp"){
    map <- function(x, y){
      xl <- x - xC
      yl <- y - xC
      x <-  cs * xl + sn * yl - u + xC
      y <- -sn * xl + cs * yl - v + xC
      return(list(x = x, y = y))
    }
    surf <- imager::imwarp(surf, map = map, direction="backward", interpolation = "cubic")
  }
  else if(method == "taylor"){
    g <- imgradient(surf, "xy")
    x <- cimg(array(  replicate(nrow(surf), 1:ncol(surf)  - xC), dim(surf)))
    y <- cimg(array(t(replicate(ncol(surf), 1:nrow(surf)) - yC), dim(surf)))
    surf <- surf - (u - y*theta + x*theta^2/2) * g$x - (v + x*theta + y*theta^2/2) * g$y
  }
  else
    stop('Undefined method')

  return(surf)
}
