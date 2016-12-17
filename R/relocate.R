#' Relocation of a surface
#'
#' This function relocates a surface to match some reference sample.
#'
#' @export
#' @param surf \code{cimg} object
#' @param targ \code{cimg} object
#' @return a relocated \code{cimg} object
#' @examples
#' file <- system.file("extdata", "sur1.txt", package = "surf")
#' sur1 <- read.surf(file)
#' file <- system.file("extdata", "sur2.txt", package = "surf")
#' sur2 <- read.surf(file)
#' sur3 <- relocate(sur2, sur1)
#' par(mfrow = c(2,2), mar = c(2,2,1,1))
#' plot(sur1, asp = 1)
#' plot(sur2, asp = 1)
#' plot(sur3, asp = 1)
#' plot(error(sur1, sur3), asp = 1)
#' mean(error(sur1, sur3))
relocate <- function(surf, targ){

  lim <- c(25,dim(surf)[1]*0.1,dim(surf)[2]*0.1)

  fn <- function(par, surf, targ, return_surface = FALSE){

    surf[1:50,] <- 0
    surf[,1:50] <- 0
    surf[256:207,] <- 0
    surf[,256:207] <- 0

    ang <- par[1]
    u <- par[2]
    v <- par[3]

    surf <- imager::imrotate(surf, ang)
    surf <- imager::imshift(surf, u, v)
    if(any(dim(targ) != dim(surf)))
      surf <- imager::crop.borders(surf, (dim(surf)[1] - dim(targ)[1])/2, (dim(surf)[2] - dim(targ)[2])/2)

    S <- mean(error(surf, targ))
    if (return_surface)
      return(surf)
    else
      return(S)
  }

  res <- rgenoud::genoud(fn = fn, nvars = 3, Domains = cbind(-lim,lim),
                         pop.size=150, max.generations=15, BFGSburnin=10, BFGS = FALSE,
                         gradient.check=FALSE, boundary.enforcement = 2, print.level = 1,
                         surf = surf, targ = targ, solution.tolerance = 1e-3)

  res2 <- GenSA::GenSA(par = res$par, fn = fn, lower = - lim, upper = lim, control = list(max.time = 2),
                       surf, targ, return_surface = FALSE)

  surf <- fn(res$par, surf, targ, TRUE)

  return(surf)
}
