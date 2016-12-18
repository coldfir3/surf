#' Relocation of a surface
#'
#' This function relocates a surface to match some reference sample.
#'
#' @export
#' @param surf \code{cimg} object
#' @param targ \code{cimg} object
#' @param control for the optimization algorithm
#' @return a relocated \code{cimg} object
#' @examples
#' r_ground2 <- relocate(ground2, ground1, control = list(max.time = 4))
#' par(mfrow = c(2,2), mar = c(2,2,1,1))
#' plot(ground1, asp = 1)
#' plot(ground2, asp = 1)
#' plot(r_ground2, asp = 1)
#' plot(error(ground1, r_ground2), asp = 1)
#' mean(error(ground1, r_ground2))
relocate <- function(surf, targ, control = NULL){

  fn <- function(par, surf, targ, return_surface = FALSE){

#    if(!return_surface){
#      surf[1:50,] <- 0
#      surf[,1:50] <- 0
#      surf[256:207,] <- 0
#      surf[,256:207] <- 0
#    }


    surf <- transform(surf, par[1], par[2], par[3])

    S <- mean(error(surf, targ))
    if (return_surface)
      return(surf)
    else
      return(S)
  }

#  res <- rgenoud::genoud(fn = fn, nvars = 3, Domains = cbind(-lim,lim),
#                         pop.size=50, max.generations=10, BFGSburnin=7, BFGS = FALSE,
#                         gradient.check=FALSE, boundary.enforcement = 2, print.level = 1,
#                         surf = surf, targ = targ, solution.tolerance = 1e-3)

  if (is.null(control$max.time))
    control$max.time = 60
  if (is.null(control$verbose))
    control$verbose = TRUE
#  if (is.null(control$maxit))
#    control$maxit = 30
  if (is.null(control$smooth))
    control$smooth = FALSE
  if (is.null(control$threshold.stop))
    control$threshold.stop = 0
  if (is.null(control$lower))
    lower <- -0.1 * c(180, dim(surf)[1], dim(surf)[2])
  else
    lower <- control$lower
  if (is.null(control$upper))
    upper <- 0.1 * c(180, dim(surf)[1], dim(surf)[2])
  else
    upper <- control$upper

  control$lower <- control$upper <- NULL

  res <- GenSA::GenSA(par = NULL, fn = fn, lower = lower, upper = upper,
                      control = control, surf, targ)

  surf <- fn(res$par, surf, targ, TRUE)

  return(surf)
}
