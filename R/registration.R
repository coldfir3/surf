# devtools::use_package('rgenoud')
#' Registration based on the NMI criteria
#'
#' bla bla
#'
#' @export
#' @param surf,targ cimg objects
#' @param trans logical indicating if the relocated surface should
#'  be returned instead of the registrated parameters
#' @param control list to control the parameters of the optimization routine
#' @param method character indicating the method to be used
#' @param par0 numeric vector for the initial guess for the registration parameters
#' @param lower,upper numeric vector for the bounds of the optimization problem
#' @return bla bla
#' @examples
#' # simulated shif to check if the taylor approximation is correct
#' imA <- ground[[1]]
#' imB <- transform(imA, 0.5, -0.2, 0, method = 'taylor')
#' round(register(imB, imA, method = 'taylor'),2)
#' # see that the obtained value for the registration parameters are equal
#' # to the oposite applyed transformation values.
#' ##
#' ## The following examples were exluded from the automatic
#' ## run to save compilation time. However, they should all be working.
#' # Now lets check the differences between the algorithms for a real image
#' \dontrun{
#' imB <- ground[[20]]
#' (par.taylor <- register(imB, imA, method = 'taylor'))
#' (par.grad   <- register(imB, imA, method = 'grad'))
#' (par.gen    <- register(imB, imA, method = 'gen'))
#' imB.grad <- transform(imB, par.grad[1], par.grad[2], par.grad[3])
#' imB.gen  <- transform(imB, par.gen[1], par.gen[2], par.gen[3])
#' NMI(imA,imB)
#' NMI(imA,imB.grad)
#' NMI(imA,imB.gen)
#' par(mfrow = c(1,2))
#' plot(rm.outliers(imA - imB.grad))
#' plot(rm.outliers(imA - imB.gen))
#' }
#' # Now, lets see if we can visualize the motion of the sample
#' \dontrun{
#' pars.taylor <- t(sapply(ground, register, targ = ground[[1]], method = 'taylor'))
#' pars.grad   <- t(sapply(ground, register, targ = ground[[1]], method = 'grad'))
#' pars.gen    <- t(sapply(ground, register, targ = ground[[1]], method = 'gen'))
#' plot(0, pch = NA, asp = 1,
#'  xlim = range(rbind(pars.taylor[,1], pars.grad[,1], pars.gen[,1])),
#'  ylim = range(rbind(pars.taylor[,2], pars.grad[,2], pars.gen[,2])))
#' points(pars.taylor[,1:2], col = 'orange')
#' points(pars.grad[,1:2], col = 'skyblue')
#' points(pars.gen[,1:2], col = 'black')
#' lines(pars.taylor[,1:2], col = 'orange')
#' lines(pars.grad[,1:2], col = 'skyblue')
#' lines(pars.gen[,1:2], col = 'black')
#' summary(pars.taylor[,3])
#' summary(pars.grad[,3])
#' summary(pars.gen[,3])
#' }
register <- function(surf, targ, method = 'gen', par0 = NULL, lower = NULL, upper = NULL, trans = FALSE, control = NULL){

  if(is.null(par0) | method == 'taylor'){
    xC <- (ncol(targ)+1)/2
    yC <- (nrow(targ)+1)/2

    g <- imgradient(targ, "xy")
    x <- cimg(array(  replicate(nrow(targ), 1:ncol(targ)  - xC), dim(targ)))
    y <- cimg(array(t(replicate(ncol(targ), 1:nrow(targ)) - yC), dim(targ)))

    D <- (x*g$y - y*g$x)
    Dif <- targ - surf
    A11 <- sum(g$x^2)
    A12 <- sum(g$x*g$y)
    A13 <- sum(D*g$x)
    A21 <- A12
    A22 <- sum(g$y^2)
    A23 <- sum(D*g$y)
    A31 <- A13
    A32 <- A23
    A33 <- sum(D^2)

    A <- matrix(c(A11, A12, A13, A21, A22, A23, A31, A32, A33), nrow = 3, byrow = TRUE)
    b <- c(sum(g$x * Dif), sum(g$y * Dif), sum(D * Dif))

    par0 <- solve(A,b)
  }
  if(is.null(lower))
    lower <- par0 - 0.5
  if(is.null(upper))
    upper <- par0 + 0.5

  if(method == 'grad'){
    if(is.null(control$method))
      method <- "L-BFGS-B"
    if(is.null(control$factr))
      control$factr <- 1e-3
    fn <- function(par, surf, targ){
      surf <- transform(surf, par[1], par[2], par[3], method = "imwarp")
      return(-NMI(surf, targ))
    }
    par <- stats::optim(par0, fn, method = method, lower = lower, upper = upper,
                 surf = surf, targ = targ, control = control)$par
    if (min(upper - par) < 1e-2)
      warning('Par is close to the upper bound, consider increasing that value')
    if (min(par - lower) < 1e-2)
      warning('Par is close to the lower bound, consider increasing that value')
  }

  else if(method == 'taylor'){
    if(is.null(control$tol))
      control$tol = 1e-3
    par <- par.acc <- par0
    iter <- 0
    while(max(abs(par)) > control$tol | iter < 20){
      surf <- transform(surf, -par[1], -par[2], -par[3], method = "taylor")
      Dif <- targ - surf
      b <- c(sum(g$x * Dif), sum(g$y * Dif), sum(D * Dif))
      par <- solve(A,b)
      par.acc <- par.acc + par
      iter <- iter + 1
    }
    par <- - par.acc
  }

  else if(method == 'gen'){
    fn <- function(par, surf, targ){
      surf <- transform(surf, par[1], par[2], par[3], method = "imwarp")
      return(-NMI(surf, targ))
    }

    if (is.null(control$pop.size))
      control$pop.size <- 50
    if (is.null(control$max.generations))
      control$max.generations <- 15
    if (is.null(control$wait.generations))
      control$wait.generations <- 3
    if (is.null(control$BFGSburnin))
      control$BFGSburnin <- -1
    if (is.null(control$solution.tolerance))
      control$solution.tolerance <- 1e-4
    if (is.null(control$BFGS))
      control$BFGS <- TRUE
    if (is.null(control$lower))
      control$lower <- par0 - 0.5
    if (is.null(control$upper))
      control$upper <- par0 + 0.5

    par <- rgenoud::genoud(fn = fn, nvars = 3,
                           Domains = cbind(control$lower,control$upper),
                           pop.size = control$pop.size,
                           max.generations = control$max.generations,
                           wait.generations = control$wait.generations,
                           starting.values = par0,
                           BFGSburnin = control$BFGSburnin,
                           gradient.check = TRUE,
                           BFGS = control$BFGS,
                           boundary.enforcement = 2,
                           print.level = 1,
                           solution.tolerance = control$solution.tolerance,
                           optim.method = "L-BFGS-B",
                           surf = surf, targ = targ)$par
    if (min(control$upper - par) < 1e-2)
      warning('Par is close to the upper bound, consider increasing that value')
    if (min(par - control$lower) < 1e-2)
      warning('Par is close to the lower bound, consider increasing that value')
  }

  else
    stop('Undefined method')

  if(trans == TRUE)
    return(transform(surf, par[1], par[2], par[3], method = "imwarp"))
  else
    return(par)
}
