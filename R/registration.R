# devtools::use_package('rgenoud')
# devtools::use_package('GenSA')
#' Registration based on the NMI criteria
#'
#' bla bla
#'
#' @export
#' @param src,tar cimg objects
#' @param control list to control the parameters of the optimization routine
#' @param method character indicating the method to be used
#' @param ran numeric vector for the range around par0 for lower and upper (when not provided)
#' @param par0 numeric vector for the initial guess for the registration parameters
#' @param lower,upper numeric vector for the bounds of the optimization problem
#' @param verbosity Numeric indicating the level of verbosity is displayed
#' @return bla bla
#' @examples
#' # simulated shif to check if the taylor approximation is correct
#' imA <- ground[[1]]
#' imB <- transform(imA, 0.51, -0.23, 0, method = 'taylor')
#' round(register(imA, imB, method = 'taylor'),2)
#'
#' ##################
#' ## The following examples were exluded from the automatic
#' ## run to save compilation time. However, they should all be working.
#' # Now lets check the differences between the algorithms for a real pair of images
#' \dontrun{
#' imB <- ground[[10]]
#' par.taylor <- register(imA, imB, method = 'taylor')
#' par.grad   <- register(imA, imB, method = 'grad')
#' par.gen    <- register(imA, imB, method = 'gen')
#' par.sa    <- register(imA, imB, method = 'sa')
#' imA.grad <- transform(imA, par.grad[1], par.grad[2], 0)
#' imA.gen  <- transform(imA, par.gen[1], par.gen[2], 0)
#' imA.sa   <- transform(imA, par.sa[1], par.sa[2], 0)
#' NMI(imA,imB)
#' NMI(imA.grad,imB)
#' NMI(imA.gen,imB)
#' NMI(imA.sa,imB)
#' }
#' # Now, lets see if we can visualize the motion of the sample
#' \dontrun{
#' pars.taylor <- t(sapply(ground, register, tar = ground[[1]], method = 'taylor'))
#' pars.grad   <- t(sapply(ground, register, tar = ground[[1]], method = 'grad'))
#' pars.gen    <- t(sapply(ground, register, tar = ground[[1]], method = 'gen'))
#' pars.sa     <- t(sapply(ground, register, tar = ground[[1]], method = 'sa'))
#' plot(0, pch = NA, asp = 1,
#'  xlim = range(rbind(pars.taylor, pars.grad, pars.gen, pars.sa)[,1]),
#'  ylim = range(rbind(pars.taylor, pars.grad, pars.gen, pars.sa)[,2]))
#' points(pars.taylor[,1:2], col = 'orange')
#' points(pars.grad[,1:2], col = 'skyblue')
#' points(pars.gen[,1:2], col = 'black')
#' points(pars.sa[,1:2], col = 'red')
#' lines(pars.taylor[,1:2], col = 'orange')
#' lines(pars.grad[,1:2], col = 'skyblue')
#' lines(pars.gen[,1:2], col = 'black')
#' lines(pars.sa[,1:2], col = 'red')
#' plot(0, pch = NA, xlim = c(2,80), ylim = c(0.5,0.7))
#' lines(sapply(2:80, function(i) NMI(ground[[1]], transform(ground[[i]],
#'  pars.taylor[i,1], pars.taylor[i,2], pars.taylor[i,3]))), col = 'orange')
#' lines(sapply(2:80, function(i) NMI(ground[[1]], transform(ground[[i]],
#'  pars.grad[i,1], pars.grad[i,2], pars.grad[i,3]))), col = 'skyblue')
#' lines(sapply(2:80, function(i) NMI(ground[[1]], transform(ground[[i]],
#'  pars.gen[i,1], pars.gen[i,2], pars.gen[i,3]))), col = 'black')
#' lines(sapply(2:80, function(i) NMI(ground[[1]], transform(ground[[i]],
#'  pars.sa[i,1], pars.sa[i,2], pars.sa[i,3]))), col = 'red')
#' }
register <- function(src, tar, method = 'gen', par0 = NULL, lower = NULL, upper = NULL,
                     ran = c(0.25, 0.25, 0), control = NULL, verbosity = 2){

#  parC <- reg.coarse(src, tar)
#  src <- imager::imshift(src, parC[1], parC[2])

  if (is.null(par0) & method != 'taylor')
    par0 <- reg.taylor(src, tar, verbosity = verbosity)
  else
    par0 <- c(0,0,0)

  if (method == 'taylor')
    par <- reg.taylor(src, tar, par0 = par0, verbosity = verbosity)
  else
    par <- reg.imwarp(src, tar, par0, method, lower, upper, ran, control, verbosity = verbosity)

  return(par)
}

reg.taylor <- function(src, tar, niter = 15, tol = 1e-3, par0 = c(0,0,0), allow.rotation = FALSE, verbosity = 2){

  par <- par0

  if (allow.rotation){
    stop("registration with rotation not yet correclty implemented")
#    xC <- (ncol(targ)+1)/2
#    yC <- (nrow(targ)+1)/2
#    g <- imgradient(targ, "xy")
#    x <- cimg(array(  replicate(nrow(targ), 1:ncol(targ)  - xC), dim(targ)))
#    y <- cimg(array(t(replicate(ncol(targ), 1:nrow(targ)) - yC), dim(targ)))
#    D <- (x*g$y - y*g$x)
#    Dif <- targ - surf
#    A11 <- sum(g$x^2)
#    A12 <- sum(g$x*g$y)
#    A13 <- sum(D*g$x)
#    A21 <- A12
#    A22 <- sum(g$y^2)
#    A23 <- sum(D*g$y)
#    A31 <- A13
#    A32 <- A23
#    A33 <- sum(D^2)
#    A <- matrix(c(A11, A12, A13, A21, A22, A23, A31, A32, A33), nrow = 3, byrow = TRUE)
#    b <- c(sum(g$x * Dif), sum(g$y * Dif), sum(D * Dif))
  }
  else{
    if(verbosity > 1)
      cat("\nDetermining registration parameters by Taylor series:\n")
    par.acc <- par0
    if(verbosity > 1)
      cat("iteration number:", 0,"|", round(par.acc,3), '\n')
    for (i in 1:niter){
      src <- transform(src, par[1], par[2], par[3], method = 'taylor')

      dif <- tar - src
      src_g <- imager::imgradient(src, "xy")

      A11 <- sum(src_g$x^2)
      A12 <- A21 <- sum(src_g$x * src_g$y)
      A22 <- sum(src_g$y^2)

      A <- matrix(c(A11, A21, A12, A22), nrow = 2)
      b <- c(sum(src_g$x * dif), sum(src_g$y * dif))

      par <- c(solve(A, b), 0)
      par.acc <- par.acc + par

      if (i %in% round(2^seq(0, log2(niter), length.out = 6)) & verbosity > 1)
        cat("iteration number:", i,"|", round(par.acc,3), '\n')

      if (max(abs(par)) < tol)
        break
    }
  }
  if(verbosity > 0)
    cat("Final par value:", round(par.acc,3), '\n')
  return(par.acc)
}

reg.imwarp <- function(src, tar, par0 = c(0,0,0), method, lower = NULL, upper = NULL,
                       ran = c(1, 1, 0), control = NULL, allow.rotation = FALSE, verbosity = 2){

  src <- imager::isoblur(src, sigma = 0.6)
  tar <- imager::isoblur(tar, sigma = 0.6)

  if (!allow.rotation){
    par0 <- par0[-3]
    ran <- ran[-3]

    fn <- function(par, src, tar){
      src <- transform(src, par[1], par[2], 0, method = "imwarp")
      return(-NMI(src, tar))
    }
  }

  else{
    fn <- function(par, src, tar){
      src <- transform(src, par[1], par[2], par[3], method = "imwarp")
      return(-NMI(src, tar))
    }
  }

  if(is.null(lower))
    lower <- par0 - ran
  if(is.null(upper))
    upper <- par0 + ran


  if(method == 'grad'){
    if(is.null(control$factr))
      control$factr <- 1e-3
    if(verbosity > 1)
      cat("\nDetermining registration parameters by L-BFGS-B over the NMI metric: ")
    res <- stats::optim(par0, fn, method = "L-BFGS-B", lower = lower, upper = upper,
                        src = src, tar = tar, control = control)
  }

  else if (method == 'sa'){
    if (is.null(control$max.time))
      control$max.time = 10
    if (is.null(control$max.call))
      control$max.call = 1e3
    if (is.null(control$simple.function))
      control$simple.function = TRUE
    if (verbosity == 0)
      control$verbose = FALSE
    if(verbosity > 1)
      cat("\nDetermining registration parameters by SA over the NMI metric: ")
    res <- GenSA::GenSA(par0, fn, lower, upper, control = control, src = src, tar = tar)
  }

  else if (method == 'gen'){
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

    if(verbosity > 1)
      cat("\nDetermining registration parameters by GA over the NMI metric: ")
    res <- rgenoud::genoud(fn = fn, nvars = length(par0),
                           Domains = cbind(lower, upper),
                           pop.size = control$pop.size,
                           max.generations = control$max.generations,
                           wait.generations = control$wait.generations,
                           starting.values = par0,
                           BFGSburnin = control$BFGSburnin,
                           gradient.check = TRUE,
                           BFGS = control$BFGS,
                           boundary.enforcement = 2,
                           print.level = 0,
                           solution.tolerance = control$solution.tolerance,
                           optim.method = "L-BFGS-B",
                           src = src, tar = tar)
  }

  else
    stop('Undefined method')

  par <- res$par

  if (min(upper - par) < 1e-3)
    warning('Par is too close to the upper bound, consider increasing that value')
  if (min(par - lower) < 1e-3)
    warning('Par is too close to the lower bound, consider increasing that value')

  if (!allow.rotation)
    par <- c(par,0)
  if(verbosity > 0)
    cat("\nOptimal value of", -round(res$value,3), "at par", round(par,3), "\n")

  return(par)
}


reg.coarse <- function(src, tar, ran = c(2,2), verbosity = 2){

  fn <- function(par, im){
    im <- imshift(im, par[1], par[2])
    return(NMI(im, tar))
  }

  if(verbosity > 1)
    cat("\nDetermining COARSE registration parameters over the NMI metric: ")
  grid <- expand.grid(u = seq(-ran[1], ran[1], 1), v = seq(-ran[2], ran[2], 1))

  res <- apply(grid, 1, fn, im = src)

  par <- unlist(cbind(grid[which.max(res),],theta = 0))
  res <- max(res)

  if(verbosity > 0)
    cat("\nOptimal value of", round(res,3), "at par", round(par,3), "\n")

  return(res)
}
