#' High resolution image formation based on low resolution frames
#'
#' bla bla
#'
#' @param LRf \code{imlist} object containing the Low Resolution frames
#' @param uscale numerical indicating the upscale factor
#' @param ref_frame reference frame index.
#' @param alpha numeric indicating the alpha parameter
#' @param step numeric indicating the step size in the gradient descent procedure
#'
#' @export
#' @examples
#' \dontrun{
#' HR_set1 <- super_res(ground[(1:40)*2], uscale = 4, alpha = 5)
#' HR_set2 <- super_res(ground[(1:40)*2 - 1], uscale = 4, alpha = 5)
#' r <- register(HR_set1, HR_set2, method = 'taylor')
#' rHR_set1 <- transform(HR_set1, r[1], r[2], r[3])
#' mean(abs(HR_set1 - HR_set2))
#' mean(abs(rHR_set1 - HR_set2))
#' mean(sapply(lapply(ground[1:79] - ground[2:80], abs), mean))
#' par(mfrow = c(1,3))
#' plot(ground[[20]], axes = FALSE, interpolate = FALSE)
#' plot(rHR_set1, axes = FALSE, interpolate = FALSE)
#' plot(abs(rHR_set1 - HR_set2), axes = FALSE, interpolate = FALSE)
#' }
super_res <- function(LRf, uscale = 2, alpha = 1, ref_frame = NULL, step = NULL){

#  uscale = 2
#  alpha = 1

  Ni = 3
  No = 5

  W <- function(im, par = c(0,0,0), scale){
    im <- transform(im, par[1], par[2], par[3]) # combine in a single warp function
    im <- imager::resize(im, -100*scale, -100*scale, interpolation_type = 5)
    return(im)
  }

  p <- length(LRf)
  if (is.null(ref_frame))
    ref_frame <- round(p/2)

  z <- W(LRf[[ref_frame]], scale = uscale)
  y <- LRf

  if (is.null(step)){
    D <- matrix(c(0,-1/4,0,-1/4,1,-1/4,0,-1/4,0), nrow = 3)
    phi_max <- max(eigen(D %*% D)$values)
    step <- 1/p * ((uscale^4)/(uscale^4*phi_max + 1))
  }

  L <- function(z, s){
    sdif <- sum(sapply(1:p, function(i) sum((y[[i]] - W(z, s[i,], 1/uscale))^2)))
    penalty <- alpha * sum(imager::imlap(z)^2)
    return((sdif + penalty)/prod(dim(z)))
  }

  grad <- function(z, s){
    dif <- lapply(1:p, function(i) y[[i]] - W(z, s[i,], 1/uscale))
    HRdif <- lapply(dif, function(im) W(im, c(0,0,0), uscale))
    err <- 2 * Reduce("+", HRdif)
    penalty <- alpha*imager::imlap(z)
    return(err + penalty)
  }

  z_star <- z
  s <- t(replicate(p, c(0,0,0)))
  loss <- NULL
  for (k in 1:No){
    cat("\nDetermining", p, "registration parameters:")
    for (i in 1:p){
      s[i, ] <- register(src = W(z_star, scale = 1/uscale), tar = y[[i]], par0 = s[i,], method = 'taylor', verbosity = 0)
      cat('.', sep = '')
    }
    cat("\nMaximum A Priori (MAP) procedure (" , k, "of", No, ")\n")
    for (i in 1:Ni){
      z_star <- z_star + step * grad(z_star, s)
      loss <- c(loss, L(z_star, s))
#      par(mfrow = c(1,2))
#      plot(z_star, axes = FALSE)
#      plot(loss)
      cat("pass:", i, "of", Ni, "| Loss:", round(loss[Ni*(k - 1) + i]*1e3,3),'x 10^-3\n')
    }
#    par(mfrow = c(1,2))
#    plot(z_star, axes = FALSE)
#    plot(loss)
  }

  return(z_star)
}
