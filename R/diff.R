#' difference between two \code{cimg} objects
#'
#' @export
#' @param surf1 \code{cimg} object
#' @param surf2 \code{cimg} object
#'
#' @examples
#' file <- system.file("extdata", "sur1.txt", package = "surf")
#' sur1 <- read.surf(file)
#' file <- system.file("extdata", "sur2.txt", package = "surf")
#' sur2 <- read.surf(file)
#' plot(error(sur1, sur2)) #melhorar exemplo
error <- function(surf1, surf2){

  surf1[which(surf2 == 0)] <- 0
  surf2[which(surf1 == 0)] <- 0

  surf <- abs(surf1 - surf2)

  return(surf)
}
