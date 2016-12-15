#' Reads standard surface '.txt' file
#'
#' This function reads a standard '.txt' file and converts it into a \code{cimg} object
#'
#' @export
#' @param file A string withe the name of the '.txt' file to be loaded.
#' @param res Resolution for the input data, if \code{NULL} the algorithm will guess its value
#' @return a \code{\link[imager]{cimg}} object.
#'
#' @examples
#' file <- system.file("extdata", "ground.txt", package = "surf")
#' surf <- read.surf(file)
#' plot(surf, asp = 1)
read.surf <- function(file, res = NULL){

  data <- as.matrix(utils::read.table(file))
  na <- suppressWarnings(storage.mode(data) <- "numeric") # ask on stack exchange how to fix this shit!
  data <- unname(data)
  data <- zoo::na.approx(data)
  data[which(is.na(data))] <- 0
  data <- imager::as.cimg(data)

  return(data)
}

#' Reads all surface '.txt' files inside a ".zip" file
#'
#' This function reads a standard '.txt' file and converts it into a list of \code{cimg} objects
#'
#' @export
#' @param file A string withe the name of the '.txt' file to be loaded.
#' @param res Resolution for the input data, if \code{NULL} the algorithm will guess its value
#' @return a \code{\link[imager]{imlist}} object.
#'
#' @examples
#' file <- system.file("extdata", "ground.zip", package = "surf")
#' surf <- read.zip(file)
#' par(mfrow = c(1,2))
#' lapply(surf, plot, asp = 1)
read.zip <- function(file, res = NULL){

  if(is.null(res)) # arrumar detecao automatica

  filenames <- utils::unzip(file, list = TRUE)$Name
  data <- lapply(filenames, function(filename) surf::read.surf(base::unz(file, filename)))
  data <- imager::imlist(data)

  return(data)
}
