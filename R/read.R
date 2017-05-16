#' Reads a surface '.txt' file
#'
#' This function reads a standard '.txt' file and converts it into a \code{cimg}
#' object
#'
#' @export
#' @param file A string withe the name of the '.txt' file to be loaded.
#' @param cutoff cutoff
#' @param na.rm interpolation algorithm
#' @return a \code{\link[imager]{cimg}} object.
#'
#' @examples
#' file <- system.file("extdata", "ground.txt", package = "surf")
#' surf <- read.surf(file)
#' plot(surf)
read.surf <- function(file, cutoff = c(0.001,0.999), na.rm = TRUE){

  data <- as.matrix(utils::read.table(file))
  na <- suppressWarnings(storage.mode(data) <- "numeric")
  data <- unname(data)
  data <- rm.outliers(data, cutoff[1], cutoff[2])
  im <- imager::as.cimg(data)
  if(na.rm)
    im <- try(interp(im))

  return(im)
}


#' Reads all surface '.txt' files inside a ".zip" file
#'
#' This function reads a standard '.txt' file and converts it into a list of
#' \code{cimg} objects
#'
#' @export
#' @inheritParams read.surf
#' @param file.n the number(s) of file(s) to be read, default is NULL which read all files
#' @return a \code{\link[imager]{imlist}} object.
#'
#' @examples
#' file <- system.file("extdata", "ground.zip", package = "surf")
#' surf <- read.zip(file)
#' par(mfrow = c(1,2))
#' plot(surf, layout = "row")
read.zip <- function(file, file.n = NULL, cutoff = c(0.001,0.999), na.rm = TRUE){

  if (is.null(file.n))
    filenames <- utils::unzip(file, list = TRUE)$Name
  else
    filenames <- utils::unzip(file, list = TRUE)$Name[file.n]
  data <- list()
  for (filename in filenames){
    sys.t <- system.time(data <- append(data, list(
      surf::read.surf(base::unz(file, filename), cutoff, na.rm))))
    print(paste(filename, "was read in", round(sys.t[3],2), "seconds."))
    }
  data <- imager::as.imlist(data[!is.na(data)])

  return(data)
}
