#' Reads a surface '.txt' file
#'
#' This function reads a standard '.txt' file and converts it into a \code{cimg}
#' object
#'
#' @export
#' @param file A string withe the name of the '.txt' file to be loaded.
#' @param cutoff cutoff
#' @return a \code{\link[imager]{cimg}} object.
#'
#' @examples
#' file <- system.file("extdata", "ground.txt", package = "surf")
#' surf <- read.surf(file)
#' plot(surf, asp = 1)
read.surf <- function(file, cutoff = c(0.001,0.999)){

  data <- as.matrix(utils::read.table(file))
  na <- suppressWarnings(storage.mode(data) <- "numeric")
  data <- unname(data)
  data[data < stats::quantile(data, cutoff[1], na.rm = TRUE)] <- NA
  data[data > stats::quantile(data, cutoff[2], na.rm = TRUE)] <- NA
  im <- imager::as.cimg(data)
  im <- interp(im)

  return(im)
}


#' Reads all surface '.txt' files inside a ".zip" file
#'
#' This function reads a standard '.txt' file and converts it into a list of
#' \code{cimg} objects
#'
#' @export
#' @param file A string withe the name of the '.txt' file to be loaded.
#' @param res Resolution for the input data, if \code{NULL}, and the file is not
#'   a matrix, the algorithm will set is as the square root of the number of
#'   points (it will consider the data as square).
#' @param file.n the number(s) of file(s) to be read, default is NULL which read all files
#' @return a \code{\link[imager]{imlist}} object.
#'
#' @examples
#' file <- system.file("extdata", "ground.zip", package = "surf")
#' surf <- read.zip(file)
#' par(mfrow = c(1,2))
#' lapply(surf, plot, asp = 1)
read.zip <- function(file, res = NULL, file.n = NULL){

  if (is.null(file.n))
    filenames <- utils::unzip(file, list = TRUE)$Name
  else
    filenames <- utils::unzip(file, list = TRUE)$Name[file.n]
  data <- list()
  for (filename in filenames){
    sys.t <- system.time(data <- append(data, list(surf::read.surf(base::unz(file, filename)))))
    print(paste(filename, "was read in", round(sys.t[3],2), "seconds."))
    }
  data <- imager::imlist(data)

  return(data)
}
