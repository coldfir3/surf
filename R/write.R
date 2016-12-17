#' Write standard surface '.txt' file
#'
#' This function reads a standard '.txt' file and converts it into a \code{cimg}
#' object
#'
#' @export
#' @param x a \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}}
#'   object.
#' @param file A string withe the name of the file to be writen
#'
#' @examples
#' file <- system.file("extdata", "ground.txt", package = "surf")
#' surf <- read.surf(file)
#' surf <- surf + 1
#' write.cimg(surf)
write.cimg <- function(x, file = "data"){

  file <- paste0(file, '.txt')
  utils::write.table(x[,,1,1], file = file, sep = '\t', na = '***',
              dec = '.', row.names = FALSE, col.names = FALSE)
}

#' Write a list of standard surface '.txt' file
#'
#' This function reads a standard '.txt' file and converts it into a \code{cimg}
#' object
#'
#' @export
#' @param x a \code{\link[imager]{cimg}} or a \code{\link[imager]{imlist}}
#'   object.
#' @param file A string withe the name of the file to be writen
#'
#' @examples
#' file <- system.file("extdata", "ground.zip", package = "surf")
#' surf <- read.zip(file)
#' write.imlist(surf)
write.imlist <- function(x, file = "data"){
  files <- paste0(file, 1:length(x))
  for(i in 1:length(x))
    write.cimg(x[[i]], files[i])
  utils::zip(zipfile = file, files = paste0('./', files,'.txt'))
}
