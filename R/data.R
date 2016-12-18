#' Object for ground surfaces
#'
#' @format A cimg object with 256x256 pixles.
'ground1'

#' Object for ground surfaces
#'
#' This object is the same of ground1 but have being rotated by 5 dg and then displaced 5 pixels to the right and 10 to the up.
#'
#' @format A cimg object with 256x256 pixles.
'ground2'

#' Object for ground surfaces
#'
#' This object is the same of ground1 but have being displaced 5 pixels to the right and 10 to the up followed by a rotation of 5 dg.
#'
#' @format A cimg object with 256x256 pixles.
'ground3'

# ground <- read.surf('C:/Dropbox/Adriano/Doutorado/pkg/ground.txt')
# ground <- resize(ground, size_x=256+100, size_y=256+100)
# ground1 <- crop.borders(ground, nPix = 50)
# ground2 <- transform(ground, 5, 10, -10)
# ground2 <- crop.borders(ground2, nPix = (dim(ground2)[1] - 256)/2)
# ground3 <- transform(ground, 5, 10, -10, 'TR')
# ground3 <- crop.borders(ground3, nPix = (dim(ground3)[1] - 256)/2)
# devtools::use_data(ground1)
# devtools::use_data(ground2)
# devtools::use_data(ground3)
