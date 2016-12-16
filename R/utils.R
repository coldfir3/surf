#' Custom sum of two \code{imlist} objects
#'
#' @param surf1 \code{imlist} object
#' @param surf2 \code{imlist} object
#' @export
`+.imlist` <- function(surf1, surf2){

  surf <- NULL

  if(is.list(surf1) & is.list(surf2))
    if(length(surf1) != length(surf2))
      stop("surf1 must be of same length of surf2")
    else
      for (i in 1:length(surf1))
        surf[[i]] <- surf1[[i]] + surf2[[i]]
  else if(is.numeric(surf2) & is.list(surf1))
    for (i in 1:length(surf1))
      surf[[i]] <- surf1[[i]] + surf2
  else if(is.numeric(surf1) & is.list(surf2))
    for (i in 1:length(surf2))
      surf[[i]] <- surf1 + surf2[[i]]

  surf <- imager::imlist(surf)
  return(surf)
}

#' Custom subtraction of two \code{imlist} objects
#'
#' @param surf1 \code{imlist} object
#' @param surf2 \code{imlist} object
#' @export
`-.imlist` <- function(surf1, surf2){

  surf <- NULL

  if(is.list(surf1) & is.list(surf2))
    if(length(surf1) != length(surf2))
      stop("surf1 must be of same length of surf2")
    else
      for (i in 1:length(surf1))
        surf[[i]] <- surf1[[i]] - surf2[[i]]
  else if(is.numeric(surf2) & is.list(surf1))
    for (i in 1:length(surf1))
      surf[[i]] <- surf1[[i]] - surf2
  else if(is.numeric(surf1) & is.list(surf2))
    for (i in 1:length(surf2))
      surf[[i]] <- surf1 - surf2[[i]]

  surf <- imager::imlist(surf)
  return(surf)
}

#' Custom multiplication of two \code{imlist} objects
#'
#' @param surf1 \code{imlist} object
#' @param surf2 \code{imlist} object
#' @export
`*.imlist` <- function(surf1, surf2){

  surf <- NULL

  if(is.list(surf1) & is.list(surf2))
    if(length(surf1) != length(surf2))
      stop("surf1 must be of same length of surf2")
    else
      for (i in 1:length(surf1))
        surf[[i]] <- surf1[[i]] * surf2[[i]]
  else if(is.numeric(surf2) & is.list(surf1))
    for (i in 1:length(surf1))
      surf[[i]] <- surf1[[i]] * surf2
  else if(is.numeric(surf1) & is.list(surf2))
    for (i in 1:length(surf2))
      surf[[i]] <- surf1 * surf2[[i]]

  surf <- imager::imlist(surf)
  return(surf)
}

#' Custom division of two \code{imlist} objects
#'
#' @param surf1 \code{imlist} object
#' @param surf2 \code{imlist} object
#' @export
`/.imlist` <- function(surf1, surf2){

  surf <- NULL

  if(is.list(surf1) & is.list(surf2))
    if(length(surf1) != length(surf2))
      stop("surf1 must be of same length of surf2")
    else
      for (i in 1:length(surf1))
        surf[[i]] <- surf1[[i]] / surf2[[i]]
  else if(is.numeric(surf2) & is.list(surf1))
    for (i in 1:length(surf1))
      surf[[i]] <- surf1[[i]] / surf2
  else if(is.numeric(surf1) & is.list(surf2))
    for (i in 1:length(surf2))
      surf[[i]] <- surf1 / surf2[[i]]

  surf <- imager::imlist(surf)
  return(surf)
}

#' Custom power of two \code{imlist} objects
#'
#' @param surf1 \code{imlist} object
#' @param surf2 \code{imlist} object
#' @export
`^.imlist` <- function(surf1, surf2){

  surf <- NULL

  if(is.list(surf1) & is.list(surf2))
    if(length(surf1) != length(surf2))
      stop("surf1 must be of same length of surf2")
    else
      for (i in 1:length(surf1))
        surf[[i]] <- surf1[[i]] ^ surf2[[i]]
  else if(is.numeric(surf2) & is.list(surf1))
    for (i in 1:length(surf1))
      surf[[i]] <- surf1[[i]] ^ surf2
  else if(is.numeric(surf1) & is.list(surf2))
    for (i in 1:length(surf2))
      surf[[i]] <- surf1 ^ surf2[[i]]

  surf <- imager::imlist(surf)
  return(surf)
}
