% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlogram.R
\name{correlogram}
\alias{correlogram}
\title{bla bla}
\usage{
correlogram(im, samples = 5000, size = 11, pos = NULL, stencil = NULL)
}
\arguments{
\item{im}{cimg object to compute a correlogram}

\item{samples}{numeric corresponding to the number of sample pixels to be drawn}

\item{size}{numeric value for the size of the cross-shaped stencil (\code{size} x \code{size})}

\item{pos}{numeric data.frame for the pixel position for where to sample, it must be inside the image considering a border marging of \code{size/2}. Also will overide the \code{samples} parameter}

\item{stencil}{numeric data.frame that can override the default stencil}
}
\value{
a numeric data.frame for the correlogram data
}
\description{
bla bla
}
