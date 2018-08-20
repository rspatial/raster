\name{modal}

\alias{modal}
\alias{modal,ANY-method}
\alias{modal,Raster-method}

\title{modal value}

\description{
Compute the mode for a vector of numbers, or across raster layers. The mode, or modal value, is the most frequent value in a set of values. 
}

\usage{
\S4method{modal}{ANY}(x, ..., ties='random', na.rm=FALSE, freq=FALSE)

\S4method{modal}{Raster}(x, ..., ties='random', na.rm=FALSE, freq=FALSE)
}

\arguments{
  \item{x}{vector of numbers (typically integers), characters, logicals, or factors, or a Raster* object}
  \item{...}{additional argument of the same type as \code{x}}
  \item{ties}{character. Indicates how to treat ties. Either 'random', 'lowest', 'highest', 'first', or 'NA'}
  \item{na.rm}{logical. If \code{TRUE}, \code{NA} values are ignored. If \code{FALSE}, \code{NA} is returned if \code{x} has any \code{NA} values}
  \item{freq}{return the frequency of the modal value, instead of the modal value}
}

\value{
vector or RasterLayer. The vector has length 1 and is of the same type as \code{x}, except when \code{x} is a factor and additional arguments (values) are supplied, in which case the values are coerced to characters and a character value is returned.
}

\examples{
data <- c(0,1,2,3,3,3,3,4,4,4,5,5,6,7,7,8,9,NA)
modal(data, na.rm=TRUE)
}

\keyword{univar}
\keyword{math}
