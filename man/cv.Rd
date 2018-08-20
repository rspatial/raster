\name{cv}

\alias{cv}
\alias{cv,ANY-method}
\alias{cv,Raster-method}

\title{Coefficient of variation}

\description{
Compute the coefficient of variation (expressed as a percentage). If there is only a single value, \code{sd} is \code{NA} and \code{cv} returns \code{NA} if \code{aszero=FALSE} (the default). However, if (\code{aszero=TRUE}), \code{cv} returns \code{0}.
}

\usage{
\S4method{cv}{ANY}(x, ..., aszero=FALSE, na.rm = FALSE)

\S4method{cv}{Raster}(x, ..., aszero=FALSE, na.rm = FALSE)
}

\arguments{
  \item{x}{A vector of numbers (typically integers for modal), or a Raster* object}
  \item{...}{additional (vectors of) numbers, or Raster objects}
  \item{aszero}{logical. If \code{TRUE}, a zero is returned (rather than an NA) if the cv of single value is computed}
  \item{na.rm}{Remove (ignore) NA values}
}

\value{
vector or RasterLayer
}



\examples{
data <- c(0,1,2,3,3,3,3,4,4,4,5,5,6,7,7,8,9,NA)
cv(data, na.rm=TRUE)
}

\keyword{univar}
\keyword{math}
