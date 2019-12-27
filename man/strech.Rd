\name{stretch}

\alias{stretch}
\alias{stretch,Raster-method}
  
\title{Stretch}

\description{
Linear stretch of values in a Raster object. Provide the desired output range (minv and maxv) and the lower and upper bounds in the original data, either as quintiles (if \code{minq=0} and \code{maxq=1} you use the minimum and maximum cell values), or as actual values (\code{smin} and \code{smax}; e.g. precomputed quantile values). If \code{smin} and \code{smax} are both not \code{NA}, \code{minq} and \code{maxq} are ignored.
}

\usage{
\S4method{stretch}{Raster}(x, minv=0, maxv=255, minq=0, maxq=1, smin=NA, smax=NA,
          samplesize=1000000, filename='', ...)
}

\arguments{
\item{x}{Raster object} 
\item{minv}{numeric >= 0 and smaller than maxv. lower bound of stretched value}
\item{maxv}{numeric <= 255 and larger than maxv. upper bound of stretched value}
\item{minq}{numeric >= 0 and smaller than maxq. lower quintile bound of original value. Ignored if qmin is supplied}
\item{maxq}{numeric <= 1 and larger than minq. upper quintile bound of original value. Ignored if qmax is supplied}
\item{smin}{numeric < smax. user supplied lower value for the layers, to be used instead of a quintile computed by the function itself}
\item{smax}{numeric > smin. user supplied upper value for the layers, to be used instead of a quintile computed by the function itself}

\item{samplesize}{numeric > 1. If samplesize < ncell(x), a regular sample of samplesize is taken from x to compute the quantiles (to speed things up)}
\item{filename}{character. Filename for the output Raster object (optional)}
\item{...}{ additional arguments as for \code{\link{writeRaster}}}
}
 
 
\value{
Raster
}
  
\seealso{stretch argument in \code{\link{plotRGB}}}

\examples{
r <- raster(nc=10, nr=10)
values(r) <- rep(1:2, 50)
stretch(r)
s <- stack(r, r*2)
stretch(s)
}

\keyword{spatial}

