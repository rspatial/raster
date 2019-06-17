\name{stretch}

\alias{stretch}
\alias{stretch,Raster-method}
  
\title{Stretch}

\description{
Linear stretch of values in a Raster object
}

\usage{
\S4method{stretch}{Raster}(x, minv=0, maxv=255, minq=0, maxq=1, filename='', ...) 
}

\arguments{
\item{x}{Raster object} 
\item{minv}{numeric >= 0 and smaller than maxv. lower bound of streched value}
\item{maxv}{numeric <= 255 and larger than maxv. upper bound of streched value}
\item{minq}{numeric >= 0 and smaller than maxq. lower quitile bound of original value}
\item{maxq}{numeric <= 1 and larger than minq. upper quitile bound of original value}
\item{filename}{character. Filename for the output Raster object (optional)}
\item{...}{ additional arguments as for \code{\link{writeRaster}}}
}
 
 
\value{
Raster
}
  
\seealso{stretch argument in \code{\link{plotRGB}}}

\examples{
r <- raster(nc=10, nr=10)
r[] <- 1:100 * 10
stretch(r)
s <- stack(r, r*2)
stretch(s)

}

\keyword{spatial}
