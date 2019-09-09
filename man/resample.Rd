\name{resample}

\alias{resample}
\alias{resample,Raster,Raster-method}

\title{Resample a Raster object}

\description{
Resample transfers values between non matching Raster* objects (in terms of origin and resolution). Use \code{\link[raster]{projectRaster}} if the target has a different coordinate reference system (projection).

Before using resample, you may want to consider using these other functions instead: \code{\link[raster]{aggregate}}, \code{\link[raster]{disaggregate}}, \code{\link[raster]{crop}}, \code{\link[raster]{extend}}, \code{\link[raster]{merge}}.
}


\usage{
\S4method{resample}{Raster,Raster}(x, y, method="bilinear", filename="", ...)
}

\arguments{
  \item{x}{Raster* object to be resampled}
  \item{y}{Raster* object with parameters that \code{x} should be resampled to}
  \item{method}{method used to compute values for the new RasterLayer, should be \code{"bilinear"} for bilinear interpolation, or \code{"ngb"} for using the nearest neighbor}
  \item{filename}{character. Output filename (optional) }
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}  
}


\value{
RasterLayer or RasterBrick object 
}

\author{Robert J. Hijmans and Joe Cheng}


\seealso{ \code{\link[raster]{aggregate}}, \code{\link[raster]{disaggregate}}, \code{\link[raster]{crop}}, \code{\link[raster]{extend}}, \code{\link[raster]{merge}}, \code{\link[raster]{projectRaster}} }


\examples{
r <- raster(nrow=3, ncol=3)
values(r) <- 1:ncell(r)
s <- raster(nrow=10, ncol=10)
s <- resample(r, s, method='bilinear')
#par(mfrow=c(1,2))
#plot(r)
#plot(s)
}

\keyword{spatial}

