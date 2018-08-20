\name{extremeValues}

\alias{minValue}
\alias{maxValue}
\alias{minValue,RasterLayer-method}
\alias{minValue,RasterStack-method}
\alias{minValue,RasterBrick-method}
\alias{maxValue,RasterLayer-method}
\alias{maxValue,RasterStack-method}
\alias{maxValue,RasterBrick-method}
  
  
\title{Minimum and maximum values}
\description{
Returns the minimum or maximum value of a RasterLayer or layer in a RasterStack
}

\usage{
minValue(x, ...)
maxValue(x, ...)
}

\arguments{
  \item{x}{RasterLayer or RasterStack object}
  \item{...}{Additional argument: layer number (for RasterStack or RasterBrick objects) }
}
  
\value{
a number
}

\details{
If a Raster* object is created from a file on disk, the min and max values are often not known (depending on the file format). You can use \code{\link[raster]{setMinMax}} to set them in the Raster* object.
}


\examples{
r <- raster()
r <- setValues(r, 1:ncell(r))
minValue(r)
maxValue(r)
r <- setValues(r, round(100 * runif(ncell(r)) + 0.5))
minValue(r)
maxValue(r)

r <- raster(system.file("external/test.grd", package="raster"))
minValue(r)
maxValue(r)
}

\keyword{spatial}
