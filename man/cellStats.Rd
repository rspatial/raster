\name{cellStats}

\alias{cellStats}
\alias{cellStats,RasterLayer-method}
\alias{cellStats,RasterStackBrick-method}

\title{Statistics across cells}


\description{
Compute statistics for the cells of each layer of a Raster* object. In the \code{raster} package, functions such as max, min, and mean, when used with Raster* objects as argument, return a new Raster* object (with a value computed for each cell). In contrast, cellStats returns a single value, computed from the all the values of a layer. Also see \code{\link{layerStats}}
}

\usage{
\S4method{cellStats}{RasterLayer}(x, stat='mean', na.rm=TRUE, asSample=TRUE, ...)

\S4method{cellStats}{RasterStackBrick}(x, stat='mean', na.rm=TRUE, asSample=TRUE, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{stat}{The function to be applied. See Details}
 \item{na.rm}{Logical. Should \code{NA} values be removed?}
 \item{asSample}{Logical. Only relevant for \code{stat=sd} in which case, if \code{TRUE}, the standard deviation for a sample (denominator is \code{n-1}) is computed, rather than for the population (denominator is \code{n})}
 \item{...}{Additional arguments }  
}

\value{
Numeric
}

\details{
\code{cellStats} will fail (gracefully) for very large Raster* objects except for a number of known functions: sum, mean, min, max, sd, 'skew' and 'rms'. 'skew' (skewness) and 'rms' (Root Mean Square) must be supplied as a character value (with quotes), the other known functions may be supplied with or without quotes. For other functions you could perhaps use a sample of the RasterLayer that can be held in memory (see \code{\link[raster]{sampleRegular}} )
}

\seealso{ 
\code{\link[raster]{freq}}, \code{\link[raster]{quantile}}, \code{\link[raster]{minValue}}, \code{\link[raster]{maxValue}}, \code{\link[raster]{setMinMax}}
}



\examples{
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
# works for large files
cellStats(r, 'mean')
# same, but does not work for very large files
cellStats(r, mean)
# multi-layer object
cellStats(brick(r,r), mean)
}

\keyword{spatial}
\keyword{univar}
