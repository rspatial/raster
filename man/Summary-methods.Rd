\name{Summary-methods}

\docType{methods}

\alias{Summary-methods}
\alias{mean,Raster-method}
\alias{Summary,Raster-method}

\title{ Summary methods }

\description{
The following summary methods are available for Raster* objects:
 
 \code{mean, max, min, range, prod, sum, any, all}


All methods take \code{na.rm} as an additional logical argument. Default is \code{na.rm=FALSE}. If \code{TRUE}, \code{NA} values are removed from calculations. 
These methods compute a summary statistic based on cell values of RasterLayers and the result of these methods is always a single RasterLayer (except for range, which returns a RasterBrick with two layers).  See \code{\link{calc}} for functions not included here (e.g. median) or any other custom functions.
 
You can mix RasterLayer, RasterStack and RasterBrick objects with single numeric or logical values. However, because generic functions are used, the method applied is chosen based on the first argument: '\code{x}'. This means that if \code{r} is a RasterLayer object, \code{mean(r, 5)} will work, but \code{mean(5, r)} will not work.
 
 
To summarize all cells within a single RasterLayer, see \code{\link[raster]{cellStats}} and \code{\link[raster]{maxValue}} and \code{\link[raster]{minValue}}
}

\value{a RasterLayer}

\seealso{ \code{\link{calc}} }



\examples{
r1 <- raster(nrow=10, ncol=10)
r1 <- setValues(r1, runif(ncell(r1)))
r2 <- setValues(r1, runif(ncell(r1)))
r3 <- setValues(r1, runif(ncell(r1)))
r <- max(r1, r2, r3)
r <- range(r1, r2, r3, 1.2)

s <- stack(r1, r2, r3)
r <- mean(s, 2)
}

\keyword{methods}
\keyword{spatial}

