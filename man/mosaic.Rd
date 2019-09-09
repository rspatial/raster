\name{mosaic}

\docType{methods}

\alias{mosaic}
\alias{mosaic,Raster,Raster-method}

\title{ 
Merge Raster* objects using a function for overlapping areas 
}

\description{
Mosaic Raster* objects to form a new object with a larger spatial extent. A function is used to compute cell values in areas where layers overlap (in contrast to the \code{\link[raster]{merge}} function  which uses the values of the 'upper' layer). All objects must have the same origin, resolution, and coordinate reference system.
}

\usage{
\S4method{mosaic}{Raster,Raster}(x, y, ..., fun, tolerance=0.05, filename="")
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{Raster* object}
  \item{...}{Additional Raster or Extent objects (and/or arguments for writing files as in \code{\link{writeRaster})}}
  \item{fun}{Function. E.g. mean, min, or max. Must be a function that accepts a 'na.rm' argument}
  \item{tolerance}{Numeric. permissible difference in origin (relative to the cell resolution). See \code{\link[base]{all.equal}}}
  \item{filename}{Character. Output filename (optional)}
}


\details{
The Raster objects must have the same origin and resolution. 
}

\value{
RasterLayer or RasterBrick object.
}


\seealso{ \code{\link[raster]{merge}}, \code{\link[raster]{extend}}}

\examples{
r <- raster(ncol=100, nrow=100)
r1 <- crop(r, extent(-10, 11, -10, 11))
r2 <- crop(r, extent(0, 20, 0, 20))
r3 <- crop(r, extent(9, 30, 9, 30))

values(r1) <- 1:ncell(r1)
values(r2) <- 1:ncell(r2)
values(r3) <- 1:ncell(r3)

m1 <- mosaic(r1, r2, r3, fun=mean)

s1 <- stack(r1, r1*2)
s2 <- stack(r2, r2/2)
s3 <- stack(r3, r3*4)
m2 <- mosaic(s1, s2, s3, fun=min)

# if you have a list of Raster objects, you can use do.call
x <- list(r1, r2, r3)
names(x)[1:2] <- c('x', 'y')
x$fun <- mean
x$na.rm <- TRUE

y <- do.call(mosaic, x)

}

\keyword{methods}
\keyword{spatial}

