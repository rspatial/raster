\name{ifel}

\alias{ifel}
\alias{ifel,Raster-method}

\title{ifelse for Raster* objects}

\description{
This functions like \code{\link[base]{ifelse}} but this one works for Raster* objects. This is "syntactic sugar", that is, it can be easier to express what is desired; but there is no real need for it, as you can achieve the same things with combinations of \code{\link{calc}}, \code{\link{reclassify}}, \code{\link{mask}}, and \code{\link{cover}}. 

In ArcMap (arcpy), \code{ifel} is called \code{Con}. 
}

\usage{
\S4method{ifel}{Raster}(test, yes, no, filename, ...)
}

\arguments{
  \item{test}{Raster* object}
  \item{yes}{Raster* object or numeric}
  \item{no}{Raster* object or numeric}
  \item{filename}{character. Output filename (optional)}
  \item{...}{if \code{x} is a Raster* object, additional arguments as for \code{\link{writeRaster}}}
}

\details{
This is a new method that has not been tested much yet. User beware.
}

\value{
Raster* object
}

\examples{
r <- raster(nrows=5, ncols=5, vals=-10:14)

x <- ifel(r > 1, 1, r)
# same as 
a <- reclassify(r, cbind(1, Inf, 1))
b <- calc(r, function(i) {i[i > 1] <- 1; i})
d <- clamp(r, -Inf, 1)

y <- ifel(r > 1, 1, ifel(r < -1, -1, r))

z <- ifel(r > -2 & r < 2, 100, 0)

k <- ifel(r > 0, r+10, ifel(r < 0, r-10, 3))
plot(k)
text(k)
}

\keyword{spatial}
