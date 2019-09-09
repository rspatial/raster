\name{merge}

\docType{methods}

\alias{merge}
\alias{merge,Raster,Raster-method}
\alias{merge,RasterStackBrick,missing-method}
\alias{merge,Extent,ANY-method}

\title{ 
Merge Raster* objects
}

\description{
Merge Raster* objects to form a new Raster object with a larger spatial extent. If objects overlap, the values get priority in the same order as the arguments, but \code{NA} values are ignored (except when \code{overlap=FALSE}). See \code{\link[raster]{subs}} to merge a \code{Raster*} object and a \code{data.frame}.
}

\usage{
\S4method{merge}{Raster,Raster}(x, y, ..., tolerance=0.05, filename="", overlap=TRUE, ext=NULL)

\S4method{merge}{RasterStackBrick,missing}(x, ..., tolerance=0.05, filename="", ext=NULL)

\S4method{merge}{Extent,ANY}(x, y, ...)
}

\arguments{
  \item{x}{Raster* or Extent object}
  \item{y}{Raster* if \code{x} is a Raster* object (or missing). If \code{x} is an Extent, \code{y} can be an Extent or object from which an Extent can be extracted}
  \item{...}{additional Raster or Extent objects (and/or arguments for writing files as in \code{\link{writeRaster})}}
  \item{tolerance}{numeric. permissible difference in origin (relative to the cell resolution). See \code{\link[base]{all.equal}}}
  \item{filename}{character. Output filename (optional)}
  \item{overlap}{logical. If \code{FALSE} values of overlapping objects are based on the first layer, even if they are \code{NA}}
  \item{ext}{Extent object (optional) to limit the output to that extent}
} 

\details{
The Raster objects must have the same origin and resolution. In areas where the Raster objects overlap, the values of the Raster object that is first in the sequence of arguments will be retained. If you would rather use the average of cell values, or do another computation, you can use \code{\link[raster]{mosaic}} instead of merge.
}

\value{
RasterLayer or RasterBrick
}




\examples{
r1 <- raster(xmx=-150, ymn=60, ncols=30, nrows=30)
values(r1) <- 1:ncell(r1)
r2 <- raster(xmn=-100, xmx=-50, ymx=50, ymn=30)
res(r2) <- c(xres(r1), yres(r1))
values(r2) <- 1:ncell(r2)
rm <- merge(r1, r2)

# if you have many RasterLayer objects in a list
# you can use do.call:
x <- list(r1, r2)
# add arguments such as filename
# x$filename <- 'test.tif'
m <- do.call(merge, x)
}

\keyword{methods}
\keyword{spatial}

