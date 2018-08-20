\name{rasterToContour}

\alias{rasterToContour}

\title{ Raster to contour lines conversion}

\description{
RasterLayer to contour lines. This is a wrapper around \code{\link[grDevices]{contourLines}}
}

\usage{
rasterToContour(x, maxpixels=100000, ...)
}

\arguments{
 \item{x}{ a RasterLayer object }
 \item{maxpixels}{ Maximum number of raster cells to use; this function fails when too many cells are used}
 \item{...}{Any argument that can be passed to \code{\link[grDevices]{contourLines}} }  
}

\details{
Most of the code was taken from maptools::ContourLines2SLDF, by Roger Bivand & Edzer Pebesma 
}

\value{
SpatialLinesDataFrame 
}




\examples{
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
x <- rasterToContour(r)
class(x)
plot(r)
plot(x, add=TRUE)
}

\keyword{ spatial }
