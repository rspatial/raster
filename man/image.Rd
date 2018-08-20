\name{image}

\docType{methods}

\alias{image}
\alias{image,RasterLayer-method}
\alias{image,RasterStackBrick-method}

\title{Image}

\description{
Create an "image" type plot of a RasterLayer. This is an implementation of a generic function in the graphics package. In most cases the 
 \code{\link[raster]{plot}} function would be preferable because it produces a legend (and has some additional options).
}

\usage{
image(x, ...) 
\S4method{image}{RasterLayer}(x, maxpixels=500000, useRaster=TRUE, ...)

\S4method{image}{RasterStackBrick}(x, y=1, maxpixels=100000, useRaster=TRUE, main, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{maxpixels}{integer > 0. Maximum number of cells to use for the plot. If \code{maxpixels < ncell(x)}, \code{sampleRegular} is used before plotting}
  \item{useRaster}{If \code{TRUE}, the rasterImage function is used for plotting. Otherwise the image function is used. This can be useful if rasterImage does not work well on your system (see note)}
  \item{main}{character. Main plot title}
  \item{...}{Any argument that can be passed to \code{\link[graphics]{image}} (graphics package)}  
  \item{y}{If \code{x} is a RasterStack or RasterBrick: integer, character (layer name(s)), or missing to select which layer(s) to plot}
}


\seealso{\code{\link[raster]{plot}}, \code{\link[graphics]{image}}, \code{\link[raster]{contour}}}

\note{
raster uses \code{\link[graphics]{rasterImage}} from the graphics package. For unknown reasons this does not work on Windows Server and on a few versions of Windows XP. On that system you may need to use argument \code{useRaster=FALSE} to get a plot.
}

\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
image(r)
}

\keyword{methods}
\keyword{spatial}

