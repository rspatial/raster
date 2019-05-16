\name{persp}

\docType{methods}

\alias{persp}
\alias{persp,RasterLayer-method}
\alias{persp,RasterStackBrick-method}

\title{Perspective plot}

\description{
Perspective plot of a RasterLayer. This is an implementation of a generic function in the graphics package.
}


\usage{
\S4method{persp}{RasterLayer}(x,  maxpixels=1e+05, ext=NULL, ...)

\S4method{persp}{RasterStackBrick}(x, y=1, maxpixels=10000, ext=NULL, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{integer \code{> 0 & <= nlayers(x)} to select the layer of \code{x} if \code{x} is a RasterLayer or RasterBrick}
  \item{maxpixels}{integer > 0. Maximum number of cells to use for the plot. If \code{maxpixels < ncell(x)}, \code{sampleRegular} is used before plotting}
  \item{ext}{Extent. Van be used to zoom in a region (see also \code{\link{zoom}} and \code{\link{crop}(x, \link{drawExtent}())}}
  \item{...}{Any argument that can be passed to \code{\link[graphics]{persp}} (graphics package)}  
}


\seealso{ \code{\link[rasterVis:plot3d]{plot3D}}, \code{\link[graphics]{persp}}, \code{\link[raster]{contour}}, \code{\link[raster]{plot}} }



\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
persp(r)
}

\keyword{methods}
\keyword{spatial}

