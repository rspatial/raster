\name{mask}

\docType{methods}

\alias{mask}
\alias{mask,RasterLayer,RasterLayer-method}
\alias{mask,RasterStackBrick,RasterLayer-method}
\alias{mask,RasterLayer,RasterStackBrick-method}
\alias{mask,RasterStackBrick,RasterStackBrick-method}
\alias{mask,Raster,Spatial-method}
\alias{mask,Raster,sf-method}

\title{Mask values in a Raster object}

\description{
Create a new Raster* object that has the same values as \code{x}, except for the cells that are \code{NA} (or other \code{maskvalue}) in a 'mask'. These cells become \code{NA} (or other \code{updatevalue}). The mask can be either another Raster* object of the same extent and resolution, or a Spatial* object (e.g. SpatialPolygons) in which case all cells that are not covered by the Spatial object are set to \code{updatevalue}. You can use \code{inverse=TRUE} to set the cells that are not \code{NA} (or other \code{maskvalue}) in the mask, or not covered by the Spatial* object, to \code{NA} (or other \code{updatvalue}).
} 

\usage{
\S4method{mask}{RasterLayer,RasterLayer}(x, mask, filename="", inverse=FALSE, 
      maskvalue=NA, updatevalue=NA, updateNA=FALSE, ...)

\S4method{mask}{RasterStackBrick,RasterLayer}(x, mask, filename="", inverse=FALSE,
      maskvalue=NA, updatevalue=NA, updateNA=FALSE, ...)

\S4method{mask}{RasterLayer,RasterStackBrick}(x, mask, filename="", inverse=FALSE, 
      maskvalue=NA, updatevalue=NA, updateNA=FALSE, ...)

\S4method{mask}{RasterStackBrick,RasterStackBrick}(x, mask, filename="", inverse=FALSE, 
      maskvalue=NA, updatevalue=NA, updateNA=FALSE, ...)

\S4method{mask}{Raster,Spatial}(x, mask, filename="", inverse=FALSE, 
      updatevalue=NA, updateNA=FALSE, ...)
}


\arguments{
\item{x}{Raster* object}
\item{mask}{Raster* object or a Spatial* object}
\item{filename}{character. Optional output filename}
\item{inverse}{logical. If \code{TRUE}, areas on mask that are _not_ the \code{maskvalue} are masked}
\item{maskvalue}{numeric. The value in \code{mask} that indicates the cells of \code{x} that should become \code{updatevalue} (default = \code{NA})}
\item{updatevalue}{numeric. The value that cells of \code{x} should become if they are not covered by \code{mask} (and not \code{NA})}
\item{updateNA}{logical. If \code{TRUE}, \code{NA} values outside the masked area are also updated to the the \code{updatevalue} (only relevant if the \code{updatevalue} is not \code{NA}}
\item{...}{additional arguments as in \code{\link{writeRaster}}}
}

\value{Raster* object}

\seealso{\code{\link{rasterize}, \link{crop}}}


\examples{
r <- raster(ncol=10, nrow=10)
m <- raster(ncol=10, nrow=10)
r[] <- runif(ncell(r)) * 10
m[] <- runif(ncell(r))
m[m < 0.5] <- NA
mr <- mask(r, m)

m2 <- m > .7
mr2 <- mask(r, m2, maskvalue=TRUE)
}

\keyword{methods}
\keyword{spatial}

