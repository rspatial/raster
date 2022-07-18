\name{cover}

\docType{methods}

\alias{cover}
\alias{cover,RasterLayer,RasterLayer-method}
\alias{cover,RasterStackBrick,Raster-method}
\alias{cover,SpatialPolygons,SpatialPolygons-method}


\title{ Replace NA values with values of other layers }

\description{
For Raster* objects: Replace \code{NA} values in the first Raster object (\code{x}) with the values of the second (\code{y}), and so forth for additional Rasters. If \code{x} has multiple layers, the subsequent Raster objects should have the same number of layers, or have a single layer only (which will be recycled).

For SpatialPolygons* objects: Areas of \code{x} that overlap with \code{y} are replaced by (or intersected with) \code{y}.
}

\usage{
\S4method{cover}{RasterLayer,RasterLayer}(x, y, ..., filename='')

\S4method{cover}{RasterStackBrick,Raster}(x, y, ..., filename='')

\S4method{cover}{SpatialPolygons,SpatialPolygons}(x, y, ..., identity=FALSE)
}

\arguments{
  \item{x}{Raster* or SpatialPolygons* object}
  \item{y}{Same as \code{x}}
  \item{filename}{character. Output filename (optional)}
  \item{...}{Same as \code{x}. If \code{x} is a Raster* object, also additional arguments as for \code{\link{writeRaster}}} 
  \item{identity}{logical. If \code{TRUE} overlapping areas are intersected rather than replaced}
}


\value{
RasterLayer or RasterBrick object, or SpatialPolygons object
}

\examples{
# raster objects
r1 <- raster(ncols=36, nrows=18)
values(r1) <- 1:ncell(r1)
r2 <- setValues(r1, runif(ncell(r1)))
r2[r2 < 0.5] <- NA
r3 <- cover(r2, r1)


#SpatialPolygons
p <- shapefile(system.file("external/lux.shp", package="raster"))
b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
crs(b) <- crs(p)
b <- SpatialPolygonsDataFrame(b, data.frame(ID_1=9))
	
cv1 <- cover(p, b)
cv2 <- cover(p, b, identity=TRUE)

}


\keyword{methods}
\keyword{spatial}
