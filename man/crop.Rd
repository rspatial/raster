\name{crop}

\alias{crop}
\alias{crop,Raster-method}
\alias{crop,Spatial-method}
\alias{crop,Raster,ANY-method}
\alias{crop,Spatial,ANY-method}

\title{Crop}

\description{
crop returns a geographic subset of an object as specified by an Extent object (or object from which an extent object can be extracted/created). 
If \code{x} is a Raster* object, the Extent is aligned to \code{x}. Areas included in \code{y} but outside the extent of \code{x} are ignored (see \code{\link{extend}} if you want a larger area).
}

\usage{
\S4method{crop}{Raster}(x, y, filename="", snap='near', datatype=NULL, ...)

\S4method{crop}{Spatial}(x, y, ...)
}

\arguments{
  \item{x}{Raster* object or SpatialPolygons*, SpatialLines*, or SpatialPoints* object}
  \item{y}{Extent object, or any object from which an Extent object can be extracted (see Details)}
  \item{filename}{Character, output filename. Optional}
  \item{snap}{Character. One of 'near', 'in', or 'out', for use with \code{\link{alignExtent}}}  
  \item{datatype}{Character. Output \code{\link{dataType}} (by default it is the same as the input datatype)}  
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}  
}

\note{values within the extent of a Raster* object can be set to NA with \link[raster]{mask}}

\details{
Objects from which an Extent can be extracted/created include RasterLayer, RasterStack, RasterBrick and objects of the Spatial* classes from the sp package. You can check this with the \code{\link[raster]{extent}} function. New Extent objects can also be created with function \code{\link{extent}} and \code{\link{drawExtent}} by clicking twice on a plot.

To crop by row and column numbers you can create an extent like this (for Raster \code{x}, row 5 to 10, column 7 to 12)
\code{crop(x, extent(x, 5, 10, 7, 12))}
}

\value{
RasterLayer or RasterBrick object; or SpatialLines or SpatialPolygons object.
}

\seealso{ 
\code{\link[raster]{extend}}, \code{\link[raster]{merge}}
}

\examples{
r <- raster(nrow=45, ncol=90)
values(r) <- 1:ncell(r)
e <- extent(-160, 10, 30, 60)
rc <- crop(r, e)	

# use row and column numbers:
rc2 <- crop(r, extent(r, 5, 10, 7, 15))

# crop Raster* with Spatial* object
b <- as(extent(6, 6.4, 49.75, 50), 'SpatialPolygons')
crs(b) <- crs(r)
rb <- crop(r, b)

# crop a SpatialPolygon* object with another one
p <- shapefile(system.file("external/lux.shp", package="raster"))
pb <- crop(p, b)
}

\keyword{spatial}
