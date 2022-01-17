\name{area}

\alias{area}
\alias{area,RasterLayer-method}
\alias{area,RasterStackBrick-method}
\alias{area,SpatialPolygons-method}
  
\title{Size of cells}

\description{
Raster objects: Compute the approximate surface area of cells in an unprojected (longitude/latitude) Raster object. It is an approximation because area is computed as the height (latitudinal span) of a cell (which is constant among all cells) times the width (longitudinal span) in the (latitudinal) middle of a cell. The width is smaller at the poleward side than at the equator-ward side of a cell. This variation is greatest near the poles and the values are thus not very precise for very high latitudes. 

SpatialPolygons: Compute the area of the spatial features. Works for both planar and angular (lon/lat) coordinate reference systems
}

\usage{
\S4method{area}{RasterLayer}(x, filename="", na.rm=FALSE, weights=FALSE, ...)

\S4method{area}{RasterStackBrick}(x, filename="", na.rm=FALSE, weights=FALSE, ...)

\S4method{area}{SpatialPolygons}(x, ...)
}

\arguments{
\item{x}{Raster* or SpatialPolygons object}
\item{filename}{character. Filename for the output Raster object (optional)}
\item{na.rm}{logical. If \code{TRUE}, cells that are \code{NA} are ignored}
\item{weights}{logical. If \code{TRUE}, the area of each cells is divided by the total area of all cells that are not \code{NA}}
\item{...}{additional arguments as for \code{\link{writeRaster}}}  
}

\details{
If \code{x} is a RasterStack/Brick, a RasterBrick will be returned if \code{na.rm=TRUE}. However, if \code{na.rm=FALSE}, a RasterLayer is returned, because the values would be the same for all layers.
}

\value{
If \code{x} is a Raster* object: RasterLayer or RasterBrick. Cell values represent the size of the cell in km2, or the relative size if \code{weights=TRUE}. If the CRS is not longitude/latitude the values are in squared map units (typically meter)

If \code{x} is a SpatialPolygons* object: area of each spatial object in squared meters if the CRS is longitude/latitude, or in squared map units (typically meter)
}



\examples{
r <- raster(nrow=18, ncol=36)
a <- area(r)

if (require(rgdal) & require(rgeos)) {
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	p$area <- round(area(p) / 10000000,1)
	p$area
}
}

\keyword{methods}
\keyword{spatial}
