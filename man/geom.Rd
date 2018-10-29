\name{geom}

\docType{methods}

\alias{geom}
\alias{geom,SpatialPolygons-method}
\alias{geom,SpatialLines-method}
\alias{geom,SpatialPoints-method}


\title{Get the coordinates of a vector type Spatial* object}

\description{
Extract the coordinates of a Spatial object
}

\usage{
\S4method{geom}{SpatialPolygons}(x, sepNA=FALSE, ...)
}

\arguments{
  \item{x}{SpatialPolygons*, SpatialLines*, or SpatialPoints* object}
  \item{sepNA}{logical. If \code{TRUE}, geometries are separated by a row with \code{NA} values}
  \item{...}{additional arguments, none implemented}
}


\value{
Matrix with 6, (5 SpatialLines), or 3 (SpatialPoints) columns. object (sequential object number) part (sequential part number within the object; not for SpatialPoints), cump (cumulative part number; not for SpatialPoints), hole (is this a hole or not; only for SpatialPolygons), x (x coordinate or longitude), y (y coordinate or latitude)
}

\seealso{ \code{\link[sp]{coordinates}}, \code{\link[sp]{geometry}} }


\examples{
if (require(rgdal) & require(rgeos)) {
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	x <- geom(p)
	head(x)
	
# and back to a SpatialPolygonsDataFrame	
	x <- data.frame(x)
	sp <- as(x, "SpatialPolygons")
	crs(sp) <- crs(p)
	spdf <- SpatialPolygonsDataFrame(sp, data.frame(p), match.ID=FALSE)
}
}

\keyword{methods}
\keyword{spatial}

