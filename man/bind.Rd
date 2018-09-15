\name{bind}

\docType{methods}

\alias{bind}

\alias{bind,SpatialPolygons,SpatialPolygons-method}
\alias{bind,SpatialLines,SpatialLines-method}
\alias{bind,SpatialPoints,SpatialPoints-method}
\alias{bind,matrix,matrix-method}
\alias{bind,matrix,missing-method}
\alias{bind,data.frame,data.frame-method}
\alias{bind,data.frame,missing-method}
\alias{bind,list,missing-method}


\title{ 
Bind Spatial* objects
}

\description{
Bind (append) Spatial* objects into a single object. All objects must be of the same vector type base class (SpatialPoints, SpatialLines, or SpatialPolygons)
}

\usage{
\S4method{bind}{SpatialPolygons,SpatialPolygons}(x, y, ..., keepnames=FALSE)

\S4method{bind}{SpatialLines,SpatialLines}(x, y, ..., keepnames=FALSE)

\S4method{bind}{SpatialPoints,SpatialPoints}(x, y, ..., keepnames=FALSE)

\S4method{bind}{data.frame,data.frame}(x, y, ..., variables=NULL)

\S4method{bind}{list,missing}(x, y, ..., keepnames=FALSE)
}


\arguments{
 \item{x}{Spatial* object or data.frame, or a list of Spatial* objects}
 \item{y}{Spatial* object or data.frame, or missing}
 \item{...}{Additional Spatial* objects}
 \item{keepnames}{Logical. If \code{TRUE} the row.names are kept (if unique)} 
 \item{variables}{character. Variable (column) names to keep, If \code{NULL}, all variables are kept} 
}

\value{
Spatial* object
}


\seealso{ \code{\link[sp]{merge}} }


\examples{
if (require(rgdal) & require(rgeos)) {
	p <- shapefile(system.file("external/lux.shp", package="raster"))
	mersch <- p[p$NAME_2=='Mersch', ]
	diekirch <- p[p$NAME_2=='Diekirch', ]
	remich <- p[p$NAME_2=='Remich', ]
	remich$NAME_1 <- NULL
	x <- bind(mersch, diekirch, remich)
	plot(x)
	data.frame(x)
}
}

\keyword{methods}
\keyword{spatial}
