\name{rasterToPolygons}

\alias{rasterToPolygons}

\title{ Raster to polygons conversion}

\description{
Raster to polygons conversion. Cells with NA are not converted. A function can be used to select a subset of the raster cells (by their values).
}

\usage{
rasterToPolygons(x, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
}

\arguments{
 \item{x}{ Raster* object }
 \item{fun}{ function to select a subset of raster values (only allowed if \code{x} has a single layer)}
 \item{n}{ integer. The number of nodes for each polygon. Only 4, 8, and 16 are allowed }
 \item{na.rm}{ If \code{TRUE}, cells with \code{NA} values in all layers are ignored }
 \item{digits}{ number of digits to round the coordinates to }
 \item{dissolve}{logical. If \code{TRUE}, polygons with the same attribute value will be dissolved into multi-polygon regions. This option requires the rgeos package}
}

\details{
\code{fun} should be a simple function returning a logical value. 

E.g.: \code{fun=function(x){x==1}} or  \code{fun=function(x){x>3 & x<6}}
}

\value{
SpatialPolygonsDataFrame
}



\examples{ 
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
r[r>8] <- NA
pol <- rasterToPolygons(r, fun=function(x){x>6})

#plot(r > 6)
#plot(pol, add=TRUE, col='red')
}

\keyword{ spatial }
