\name{as.data.frame}

\alias{as.data.frame}
\alias{as.data.frame,Raster-method}
\alias{as.data.frame,SpatialPolygons-method}
\alias{as.data.frame,SpatialLines-method}
  

\title{Get a data.frame with raster cell values, or coerce SpatialPolygons, Lines, or Points to a data.frame}

\description{
\code{as.matrix} returns all values of a Raster* object as a matrix. For RasterLayers, rows and columns in the matrix represent rows and columns in the RasterLayer object. For other Raster* objects, the matrix returned by \code{as.matrix} has columns for each layer and rows for each cell.

\code{as.array} returns an array of matrices that are like those returned by \code{as.matrix} for a RasterLayer

If there is insufficient memory to load all values, you can use \code{\link{getValues}} or \code{\link{getValuesBlock}} to read chunks of the file. You could also first use \code{\link{sampleRegular}}

The methods for Spatial* objects allow for easy creation of a data.frame with the coordinates and attributes; the default method only returns the attributes data.frame
}

\usage{
\S4method{as.data.frame}{Raster}(x, row.names=NULL, optional=FALSE, xy=FALSE, 
              na.rm=FALSE, long=FALSE, ...)

\S4method{as.data.frame}{SpatialPolygons}(x, row.names=NULL, optional=FALSE,
              xy=FALSE, centroids=TRUE, sepNA=FALSE, ...)

\S4method{as.data.frame}{SpatialLines}(x, row.names=NULL, optional=FALSE, 
              xy=FALSE, sepNA=FALSE, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{row.names}{\code{NULL} or a character vector giving the row names for the data frame. Missing values are not allowed} 
 \item{optional}{logical. If \code{TRUE}, setting row names and converting column names (to syntactic names: see make.names) is optional}
 \item{xy}{logical. If \code{TRUE}, also return the spatial coordinates}
 \item{na.rm}{logical. If \code{TRUE}, remove rows with NA values. This can be particularly useful for very large datasets with many NA values}
 \item{long}{logical. If \code{TRUE}, values are \code{\link{reshape}d} from a wide to a long format}
 \item{centroids}{logical. If \code{TRUE} return the centroids instead of all spatial coordinates (only relevant if \code{xy=TRUE})}
 \item{sepNA}{logical. If \code{TRUE} the parts of the spatial objects are separated by lines that are \code{NA} (only if \code{xy=TRUE} and, for polygons, if \code{centroids=FALSE}}
 \item{...}{Additional arguments (none)} 
}
  
\value{
data.frame
}



\examples{
r <- raster(ncol=3, nrow=3)
values(r) <- sqrt(1:ncell(r))
r[3:5] <- NA
as.data.frame(r)
s <- stack(r, r*2)
as.data.frame(s)
as.data.frame(s, na.rm=TRUE)
}

\keyword{spatial}
\keyword{methods}
