\name{Extract by index}

\docType{methods}

\alias{[[,Raster,ANY,ANY-method}

\alias{[,Raster,Spatial,missing-method}
\alias{[,Raster,RasterLayer,missing-method}
\alias{[,Raster,Extent,missing-method}
\alias{[,Raster,numeric,numeric-method}
\alias{[,Raster,numeric,missing-method}
\alias{[,Raster,missing,numeric-method}
\alias{[,Raster,matrix,missing-method}
\alias{[,Raster,missing,missing-method}
\alias{[,Raster,logical,missing-method}
\alias{[,Extent,numeric,missing-method}
\alias{[,Extent,missing,missing-method}


\title{Indexing to extract values of a Raster* object}

\description{

These are shorthand methods that call other methods that should normally be used, such as \code{\link{getValues}}, \code{\link{extract}}, \code{\link{crop}}. 

\code{object[i]} can be used to access values of a Raster* object, using cell numbers. You can also use row and column numbers as index, using \code{object[i,j]} or \code{object[i,]} or \code{object[,j]}. In addition you can supply an Extent, SpatialPolygons, SpatialLines or SpatialPoints object. 

If \code{drop=TRUE} (the default) cell values are returned (a vector for a RasterLayer, a matrix for a RasterStack or RasterBrick). If \code{drop=FALSE} a Raster* object is returned that has the extent covering the requested cells, and with all other non-requested cells within this extent set to \code{NA}.

If you supply a RasterLayer, its values will be used as logical (TRUE/FALSE) indices if both Raster objects have the same extent and resolution; otherwise the cell values within the extent of the RasterLayer are returned.

Double brackes '[[ ]]' can be used to extract one or more layers from a multi-layer object.
}

\section{Methods}{
\describe{
\code{x[i]}

\code{x[i,j]}

Arguments
\tabular{rll}{
\tab \code{x} \tab a Raster* object \cr
\tab \code{i} \tab cell number(s), row number(s), a (logical) RasterLayer, Spatial* object \cr
\tab \code{j} \tab column number(s) (only available if i is (are) a row number(s)) \cr
\tab \code{drop} \tab If \code{TRUE}, cell values are returned. Otherwise, a Raster* object is returned \cr
}

}}

\seealso{ \code{\link{getValues}, \link{setValues}, \link{extract}, \link{crop}, \link{rasterize}} }


\examples{
r <- raster(ncol=10, nrow=5)
values(r) <- 1:ncell(r) 

r[1]
r[1:10]
r[1,]
r[,1]
r[1:2, 1:2]

s <- stack(r, sqrt(r))
s[1:3]
s[[2]]

}

\keyword{methods}
\keyword{spatial}
