\name{getValues}

\alias{values}
\alias{values,Raster-method}
\alias{getValues}

\alias{getValues,RasterLayer,missing,missing-method}
\alias{getValues,RasterLayerSparse,missing,missing-method}
\alias{getValues,RasterStack,missing,missing-method}
\alias{getValues,RasterBrick,missing,missing-method}
\alias{getValues,RasterLayer,numeric,missing-method}
\alias{getValues,RasterLayerSparse,numeric,missing-method}
\alias{getValues,RasterStack,numeric,missing-method}
\alias{getValues,RasterBrick,numeric,missing-method}
\alias{getValues,RasterLayer,numeric,numeric-method}
\alias{getValues,RasterLayerSparse,numeric,numeric-method}
\alias{getValues,RasterStack,numeric,numeric-method}
\alias{getValues,RasterBrick,numeric,numeric-method}
  
\title{Get raster cell values}

\description{
getValues returns all values or the values for a number of rows of a Raster* object. Values returned for a RasterLayer are a vector. The values returned for a RasterStack or RasterBrick are always a matrix, with the rows representing cells, and the columns representing layers

\code{values} is a shorthand version of getValues (for all rows).
}

\usage{
getValues(x, row, nrows, ...)

values(x, ...)
}

\arguments{
 \item{x}{Raster* object}
 \item{row}{Numeric. Row number, should be between 1 and nrow(x), or missing in which case all values are returned}
 \item{nrows}{Numeric. Number of rows. Should be an integer > 0, or missing}
 \item{...}{Additional arguments. When x is a \code{RasterLayer}: \code{format} to specify the output format. Either "matrix" or, the default "", in which case a vector is returned}
}

\value{
vector or matrix of raster values
}

\seealso{\code{\link{getValuesBlock}, \link{getValuesFocal}, \link{setValues}} }


\examples{
r <- raster(system.file("external/test.grd", package="raster"))
r
v <- getValues(r)
length(v)
head(v)
getValues(r, row=10)
}

\keyword{spatial}
\keyword{methods}
