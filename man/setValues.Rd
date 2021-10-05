\name{setValues}

\alias{values<-}
\alias{values<-,RasterLayer,ANY-method}
\alias{values<-,RasterLayerSparse,ANY-method}
\alias{values<-,RasterStack,ANY-method}
\alias{values<-,RasterBrick,ANY-method}

\alias{setValues}
\alias{setValues,RasterLayer-method}
\alias{setValues,RasterLayerSparse-method}
\alias{setValues,RasterStack-method}
\alias{setValues,RasterBrick-method}

\title{Set values of a Raster object}

\description{
Assign (new) values to a Raster* object.
}

\usage{
\S4method{setValues}{RasterLayer}(x, values, ...)

\S4method{setValues}{RasterBrick}(x, values, layer=-1, ...)

\S4method{setValues}{RasterStack}(x, values, layer=-1, ...)

\S4method{setValues}{RasterLayerSparse}(x, values, index=NULL, ...)

values(x) <- value
}

\arguments{
  \item{x}{A \code{Raster*} }
  \item{values}{Cell values to associate with the Raster* object. There should be values for all cells}
  \item{value}{Cell values to associate with the Raster* object. There should be values for all cells}
  \item{layer}{Layer number (only relevant for RasterBrick and RasterStack objects). If missing, the values of all layers is set}  
  \item{index}{Cell numbers corresponding to the values}
  \item{...}{Additional arguments (none implemented)}
}


\seealso{ \code{\link[raster]{replacement}} }


\value{
a Raster* object
}


\note{
While you can access the 'values' slot of the objects directly, you would do that at your own peril because when setting values, multiple slots need to be changed; which is what setValues takes care of. 
}
 


\examples{ 
r <- raster(ncol=10, nrow=10)
vals <- 1:ncell(r)
r <- setValues(r, vals)
# equivalent to
values(r) <- vals
}


\keyword{ spatial }
\keyword{ methods }

