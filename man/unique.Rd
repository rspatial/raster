\name{unique}

\docType{methods}

\alias{unique}
\alias{unique,RasterLayer,missing-method}
\alias{unique,RasterStackBrick,missing-method}

\title{Unique values}

\description{
This function returns the unique values in a RasterLayer object or the unique combinations of the layers in a multilayer object.
}

\usage{
\S4method{unique}{RasterLayer,missing}(x, incomparables=FALSE, na.last=NA, progress="", ...) 

\S4method{unique}{RasterStackBrick,missing}(x, incomparables=FALSE, na.last=NA, progress="", ...) 
}

\arguments{
  \item{x}{Raster object}
  \item{incomparables}{must be missing. The default value \code{FALSE} is used. See \code{\link[base]{unique}}}
  \item{na.last}{logical. for controlling the treatment of \code{NA}s. If \code{TRUE}, missing values in the data are put last; if \code{FALSE}, they are put first; if \code{NA}, they are removed.}
  \item{progress}{character. Use "text" or "window" for a progress indicator}
  \item{...}{additional arguments. as in \code{\link[base]{unique}}}  
}

\seealso{ \code{\link[base]{unique}} }

\value{
vector or matrix
}



\examples{
r <- raster(ncol=10, nrow=10)
values(r) <- round(runif(ncell(r))*10)
unique(r)
unique(stack(r, round(r/2)))
}

\keyword{spatial}
