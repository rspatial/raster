\name{as.list}

\alias{as.list,Raster-method}
 
\title{Create a list of RasterLayer objects}

\description{
Create a list of RasterLayer objects from Raster* objects
}

\usage{
\S4method{as.list}{Raster}(x, ...)
}

\arguments{
 \item{x}{ Raster* object }
 \item{...}{additional Raster* objects}
}  

\value{
list
}

\examples{
r <- raster(ncol=3, nrow=3)
r[] <- 1:ncell(r)
as.list(r)

s <- stack(r,r*2,r*3)
as.list(s, r)
}

\keyword{spatial}
\keyword{methods}
