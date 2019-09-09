\name{rasterToPoints}

\alias{rasterToPoints}

\title{ Raster to points conversion}

\description{
Raster to point conversion. Cells with NA are not converted. A function can be used to select a subset of the raster cells (by their values).
}

\usage{
rasterToPoints(x, fun=NULL, spatial=FALSE, ...)
}

\arguments{
 \item{x}{A Raster* object }
 \item{fun}{Function to select a subset of raster values}
 \item{spatial}{Logical. If \code{TRUE}, the function returns a SpatialPointsDataFrame object }
 \item{...}{Additional arguments. Currently only \code{progress} to specify a progress bar. "text", "window", or "" (the default, no progress bar)}
}

\details{
\code{fun} should be a simple function returning a logical value. 

E.g.: \code{fun=function(x){x==1}} or  \code{fun=function(x){x>3}}
}

\value{
A matrix with three columns: x, y, and v (value), or a SpatialPointsDataFrame object
}



\examples{ 
r <- raster(nrow=18, ncol=36)
values(r) <- runif(ncell(r)) * 10
r[r>8] <- NA
p <- rasterToPoints(r)
p <- rasterToPoints(r, fun=function(x){x>6})
#plot(r)
#points(p)
}

\keyword{ spatial }
