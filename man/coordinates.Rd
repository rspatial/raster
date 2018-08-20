\name{Extreme coordinates}

\alias{xmin}
\alias{xmax}
\alias{ymin}
\alias{ymax}
\alias{xmin<-}
\alias{xmax<-}
\alias{ymin<-}
\alias{ymax<-}

\alias{xmin,BasicRaster-method}
\alias{xmax,BasicRaster-method}
\alias{ymin,BasicRaster-method}
\alias{ymax,BasicRaster-method}
\alias{xmin,Extent-method}
\alias{xmax,Extent-method}
\alias{ymin,Extent-method}
\alias{ymax,Extent-method}
\alias{xmin,Spatial-method}
\alias{xmax,Spatial-method}
\alias{ymin,Spatial-method}
\alias{ymax,Spatial-method}

  
\title{Coordinates of the Extent of a Raster object}

\description{
These functions return or set the extreme coordinates of a Raster* object; and return them for Spatial* objects.
}

\usage{
xmin(x)
xmax(x)
ymin(x)
ymax(x)

xmin(x) <- value
xmax(x) <- value
ymin(x) <- value
ymax(x) <- value
}

\arguments{
  \item{x}{A Raster* object}
  \item{value}{A new x or y coordinate}  
}


\value{
a single number
}

\seealso{ \code{\link[raster]{extent}},  \code{\link[raster]{dimensions}} }


\examples{

r <- raster(xmn=-0.5, xmx = 9.5, ncols=10)
xmin(r)
xmax(r)
ymin(r)
ymax(r)
xmin(r) <- -180
xmax(r) <- 180
}

\keyword{spatial}
