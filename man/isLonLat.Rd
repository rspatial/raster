\name{isLonLat}

\alias{isLonLat}
\alias{isLonLat,BasicRaster-method}
\alias{isLonLat,Spatial-method}
\alias{isLonLat,CRS-method}
\alias{isLonLat,character-method}
\alias{isLonLat,ANY-method}
\alias{couldBeLonLat}
\alias{couldBeLonLat,BasicRaster-method}
\alias{couldBeLonLat,Spatial-method}

  
\title{Is this longitude/latitude data?}

\description{
Test whether a Raster* or other object has a longitude/latitude coordinate reference system (CRS) by inspecting the PROJ.4 coordinate reference system description. \code{couldBeLonLat} also returns \code{TRUE} if the CRS is \code{NA} but the x coordinates are within -365 and 365 and the y coordinates are within -90.1 and 90.1.
}

\usage{
\S4method{isLonLat}{BasicRaster}(x, ...)
\S4method{isLonLat}{Spatial}(x, ...)
\S4method{couldBeLonLat}{BasicRaster}(x, warnings=TRUE, ...)
\S4method{couldBeLonLat}{Spatial}(x, warnings=TRUE, ...)
}

\arguments{
\item{x}{Raster* or Spatial* object}
\item{warnings}{logical. If \code{TRUE}, a warning is given if the CRS is \code{NA} or when the CRS is longitude/latitude but the coordinates do not match that}
\item{...}{additional arguments. None implemented}
}


\value{
Logical
}



\examples{
r <- raster()
isLonLat(r)
crs(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
isLonLat(r)
}

\keyword{spatial}
