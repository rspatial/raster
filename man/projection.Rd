\name{projection}

\alias{crs}
\alias{crs,ANY-method}
\alias{crs<-}
\alias{projection}
\alias{projection<-}
\alias{proj4string}
\alias{proj4string<-}
\alias{as.character,CRS-method}
\alias{is.na,CRS-method}
\alias{crs<-,BasicRaster-method}
\alias{crs<-,Spatial-method}


\title{ Get or set a coordinate reference system (projection) }

\description{
Get or set the coordinate reference system (CRS) of a Raster* object. 
}

\usage{
\S4method{crs}{ANY}(x, asText=FALSE, ...)

crs(x, ...) <- value

projection(x, asText=TRUE)
projection(x) <- value
}

\arguments{
  \item{x}{Raster* or Spatial object }
  \item{asText}{logical. If \code{TRUE}, the projection is returned as text. Otherwise a \code{\link[sp]{CRS-class}} object is returned}
  \item{...}{additional arguments. None implemented}
  \item{value}{\code{CRS} object or a character string describing a projection and datum in the PROJ.4 format }
}


\value{
Raster*, Spatial*, CRS, or character object
}

\note{
\code{crs} replaces earlier function \code{projection}.  
For compatibility with \code{sp} you can use \code{proj4string} instead of \code{crs}.
}

\seealso{ \code{\link[raster]{projectRaster}, \link[rgdal]{CRS-class}, \link[sp]{spTransform}, \link[rgdal]{projInfo}}}

\details{ projections are done by with the PROJ.4 library exposed by rgdal }



\examples{ 
r <- raster()
crs(r)
crs(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
crs(r)
}

\keyword{ spatial }

