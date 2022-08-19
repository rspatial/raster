\name{projection}

\alias{wkt}
\alias{wkt,ANY-method}
\alias{wkt,Raster-method}
\alias{crs}
\alias{crs,ANY-method}
\alias{crs<-}
\alias{projection}
\alias{projection<-}
\alias{proj4string}
\alias{proj4string,BasicRaster-method}
\alias{proj4string,CRS-method}
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
\S4method{wkt}{Raster}(obj)
crs(x, ...) <- value

projection(x, asText=TRUE)
projection(x) <- value
}

\arguments{
  \item{x}{Raster* or Spatial object }
  \item{obj}{Raster*, Spatial, or CRS object }
  \item{asText}{logical. If \code{TRUE}, the projection is returned as text. Otherwise a \code{\link[sp]{CRS-class}} object is returned}
   \item{...}{additional arguments. None implemented}
  \item{value}{\code{CRS} object or a character string describing a projection and datum in the PROJ.4 format }
}


\value{
Raster*, Spatial*, or character object
}

\note{
\code{crs} replaces earlier function \code{projection}.  
For compatibility with \code{sp} you can use \code{proj4string} instead of \code{crs}.
\code{wkt} returns the "well-known-text" representation of the crs.  
}

\seealso{ \code{\link[raster]{projectRaster}, \link[sp]{spTransform}}}

\details{ projections are done by with the PROJ library }



\examples{ 
r <- raster()
crs(r)
crs(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
crs(r)
w <- wkt(r)
w
cat(w, "\n")
}

\keyword{ spatial }

