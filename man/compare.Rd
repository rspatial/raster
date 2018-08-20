\name{compareRaster}

\alias{compareRaster}
\alias{all.equal,Raster,Raster-method}
  
\title{Compare Raster objects}

\description{
Evaluate whether a two or more Raster* objects have the same extent, number of rows and columns, projection, resolution, and origin (or a subset of these comparisons). 

all.equal is a wrapper around compareRaster with options \code{values=TRUE}, \code{stopiffalse=FALSE} and \code{showwarning=TRUE}.
}

\usage{
compareRaster(x, ..., extent=TRUE, rowcol=TRUE, crs=TRUE, res=FALSE, orig=FALSE,
         rotation=TRUE, values=FALSE, tolerance, stopiffalse=TRUE, showwarning=FALSE)
}

\arguments{
  \item{x}{Raster* object }
  \item{...}{Raster* objects}
  \item{extent}{logical. If \code{TRUE}, bounding boxes are compared}
  \item{rowcol}{logical. If \code{TRUE}, number of rows and columns of the objects are compared}
  \item{crs}{logical. If \code{TRUE}, coordinate reference systems are compared.}
  \item{res}{logical. If \code{TRUE}, resolutions are compared (redundant when checking extent and rowcol)}
  \item{orig}{logical. If \code{TRUE}, origins are compared}
  \item{rotation}{logical. If \code{TRUE}, rotations are compared}  
  \item{values}{logical. If \code{TRUE}, cell values are compared}
  \item{tolerance}{numeric between 0 and 0.5. If not supplied, the default value is used (see \code{\link{rasterOptions}}. It sets difference (relative to the cell resolution) that is permissible for objects to be considered 'equal', if they have a non-integer origin or resolution. See \link{all.equal}. }
  \item{stopiffalse}{logical. If \code{TRUE}, an error will occur if the objects are not the same}
  \item{showwarning}{logical. If \code{TRUE}, an warning will be given if objects are not the same. Only relevant when \code{stopiffalse} is \code{TRUE}}
 }
  


\examples{
r1 <- raster()
r2 <- r1
r3 <- r1
compareRaster(r1, r2, r3)
nrow(r3) <- 10

# compareRaster(r1, r3)
compareRaster(r1, r3, stopiffalse=FALSE)
compareRaster(r1, r3, rowcol=FALSE)

all.equal(r1, r2)
all.equal(r1, r3)
}

\keyword{ spatial }

