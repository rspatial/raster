\name{stack}

\docType{methods}

\alias{stack}

\alias{stack,character-method}
\alias{stack,Raster-method}
\alias{stack,list-method}
\alias{stack,missing-method}
\alias{stack,SpatialPixelsDataFrame-method}
\alias{stack,SpatialGridDataFrame-method}
\alias{stack,kasc-method}


\title{Create a RasterStack object}

\description{
A RasterStack is a collection of RasterLayer objects with the same spatial extent and resolution. A RasterStack can be created from RasterLayer objects, or from raster files, or both. It can also be created from a SpatialPixelsDataFrame or a SpatialGridDataFrame object.
}

\usage{
\S4method{stack}{character}(x, ..., bands=NULL, varname="", native=FALSE, RAT=TRUE, quick=FALSE)

\S4method{stack}{Raster}(x, ..., layers=NULL)

\S4method{stack}{missing}(x)

\S4method{stack}{list}(x, bands=NULL, native=FALSE, RAT=TRUE, ...)
}


\arguments{
\item{x}{filename (character), Raster* object, missing (to create an empty RasterStack), SpatialGrid*, SpatialPixels*, or list (of filenames and/or Raster* objects). If \code{x} is a list, additional arguments \code{...} are ignored}
\item{bands}{integer. which bands (layers) of the file should be used (default is all layers)}
\item{layers}{integer (or character with layer names) indicating which layers of a RasterBrick should be used (default is all layers)}
\item{native}{logical. If \code{TRUE} native drivers are used instead of gdal drivers (where available, such as for  BIL and Arc-ASCII files)}
\item{RAT}{logical. If \code{TRUE} a raster attribute table is created for files that have one}
\item{quick}{logical. If \code{TRUE} the extent and resolution of the objects are not compared. This speeds up the creation of the RasteStack but should be use with great caution. Only use this option when you are absolutely sure that all the data in all the files are aligned, and you need to create RasterStack for many (>100) files}
\item{varname}{character. To select the variable of interest in a NetCDF file (see \code{\link{raster}}}
\item{...}{additional filenames or Raster* objects}
}


\value{
RasterStack
}

\seealso{ \code{\link[raster]{addLayer}, \link[raster:addLayer]{dropLayer}, \link[raster]{raster}, \link[raster]{brick}} }

\examples{
# file with one layer
fn <- system.file("external/test.grd", package="raster")
s <- stack(fn, fn)
r <- raster(fn)
s <- stack(r, fn) 
nlayers(s)

# file with three layers
slogo <- stack(system.file("external/rlogo.grd", package="raster")) 
nlayers(slogo)
slogo
}

\keyword{methods}
\keyword{spatial}
