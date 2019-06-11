\name{datasource}

\alias{fromDisk}
\alias{inMemory}
\alias{hasValues}
\alias{hasValues,BasicRaster-method}


\title{Are values in memory and/or on disk?}

\description{ 
These are helper functons for programmers and for debugging that provide information about whether a Raster object has associated values, and if these are in memory or on disk. 

\code{fromDisk} is \code{TRUE} if the data source is a file on disk; and \code{FALSE} if the object only exists in memory. 

\code{inMemory} is \code{TRUE} if all values are currently in memory (RAM); and \code{FALSE} if not (in which case they either are on disk, or there are no values).

\code{hasValues} is \code{TRUE} if the object has cell values.
}

\usage{
fromDisk(x)
inMemory(x)
\S4method{hasValues}{BasicRaster}(x, ...)
}

\arguments{
  \item{x}{ Raster* object }
  \item{\dots}{ additional arguments. None implemented }
}

\value{
Logical
}

\examples{
rs <- raster(system.file("external/test.grd", package="raster"))
inMemory(rs)
fromDisk(rs)
rs <- readAll(rs)
inMemory(rs)
fromDisk(rs)
rs <- rs + 1
inMemory(rs)
fromDisk(rs)
rs <- raster(rs)
inMemory(rs)
fromDisk(rs)
rs <- setValues(rs, 1:ncell(rs))
inMemory(rs)
fromDisk(rs)
rs <- writeRaster(rs, filename='test', overwrite=TRUE)
inMemory(rs)
fromDisk(rs)
}

\keyword{ spatial }
