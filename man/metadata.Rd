\name{metadata}

\alias{metadata}
\alias{metadata,Raster-method}
\alias{metadata<-}

\title{ Metadata }

\description{
Get or set a metadata to a Raster object 
}

\usage{
\S4method{metadata}{Raster}(x, ...)
metadata(x) <- value
}

\arguments{
  \item{x}{Raster* object }
  \item{...}{additional arguments (none implemented)}
  \item{value}{list with named elements. Each element may be another list of named elements (but these nested lists are not allowed to be lists themselves)}
}

\note{
The metadata can contain single values or vectors of basic data types (character, integer, numeric) and Date. Some other types may also be supported. You cannot use a matrix or data.frame as a meta-data element.
}


\value{
Raster* object or list
}


\examples{ 
r <- raster(nc=10, nr=10)
values(r) <- 1:ncell(r)

m <- list(wave=list(a=1, b=2, c=c('cool', 'important')), that=list(red='44', blue=1:5,
       days=as.Date(c('2014-1-15','2014-2-15'))), this='888 miles from here', today=NA)

metadata(r) <- m

\dontrun{

x <- writeRaster(r, rasterTmpFile(), overwrite=TRUE)
metax <- metadata(x)

identical(metax, m)

# nested too deep
badmeta1 <- list(wave=list(a=1, b=2, c='x'), that=list(red='4', blue=list(bad=5)))
metadata(r) <- badmeta1

# missing names
badmeta2 <- list(wave=list(1, 2, c='x'), that=list(red='44', blue=14), this='8m')
metadata(r) <- badmeta2

# matrix not allowed
badmeta3 <- list(wave=list(a=1, b=matrix(1:4, ncol=2), c='x'), that=list(red='4'))
metadata(r) <- badmeta3
}
}

\keyword{ spatial }

