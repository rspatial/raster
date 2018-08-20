\name{names}

\alias{labels,Raster-method}
\alias{names}
\alias{names<-}
\alias{names,Raster-method}
\alias{names,RasterStack-method}
\alias{names<-,Raster-method}

  
\title{Names of raster layers}

\description{
Get or set the names of the layers of a Raster* object 
}

\usage{
\S4method{names}{Raster}(x)

\S4method{names}{Raster}(x)<-value

\S4method{labels}{Raster}(object)
}

\arguments{
  \item{x}{Raster* object}
  \item{object}{Raster* object}  
  \item{value}{character (vector)}
}

\value{
Character 
}


\seealso{ \code{\link{nlayers}, \link[raster]{bands}} }


\examples{
r <- raster(ncols=5, nrows=5)
r[] <- 1:ncell(r)
s <- stack(r, r, r)
nlayers(s)
names(s)
names(s) <- c('a', 'b', 'c')
names(s)[2] <- 'hello world'
names(s)
s
labels(s)
}

\keyword{spatial}
