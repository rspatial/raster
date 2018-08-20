\name{z-values}

\alias{getZ}
\alias{setZ}
  
\title{Get or set z-values}

\description{
Initial functions for a somewhat more formal approach to get or set z values (e.g. time) associated with layers of Raster* objects. In development.
}

\usage{
setZ(x, z, name='time')
getZ(x)
}

\arguments{
  \item{x}{Raster* object}
  \item{z}{vector of z values of any type (e.g. of class 'Date')}
  \item{name}{character label}
}


\value{
setZ: Raster* object

getZ: vector
}



\examples{
r <- raster(ncol=10, nrow=10)
s <- stack(lapply(1:3, function(x) setValues(r, runif(ncell(r)))))
s <- setZ(s, as.Date('2000-1-1') + 0:2)
s
getZ(s)
}

\keyword{spatial}
