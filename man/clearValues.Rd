
\name{clearValues}

\alias{clearValues}

\title{Clear values}

\description{
Clear cell values of a Raster* object from memory
}

\usage{
clearValues(x)
}

\arguments{
  \item{x}{Raster* object }
}


\seealso{ \code{\link[raster]{values}}, \code{\link[raster]{replacement} }}


\value{
a Raster* object
}



\examples{ 
r <- raster(ncol=10, nrow=10)
r[] <- 1:ncell(r)
r <- clearValues(r)
}


\keyword{ spatial }
\keyword{ methods }

