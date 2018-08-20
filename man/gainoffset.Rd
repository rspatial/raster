\name{Gain and offset}

\alias{gain}
\alias{offs}
\alias{gain<-}
\alias{offs<-}

\title{Gain and offset of values on file}


\description{
These functions can be used to get or set the gain and offset parameters used to transform values when reading them from a file. The gain and offset parameters are applied to the raw values using the formula below: 

\code{value <- value * gain + offset}

The default value for gain is 1 and for offset is 0. 'gain' is sometimes referred to as 'scale'.

Note that setting gain and/or offset are intended to be used with values that are stored in a file. For a Raster* object with values in memory, assigning gain or offset values will lead to the inmediate computation of new values; in such cases it would be clearer to use \code{\link[raster]{Arith-methods}}.
}

\usage{
gain(x)
gain(x) <- value
offs(x)
offs(x) <- value
}

\arguments{
  \item{x}{Raster* object}
  \item{value}{Single numeric value }
}

\value{
Raster* object or numeric value(s)
}



\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
gain(r)
offs(r)
r[1505:1510]
gain(r) <- 10
offs(r) <- 5
r[1505:1510]
}

\keyword{ spatial }
\keyword{ methods }
