\name{unstack}

\alias{unstack}
\alias{unstack,RasterStack-method}
\alias{unstack,RasterBrick-method}

\title{ Unstack }

\description{
Create a list of RasterLayer objects from a RasterStack or RasterBrick
}

\usage{
unstack(x, ...)
}

\arguments{
  \item{x}{ a RasterStack object }
  \item{...}{not used. further arguments passed to or from other methods}
}

\value{
A list of RasterLayer objects
}



\seealso{ \code{\link[raster]{stack}}}

\examples{
  file <- system.file("external/test.grd", package="raster")
  s <- stack(file, file)
  list1 <- unstack(s)
  b <- brick(s)
  list2 <- unstack(b)
}

\keyword{ spatial }
\keyword{ methods }
