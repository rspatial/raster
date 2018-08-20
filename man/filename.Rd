\name{filename}

\alias{filename}

\title{Filename}

\description{
Get the filename of a Raster* object. You cannot set the filename of an object (except for RasterStack objects); but you can provide a 'filename= ' argument to a function that creates a new RasterLayer or RasterBrick* object.
}

\usage{
filename(x)
}

\arguments{
  \item{x}{A Raster* object }
}

\value{
a Raster* object
}



\examples{ 
r <- raster( system.file("external/test.grd", package="raster") )
filename(r)
}

\keyword{ spatial }

