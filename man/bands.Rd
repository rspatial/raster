\name{bands}

\alias{bandnr}
\alias{bandnr,RasterLayer-method}
\alias{nbands}
  
\title{Number of bands}

\description{
A 'band' refers to a single layer for a possibly multi-layer file. Most RasterLayer objects will refer to files with a single layer. 
The term 'band' is frequently used in remote sensing to refer to a variable (layer) in a multi-variable dataset as these variables typically reperesent reflection in different bandwidths in the electromagnetic spectrum. But in that context, bands could be stored in a single or in separate files. In the context of the raster package, the term band is equivalent to a layer in a raster file.

\code{nbands} returns the number of bands of the file that a RasterLayer points to (and 1 if it does not point at any file). This functions also works for a RasterStack for which it is equivalent to \code{\link{nlayers}}.

\code{band} returns the specific band the RasterLayer refers to (1 if the RasterLayer points at single layer file or does not point at any file). 
}

\usage{
nbands(x)
bandnr(x, ...)
}

\arguments{
  \item{x}{RasterLayer}
  \item{...}{Additional arguments (none at this time)}
}

\seealso{\code{\link[raster]{nlayers}}}

\value{
numeric >= 1
}


\examples{
f <- system.file("external/rlogo.grd", package="raster")
r <- raster(f, layer=2)
nbands(r)
bandnr(r)
}

\keyword{spatial}
