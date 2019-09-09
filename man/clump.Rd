\name{clump}

\alias{clump}
\alias{clump,RasterLayer-method}

\title{Detect clumps}

\description{
Detect clumps (patches) of connected cells. Each clump gets a unique ID. NA and zero are used as background values (i.e. these values are used to separate clumps). You can use queen's or rook's case, using the \code{directions} argument. For larger files that are processed in chunks, the highest clump number is not necessarily equal to the number of clumps (unless you use argument \code{gaps=FALSE}).
}

\usage{
\S4method{clump}{RasterLayer}(x, filename="", directions=8, gaps=TRUE, ...)
}

\arguments{
  \item{x}{RasterLayer}
  \item{filename}{Character. Filename for the output RasterLayer (optional)}  
  \item{directions}{Integer. Which cells are considered adjacent? Should be 8 (Queen's case) or 4 (Rook's case) }
  \item{gaps}{Logical. If \code{TRUE} (the default), there may be 'gaps' in the chunk numbers (e.g. you may have clumps with IDs 1, 2, 3 and 5, but not 4). If it is \code{FALSE}, these numbers will be recoded from 1 to n (4 in this example)}  
  \item{...}{Additional arguments as for \code{\link{writeRaster}}}  
}

\note{
This function requires that the igraph package is available.
}

\value{
RasterLayer
}

\author{Robert J. Hijmans and Jacob van Etten}

\examples{
r <- raster(ncols=12, nrows=12)
set.seed(0)
values(r) <- round(runif(ncell(r))*0.7 )
rc <- clump(r) 
freq(rc)
plot(rc)
}
\keyword{spatial}

