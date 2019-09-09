\name{zonal}

\alias{zonal}

\alias{zonal,RasterLayer,RasterLayer-method}
\alias{zonal,RasterStackBrick,RasterLayer-method}

\title{Zonal statistics}

\description{
Compute zonal statistics, that is summarized values of a Raster* object for each "zone" defined by a RasterLayer. 

If \code{stat} is a true \code{function}, \code{zonal} will fail (gracefully) for very large Raster objects, but it will in most cases work for functions that can be defined as by a character argument ('mean', 'sd', 'min', 'max', or 'sum'). In addition you can use 'count' to count the number of cells in each zone (only useful with \code{na.rm=TRUE}, otherwise \code{freq(z)} would be more direct.

If a function is used, it should accept a \code{na.rm} argument (or at least a \code{...} argument)
}

\usage{
\S4method{zonal}{RasterLayer,RasterLayer}(x, z, fun='mean', digits=0, na.rm=TRUE, ...) 

\S4method{zonal}{RasterStackBrick,RasterLayer}(x, z, fun='mean', digits=0, na.rm=TRUE, ...) 
}

\arguments{
  \item{x}{Raster* object}
  \item{z}{RasterLayer with codes representing zones}
  \item{fun}{function to be applied to summarize the values by zone. Either as character: 'mean', 'sd', 'min', 'max', 'sum'; or, for relatively small Raster* objects, a proper function}
  \item{digits}{integer. Number of digits to maintain in 'zones'. By default averaged to an integer (zero digits)}
  \item{na.rm}{logical. If \code{TRUE}, \code{NA} values in \code{x} are ignored}
  \item{...}{additional arguments. One implemented: \code{progress}, as in \code{\link{writeRaster}}}
}

\value{
A matrix with a value for each zone (unique value in \code{zones})
}

\seealso{ See \code{\link{cellStats}} for 'global' statistics (i.e., all of \code{x} is considered a single zone), and \code{\link{extract}} for summarizing values for polygons}



\examples{
r <- raster(ncols=10, nrows=10)
values(r) <- runif(ncell(r)) * 1:ncell(r)
z <- r
values(z) <- rep(1:5, each=20)
# for big files, use a character value rather than a function
zonal(r, z, 'sum')

# for smaller files you can also provide a function
\dontrun{
zonal(r, z, mean)
zonal(r, z, min)
}

# multiple layers
zonal(stack(r, r*10), z, 'sum')
}

\keyword{spatial}

