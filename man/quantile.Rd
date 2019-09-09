\name{quantile}

\docType{methods}

\alias{quantile}
\alias{quantile,Raster-method}

\title{Raster quantiles}

\description{
Compute quantiles for the cell values of a RasterLayer. If you want to compute quantiles for each cell across a number of layers, you can use \code{\link{calc}(x, fun=quantile)}. 
}

\usage{
quantile(x, ...) 
}

\arguments{
  \item{x}{Raster object}
  \item{...}{Additional arguments: \code{na.rm=TRUE}, \code{ncells=NULL}, and additional arguments to the stats::quantile function, see \code{\link[stats]{quantile}}}  
       ncells can be used to set the number of cells to be sampled, for very large raster datasets.
}

\value{
A vector of quantiles
}

\seealso{ \code{\link[raster]{density}}, \code{\link[raster]{cellStats}}  }



\examples{
r <- raster(ncol=100, nrow=100)
values(r) <- rnorm(ncell(r), 0, 50)
quantile(r)
quantile(r, probs = c(0.25, 0.75), type=7,names = FALSE)
}

\keyword{methods}
\keyword{spatial}

