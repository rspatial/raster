\name{corLocal}

\docType{methods}

\alias{corLocal}
\alias{corLocal,RasterLayer,RasterLayer-method}
\alias{corLocal,RasterStackBrick,RasterStackBrick-method}


\title{Local correlation coefficient}

\description{
Local correlation coefficient for two RasterLayer objects (using a focal neighborhood) or for two RasterStack or Brick objects (with the same number of layers (> 2))
}

\usage{
\S4method{corLocal}{RasterLayer,RasterLayer}(x, y, ngb=5, 
     method=c("pearson", "kendall", "spearman"), test=FALSE, filename='', ...)


\S4method{corLocal}{RasterStackBrick,RasterStackBrick}(x, y,  
     method=c("pearson", "kendall", "spearman"), test=FALSE, filename='', ...)
}

\arguments{
  \item{x}{RasterLayer or RasterStack/RasterBrick}
  \item{y}{object of the same class as \code{x}, and with the same number of layers}
  \item{ngb}{neighborhood size. Either a single integer or a vector of two integers c(nrow, ncol)}
  \item{method}{character indicating which correlation coefficient is to be used. One of \code{"pearson"}, \code{"kendall"}, or \code{"spearman"}}
  \item{test}{logical. If \code{TRUE}, return a p-value} 
  \item{filename}{character. Output filename (optional)}
  \item{...}{additional arguments as for \code{\link{writeRaster}}}
}

\note{\code{NA} values are omitted}

\value{
RasterLayer
}

\seealso{ \code{\link{cor}}, \code{\link{cor.test}} }


\examples{
b <- stack(system.file("external/rlogo.grd", package="raster"))
b <- aggregate(b, 2, mean)

set.seed(0)
b[[2]] <- flip(b[[2]], 'y') + runif(ncell(b))
b[[1]] <- b[[1]] + runif(ncell(b))

x <- corLocal(b[[1]], b[[2]], test=TRUE )
# plot(x)

# only cells where the p-value < 0.1
xm <- mask(x[[1]], x[[2]] < 0.1, maskvalue=FALSE)
plot(xm)


# for global correlation, use the cor function
x <- as.matrix(b)
cor(x, method="spearman")
 
# use sampleRegular for large datasets
x <- sampleRegular(b, 1000)
cor.test(x[,1], x[,2])

# RasterStack or Brick objects
y <- corLocal(b, flip(b, 'y'))
}

\keyword{methods}
\keyword{spatial}

