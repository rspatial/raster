\name{hist}

\docType{methods}

\alias{hist}
\alias{hist,Raster-method}

\title{Histogram}

\description{
Create a histogram of the values of a RasterLayer. For large datasets a sample is used.
}

\usage{
\S4method{hist}{Raster}(x, layer, maxpixels=100000, plot=TRUE, main, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{layer}{integer (or character) to indicate layer number (or name). Can be used to subset the layers to plot in a multilayer Raster* object} 
  \item{maxpixels}{integer. To regularly subsample very large objects}
  \item{plot}{logical. Plot the histogram or only return the histogram values}
  \item{main}{character. Main title(s) for the plot. Default is the value of \code{\link{names}}}
  \item{...}{Additional arguments. See under Methods and at \code{\link[graphics]{hist}}}  
}


\value{
This function is principally used for the side-effect of plotting a histogram, but it also returns an S3 object of class 'histogram' (invisibly if \code{plot=TRUE}).
}


\seealso{ \code{\link{pairs}, \link{boxplot}} }

\examples{ 
r1 <- raster(nrows=50, ncols=50)
r1 <- setValues(r1, runif(ncell(r1)))
r2 <- setValues(r1, runif(ncell(r1)))
rs <- r1 + r2
rp <- r1 * r2
par(mfrow=c(2,2))
plot(rs, main='sum')
plot(rp, main='product')
hist(rs)
a = hist(rp)
a
}

\keyword{methods}
\keyword{spatial}

