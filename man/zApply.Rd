\name{zApply}

\alias{zApply}
  
\title{z (time) apply}

\description{
Experimental function to apply a function over a (time) series of layers of a Raster object
}

\usage{
zApply(x, by, fun=mean, name='', ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{by}{aggregation indices or function }
  \item{fun}{function to compute aggregated values }
  \item{name}{character label of the new time series }  
  \item{...}{additional arguments}  
}


\value{
Raster* object
}

\author{Oscar Perpinan Lamigueiro & Robert J. Hijmans}

\examples{
# 12 values of irradiation, 1 for each month
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
# RasterBrick with 12 layers based on G0dm + noise
r <- raster(nc=10, nr=10)
s <- brick(lapply(1:12, function(x) setValues(r, G0dm[x]+100*rnorm(ncell(r)) )))

# time
tm <- seq(as.Date('2010-01-15'), as.Date('2010-12-15'), 'month')
s <- setZ(s, tm, 'months')

# library(zoo)
# x <- zApply(s, by=as.yearqtr, fun=mean, name='quarters')
}

\keyword{spatial}
