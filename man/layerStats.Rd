\name{layerStats}

\alias{layerStats}


\title{Correlation and (weighted) covariance}


\description{
Compute correlation and (weighted) covariance for multi-layer Raster objects. Like \code{\link{cellStats}} this function returns a few values, not a Raster* object (see \code{\link{Summary-methods}} for that). 
}


\usage{
layerStats(x, stat, w, asSample=TRUE, na.rm=FALSE, ...)
}


\arguments{
  \item{x}{RasterStack or RasterBrick for which to compute a statistic}

  \item{stat}{Character. The statistic to compute: either 'cov' (covariance), 'weighted.cov' (weighted covariance), or 'pearson' (correlation coefficient)}

  \item{w}{RasterLayer with the weights (should have the same extent, resolution and number of layers as \code{x}) to compute the weighted covariance}

  \item{asSample}{Logical. If \code{TRUE}, the statistic for a sample (denominator is \code{n-1}) is computed, rather than for the population (denominator is \code{n})}
    
  \item{na.rm}{Logical. Should missing values be removed?}
  
  \item{...}{Additional arguments (none implemetned)} 
}


\value{
List with two items: the correlation or (weighted) covariance matrix, and the (weighted) means.
}


\author{Jonathan A. Greenberg & Robert Hijmans. Weighted covariance based on code by Mort Canty}


\references{
For the weighted covariance:
\itemize{
\item {Canty, M.J. and A.A. Nielsen, 2008. Automatic radiometric normalization of multitemporal satellite imagery with the iteratively re-weighted MAD transformation. Remote Sensing of Environment 112:1025-1036.}
\item {Nielsen, A.A., 2007. The regularized iteratively reweighted MAD method for change detection in multi- and hyperspectral data. IEEE Transactions on Image Processing 16(2):463-478.} 
}
}

\seealso{
 \code{\link{cellStats}}, \code{\link{cov.wt}}, \code{\link[raster]{weighted.mean}}
}

\examples{
b <- brick(system.file("external/rlogo.grd", package="raster"))
layerStats(b, 'pearson')

layerStats(b, 'cov')

# weigh by column number
w <- init(b, v='col')
layerStats(b, 'weighted.cov', w=w)
}

