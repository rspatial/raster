\name{autocorrelation}

\alias{Geary}
\alias{Moran}
\alias{MoranLocal}
\alias{GearyLocal}

\title{Spatial autocorrelation}

\description{
Compute Moran's I or Geary's C measures of global spatial autocorrelation in a RasterLayer, or compute the the local Moran or Geary index (Anselin, 1995).
}

\usage{
Geary(x, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
Moran(x, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
MoranLocal(x, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
GearyLocal(x, w=matrix(c(1,1,1,1,0,1,1,1,1), 3,3))
}

\arguments{
  \item{x}{RasterLayer}
  \item{w}{Spatial weights defined by or a rectangular matrix with odd length (3, 5, ...) sides (as in \code{\link{focal}}) }
}


\value{
A single value (Moran's I or Geary's C) or a RasterLayer (Local Moran or Geary values)
}

\details{
The default setting uses a 3x3 neighborhood to compute "Queen's case" indices. You can use a filter (weights matrix) to do other things, such as "Rook's case", or different lags. 
}

\seealso{
The spdep package for additional and more general approaches for computing indices of spatial autocorrelation
}

\author{Robert J. Hijmans and Babak Naimi}

\references{
Moran, P.A.P., 1950. Notes on continuous stochastic phenomena. Biometrika 37:17-23

Geary, R.C., 1954. The contiguity ratio and statistical mapping. The Incorporated Statistician 5: 115-145

Anselin, L., 1995. Local indicators of spatial association-LISA. Geographical Analysis 27:93-115

\url{http://en.wikipedia.org/wiki/Indicators_of_spatial_association}
}

\examples{
r <- raster(nrows=10, ncols=10)
r[] <- 1:ncell(r)

Moran(r)
# Rook's case
f <- matrix(c(0,1,0,1,0,1,0,1,0), nrow=3)
Moran(r, f)

Geary(r)

x1 <- MoranLocal(r)

# Rook's case
x2 <- MoranLocal(r, w=f)
}

\keyword{spatial}
