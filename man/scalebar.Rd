\name{scalebar}

\alias{scalebar}
  
\title{scalebar}

\description{
Add a scalebar to a plot
}

\usage{
scalebar(d, xy = NULL, type = "line", divs = 2, below = "", 
       lonlat = NULL, label, adj=c(0.5, -0.5), lwd = 2, ...)
}

\arguments{
\item{d}{distance covered by scalebar}
\item{xy}{x and y coordinate to place the plot. Can be NULL. Use \code{xy=click()} to make this interactive }
\item{type}{"line" or "bar"}
\item{divs}{Number of divisions for a bar type. 2 or 4}
\item{below}{Text to go below scalebar (e.g., "kilometers")}
\item{lonlat}{Logical or NULL. If logical, \code{TRUE} indicates if the plot is using longitude/latitude coordinates. If \code{NULL} this is guessed from the plot's coordinates}
\item{adj}{adjustment for text placement}
\item{label}{Vector of three numbers to label the scale bar (beginning, midpoint, end)}
\item{lwd}{line width for the "line" type scalebar}
\item{...}{arguments to be passed to other methods }
}


\value{
None. Use for side effect of a scalebar added to a plot
}

\seealso{ \code{\link[raster]{plot}} }


\author{Robert J. Hijmans; partly based on a function by Josh Gray  }

\examples{
f <- system.file("external/test.grd", package="raster")
r <- raster(f)
plot(r)
scalebar(1000)
scalebar(1000, xy=c(178000, 333500), type='bar', divs=4)
}

\keyword{spatial}
