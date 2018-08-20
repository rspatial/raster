\name{draw}

\alias{drawPoly}
\alias{drawLine}

\title{ Draw a line or polygon }

\description{
Draw a line or polygon on a plot (map) and save it for later use. After calling the function, start clicking on the map. To finish, right-click and select 'stop'.
}

\usage{
drawPoly(sp=TRUE, col='red', lwd=2, ...)
drawLine(sp=TRUE, col='red', lwd=2, ...)
}

\arguments{
  \item{sp}{logical. If \code{TRUE}, the output will be a sp object (SpatialPolygons or SpatialLines). Otherwise a matrix of coordinates is returned}  
  \item{col}{the color of the lines to be drawn}
  \item{lwd}{the width of the lines to be drawn}
  \item{...}{additional arguments padded to locator}
}
 
\value{
If \code{sp==TRUE} a SpatialPolygons or SpatialLines object; otherwise a matrix of coordinates
}

\seealso{ \code{\link[graphics]{locator}}  }
 
 



\keyword{ spatial }
