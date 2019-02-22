\name{adjacent}

\alias{adjacent}
\alias{adjacent,BasicRaster-method}

\title{Adjacent cells}

\description{
Identify cells that are adjacent to a set of cells on a raster. 
}

\usage{
\S4method{adjacent}{BasicRaster}(x, cells, directions=4, pairs=TRUE, target=NULL, sorted=FALSE, 
         include=FALSE, id=FALSE, ...) 
}

\arguments{
  \item{x}{Raster* object}
  \item{cells}{vector of cell numbers for which adjacent cells should be found. Cell numbers start with 1 in the upper-left corner and increase from left to right and from top to bottom}
  \item{directions}{the number of directions in which cells should be connected: 4 (rook's case), 8 (queen's case), 16 (knight and one-cell queen moves), or 'bishop' to connect cells with one-cell diagonal moves. Or a neigborhood matrix (see Details)}
  \item{pairs}{logical. If \code{TRUE}, a matrix of pairs of adjacent cells is returned. If \code{FALSE}, a vector of cells adjacent to \code{cells} is returned}
  \item{target}{optional vector of target cell numbers that should be considered. All other adjacent cells are ignored}
  \item{sorted}{logical. Should the results be sorted? }
  \item{include}{logical. Should the focal cells be included in the result? }
  \item{id}{logical. Should the id of the cells be included in the result? (numbered from 1 to length(cells) }
  \item{...}{additional arguments. None implemented }
}

\details{
A neighborhood matrix identifies the cells around each cell that are considered adjacent. The matrix should have one, and only one, cell with value 0 (the focal cell); at least one cell with value 1 (the adjacent cell(s)); All other cells are not considered adjacent and ignored.
}


\value{
matrix or vector with adjacent cells. 
}


\author{Robert J. Hijmans and Jacob van Etten}

\examples{
r <- raster(nrows=10, ncols=10)
adjacent(r, cells=c(1, 55), directions=8, pairs=TRUE) 

a <- adjacent(r, cell = c(1,55,90), directions=4, sorted=TRUE) 
a

r[c(1,55,90)] <- 1
r[a] <- 2
plot(r)

# same result as above
rook <- matrix(c(NA, 1, NA, 
                  1, 0,  1, 
                 NA, 1, NA), ncol=3, byrow=TRUE)

adjacent(r, cells = c(1,55,90), directions=rook, sorted=TRUE) 


# Count the number of times that a cell with a certain value
# occurs next to a cell with a certain value
set.seed(0)
r <- raster(ncol=10, nrow=10)
values(r) <- round(runif(ncell(r)) * 5)
a <- adjacent(r, 1:ncell(r), 4, pairs=TRUE)
tb <- table(r[a[,1]], r[a[,2]])
tb
# make a matrix out of the 'table' object
tb <- unclass(tb)
plot(raster(tb, xmn=-0.5, xmx=5.5, ymn=-0.5, ymx=5.5))
}

\keyword{spatial}
