\name{blockSize}

\alias{blockSize}

\title{Block size for writing files}

\description{
This function can be used to suggest chunk sizes (always a number of entire rows), and corresponding row numbers, to be used when processing Raster* objects in chunks. Normally used together with \code{\link{writeValues}}.
}

\usage{
blockSize(x, chunksize, n=nlayers(x), minblocks=4, minrows=1)
}

\arguments{
  \item{x}{Raster* object}
  \item{chunksize}{Integer, normally missing. Can be used to set the block size; unit is number of cells. Block size is then computed in units of number of rows (always >= 1)  }
  \item{n}{Integer. number of layers to consider. The function divides chunksize by n to determine blocksize }
  \item{minblocks}{Integer. Minimum number of blocks }
  \item{minrows}{Integer. Minimum number of rows in each block }
}


\value{
A list with three elements: 

\code{rows}, the suggested row numbers at which to start the blocks for reading and writing, 

\code{nrows}, the number of rows in each block, and,

\code{n}, the total number of blocks 
}


\seealso{ \code{\link[raster]{writeValues}} }  

\examples{
r <- raster(system.file("external/test.grd", package="raster"))
blockSize(r)
}

\keyword{ spatial }
\keyword{ methods }
