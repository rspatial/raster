\name{Programming}

\alias{readStart}
\alias{readStop}
\alias{readStart,Raster-method}
\alias{readStart,RasterStack-method}
\alias{readStop,Raster-method}
\alias{readStop,RasterStack-method}

\alias{canProcessInMemory}
\alias{pbCreate} 
\alias{pbStep} 
\alias{pbClose} 
\alias{getCluster}
\alias{returnCluster}

\title{Helper functions for programming}

\description{ 
These are low level functions that can be used by programmers to develop new functions. If in doubt, it is almost certain that you do not need these functions as these are already embedded in all other functions in the raster package.

\code{canProcessInMemory} is typically used within functions. In the raster package this function is used to determine if the amount of memory needed for the function is available. If there is not enough memory available, the function returns \code{FALSE}, and the function that called it will write the results to a temporary file. 

readStart opens file connection(s) for reading, readStop removes it. 

pbCreate creates a progress bar, pbStep sets the progress, and pbClose closes it.
}

\usage{
canProcessInMemory(x, n=4, verbose=FALSE)
pbCreate(nsteps, progress, style=3, label='Progress', ...)
pbStep(pb, step=NULL, label='')
pbClose(pb, timer)
readStart(x, ...)
readStop(x)
getCluster()
returnCluster()
}

\arguments{
  \item{x}{RasterLayer or RasterBrick object (for connections) or RasterStack object (canProcessInMemory)}
  \item{n}{integer. The number of copies of the Raster* object cell values that a function needs to be able to have in memory}  
  \item{verbose}{logical. If \code{TRUE} the amount of memory needed and available is printed}
  \item{nsteps}{integer. Number of steps the progress bar will make from start to end (e.g. nrow(raster)) }
  \item{progress}{character. 'text', 'window', or ''}
  \item{style}{style for text progress bar. See \code{\link[utils]{txtProgressBar}} }
  \item{label}{character. Label for the window type prograss bar}
  \item{...}{additional arguments (None implemented, except for 'silent=TRUE' for readStart for files read with gdal, and other arguments passed to gdal.open)}
  \item{pb}{ progress bar object created with pbCreate }
  \item{step}{which step is this ( 1 <= step <= nsteps ). If step is \code{NULL}, a single step is taken }
  \item{timer}{logical. If \code{TRUE}, time to completion will be printed. If missing, the value will be taken from the rasterOptions}
}

\value{
canProcessInMemory: logical

closeConnection: RasterLayer or RasterBrick object

getCluster: snow cluster object
}


\examples{
r <- raster(nrow=100, ncol=100)
canProcessInMemory(r, 4)
r <- raster(nrow=100000, ncol=100000)
canProcessInMemory(r, 2)
}

\keyword{ spatial }

