\name{writeValues}


\alias{writeStart}
\alias{writeStart,RasterLayer,character-method}
\alias{writeStart,RasterBrick,character-method}
\alias{writeStop}
\alias{writeStop,RasterLayer-method}
\alias{writeStop,RasterBrick-method}
\alias{writeValues}
\alias{writeValues,RasterLayer,vector-method}
\alias{writeValues,RasterBrick,matrix-method}

\title{Write values to a file}

\description{
Functions for writing blocks (>= 1 row(s)) of values to files. Writing has to start at the first cell of a row (identified with argument \code{start}) and the values written must represent 1 or more entire rows. Begin by opening a file with \code{writeStart}, then write values to it in chunks. When writing is done close the file with \code{writeStop}. 

If you want to write all values of a Raster* object at once, you can also use \code{\link{writeRaster}} which is easier to use but more limited. The functions described here allow writing values to file using chunks of different sizes (e.g. 1 or 10 rows). Function \code{\link{blockSize}} can be used to suggest a chunk size to use.
}

\usage{
writeStart(x, filename, ...)
writeValues(x, v, start)
writeStop(x)
}

\arguments{
  \item{x}{Raster* object}
  \item{filename}{character. Output file name}
  \item{...}{additional arguments as for \code{\link{writeRaster}}}
  \item{v}{vector (RasterLayer) or matrix (RasterBrick) of values}
  \item{start}{Integer. Row number (counting starts at 1) from where to start writing \code{v}}

}


\value{
RasterLayer or RasterBrick 
}



\seealso{ \code{\link{writeRaster}, \link{blockSize}, \link{update}} }  

\examples{ 

\dontrun{
r <- raster(system.file("external/test.grd", package="raster"))
# write to a new binary file in chunks
s <- raster(r)
# 
tr <- blockSize(r)
tr
s <- writeStart(s, filename='test.grd',  overwrite=TRUE)
for (i in 1:tr$n) {
	v <- getValuesBlock(r, row=tr$row[i], nrows=tr$nrows[i])
	s <- writeValues(s, v, tr$row[i])
}
s <- writeStop(s)

if(require(rgdal)){
s2 <- writeStart(s, filename='test2.tif', format='GTiff', overwrite=TRUE)
# writing last row first
for (i in tr$n:1) {
	v <- getValuesBlock(r, row=tr$row[i], nrows=tr$nrows[i])
	s2 <- writeValues(s2, v, tr$row[i])
}
# row number 5 once more
v <- getValuesBlock(r, row=5, nrows=1)
writeValues(s2, v, 5)
s2 <- writeStop(s2)
}

## write values of a RasterStack to a RasterBrick
s <- stack(system.file("external/rlogo.grd", package="raster"))
# create empty brick
b <- brick(s, values=FALSE)  
b <- writeStart(b, filename="test.grd", format="raster",overwrite=TRUE)
tr <- blockSize(b)
for (i in 1:tr$n) {
	v <- getValuesBlock(s, row=tr$row[i], nrows=tr$nrows[i])
	b <- writeValues(b, v, tr$row[i])
}
b <- writeStop(b)
# note that the above is equivalent to 
# b <- writeRaster(s, filename="test.grd", format="raster",overwrite=TRUE)
}
}

\keyword{ spatial }
\keyword{ methods }
