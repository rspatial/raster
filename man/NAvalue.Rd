\name{NAvalue}

\alias{NAvalue<-}
\alias{NAvalue}

\title{Set the NA value of a RasterLayer }

\description{
NAvalue returns the value that is used to write NA values to disk (in 'raster' type files). 
If you set the NA value of a Raster* object, this value will be interpreted as NA when reading the values from a file. 
Values already in memory will not be affected. 

If the NA value is smaller than zero, all values smaller or equal to that number will be set to NA.
}

\usage{
NAvalue(x) <- value
NAvalue(x)
}

\arguments{
 \item{x}{A \code{Raster} object}
 \item{value}{the value to be interpreted as NA; set this before reading the values from the file. Integer values are matched exactly; for decimal values files any value <= the value will be interpreted as NA}  
}


\value{
Returns or set the NA value used for storage on disk.
}




\examples{ 
r1 <- raster(system.file("external/rlogo.grd", package="raster"))
r2 <- r1
NAvalue(r2)
NAvalue(r2) <- 255
#plot(r1)
#x11()
#plot(r2)
}

\keyword{ spatial }

