\name{stackSave}

\alias{stackSave}
\alias{stackOpen}

\title{Save or open a RasterStack file}

\description{
A RasterStack is a collection of RasterLayers with the same spatial extent and resolution. They can be created from RasterLayer objects,
or from file names. These two functions allow you to save the references to raster files and recreate a rasterStack object later. They only work if the RasterStack points to layers that have their values on disk. The values are not saved, only the references to the files.
}

\usage{
stackOpen(stackfile)
stackSave(x, filename)
}

\arguments{
  \item{stackfile}{ Filename for the RasterStack (to save it on disk) }
  \item{x}{ RasterStack object }
  \item{filename}{File name }
}

\details{
When a RasterStack is saved to a file, only pointers (filenames) to raster datasets are saved, not the data. 
If the name or location of a raster file changes, the RasterStack becomes invalid.
}

\value{
RasterStack object
}



\seealso{ \code{\link[raster]{writeRaster}}, \code{\link[raster]{stack}}, \code{\link[raster]{addLayer}}}

\examples{
file <- system.file("external/test.grd", package="raster")
s <- stack(c(file, file))
s <- stackSave(s, "mystack")
# note that filename adds an extension .stk to a stackfile  
\dontrun{
s2 <- stackOpen("mystack.stk")
s2
}
}
\keyword{ spatial }

