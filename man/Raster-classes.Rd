\name{Raster-class}

\docType{class}

\alias{BasicRaster-class}
\alias{Raster-class}
\alias{RasterLayer-class}
\alias{RasterLayerSparse-class}
\alias{RasterStack-class}
\alias{RasterBrick-class}
\alias{RasterStackBrick-class}
\alias{VectorLayer-class}
\alias{SpatialVector-class}


\alias{print,Raster-method}
\alias{show,BasicRaster-method}
\alias{show,RasterLayer-method}
\alias{show,RasterStack-method}
\alias{show,RasterBrick-method}

\alias{print,Spatial-method}


\title{ Raster* classes}

\description{

A raster is a database organized as a rectangular grid that is sub-divided into rectangular cells of equal area (in terms of the units of the coordinate reference system). The 'raster' package defines a number of "S4 classes" to manipulate such data.

The main user level classes are \code{RasterLayer}, \code{RasterStack} and \code{RasterBrick}. They all inherit from \code{BasicRaster} and can contain values for the raster cells.

An object of the \code{RasterLayer} class refers to a single layer (variable) of raster data. The object can point to a file on disk that holds the values of the raster cells, or hold these values in memory. Or it can not have any associated values at all.

A \code{RasterStack} represents a collection of \code{RasterLayer} objects with the same extent and resolution. Organizing \code{RasterLayer} objects in a \code{RasterStack} can be practical when dealing with multiple layers; for example to summarize their values (see \code{\link[raster]{calc}}) or in spatial modeling (see \code{\link[raster]{predict}}). 

An object of class \code{RasterBrick} can also contain multiple layers of raster data, but they are more tightly related. An object of class  \code{RasterBrick} can refer to only a single (multi-layer) data file, whereas each layer in a \code{RasterStack} can refer to another file (or another band in a multi-band file). This has implications for processing speed and flexibility. A \code{RasterBrick} should process quicker than a \code{RasterStack} (irrespective if values are on disk or in memory). However, a \code{RasterStack} is more flexible as a single object can refer to layers that have values stored on disk as well as in memory. If a layer that does not refer to values on disk (they only exists in memory) is added to a \code{RasterBrick}, it needs to load all its values into memory (and this may not be possible because of memory size limitations). 
 
Objects can be created from file or from each other with the following functions: \code{\link[raster]{raster}, \link[raster]{brick}} and \link[raster]{stack}.
 
\code{Raster*} objects can also be created from SpatialPixels* and SpatialGrid* objects from the sp package using \code{as}, or simply with the function \code{\link[raster]{raster}}, \code{\link[raster]{brick}}, or \code{\link[raster]{stack}}. Vice versa, \code{Raster*} objects can be coerced into a sp type object with \code{as( , )}, e.g. \code{as(x, 'SpatialGridDataFrame')} . 
 
Common generic methods implemented for these classes include: 
 
\code{summary}, \code{show}, \code{dim}, and \code{plot, ...} 
 
\code{[} is implemented for RasterLayer. 
 

The classes described above inherit from the \code{BasicRaster} class which inherits from \code{BasicRaster}. The \code{BasicRaster} class describes the main properties of a raster such as the number of columns and rows, and it contains an object of the \code{link[raster]{Extent-class}} to describe its spatial extent (coordinates). It also holds the 'coordinate reference system' in a slot of class  \code{\link[sp]{CRS-class}} defined in the \code{sp} package. A \code{BasicRaster} cannot contain any raster cell values and is therefore seldomly used.

The \code{Raster*} class inherits from \code{BasicRaster}. It is a virtual class; which means that you cannot create an object of this class. It is used only to define methods for all the classes that inherit from it (\code{RasterLayer}, \code{RasterStack} and \code{RasterBrick}). Another virtual class is the \code{RasterStackBrick} class. It is formed by a class union of \code{RasterStack} and \code{RasterBrick}. You cannot make objects of it, but methods defined for objects of this class as arguments will accept objects of the \code{RasterLayer} and \code{RasterStack} as that argument.

Classes \code{RasterLayer} and \code{RasterBrick} have a slot with an object of class \code{RasterFile} that describes the properties of the file they point to (if they do).  \code{RasterLayer} has a slot with an object of class \code{SingleLayerData}, and the \code{RasterBrick} class has a slot with an object of class \code{MultipleLayerData}. These 'datalayer' classes can contain (some of) the values of the raster cells.
 
These classes are not further described here because users should not need to directly access these slots. The 'setter' functions such as \code{setValues} should be used instead. Using such 'setter' functions is much safer because a change in one slot should often affect the values in other slots. 
}


\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RasterLayer", ...)}, or with the helper functions such as \code{raster}.
}

\section{Slots}{
Slots for RasterLayer and RasterBrick objects
	\describe{
    \item{\code{title}:}{Character} 
    \item{\code{file}:}{Object of class \code{".RasterFile"} }
    \item{\code{data}:}{Object of class \code{".SingleLayerData"} or \code{".MultipleLayerData"}}
    \item{\code{history}:}{To record processing history, not yet in use }
    \item{\code{legend}:}{Object of class \code{.RasterLegend}, Default legend. Should store preferences for plotting. Not yet implemented except that it stores the color table of images, if available}
    \item{\code{extent}:}{Object of \code{\link{Extent-class}} }
    \item{\code{ncols}:}{Integer} 
    \item{\code{nrows}:}{Integer} 
    \item{\code{crs}:}{Object of class \code{"CRS"}, i.e. the coordinate reference system. In Spatial* objects this slot is called 'proj4string' }
  }
}






\examples{
showClass("RasterLayer")
}

\keyword{classes}
\keyword{spatial}
