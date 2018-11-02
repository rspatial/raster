\name{raster-package}

\alias{raster-package}

\docType{package}

\title{ 
Overview of the functions in the raster package
}

\description{
The raster package provides classes and functions to manipulate geographic (spatial) data in 'raster' format. Raster data divides space into cells (rectangles; pixels) of equal size (in units of the coordinate reference system). Such continuous spatial data are also referred to as 'grid' data, and be contrasted with discrete (object based) spatial data (points, lines, polygons).

The package should be particularly useful when using very large datasets that can not be loaded into the computer's memory. Functions will work correctly, because they they process large files in chunks, i.e., they read, compute, and write blocks of data, without loading all values into memory at once.

Below is a list of some of the most important functions grouped by theme. See the vignette for more information and some examples (you can open it by running this command: \code{vignette('Raster')})
}


\details{
 The package implements classes for Raster data (see \link{Raster-class}) and supports
  \itemize{
    \item Creation of Raster* objects from scratch or from file
    \item Handling extremely large raster files
    \item Raster algebra and overlay functions
    \item Distance, neighborhood (focal) and patch functions
    \item Polygon, line and point to raster conversion
    \item Model predictions
    \item Summarizing raster values
    \item Easy access to raster cell-values
    \item Plotting (making maps)
    \item Manipulation of raster extent, resolution and origin
    \item Computation of row, col and cell numbers to coordinates and vice versa
    \item Reading and writing various raster file types
  }
. 
}


\section{I. Creating Raster* objects}{

RasterLayer, RasterStack, and RasterBrick objects are, as a group, referred to as Raster* objects. Raster* objects can be created, from scratch, files, or from objects of other classes, with the following functions:
  \tabular{ll}{
    \code{\link{raster}}\tab To create a RasterLayer \cr
    \code{\link{stack}} \tab To create a RasterStack (multiple layers)\cr
    \code{\link{brick}} \tab To create a RasterBrick (multiple layers)\cr
    \code{\link{subset}} \tab Select layers of a RasterStack/Brick\cr
    \code{\link{addLayer}} \tab Add a layer to a Raster* object\cr
    \code{\link{dropLayer}} \tab Remove a layer from a RasterStack or RasterBrick \cr
    \code{\link{unstack}} \tab Create a list of RasterLayer objects from a RasterStack \cr
  --------------------------- \tab --------------------------------------------------------------------------------------------------- \cr  
}
}


\section{II. Changing the spatial extent and/or resolution of Raster* objects}{

  \tabular{ll}{
    \code{\link{merge}} \tab Combine Raster* objects with different extents (but same origin and resolution) \cr
    \code{\link{mosaic}} \tab Combine RasterLayers with different extents and a function for overlap areas \cr
    \code{\link{crop}} \tab Select a geographic subset of a Raster* object \cr
    \code{\link{extend}} \tab Enlarge a Raster* object \cr
    \code{\link{trim}} \tab Trim a Raster* object by removing exterior rows and/or columns that only have NAs\cr
    \code{\link{aggregate}} \tab Combine cells of a Raster* object to create larger cells \cr
    \code{\link{disaggregate}} \tab Subdivide cells \cr
    \code{\link{resample}} \tab Warp values to a Raster* object with a different origin or resolution \cr	
    \code{\link{projectRaster}} \tab project values to a raster with a different coordinate reference system \cr	
    \code{\link{shift}} \tab Move the location of Raster \cr
    \code{\link{flip}} \tab  Flip values horizontally or vertically \cr
    \code{\link{rotate}} \tab Rotate values around the date-line (for lon/lat data) \cr
    \code{\link{t}} \tab Transpose a Raster* object\cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
  }
}


\section{III. Raster algebra}{
	\tabular{ll}{
    \code{\link{Arith-methods}} \tab Arith functions (\code{+, -, *, ^, \%\%, \%/\%, /}) \cr
	\code{\link{Math-methods}} \tab Math functions like \code{abs, sqrt, trunc, log, log10, exp, sin, round} \cr	
    \code{\link{Logic-methods}} \tab Logic functions (\code{!, &, |}) \cr
    \code{\link{Summary-methods}} \tab Summary functions (\code{mean, max, min, range, prod, sum, any, all}) \cr
    \code{\link{Compare-methods}} \tab Compare functions (\code{==, !=, >, <, <=, >=}) \cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
	}
}  
  
  
\section{IV. Cell based computation}{

  \tabular{ll}{
    \code{\link{calc}} \tab Computations on a single Raster* object \cr
    \code{\link{overlay}} \tab Computations on multiple RasterLayer objects \cr
    \code{\link{cover}} \tab First layer covers second layer except where the first layer is \code{NA} \cr
    \code{\link{mask}} \tab Use values from first Raster except where cells of the mask Raster are \code{NA}\cr
    \code{\link{cut}} \tab Reclassify values using ranges \cr
    \code{\link{subs}} \tab Reclassify values using an 'is-becomes' matrix \cr	
    \code{\link{reclassify}} \tab Reclassify using a 'from-to-becomes' matrix \cr
    \code{\link{init}} \tab Initialize cells with new values \cr	
    \code{\link{stackApply}} \tab Computations on groups of layers in Raster* object \cr
    \code{\link{stackSelect}} \tab Select cell values from different layers using an index RasterLayer\cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
  }
}


\section{V. Spatial contextual computation}{

  \tabular{ll}{
    \code{\link{distance}} \tab Shortest distance to a cell that is not \code{NA}\cr
    \code{\link{gridDistance}} \tab Distance when traversing grid cells that are not \code{NA} \cr
    \code{\link{distanceFromPoints}} \tab Shortest distance to any point in a set of points \cr
    \code{\link{direction}} \tab Direction (azimuth) to or from cells that are not \code{NA}\cr
    \code{\link{focal}} \tab Focal (neighborhood; moving window) functions \cr
    \code{\link{localFun}} \tab Local association (using neighborhoods) functions \cr
    \code{\link{boundaries}} \tab Detection of boundaries (edges)\cr
    \code{\link{clump}} \tab Find clumps (patches) \cr
    \code{\link{adjacent}} \tab Identify cells that are adjacent to a set of cells on a raster \cr
    \code{\link{area}} \tab Compute area of cells (for longitude/latitude data) \cr
    \code{\link{terrain}} \tab Compute slope, aspect and other characteristics from elevation data \cr	
    \code{\link{Moran}} \tab Compute global or local Moran or Geary indices of spatial autocorrelation  \cr	
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
  }
}


\section{VI. Model predictions}{

  \tabular{ll}{
    \code{\link{predict}} \tab Predict a non-spatial model to a RasterLayer \cr
    \code{\link{interpolate}} \tab Predict a spatial model to a RasterLayer \cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}


\section{VII. Data type conversion}{

You can coerce Raster* objects to Spatial* objects using \code{as}, as in \code{as(object, 'SpatialGridDataFrame')}  

  \tabular{ll}{
    \code{\link{raster}} \tab RasterLayer from SpatialGrid*, image, or matrix objects\cr
    \code{\link{rasterize}} \tab Rasterizing points, lines or polygons\cr
    \code{\link{rasterToPoints}} \tab Create points from a RasterLayer \cr
    \code{\link{rasterToPolygons}} \tab Create polygons from a RasterLayer \cr
    \code{\link{rasterToContour}} \tab Contour lines from a RasterLayer \cr
    \code{\link{rasterFromXYZ}} \tab RasterLayer from regularly spaces points\cr
    \code{\link{rasterFromCells}} \tab RasterLayer from a Raster object and cell numbers\cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
  }
}


\section{VIII. Summarizing}{

  \tabular{ll}{
    \code{\link{cellStats}} \tab Summarize a Raster cell values with a function \cr
    \code{\link{summary}} \tab Summary of the values of a Raster* object (quartiles and mean) \cr
    \code{\link{freq}} \tab Frequency table of Raster cell values \cr
    \code{\link{crosstab}} \tab Cross-tabulate two Raster* objects\cr
    \code{\link{unique}} \tab Get the unique values in a Raster* object \cr
    \code{\link{zonal}} \tab Summarize a Raster* object by zones in a RasterLayer \cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
  }
}


\section{IX. Accessing values of Raster* object cells}{

Apart from the function listed below, you can also use indexing with \code{[} for cell numbers, and \code{[[} for row / column number combinations \cr

\tabular{ll}{
    \code{\link{getValues}} \tab Get all cell values (fails with very large rasters), or a row of values (safer) \cr
    \code{\link{getValuesBlock}} \tab Get values for a block (a rectangular area) \cr
    \code{\link{getValuesFocal}} \tab Get focal values for one or more rows\cr
    \code{\link{as.matrix}} \tab Get cell values as a matrix \cr
    \code{\link{as.array}} \tab Get cell values as an array \cr
    \code{\link{extract}} \tab Extract cell values from a Raster* object (e.g., by cell, coordinates, polygon)\cr
    \code{\link{sampleRandom}} \tab Random sample \cr
    \code{\link{sampleRegular}} \tab Regular sample \cr
    \code{\link{minValue}} \tab Get the minimum value of the cells of a Raster* object (not always known) \cr
    \code{\link{maxValue}} \tab Get the maximum value of the cells of a Raster* object (not always known) \cr
    \code{\link{setMinMax}} \tab Compute the minimum and maximum value of a Raster* object if these are not known \cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
  }
}


\section{X. Plotting}{

See the rasterVis package for additional plotting methods for Raster* objects using methods from 'lattice' and other packages.

  \tabular{ll}{
    \bold{Maps} \tab \cr
    \code{\link{plot}} \tab Plot a Raster* object. The main method to create a map \cr
    \code{\link{plotRGB}} \tab Combine three layers (red, green, blue channels) into a single 'real color' image \cr
    \code{\link{spplot}} \tab Plot a Raster* with the spplot function (sp package) \cr
    \code{\link{image}} \tab Plot a Raster* with the image function \cr
    \code{\link{persp}} \tab Perspective plot of a RasterLayer \cr
    \code{\link{contour}} \tab Contour plot of a RasterLayer \cr
    \code{\link{filledContour}} \tab Filled contour plot of a RasterLayer \cr
    \code{\link{text}} \tab Plot the values of a RasterLayer on top of a map \cr
    .\cr
    \bold{Interacting with a map} \tab \cr
    \code{\link{zoom}} \tab Zoom in to a part of a map \cr
    \code{\link{click}} \tab Query values of Raster* or Spatial* objects by clicking on a map \cr
    \code{\link{select}} \tab Select a geometric subset of a Raster* or Spatial* object \cr
    \code{\link{drawPoly}} \tab Create a SpatialPolygons object by drawing it \cr
    \code{\link{drawLine}} \tab Create a SpatialLines object by drawing it \cr
    \code{\link{drawExtent}} \tab Create an Extent object by drawing it \cr	
    .\cr
	\bold{Other plots} \tab \cr
    \code{\link{plot}} \tab x-y scatter plot of the values of two RasterLayer objects\cr
    \code{\link{hist}} \tab Histogram of Raster* object values \cr
    \code{\link{barplot}} \tab barplot of a RasterLayer \cr
    \code{\link{density}} \tab Density plot of Raster* object values \cr
    \code{\link{pairs}} \tab Pairs plot for layers in a RasterStack or RasterBrick \cr
    \code{\link{boxplot}} \tab Box plot of the values of one or multiple layers\cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
  }
}

\section{XI. Getting and setting Raster* dimensions }{

Basic parameters of existing Raster* objects can be obtained, and in most cases changed. If there are values associated with a RasterLayer object (either in memory or via a link to a file) these are lost when you change the number of columns or rows or the resolution. This is not the case when the extent is changed (as the number of columns and rows will not be affected). Similarly, with \bold{projection} you can set the projection, but this does not transform the data (see \link{projectRaster} for that).   

  \tabular{ll}{
    \code{\link{ncol}}\tab The number of columns \cr
    \code{\link{nrow}} \tab The number of rows \cr
    \code{\link{ncell}} \tab The number of cells (can not be set directly, only via ncol or nrow) \cr
    \code{\link{res}} \tab The resolution (x and y) \cr
    \code{\link{nlayers}} \tab How many layers does the object have? \cr	
    \code{\link{names}} \tab Get or set the layer names \cr	
    \code{\link{xres}} \tab The x resolution (can be set with res) \cr
    \code{\link{yres}} \tab The y resolution (can be set with res)\cr
    \code{\link{xmin}} \tab The minimum x coordinate (or longitude) \cr
    \code{\link{xmax}} \tab The maximum x coordinate (or longitude) \cr
    \code{\link{ymin}} \tab The minimum y coordinate (or latitude) \cr
    \code{\link{ymax}} \tab The maximum y coordinate (or latitude) \cr
	\code{\link{extent}} \tab The extent (minimum and maximum x and y coordinates) \cr
	\code{\link{origin}} \tab The origin of a Raster* object\cr
    \code{\link{projection}} \tab The coordinate reference system (map projection) \cr	
    \code{\link{isLonLat}} \tab Test if an object has a longitude/latitude coordinate reference system \cr
    \code{\link{filename}} \tab Filename to which a RasterLayer or RasterBrick is linked \cr	
    \code{\link{bandnr}} \tab layer (=band) of a multi-band file that this RasterLayer is linked to \cr	
    \code{\link{nbands}} \tab How many bands (layers) does the file associated with a RasterLayer object have? \cr	
    \code{\link{compareRaster}} \tab Compare the geometry of Raster* objects \cr		
    \code{\link{NAvalue}} \tab Get or set the \code{NA} value (for reading from a file) \cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
	
}
}


\section{XII. Computing row, column, cell numbers and coordinates}{

Cell numbers start at 1 in the upper-left corner. They increase within rows, from left to right, and then row by row from top to bottom. Likewise, row numbers start at 1 at the top of the raster, and column numbers start at 1 at the left side of the raster. 

  \tabular{ll}{
    \code{\link{xFromCol}} \tab x-coordinates from column numbers \cr
    \code{\link{yFromRow}} \tab y-coordinates from row numbers \cr
    \code{\link{xFromCell}} \tab x-coordinates from row numbers \cr
    \code{\link{yFromCell}} \tab y-coordinates from cell numbers \cr
    \code{\link{xyFromCell}} \tab x and y coordinates from cell numbers \cr
    \code{\link{colFromX}} \tab Column numbers from x-coordinates (or longitude) \cr
    \code{\link{rowFromY}} \tab Row numbers from y-coordinates (or latitude) \cr
    \code{\link{rowColFromCell}} \tab Row and column numbers from cell numbers\cr
    \code{\link{cellFromXY}} \tab Cell numbers from x and y coordinates \cr
    \code{\link{cellFromRowCol}} \tab Cell numbers from row and column numbers \cr
    \code{\link{cellsFromExtent}} \tab Cell numbers from extent object \cr
    \code{\link{coordinates}} \tab x and y coordinates for all cells \cr
    \code{\link{validCell}} \tab Is this a valid cell number? \cr
    \code{\link{validCol}} \tab Is this a valid column number? \cr
    \code{\link{validRow}} \tab Is this a valid row number? \cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
  }
}

\section{XIII. Writing files}{

  \tabular{ll}{
   \bold{Basic}\cr
    \code{\link{setValues}} \tab Put new values in a Raster* object \cr
    \code{\link{writeRaster}} \tab Write all values of Raster* object to disk \cr
    \code{\link{KML}} \tab Save raster as KML file \cr	
   .\cr
   \bold{Advanced}\cr
   \code{\link{blockSize}} \tab Get suggested block size for reading and writing \cr
   \code{\link{writeStart}} \tab Open a file for writing \cr
   \code{\link{writeValues}} \tab Write some values  \cr
   \code{\link{writeStop}} \tab Close the file after writing \cr
   \code{\link{update}} \tab Change the values of an existing file \cr	
    --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}	

\section{XIV. Manipulation of SpatialPolygons* and other vector type Spatial* objects}{
Some of these functions are in the \code{sp} package. The name in \bold{bold} is the equivalent command in ArcGIS. These functions build on the geometry ("spatial features") manipulation functions in package \code{rgeos}. These functions are extended here by also providing automated attribute data handling.

\tabular{ll}{
  \code{\link{bind}} \tab \bold{append} combine Spatial* objects of the same (vector) type \cr
  \code{\link{erase}} or "-" \tab \bold{erase} parts of a SpatialPolygons* object\cr
  \code{\link{intersect}} or "*"  \tab \bold{intersect} SpatialPolygons* objects\cr
  \code{\link{union}} or "+" \tab \bold{union} SpatialPolygons* objects\cr
  \code{\link{cover}} \tab \bold{update} and \bold{identity} a SpatialPolygons object with another one\cr
  \code{\link{symdif}} \tab\bold{symmetrical difference} of two SpatialPolygons* objects \cr 
  \code{\link{aggregate}} \tab \bold{dissolve} smaller polygons into larger ones \cr
  \code{\link[sp]{disaggregate}} \tab \bold{explode}: turn polygon parts into separate polygons (in the \code{sp} package) \cr
  \code{\link{crop}} \tab \bold{clip} a Spatial* object using a rectangle (Extent object)\cr
  \code{\link{select}} \tab \bold{select} - interactively select spatial features\cr
  \code{\link{click}} \tab \bold{identify} attributes by clicking on a map\cr
  \code{\link[sp]{merge}} \tab \bold{Join table} (in the \code{sp} package) \cr
  \code{\link[sp]{over}} \tab spatial queries between Spatial* objects \cr
  \code{\link{extract}} \tab spatial queries between Spatial* and Raster* objects \cr
  \code{\link{as.data.frame}} \tab coerce coordinates of \code{SpatialLines} or \code{SpatialPolygons} into a data.frame\cr
  --------------------------- \tab ------------------------------------------------------------------------------------------ \cr  
}
}


\section{XV. Extent objects}{

  \tabular{ll}{
    \code{\link{extent}} \tab Create an extent object \cr
    \code{\link{intersect}} \tab Intersect two extent objects  \cr
    \code{\link{union}} \tab Combine two extent objects \cr
    \code{\link{round}} \tab round/floor/ceiling of the coordinates of an Extent object \cr			
    \code{\link{alignExtent}} \tab Align an extent with a Raster* object \cr
    \code{\link{drawExtent}} \tab Create an Extent object by drawing it on top of a map (see plot) \cr		
    --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
  }
}

\section{XVI. Miscellaneous}{

\tabular{ll}{
    \code{\link{rasterOptions}} \tab Show, set, save or get session options \cr
    \code{\link{getData}} \tab Download and geographic data\cr
    \code{\link{pointDistance}} \tab Distance between points \cr
    \code{\link{readIniFile}} \tab Read a (windows) 'ini' file \cr
    \code{\link{hdr}} \tab Write header file for a number of raster formats \cr
    \code{\link{trim}} \tab Remove leading and trainling blanks from a character string \cr
    \code{\link{extension}} \tab Get or set the extentsion of a filename \cr
    \code{\link{cv}} \tab Coefficient of variation  \cr
    \code{\link{modal}} \tab Modal value  \cr
    \code{\link{sampleInt}} \tab Random sample of (possibly very large) range of integer values \cr
    \code{\link{showTmpFiles}} \tab Show temporary files \cr
    \code{\link{removeTmpFiles}} \tab Remove temporary files \cr
    --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}

\section{XVII. For programmers}{

\tabular{ll}{
  \code{\link{canProcessInMemory}} \tab Test whether a file can be created in memory \cr
  \code{\link{pbCreate}} \tab Initialize a progress bar  \cr
  \code{\link{pbStep}} \tab Take a progress bar step \cr
  \code{\link{pbClose}} \tab Close a progress bar \cr
  \code{\link{readStart}} \tab Open file connections for efficient multi-chunck reading \cr
  \code{\link{readStop}} \tab Close file connections \cr
  \code{\link{rasterTmpFile}}  \tab Get a name for a temporary file \cr
  \code{\link{inMemory}} \tab Are the cell values in memory? \cr
  \code{\link{fromDisk}} \tab Are the cell values read from a file? \cr	
    --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
}
}

\author{Except where indicated otherwise, the functions in this package were written by Robert J. Hijmans}

\section{Acknowledgements}{
Extensive contributions were made by Jacob van Etten, Jonathan Greenberg, Matteo Mattiuzzi, and Michael Sumner. Significant help was also provided by Phil Heilman, Agustin Lobo, Oscar Perpinan Lamigueiro, Stefan Schlaffer, Jon Olav Skoien, Steven Mosher, and Kevin Ummel. Contributions were also made by Jochen Albrecht, Neil Best, Andrew Bevan, Roger Bivand, Isabelle Boulangeat, Lyndon Estes, Josh Gray, Tim Haering, Herry Herry, Paul Hiemstra, Ned Hornig, Mayeul Kauffmann, Bart Kranstauber, Rainer Krug, Alice Laborte, John Lewis, Lennon Li, Justin McGrath, Babak Naimi, Carsten Neumann, Joshua Perlman, Richard Plant, Edzer Pebesma, Etienne Racine, David Ramsey, Shaun Walbridge, Julian Zeidler and many others.
}

\keyword{package}
\keyword{spatial}
