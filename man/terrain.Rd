\name{terrain}

\alias{terrain}
\alias{terrain,RasterLayer-method}

\title{Terrain characteristics}

\description{

Compute slope, aspect and other terrain characteristics from a raster with elevation data. The elevation data should be in map units (typically meter) for projected (planar) raster data. They should be in meters when the coordinate reference system (CRS) is longitude/latitude. 
}

\usage{
\S4method{terrain}{RasterLayer}(x, opt="slope", unit="radians", neighbors=8, filename="", ...)  
}


\arguments{
  \item{x}{RasterLayer object with elevation values. Values should have the same unit as the map units, or in meters when the crs is longitude/latitude}
  \item{opt}{Character vector containing one or more of these options: slope, aspect, TPI, TRI, roughness, flowdir (see Details)}
  \item{unit}{Character. 'degrees', 'radians' or 'tangent'. Only relevant for slope and aspect. If 'tangent' is selected that is used for slope, but for aspect 'degrees' is used (as 'tangent' has no meaning for aspect) }
  \item{neighbors}{Integer. Indicating how many neighboring cells to use to compute slope for any cell. Either 8 (queen case) or 4 (rook case). Only used for slope and aspect, see Details}
  \item{filename}{Character. Output filename (optional)}
  \item{...}{Standard additional arguments for writing Raster* objects to file}
}


\details{
When \code{neighbors=4}, slope and aspect are computed according to Fleming and Hoffer (1979) and Ritter (1987). When \code{neigbors=8}, slope and aspect are computed according to Horn (1981). The Horn algorithm may be best for rough surfaces, and the Fleming and Hoffer algorithm may be better for smoother surfaces (Jones, 1997; Burrough and McDonnell, 1998).
If slope = 0, aspect is set to 0.5*pi radians (or 90 degrees if unit='degrees'). When computing slope or aspect, the CRS (\code{\link{projection}}) of the RasterLayer \code{x} must be known (may not be \code{NA}), to be able to safely differentiate between planar and longitude/latitude data.

flowdir returns the 'flow direction' (of water), i.e. the direction of the greatest drop in elevation (or the smallest rise if all neighbors are higher). They are encoded as powers of 2 (0 to 7). The cell to the right of the focal cell 'x' is 1, the one below that is 2, and so on:
\tabular{rrr}{
32 \tab64 \tab 128\cr 
16 \tab x \tab 1 \cr 
 8 \tab 4 \tab 2 \cr }

If two cells have the same drop in elevation, a random cell is picked. That is not ideal as it may prevent the creation of connected flow networks. ArcGIS implements the approach of Greenlee (1987) and I might adopt that in the future.


The terrain indices are according to Wilson et al. (2007), as in \href{http://www.gdal.org/gdaldem.html}{gdaldem}. TRI (Terrain Ruggedness Index) is the mean of the absolute differences between the value of a cell and the value of its 8 surrounding cells. TPI (Topographic Position Index) is the difference between the value of a cell and the mean value of its 8 surrounding cells. Roughness is the difference between the maximum and the minimum value of a cell and its 8 surrounding cells.

Such measures can also be computed with the \code{\link{focal}} function:

f <- matrix(1, nrow=3, ncol=3)

TRI <- focal(x, w=f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)

TPI <- focal(x, w=f, fun=function(x, ...) x[5] - mean(x[-5]), pad=TRUE, padValue=NA)

rough <- focal(x, w=f, fun=function(x, ...) {max(x) - min(x)}, pad=TRUE, padValue=NA, na.rm=TRUE)
}


\seealso{ \code{\link{hillShade}} }



\references{
Burrough, P., and R.A. McDonnell, 1998. Principles of Geographical Information Systems. Oxford University Press.

Fleming, M.D. and Hoffer, R.M., 1979. Machine processing of landsat MSS data and DMA topographic data for forest cover type mapping. LARS Technical Report 062879. Laboratory for Applications of Remote Sensing, Purdue University, West Lafayette, Indiana.

Greenlee, D.D., 1987. Raster and vector processing for scanned linework. Photogrammetric Engineering and Remote Sensing 53:1383-1387

Horn, B.K.P., 1981. Hill shading and the reflectance map. Proceedings of the IEEE 69:14-47

Jones, K.H., 1998. A comparison of algorithms used to compute hill slope as a property of the DEM. Computers & Geosciences 24: 315-323 

Ritter, P., 1987. A vector-based slope and aspect generation algorithm. Photogrammetric Engineering and Remote Sensing 53: 1109-1111

Wilson, M.F.J., O'Connell, B., Brown, C., Guinan, J.C., Grehan, A.J., 2007. Multiscale terrain analysis of multibeam bathymetry data for habitat mapping on the continental slope. Marine Geodesy 30: 3-35.
}


\examples{


alt = getData("alt", country="USA")[[1]]
y = terrain(alt, opt="slope", unit="radians")

elevation <- getData('alt', country='CHE')
x <- terrain(elevation, opt=c('slope', 'aspect'), unit='degrees')

}

\keyword{spatial}
