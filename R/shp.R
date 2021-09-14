# Author: Robert J. Hijmans
# Date : December 2009
# Version 0.1
# Licence GPL v3


if (!isGeneric("shapefile")) {
	setGeneric("shapefile", function(x, ...)
		standardGeneric("shapefile"))
}	


setMethod('shapefile', signature(x='character'), 
	function(x, stringsAsFactors=FALSE, verbose=FALSE, warnPRJ=TRUE, ...) {
		.requireRgdal() 
		x <- normalizePath(x, winslash = "/", mustWork = FALSE)
		stopifnot(file.exists(extension(x, '.shp')))
		stopifnot(file.exists(extension(x, '.shx')))
		stopifnot(file.exists(extension(x, '.dbf')))
		if (warnPRJ & !file.exists(extension(x, '.prj'))) {
			warning('.prj file is missing')
		}
		fn <- extension(basename(x), '')
		rgdal::readOGR(dirname(x), fn, stringsAsFactors=stringsAsFactors, verbose=verbose, ...) 		
	}
)


setMethod('shapefile', signature(x='Spatial'), 
	function(x, filename='', overwrite=FALSE, ...) {
		.requireRgdal() 
		stopifnot(filename != '')
		filename <- normalizePath(filename, winslash = "/", mustWork = FALSE)
		
		extension(filename) <- '.shp'
		if (file.exists(filename)) {
			if (!overwrite) {
				stop('file exists, use overwrite=TRUE to overwrite it')
			}
		}
		layer <- basename(filename)
		extension(layer) <- ''
		if (!inherits(x, 'Spatial')) {
			stop('To write a shapefile you need to provide an object of class Spatial*')
		} else {
			if (inherits(x, 'SpatialPixels')) {
				if (.hasSlot(x, 'data')) {
					x <- as(x, 'SpatialPointsDataFrame')
				} else {
					x <- as(x, 'SpatialPoints')				
				}
				warning('Writing SpatialPixels to a shapefile. Writing to a raster file format might be more desirable')
				
			} else if ( inherits(x, 'SpatialGrid') ) {
				stop('These data cannot be written to a shapefile')
			}
			
			if (!.hasSlot(x, 'data')) {
				if (inherits(x, 'SpatialPolygons')) {
					x <- sp::SpatialPolygonsDataFrame(x, data.frame(ID=1:length(x)), match.ID=FALSE)
				} else if (inherits(x, 'SpatialLines')) {
					x <- sp::SpatialLinesDataFrame(x, data.frame(ID=1:length(x)), match.ID=FALSE)
				} else if (inherits(x, 'SpatialPoints')) {
					x <- sp::SpatialPointsDataFrame(x, data.frame(ID=1:length(x)), match.ID=FALSE)
				} else {
					stop('These data cannot be written to a shapefile')
				}
			}
		}
		rgdal::writeOGR(x, filename, layer, driver='ESRI Shapefile', overwrite_layer=overwrite, ...)
		#extension(filename) <- '.cpg'
		#writeLines(encoding, filename, sep="")
	}
)


