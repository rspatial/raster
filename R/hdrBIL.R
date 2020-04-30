# Author: Robert J. Hijmans
# Date :  October 2009
# Version 0.9
# Licence GPL v3

 
.writeHdrBIL <- function(x, layout='BIL') {
	hdrfile <- x@file@name
	extension(hdrfile) <- '.hdr'
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat("NROWS          ",  x@nrows, "\n", file = thefile)
	cat("NCOLS          ",  x@ncols, "\n", file = thefile)
	cat("NBANDS         ",  nlayers(x), "\n", file = thefile)
	cat("NBITS          ",  dataSize(x@file@datanotation) * 8, "\n", file = thefile)
	btorder <- ifelse(x@file@byteorder == "little", "I", "M")
	cat("BYTEORDER      ", btorder, "\n", file = thefile)
	
#  PIXELTYPE should work for Gdal, and perhpas ArcGIS, see:
# http://lists.osgeo.org/pipermail/gdal-dev/2006-October/010416.html	

	dtype <- .shortDataType(x@file@datanotation)
	if (dtype == 'INT' | dtype == 'LOG' ) { 
		pixtype <- ifelse(dataSigned(x@file@datanotation), "SIGNEDINT", "UNSIGNEDINT")
	} else { 
		pixtype <- "FLOAT" 
	}
	cat("PIXELTYPE      ", pixtype, "\n", file = thefile)	
	cat("LAYOUT         ", layout, "\n", file = thefile)
    cat("SKIPBYTES       0\n", file = thefile)
    cat("ULXMAP         ", as.character(xmin(x) + 0.5 * xres(x)), "\n", file = thefile) 
    cat("ULYMAP         ", as.character(ymax(x) - 0.5 * yres(x)), "\n", file = thefile) 
	cat("XDIM           ", xres(x), "\n", file = thefile)
	cat("YDIM           ", yres(x), "\n", file = thefile)
	browbytes <- round(ncol(x) * dataSize(x@file@datanotation) )
	cat("BANDROWBYTES   ", browbytes, "\n", file = thefile)
	cat("TOTALROWBYTES  ", browbytes *  nbands(x), "\n", file = thefile)
	cat("BANDGAPBYTES    0\n", file = thefile)
    cat("NODATA         ", .nodatavalue(x), "\n", file = thefile)	

	cat("\n\n", file = thefile)
	cat("The below is additional metadata, not part of the BIL/HDR format\n", file = thefile)
	cat("----------------------------------------------------------------\n", file = thefile)
	cat("CREATOR=R package:x\n", file = thefile)
	cat("CREATED=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile)
	cat("Projection=", .oldproj4string(x), "\n", file = thefile)
	cat("MinValue=",  minValue(x), "\n", file = thefile)
	cat("MaxValue=",  maxValue(x), "\n", file = thefile)

	close(thefile)
	return(invisible(TRUE))	
}
