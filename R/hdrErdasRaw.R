# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.writeHdrErdasRaw <- function(raster) {
	hdrfile <- filename(raster)
	extension(hdrfile) <- ".raw"
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat("IMAGINE_RAW_FILE\n", file = thefile)
	cat("PIXEL_FILES ", .setFileExtensionValues(raster@file@name), "\n", file = thefile)
# this may not work. Some implementations may ignore this keyword and expect the pixelfile to have the same file name, no extension.		

	cat("HEIGHT ",  nrow(raster), "\n", file = thefile)
	cat("WIDTH ",  ncol(raster), "\n", file = thefile)
	cat("NUM_LAYERS ",  nbands(raster), "\n", file = thefile)

	if (.shortDataType(raster@file@datanotation) == 'INT') { 
		dd <- "S"
	} else { 
		dd <- "F" 
	}
	nbits <- dataSize(raster@file@datanotation) * 8 
    dtype <- paste(dd, nbits, sep="")
	cat("DATA_TYPE ",  dtype, "\n", file = thefile)
#U1, U2, U4, U8, U16, U32
#S16, S32
#F32, and F64.
	if (.Platform$endian == "little") { btorder <- "LSB" 
	} else { btorder <- "MSB" }
	cat("BYTE_ORDER ", btorder, "\n", file = thefile)
#Required for DATA_TYPE values of U16, S16, U32, S32

	cat("FORMAT ", "BIL", "\n", file = thefile)
	cat("DATA_OFFSET 0\n", file = thefile)
	cat("END_RAW_FILE\n", file = thefile)
	
	cat("\n\n", file = thefile)
	cat("The below is additional metadata, not part of the ERDAS raw format\n", file = thefile)
	cat("----------------------------------------------------------------\n", file = thefile)
	cat("CREATOR=R package:raster\n", file = thefile)
	cat("CREATED=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile)
	cat("Projection=",  sp::proj4string(raster), "\n", file = thefile)
	cat("MinValue=",  minValue(raster), "\n", file = thefile)
	cat("MaxValue=",  maxValue(raster), "\n", file = thefile)
	close(thefile)	
	
	.worldFile(raster, ".rww")	
 }
 
