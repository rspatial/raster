# Author: Robert J. Hijmans
# Date :  October 2009
# Version 0.9
# Licence GPL v3

 
.writeHdrENVI <- function(r) {
	hdrfile <- filename(r)
	extension(hdrfile) <- ".hdr"
	thefile <- file(hdrfile, "w") 
	cat("ENVI\n", file = thefile)
	cat("samples = ", ncol(r), "\n", file = thefile)		
	cat("lines = ", nrow(r), "\n", file = thefile)		
	cat("bands = ", r@file@nbands, "\n", file = thefile)		
	cat("header offset = 0\n", file = thefile)		
	cat("file type = ENVI Standard\n", file = thefile)		
	dsize <- dataSize(r@file@datanotation)
	if (.shortDataType(r@file@datanotation) == 'INT') {
		if (dsize == 1) { dtype <- 1
		} else if (dsize == 2) { dtype <- 2
		} else if (dsize == 4) { dtype <- 3
		} else if (dsize == 8) { dtype <- 14
		} else { stop('what?')
		}
	} else {
		if (dsize == 4) { dtype <- 4
		} else if (dsize == 8) { dtype <- 5
		} else { stop('what?')
		}
	}	
	cat("data type = ", dtype, "\n", file = thefile)
#1=8-bit byte; 2=16-bit signed integer; 3=32-bit signed long integer; 4=32-bit floating point; 
#5=64-bit double-precision floating point; 6=2x32-bit complex, real-imaginary pair of double precision;
#9=2x64-bit double-precision complex, real-imaginary pair of double precision; 12=16-bit unsigned integer; 
#13=32-bit unsigned long integer; 14=64-bit signed long integer; and 15=64-bit unsigned long integer.

	cat("data ignore value=", .nodatavalue(r), "\n", file = thefile, sep='')
	cat("interleave = ", r@file@bandorder, "\n", file = thefile)	
	cat("sensor type = \n", file = thefile)		
	
	btorder <- as.integer(r@file@byteorder != 'little')  # little -> 0, big -> 1
	cat("byte order = ", btorder, "\n",file = thefile)		

	if (couldBeLonLat(r)) {
		cat("map info = {Geographic Lat/Lon, 1, 1,", xmin(r),", ", ymax(r),", ", xres(r),", ", yres(r), "}\n", file = thefile)
	} else {
		cat("map info = {projection, 1, 1,", xmin(r),", ", ymax(r),", ", xres(r),", ", yres(r), "}\n", file = thefile)
	}
	if (.requireRgdal(FALSE)) {
		cat("coordinate system string = {", wkt(r), "}\n", file = thefile, sep="")
	} else {
		cat("projection info =", .oldproj4string(r), "\n", file = thefile) 
	}
	cat("z plot range = {", minValue(r),", ", maxValue(r), "}\n", file = thefile) 
	
	cat("band names = {", paste(names(r),collapse=","), "}", "\n", file = thefile)
	
	close(thefile)	
}

