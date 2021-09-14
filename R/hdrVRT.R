# Author: Robert J. Hijmans
# Date :  October 2010
# Version 1.0
# Licence GPL v3

 
.writeHdrVRT <- function(x) {
	
	fn <- fname <- x@file@name

	if (tolower(extension(fn)) == '.vrt') {
		stop('cannot (over)write a vrt header for a vrt file')
	}
	if (tolower(extension(fn)) == '.grd') {
		extension(fn) <- '.gri'	
	}
	extension(fname) <- 'vrt'

	pixsize <- dataSize(x@file@datanotation)
	nbands <- nlayers(x)
	
	bandorder <- x@file@bandorder
	if (bandorder == 'BIL') {
		pixoff <- pixsize
		lineoff <- pixsize * x@ncols * nbands
		imgoff <- ((1:nbands)-1) * x@ncols * pixsize
	
	} else if (bandorder == 'BSQ') {
		pixoff <- pixsize
		lineoff <- pixsize * x@ncols
		imgoff <- ((1:nbands)-1) *  ncell(x) * pixsize

	} else if (bandorder == 'BIP') {
		pixoff <- pixsize * nbands
		lineoff <- pixsize * x@ncols * nbands
		imgoff <- (1:nbands)-1 
	}

	datatype <- .getGdalDType(x@file@datanotation)	
	
	if (x@file@byteorder == "little") { 
		byteorder <- "LSB" 
	} else { 
		byteorder <- "MSB" 
	}
	if (! x@file@toptobottom) { rotation <- 180 } else { rotation <- 0 }
	e <- x@extent
	r <- res(x)
	prj <-  sp::proj4string(x)

	f <- file(fname, "w") 
	cat('<VRTDataset rasterXSize="', x@ncols, '" rasterYSize="', x@nrows, '">\n' , sep = "", file = f)
	if (rotated(r)) {
		cat('<GeoTransform>', paste(x@rotation@geotrans, collapse=', '), '</GeoTransform>\n', sep = "", file = f)
	} else {
		cat('<GeoTransform>', e@xmin, ', ', r[1], ', ', rotation, ', ', e@ymax, ', ', 0.0, ', ', -1*r[2], '</GeoTransform>\n', sep = "", file = f)
	}
	if (! is.na(prj) ) {
		cat('<SRS>', prj ,'</SRS>\n', sep = "", file = f)
	}
	
	for (i in 1:nlayers(x)) {
		cat('\t<VRTRasterBand dataType="', datatype, '" band="', i, '" subClass="VRTRawRasterBand">\n', sep = "" , file = f)
		cat('\t\t<Description>', names(x), '</Description>\n', sep = "", file = f)
		cat('\t\t<SourceFilename relativetoVRT="1">', basename(fn), '</SourceFilename>\n', sep = "", file = f)
		cat('\t\t<ImageOffset>', imgoff[i], '</ImageOffset>\n', sep = "", file = f)
		cat('\t\t<PixelOffset>', pixoff, '</PixelOffset>\n', sep = "", file = f)
		cat('\t\t<LineOffset>', lineoff, '</LineOffset>\n', sep = "", file = f)
		cat('\t\t<ByteOrder>', byteorder, '</ByteOrder>\n', sep = "", file = f)
		cat('\t\t<NoDataValue>', x@file@nodatavalue, '</NoDataValue>\n', sep = "", file = f)
		cat('\t\t<Offset>', x@data@offset, '</Offset>\n', sep = "", file = f)
		cat('\t\t<Scale>', x@data@gain, '</Scale>\n', sep = "", file = f)
		cat('\t</VRTRasterBand>\n', sep = "", file = f)
	}
	cat('</VRTDataset>\n', sep = "", file = f)
	close(f)
	return( invisible(TRUE) )
}  

 
 
 