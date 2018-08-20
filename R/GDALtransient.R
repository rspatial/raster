# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3


.getGDALtransient <- function(r, filename, options, NAflag, ...)  {

	.GDALnodatavalue <- function(x){
		if (x == 'Float32') return(-3.4E38)
		if (x == 'Float64') return(-1.7E308)
		if (x == 'Int32') return(-2147483647)
		if (x == 'Int16') return(-32768)
		if (x == 'Int8') return(-128)
		if (x == 'Byte') return(255)
		if (x == 'UInt16') return(65535)
		if (x == 'UInt32') return(2147483647) #(4294967295) <- not supported as integer in R
		stop('cannot find matching nodata value')
	}


    nbands <- nlayers(r)
	ct <- colortable(r)
	if (length(ct) > 0 ) {
		hasCT <- TRUE
		if (is.null(list(...)$datatype)) {
			datatype <- 'INT1U'
		} else {
			datatype <- .datatype(...)
		}
	} else {
		hasCT <- FALSE
		datatype <- .datatype(...)
	}
	isFact <- is.factor(r)
	if (any(isFact)) {
		v <- levels(r)
	}
	r <- raster(r)

	overwrite <- .overwrite(...)
	gdalfiletype <- .filetype(filename=filename, ...)

	.isSupportedFormat(gdalfiletype)
	
	if (filename == "") {	
		stop('provide a filename')	
	}

	if (file.exists( filename))  {
		if (!overwrite) {
			stop("filename exists; use overwrite=TRUE")
		} else if (!file.remove( filename)) {
			stop("cannot delete existing file; permission denied.")
		}
	}	

	dataformat <- .getGdalDType(datatype, gdalfiletype)
	
	if (dataformat != 'Byte') hasCT <- FALSE
		
	if (missing(NAflag)) { 
		NAflag <- .GDALnodatavalue(dataformat) 
	}
	
	if (gdalfiletype=='GTiff') {
		bytes <- ncell(r) * dataSize(datatype) * nbands
		if (bytes > (4 * 1024 * 1024 * 1000) ) {  # ~ 4GB
			options <- c(options, 'BIGTIFF=YES')
		}
		options <- c(options, "COMPRESS=LZW")
	}

	driver <- methods::new("GDALDriver", gdalfiletype)
	
    transient <- try( methods::new("GDALTransientDataset", driver=driver, rows=r@nrows, cols=r@ncols, bands=nbands, type=dataformat, fname=filename, options=options, handle=NULL), silent=TRUE)
 	if (class(transient) == 'try-error') {
		if (dataformat == "Float64") {
			dataformat <- "Float32"
		}
	    transient <- methods::new("GDALTransientDataset", driver=driver, rows=r@nrows, cols=r@ncols, bands=nbands, type=dataformat, fname=filename, options=options, handle=NULL)
	}

	for (i in 1:nbands) {
		b <- methods::new("GDALRasterBand", transient, i)
		rgdal::GDALcall(b, "SetNoDataValue", NAflag)
		if (hasCT) {
			rgdal::GDALcall(b, "SetRasterColorTable", ct)
		}
		if (isFact[i]) {
			vv <- v[[i]]
			if (NCOL(vv) > 1) {
				rn <- data.frame(IDID=0:max(vv[,1]))
				rnvv <- merge(rn, vv, by=1, all.x=TRUE)
				rnvv <- rnvv[order(rnvv[,1]), ]
				cnms <- as.character(rnvv[,2])
				cnms[is.na(cnms)] <- ''
				rgdal::GDALcall(b, "SetCategoryNames", cnms)
			}
		}
	}
	
	if (rotated(r)) {
		gt <- r@rotation@geotrans
	} else {
		#if (flip) {
		#	gt <- c(xmin(r), xres(r), 0, 0, ymax(r), yres(r))		
		#	cat('flipping (this creates an invalid RasterLayer)\n')
		#} else {
		gt <- c(xmin(r), xres(r), 0, ymax(r), 0, -yres(r))
		#}
	}

	rgdal::GDALcall(transient, "SetGeoTransform", gt)
	# as.character to ensure NA is character
	rgdal::GDALcall(transient, "SetProject", as.character(projection(r))) 
	if (is.null(options)) {
		options <- ''
	}
	return(list(transient, NAflag, options, dataformat))
}
