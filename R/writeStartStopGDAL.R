# Author: Robert J. Hijmans
# Date : January 2009
# Version 0.9
# Licence GPL v3


.rasterNodatavalue <- function(x){
	if (x == 'FLT4S') return(-3.4E38)
	if (x == 'FLT8S') return(-1.7E308)
	if (x == 'INT4S') return(-2147483647)
	if (x == 'INT2S') return(-32768)
	if (x == 'INT1S') return(-128)
	if (x == 'INT1U') return(255)
	if (x == 'INT2U') return(65535)
	if (x == 'INT4U') return(2147483647) #(4294967295) <- not supported as integer in R
	NA
}

.startGDALwriting <- function(x, filename, gdal=NULL, setStatistics=TRUE, overwrite=FALSE, NAflag=NA, format="", datatype=NA, ...) {

	#temp <- .getGDALtransient(x, filename=filename, options=options, ...)
	#attr(x@file, "transient") <- temp[[1]]
	#x@file@nodatavalue <- temp[[2]]
	#attr(x@file, "options") <- temp[[3]]
	#attr(x@file, "stats") <- setStatistics

	#x@data@min <- rep(Inf, nlayers(x))
	#x@data@max <- rep(-Inf, nlayers(x))
	#x@data@haveminmax <- FALSE	
	#x@file@datanotation <- .getRasterDType(temp[[4]])

	
	ct <- colortable(x)
	if (length(ct) > 0 ) {
		hasCT <- TRUE
		if (is.na(datatype)) {
			datatype <- 'INT1U'
		} else {
			datatype <- .datatype(datatype)
		}
	} else {
		hasCT <- FALSE
		datatype <- .datatype(datatype)
	}
	
	if (is.na(NAflag)) {
		NAflag <- .rasterNodatavalue(datatype)
	}
		
	if (nlayers(x) > 1) {
		rr <- brick(x, values=FALSE)
	} else {
		rr <- raster(x)
	}
	r <- as(rr, "SpatRaster")

#	names(r) <- names(x)
#	nms <- paste0(extension(basename(filename), ""), "_")
#	names(r) <- paste0(nms, 1:nlyr(r))
#	if (!isTRUE(setStatistics)) ops$statistics = 6

	raster::writeStart(r, filename, overwrite=overwrite, gdal=gdal, filetype=format, datatype=datatype, progress=0, NAflag=NAflag, progressbar=FALSE) 	
	attr(x@file, "transient") <- r

	x@file@datanotation <- datatype
	x@file@driver <- 'gdal'
	x@data@fromdisk <- TRUE
	x@file@name <- filename

	return(x)
}


.stopGDALwriting <- function(x, stat=cbind(NA,NA)) {
	
	x <- attr(x@file, "transient")
	x <- writeStop(x)
	f <- sources(x)
	if (nlyr(x) == 1) {
		return(raster(f))
	} else {
		return(brick(f))	
	}

	# statistics <- cbind(x@data@min, x@data@max)	
	# if (substr(x@file@datanotation, 1, 1) != 'F') {
		# statistics <- round(statistics)
	# }

	
	# if (isTRUE( attr(x@file, "stats") ) ) {
	
		# statistics <- cbind(statistics, stat[,1], stat[,2])	

		# # could do wild guesses to avoid problems in other software
		# # but not sure if this cure would be worse. Could have an option to do this
		# #i <- is.na(statistics[,3])
		# #if (sum(i) > 0) {
		# #	statistics[i, 3] <- (statistics[i, 1] + statistics[i, 2]) / 2
		# #	statistics[i, 4] <- statistics[i, 3] * 0.2
		# #}
		
		# for (i in 1:nl) {
			# b <- methods::new("GDALRasterBand", x@file@transient, i)
			# rgdal::GDALcall(b, "SetStatistics", as.double(statistics[i,]))
		# }
	# }
	
	# if(x@file@options[1] != "") {
		# rgdal::saveDataset(x@file@transient, x@file@name, options=x@file@options)
	# } else {
		# rgdal::saveDataset(x@file@transient, x@file@name)	
	# }
	
	# rgdal::GDAL.close(x@file@transient) 
	
	# if (nl > 1) {
		# out <- brick(x@file@name)
	# } else {
		# out <- raster(x@file@name)
	# }
	
	# if (! out@data@haveminmax ) {
		# out@data@min <- statistics[, 1]
		# out@data@max <- statistics[, 2]
		# out@data@haveminmax <- TRUE
	# }

	#return(out)
}


