# Author: Robert J. Hijmans
# Date : June 2008
# Version 1.0
# Licence GPL v3


# .gdFixGeoref <- function(mdata) {
	# gdversion <- getOption('rasterGDALVersion')
	# test <- gdversion < '1.8.0'	
	# if (test) {
		# if (! is.null(mdata) ) {
			# for (i in 1:length(mdata)) {
				# if (mdata[i] == "AREA_OR_POINT=Area") {
					# return(FALSE)
				# } else if (mdata[i] == "AREA_OR_POINT=Point") {
					# return(TRUE)
				# }
			# }
		# }
	# }
	# return(FALSE)
# }



.rasterFromGDAL <- function(filename, band, type, sub=0, RAT=TRUE, silent=TRUE, warn=TRUE, crs="", ...) {	

	x <- rast(filename, sub)
	if (crs != "") {
		crs(x) <- crs
	}

	r <- as(rast(x), "Raster")
	if (type == "RasterLayer") {
		r <- as(r, "RasterLayer")
	} else {
		r <- as(r, "RasterBrick")
	}

	r@file@name <- filename
	r@file@driver <- 'gdal' 
 	r@data@fromdisk <- TRUE
	r@file@datanotation <- x@ptr$dataType[1]
	if (any(hasMinMax(x))) {
		mnmx <- minmax(x)
	} else {
		mnmx <- matrix(NA, nrow=2, ncol=nlyr(x))
	}
	minv <-	mnmx[1,]
	maxv <-	mnmx[2,]
	if ( all(c(is.finite(minv), is.finite(maxv)))) {
		r@data@haveminmax <- TRUE 
	}
	r@file@nbands <- as.integer(nlyr(x))

	bks <- terra::fileBlocksize(x)
	r@file@blockrows <- bks[,1]
	r@file@blockcols <- bks[,2]
	
	if (type == 'RasterLayer') {
		band <- as.integer(band)
		if ( band > nlyr(x) ) {
			 stop(paste("band too high. Should be between 1 and", nlyr(x)))
		 }
		if ( band < 1) { 
			stop(paste("band should be 1 or higher"))		
		}
		r@data@band <- band
		r@file@nbands <- as.integer(nlyr(x))
		r@data@min <- minv[band]
		r@data@max <- maxv[band]
	
	} else {
		r@data@min <- minv
		r@data@max <- maxv
	}
	
	return(r)

	# .requireRgdal() 
	
	# if (sub > 0) {
		# gdalinfo <- rgdal::GDALinfo(filename, silent=TRUE, returnRAT=FALSE, returnCategoryNames=FALSE)
		# sub <- round(sub)
		# subdsmdata <- attr(gdalinfo, 'subdsmdata')

		# i <- grep(paste("SUBDATASET_", sub, "_NAME", sep=''), subdsmdata)
		# if (length(i) > 0) {
			# x <- subdsmdata[i[1]]
			# filename <- unlist(strsplit(x, '='))[2]
		# } else {
			# stop(paste('subdataset "sub=', sub, '" not available', sep=''))
		# }
	# }
	
	# suppressWarnings(
		# gdalinfo <- try ( rgdal::GDALinfo(filename, silent=silent, returnRAT=RAT, returnCategoryNames=RAT) )
	# ) 

	# if ( inherits(gdalinfo, "try-error")) {
		# gdalinfo <- rgdal::GDALinfo(filename, silent=silent, returnRAT=FALSE, returnCategoryNames=FALSE)
		# warning('Could not read RAT or Category names')
	# }

	# nc <- as.integer(gdalinfo[["columns"]])
	# nr <- as.integer(gdalinfo[["rows"]])

	# xn <- gdalinfo[["ll.x"]]
	# xn <- round(xn, digits=9)

	# xx <- xn + gdalinfo[["res.x"]] * nc
	# xx <- round(xx, digits=9)

	# yn <- gdalinfo[["ll.y"]]
	# yn <- round(yn, digits=9)
	# yx <- yn + gdalinfo[["res.y"]] * nr
	# yx <- round(yx, digits=9)

	# nbands <- as.integer(gdalinfo[["bands"]])

	# if (isTRUE(attr(gdalinfo, "ysign") == 1)) {
		# warning("data seems flipped. Consider using: flip(x, direction='y')")
	# }

	# rotated <- FALSE
	# if (gdalinfo['oblique.x'] != 0 | gdalinfo['oblique.y'] != 0) {
		# rotated <- TRUE

		# ## adapted from rgdal::getGeoTransFunc
		# if (warn) {
			# warning('\n\n This file has a rotation\n Support for such files is limited and results of data processing might be wrong.\n Proceed with caution & consider using the "rectify" function\n')
		# }
		# rotMat <- matrix(gdalinfo[c('res.x', 'oblique.x', 'oblique.y', 'res.y')], 2)
		# ysign <- attr(gdalinfo, 'ysign')
		# rotMat[4] <- rotMat[4] * ysign

		# invMat <- solve(rotMat)
		
		# offset <- c(xn, yx)
		# trans <- function(x, inv=FALSE) {
			# if (inv) {
				# x <- t(t(x) - c(offset[1], offset[2]))
				# x <- round( x %*% invMat  + 0.5 )
				# x[x < 1] <- NA
				# x[x[,1] > nc  | x[,2] > nr, ] <- NA
			# } else {
				# x <- (x - 0.5) %*% rotMat
				# x <- t(t(x) + c(offset[1], offset[2])) 
			# }
			# return(x)
		# }
	
		# crd <- trans(cbind(c(0, 0, nc, nc), c(0, nr, 0, nr))+0.5)
		# rot <- methods::new(".Rotation")
		
		# gtr <- gdalinfo[c('ll.x', 'res.x', 'oblique.x', NA, 'oblique.y', 'res.y')]
		# gtr[4] <- yx
		# gtr[6] <- gtr[6] * ysign
		
		# rot@geotrans <- gtr
		# rot@transfun <- trans

		# xn  <- min(crd[,1])
		# xx  <- max(crd[,1])
		# yn  <- min(crd[,2])
		# yx  <- max(crd[,2])
		
	# } 
	
	# mdata <- attr(gdalinfo, 'mdata')
	# fixGeoref <- FALSE
	# try( fixGeoref <- .gdFixGeoref(mdata), silent=TRUE )

	# # for ENVI files
	# bnames <- unique(mdata[grep("Band_", mdata)])
	# if (length(bnames) > 0) {
		# bn <- sapply(strsplit(bnames, '='), function(x) x[2])
		# bi <- gsub("Band_", "", sapply(strsplit(bnames, '='), function(x) x[1]))
		# bnames <- try(bn[order(as.integer(bi))], silent=TRUE)
		# if ( inherits(bnames, "try-error") ) {
			# bnames <- NULL
		# }
	# } else {
		# gobj <- rgdal::GDAL.open(filename)
		# bnames <- rep("", nbands)
		# for (i in 1:nbands) {
			# objbnd <- rgdal::getRasterBand(gobj, i)
			# bnames[i] <- rgdal::getDescription(objbnd)
		# }
		# rgdal::GDAL.close(gobj)		
	# }
	
	# if (type == 'RasterBrick') {
	
		# r <- brick(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs="")
		# r@file@nbands <- r@data@nlayers <- nbands
		# band <- 1:nbands
		# #RAT <- FALSE
		
	# } else {
	
		# r <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs="")
		# r@file@nbands <- as.integer(nbands)
		# band <- as.integer(band)
		# if ( band > nbands(r) ) {
			# stop(paste("band too high. Should be between 1 and", nbands))
			# #if (warn) {
				# #stop("band too high. Set to nbands")
			# #}
			# #band <- nbands(r) 
		# }
		# if ( band < 1) { 
			# stop(paste("band should be 1 or higher"))		
			# #if (warn) {
				# #stop("band too low. Set to 1")
			# #}
			# #band <- 1 
		# }
		# r@data@band <- as.integer(band)
		# nbands <-1 
	# }
	# if (rotated) {
		# r@rotated <- TRUE
		# r@rotation <- rot
	# }	

	# prj <- attr(gdalinfo, 'projection')
	# if (!is.na(prj)) {
		# prjcom <- attr(prj, 'comment')
		# if ((!is.null(prjcom) && !is.na(prjcom))) {
			# prj <- prjcom
		# }
	# }
	# crs <- .getProj(prj, crs)
	# r@crs <- .CRS(crs, TRUE) 
	# #r@crs <- .CRS(crs, FALSE) 
	# # F to avoid warnings about other than WGS84 datums or ellipsoids  
	
# #  	r@history[[1]] <- mdata


	# bi <- attr(gdalinfo, 'df')
	# GDType <- as.character(bi[['GDType']])
	# hasNoDataValues <- bi[['hasNoDataValue']]
	# NoDataValue <- bi[['NoDataValue']]
	
# #	if (getOption('rasterNewRGDALVersion')) {	
# #		sbi <- attr(gdalinfo, 'sdf')
# #		Bmin <- sbi[['Bmin']]
# #		Bmax <- sbi[['Bmax']]	
# #	} else {
		# Bmin <- bi[['Bmin']]
		# Bmax <- bi[['Bmax']]
# #	}
	
	
	# RATlist <- attr(gdalinfo, 'RATlist')
	# CATlist <- attr(gdalinfo, 'CATlist')

	# blockrows <- integer(nbands)
	# blockcols <- integer(nbands)
	
	# x <- rgdal::GDAL.open(filename, silent=TRUE)
	# ct <- rgdal::getColorTable( x )
	# if (! is.null(ct)) { 
		# r@legend@colortable <- ct 
	# }
	# for (i in 1:nbands) {
		# bs <- rgdal::getRasterBlockSize( rgdal::getRasterBand(x, i) )
		# blockrows[i] <- bs[1]
		# blockcols[i] <- bs[2]
	# }
	# rgdal::GDAL.close(x)

	# r@file@blockrows <- blockrows
	# r@file@blockcols <- blockcols


	# if (fixGeoref) {
		# message('Fixing "AREA_OR_POINT=Point" georeference')
		# rs <- res(r)
		# xmin(r) <- xmin(r) - 0.5 * rs[1]
		# xmax(r) <- xmax(r) - 0.5 * rs[1]
		# ymin(r) <- ymin(r) + 0.5 * rs[2]
		# ymax(r) <- ymax(r) + 0.5 * rs[2]
	# }
	
	# if (type == 'RasterBrick') {
		# ub <- unique(bnames)
		# if ((!all(ub == "")) && (length(ub) == nlayers(r))) {
			# names(r) <- bnames		
		# } else {
			# names(r) <- rep(gsub(" ", "_", extension(basename(filename), "")), nbands)
		# }
	# } else {
		# lnames <- gsub(" ", "_", extension(basename(filename), ""))
		# if (nbands > 1) {
			# lnames <- paste0(lnames, '_', band)
		# }
		# names(r) <- lnames
		
	# }
	# r@file@name <- filename
	# r@file@driver <- 'gdal' 
 

	# r@data@fromdisk <- TRUE
		
	# datatype <- "FLT4S"
	# minv <-	rep(Inf, nlayers(r))
	# maxv <-	rep(-Inf, nlayers(r))
	# try ( minv <- as.numeric( Bmin ) , silent=TRUE ) 
	# try ( maxv <- as.numeric( Bmax ) , silent=TRUE ) 
	# minv[minv == -4294967295] <- Inf
	# maxv[maxv == 4294967295] <- -Inf
	# try ( datatype <- .getRasterDType ( GDType[1] ), silent=TRUE )
	
	# if ( all(c(is.finite(minv), is.finite(maxv)))) {
		# r@data@haveminmax <- TRUE 
	# }
	# r@file@datanotation <- datatype
	
	# r@data@min <- minv[band]
	# r@data@max <- maxv[band]

	# rats <- ! sapply(RATlist, is.null) 
	# if (any(rats)) {
		# att <- vector(length=nlayers(r), mode='list')
		# for (i in 1:length(RATlist)) {
			# if (! is.null(RATlist[[i]])) {
				# dr <- data.frame(RATlist[[i]], stringsAsFactors=TRUE)
				# wv <- which(colnames(dr)=='VALUE')
				# if (length(wv) > 0) {
					# if (wv != 1) {
						# dr <- data.frame(dr[,wv,drop=FALSE], dr[,-wv,drop=FALSE])
					# }
					# colnames(dr)[1] <- 'ID'
				# } else {
					# if (all((colnames(dr) %in% c('Red', 'Green', 'Blue', 'Opacity', 'Histogram')))) {
						# # this is really a color table
						# rats[i] <- FALSE
						# if (is.null(ct)) { 
							# r@legend@colortable <- grDevices::rgb(dr$Red, dr$Green, dr$Blue, dr$Opacity)
						# }
						# next
					# } else {
						# j <- which(colnames(dr) == 'Histogram')
						# if (isTRUE(j>0) & ncol(dr) > 1) {
							# dr <- data.frame(ID=0:(nrow(dr)-1), COUNT=dr[,j], dr[,-j,drop=FALSE])
						# } else {
							# dr <- data.frame(ID=0:(nrow(dr)-1), dr)
						# }
					# }
				# }				
				# att[[i]] <- dr
			# }
		# }
		
		# r@data@attributes <- att[band]
		# r@data@isfactor <- rats[band]
		
	# } else {
		# cats <- ! sapply(CATlist, is.null) 
		# if (any(cats)) {
			# att <- vector(length=nlayers(r), mode='list')
			# for (i in 1:length(CATlist)) {
				# if (! is.null(CATlist[[i]])) {
					# att[[i]] <- data.frame(ID=(1:length(CATlist[[i]]))-1, category=CATlist[[i]], stringsAsFactors=TRUE)
				# }
			# }
			# r@data@attributes <- att[band]
			# r@data@isfactor <- cats[band]
		# }
	# }
	#return(r)
}


