# Author: Robert J. Hijmans
# Date : March 2011
# Version 1.0
# Licence GPL v3


.quickStack <- function(files, nbands=1, band=1, native=FALSE) {
	r <- raster(files[[1]], native=native)
	if (length(nbands) == 1) {
		nbands <- rep(nbands, length(files))
	} else {
		stopifnot(length(files == length(nbands)))
	}
	nbands <- as.integer(nbands)
	band <- as.integer(band)

	if (length(band) == 1) {
		band <- rep(band, length(files))
	} else {
		stopifnot(length(files == length(band)))
	}

	r@data@haveminmax <- FALSE
	r@file@nbands <- nbands[1]
	r@data@band <- band[1]

	ln <- extension(basename(unlist(files)), '')
	s <- stack(r)
	s@layers <- sapply(1:length(files),
			function(i){
				r@file@name <- files[[i]]
				r@file@nbands <- nbands[i]
				r@data@band <- band[i]
				r@data@names <- ln[i]
				r
			}
		)
	s
}




.quickStackOneFile <- function(filename, bands=NULL, native=FALSE) {
	b <- brick(filename, native=native)
	.stackFromBrick(b, bands=bands)
}



.stackFromBrick <- function(b, bands=NULL) {

	nbands <- nlayers(b)
	if (is.null(bands)) {
		bands <- 1:nbands
	} else {
		if (is.character(bands)) {
			 bands <- match(bands, names(b))
		}
		bands <- bands[bands %in% 1:nbands]
		if (length(bands)==0) {
			bands <- 1:nbands
		}
	}
	bands <- as.integer(bands)

	havemnmx <- b@data@haveminmax
	if (havemnmx) {
		mn <- minValue(b)
		mx <- maxValue(b)
	}
	ln <- names(b)

	if (inMemory(b)) {
		r <- b[[ bands[1] ]]
		s <- stack(r)

		if (length(bands) > 1) {

			if (havemnmx) {
				s@layers <- sapply( bands, function(i) {
						r@data@values <- b@data@values[,i]
						r@data@names <- ln[i]
						r@data@min <- mn[i]
						r@data@max <- mx[i]
						if (b@data@isfactor[i]) {
							r@data@isfactor <- b@data@isfactor[i]
							r@data@attributes <- b@data@attributes[i]
						}
						r
					})
			} else {
				s@layers <- sapply(bands, function(i){
						r@data@values <- b@data@values[,i]
						r@data@names <- ln[i]
						if (b@data@isfactor[i]) {
							r@data@isfactor <- TRUE
							r@data@attributes <- b@data@attributes[i]
						}
						r
						})
			}
		}
		return(s)

	}



	r <- raster(b, bands[1])
	s <- stack(r)
	if (length(bands) > 1) {

		if (havemnmx) {
			s@layers <- sapply(bands, function(i){
					r@data@band <- i
					r@data@names <- ln[i]
					r@data@min <- mn[i]
					r@data@max <- mx[i]
					if (b@data@isfactor[i]) {
						r@data@isfactor <- b@data@isfactor[i]
						r@data@attributes <- b@data@attributes[i]
					}
					r
					})
		} else {
			s@layers <- sapply(bands, function(i){
					r@data@band <-  i
					r@data@names <- ln[i]
					if (b@data@isfactor[i]) {
						r@data@isfactor <- b@data@isfactor[i]
						r@data@attributes <- b@data@attributes[i]
					}
					r
					})
		}
	}
	s
}

