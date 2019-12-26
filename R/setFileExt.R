# raster package
# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.setFileExtensionValues <- function(fname, type='raster') {
	if (type == 'raster') {
		extension(fname) <- ".gri"
	} else if (type == 'SAGA') {
		extension(fname) <- ".sdat"
	} else if (type == 'IDRISI') {
		extension(fname) <- ".rst"
	} else if (type == 'IDRISIold') {
		extension(fname) <- ".img"
	} else if (type == 'BIL') {
		extension(fname) <- ".bil"
	} else if (type == 'BIP') {
		extension(fname) <- ".bip"
	} else if (type == 'BSQ') {
		extension(fname) <- ".bsq"
#	} else if (type == 'big.matrix') {
#		extension(fname) <- ".big"
	} else {
		stop('unknown file format')
	}
	return(fname)
}
 
.setFileExtensionHeader <- function(fname, type='raster') {
	if (type == 'raster') {
		extension(fname) <- ".grd"
	} else if (type == 'SAGA') {
		extension(fname) <- "sgrd"
	} else if (type == 'IDRISI') {
		extension(fname) <- ".rdc"
	} else if (type == 'IDRISIold') {
		extension(fname) <- ".doc"
	} else if (type %in% c('BIL', 'BSQ', 'BIP')) {
		extension(fname) <- ".hdr"
	} else if (type == 'big.matrix') {
		extension(fname) <- ".brd"
	} else {
		stop('unknown file format')
	}
	return(fname)
}
 