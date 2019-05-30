# Author: Robert J. Hijmans
# Date :  June 2008
# Version 1.0
# Licence GPL v3

setMethod("init", signature(x="Raster"), 
function(x, fun='cell', filename="", ...) {

	vv <- list(...)$v
	v <- NULL
	if (!is.null(vv)) {
		if (vv %in% c('x', 'y', 'row', 'col', 'cell', 'chess')) {
			v <- vv
		}
	} else if (is.character(fun) ) {
		fun <- tolower(fun[1])
		if (fun %in% c('x', 'y', 'row', 'col', 'cell', 'chess')) {
			v <- fun
		} else {
			stop("argument 'fun' is a character variable, but not one of 'x', 'y', 'row', 'col', 'cell', or 'chess'")
		}
	}

	out <- raster(x)
	filename <- trim(filename)
	
	inmem=TRUE
	if (!canProcessInMemory(out, 2)) {
		inmem=FALSE
		if (filename == '') {
			filename <- rasterTmpFile()									
		}
	}
	
	if (!is.null(v)) {
		if ( inmem ) {
			if (v == 'cell') { 
				out <- setValues(out, 1:ncell(out)) 
			} else if (v == 'row') { 
				out <- setValues(out, rep(1:nrow(out), each=ncol(out)))
			} else if (v == 'y') { 
				out <- setValues(out, rep(yFromRow(out, 1:nrow(out)), each=ncol(out)))
			} else if (v == 'col') { 
				out <- setValues(out, rep(1:ncol(out), times=nrow(out)))
			} else if (v == 'x') { 
				out <- setValues(out, rep(xFromCol(out, 1:ncol(out)), times=nrow(out))) 
			} else if (v == 'chess') {
				if ((ncol(out) %% 2) == 1) {
					out <- setValues(out, c(rep(c(0,1), floor(ncell(out)/2)), 0))
				} else {
					rs <- c(rep(c(0,1), ncol(out) / 2), rep(c(1,0), ncol(out) / 2))
					rs <- rep(rs, floor(nrow(out) / 2))
					if ((nrow(out) %% 2) == 1) {
						rs <- c(rs, rep(c(0,1), ncol(out) / 2))
					}
					out <- setValues(out, rs)	
				}
			}
		} else {
			out <- writeStart(out, filename=filename, ...)
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='init', ...)
			for (i in 1:tr$n) {
				if (v == 'cell') { 
					out <- writeValues(out, cellFromRowCol(out, tr$row[i],1):cellFromRowCol(out, tr$row[i]+tr$nrows[i]-1, ncol(out)), tr$row[i])
				} else if (v == 'row') {
					r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)
					out <-  writeValues(out, rep(r, each=ncol(out)), tr$row[i])
				} else if (v == 'col') { 
					out <- writeValues(out, rep(1:ncol(out), tr$nrows[i]), tr$row[i])
				} else if (v == 'x') { 
					out <- writeValues(out, rep(xFromCol(out, 1:ncol(out)), tr$nrows[i]), tr$row[i])
				} else if (v == 'y') { 
					r <- tr$row[i]:(tr$row[i]+tr$nrows[i]-1)	
					out <- writeValues(out, rep(yFromRow(out, r), each=ncol(out)), tr$row[i])
				} else if (v == 'chess') { 
					stop('not implemented for large files yet')
				}
				pbStep(pb, i)
			}
			pbClose(pb)
			out <- writeStop(out)
		}
	} else {
		if ( inmem ) {
			n <- ncell(out)
			out <- setValues(out, fun(n)) 
		} else {
			out <- writeStart(out, filename=filename, ...)
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='init', ...)
			for (i in 1:tr$n) {
				n <- ncol(out) * tr$nrows[i]
				out <- writeValues(out, fun(n), tr$row[i])
				pbStep(pb, r)
			}
			pbClose(pb)
			out <- writeStop(out)
		}
	}
	if (inmem & filename != '') {
		out <- writeRaster(out, filename=filename, ...)
	}
	return(out)
}
)
