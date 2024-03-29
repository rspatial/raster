# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3


# name overlap with igraph


setMethod('boundaries', signature(x='RasterLayer'), 
function(x, type='inner', classes=FALSE, directions=8, asNA=FALSE, filename="", ...) {

	stopifnot( nlayers(x) == 1 )
	stopifnot( hasValues(x) )
	filename <- trim(filename)
	
	out <- raster(x)
	gll <- as.integer( .isGlobalLonLat(out) )

	type <- tolower(type)
	if (! type %in% c('inner', 'outer')) {
		stop("type must be 'inner', or 'outer'")
	}
		
	if (type=='inner') { 
		type <- FALSE
	} else { 
		type <- TRUE
	}
	classes <- as.logical(classes)
	directions <- as.integer(directions)
	stopifnot(directions %in% c(4,8))
	
	
#	asZero <- as.integer(as.logical(asZero))
	
	
	datatype <- list(...)$datatype
	if (is.null(datatype)) {
		datatype <- 'INT2S'
	}
	
	if (canProcessInMemory(out, 4)) {
		x <- as.matrix(x)
		if (gll) {
			x <- cbind(x[, ncol(x)], x, x[, 1]) 
		} else {
			x <- cbind(x[, 1], x, x[, ncol(x)]) 
		}
		x <- rbind(x[1,], x, x[nrow(x),])
		paddim <- as.integer(dim(x))
		x <- .edge(round(t(x)), paddim, classes[1], type[1], directions[1])
		if (asNA) {
			x[x==0] <- as.integer(NA)
		}
		x <- matrix(x, nrow=paddim[1], ncol=paddim[2], byrow=TRUE)
		x <- x[2:(nrow(x)-1), 2:(ncol(x)-1)]
		x <- setValues(out, as.vector(t(x)))
		if (filename  != '') {
			x <- writeRaster(x, filename, datatype=datatype, ...)
		}
		return(x)

	} else {
	
		out <- writeStart(out, filename, datatype=datatype, ...)
		tr <- blockSize(out, minblocks=3, minrows=3)
		pb <- pbCreate(tr$n, label='boundaries', ...)
		
		nc <- ncol(out)+2
		v <- getValues(x, row=1, nrows=tr$nrows[1]+1)
		v <- matrix(v, ncol=tr$nrows[1]+1)
		if (gll) {
			v <- rbind(v[nrow(v),], v, v[1,])
		} else {
			v <- rbind(v[1,], v, v[nrow(v),])
		}
		v <- round(cbind(v[,1], v))
		
		v <- .edge(v, as.integer(c(tr$nrows[1]+2, nc)),  classes, type, directions)
		if (asNA) {
			v[v==0] <- as.integer(NA)
		}
		v <- matrix(v, ncol=nc, byrow=TRUE)
		v <- as.integer(t(v[2:(nrow(v)-1), 2:(ncol(v)-1)]))
		out <- writeValues(out, v, 1)
		pbStep(pb, 1)
		if (tr$n > 2) {
			for (i in 2:(tr$n-1)) {
				v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+2)
				v <- matrix(v, ncol=tr$nrows[1]+2)
				if (gll) {
					v <- rbind(v[nrow(v),], v, v[1,])
				} else {
					v <- rbind(v[1,], v, v[nrow(v),])
				}
				v <- .edge(round(v), as.integer(c(tr$nrows[i]+2, nc)), classes, type, directions)
				v <- matrix(v, ncol=nc, byrow=TRUE)
				v <- as.integer(t(v[2:(nrow(v)-1), 2:(ncol(v)-1)]))
				out <- writeValues(out, v, tr$row[i])
				pbStep(pb, i)
			}
		}
		i <- tr$n
		v <- getValues(x, row=tr$row[i]-1, nrows=tr$nrows[i]+1)
		v <- matrix(v, ncol=tr$nrows[i]+1)
		if (gll) {
			v <- rbind(v[nrow(v),], v, v[1,])
		} else {
			v <- rbind(v[1,], v, v[nrow(v),])
		}
		v <- round(cbind(v, v[,ncol(v)]))
		v <- .edge(v, as.integer(c(tr$nrows[i]+2, nc)), classes, type, directions)
		v <- matrix(v, ncol=nc, byrow=TRUE)
		v <- as.integer(t(v[2:(nrow(v)-1), 2:(ncol(v)-1)]))
		out <- writeValues(out, v, tr$row[i])
		pbStep(pb, tr$n)

		out <- writeStop(out)
		pbClose(pb)
	}
	return(out)
}
)


