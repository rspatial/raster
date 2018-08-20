# Author: Robert J. Hijmans
# Date : December 2010
# Version 1.0
# Licence GPL v3

	
if (!isGeneric("t")) {
	setGeneric("t", function(x)
		standardGeneric("t"))
}	


setMethod('t', signature(x='RasterLayer'), 
	function(x) {
		r <- raster(x)
		e <- eold <- extent(r)
		e@xmin <- eold@ymin
		e@xmax <- eold@ymax
		e@ymin <- eold@xmin
		e@ymax <- eold@xmax
		extent(r) <- e	
		
		dim(r) <- c(ncol(x), nrow(x))
		if (! hasValues(x)) {
			return(r)
		}
		if (canProcessInMemory(x)) {
			return(setValues(r, t(as.matrix(x))))
		} else {
			tr <- blockSize(r)
			pb <- pbCreate(tr$n)			
			r <- writeStart(r, filename=rasterTmpFile(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- getValuesBlock(x, row=1, nrows=r@ncols, col=tr$row[i], ncols=tr$nrows[i])
				v <- as.vector(matrix(v, ncol=tr$nrows[i], byrow=TRUE))
				r <- writeValues(r, v, tr$row[i])
				pbStep(pb, i) 	
			}
			r <- writeStop(r)
			pbClose(pb)
			return(r)
		}
	}
)

setMethod('t', signature(x='RasterStackBrick'), 
	function(x) {
		b <- brick(x, values=FALSE)
		e <- eold <- extent(b)
		e@xmin <- eold@ymin
		e@xmax <- eold@ymax
		e@ymin <- eold@xmin
		e@ymax <- eold@xmax
		extent(b) <- e	
		dim(b) <- c(ncol(b), nrow(b), nlayers(b))
		if (! hasValues(x)) {
			return(b)
		}
		if (canProcessInMemory(x)) {
			x <- as.array(x, transpose=TRUE)
			return( brick(x, xmn=xmin(b), xmx=xmax(b), ymn=ymin(b), ymx=ymax(b), crs=projection(b)) )
		} else {
			tr <- blockSize(b)
			pb <- pbCreate(tr$n)			
			b <- writeStart(b, filename=rasterTmpFile(), overwrite=TRUE )
			for (i in 1:tr$n) {
				v <- getValuesBlock(x, row=1, nrows=b@ncols, col=tr$row[i], ncols=tr$nrows[i])
				for (j in 1:ncol(v)) {
					v[,j] <- as.vector(matrix(v[,j], ncol=tr$nrows[i], byrow=TRUE))
				}
				b <- writeValues(b, v, tr$row[i])
				pbStep(pb, i) 	
			}
			b <- writeStop(b)
			pbClose(pb)
			return(b)
		}
	}
)

