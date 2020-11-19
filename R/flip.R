# Author: Robert J. Hijmans
# Date :  June 2008
# Version 0.9
# Licence GPL v3


setMethod('flip', signature(x='RasterLayer'), 
	function(x, direction='y', filename='', ...)  {
	
		filename <- trim(filename)
		outRaster <- .copyWithProperties(x)	
	
		if (direction[1] == 1) { 
			direction <- 'x'
		} else if (direction[1] == 2) { 
			direction <- 'y' 
		}
		if (!(direction %in% c('y', 'x'))) {
			stop('direction should be "y" or "x"')
		}
	
		if (!canProcessInMemory(outRaster, 2) && filename == '') {
			filename <- rasterTmpFile()
			inmemory = FALSE
		} else {
			inmemory = TRUE
		}
		
		if ( inmemory ) {
			x <- getValues(x, format='matrix')

			if (direction == 'y') {
				x <- x[nrow(x):1,]
			} else {
				x <- x[,ncol(x):1]
			}
			outRaster <- setValues(outRaster, as.vector(t(x)))
			if (filename != '') {
				outRaster = writeRaster(outRaster, filename=filename, ...)
			}
			
		} else {
			tr <- blockSize(outRaster)
			pb <- pbCreate(tr$n, label='flip', ...)
			outRaster <- writeStart(outRaster, filename=filename, datatype=dataType(x), ... )

			if (direction == 'y') {
				nr <- nrow(outRaster)
				for (i in 1:tr$n) {
					v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					v <- matrix(v, ncol=ncol(x), byrow=TRUE)
					v <- as.vector(t(v[nrow(v):1, ]))
					rownr <- nr - tr$row[i] - tr$nrows[i] + 2
					outRaster <- writeValues(outRaster, v, rownr)
					pbStep(pb, i) 
				}
			} else {
				for (i in 1:tr$n) {
					v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					v <- matrix(v, ncol=ncol(x), byrow=TRUE)
					v <- as.vector(t(v[, ncol(v):1]))
					outRaster <- writeValues(outRaster, v, tr$row[i])
					pbStep(pb, i) 
				}
			}
			outRaster <- writeStop(outRaster)
			pbClose(pb)
		}
		return(outRaster)
	}
)



setMethod('flip', signature(x='RasterStackBrick'), 
	function(x, direction='y', filename='', ...)  {
	
		filename <- trim(filename)
		outRaster <- brick(x, values=FALSE)

		if (direction[1] == 1) { 
			direction <- 'x'
		} else if (direction[1] == 2) { 
			direction <- 'y' 
		}
		if (!(direction %in% c('y', 'x'))) {
			stop('directions should be y or x')
		}
	
		if (!canProcessInMemory(outRaster, 2) && filename == '') {
			filename <- rasterTmpFile()
			inmemory = FALSE
		} else {
			inmemory = TRUE
		}

		nc <- outRaster@ncols
		
		if ( inmemory ) {
			x <- getValues(x)
			for (i in 1:NCOL(x)) {
				v <- matrix(x[,i], ncol=nc, byrow=TRUE)
				if (direction == 'y') {
					v <- v[nrow(v):1,]
				} else {
					v <- v[,ncol(v):1]
				}
				x[,i] <- as.vector(t(v))
			}
			outRaster <- setValues(outRaster, x)
			if (filename != '') {
				outRaster = writeRaster(outRaster, filename=filename, ...)
			}
			
		} else {

			tr <- blockSize(outRaster)
			pb <- pbCreate(tr$n, label='flip', ...)
			if (inherits(x, 'RasterStack')) { 
				dtype <- 'FLT4S'
			} else {
				dtype <- dataType(x)
			}
			outRaster <- writeStart(outRaster, filename=filename, datatype=dtype, ... )

			if (direction == 'y') {
				trinv <- tr
				trinv$row <- rev(trinv$row)
				trinv$nrows <- rev(trinv$nrows)
				trinv$newrows <- cumsum(c(1,trinv$nrows))[1:length(trinv$nrows)]
				for (i in 1:tr$n) {
					vv <- getValues(x, row=trinv$row[i], nrows=trinv$nrows[i])
					for (j in 1:NCOL(vv)) {
						v <- matrix(vv[,j], nrow=nc)
						vv[,j] <- as.vector(v[, ncol(v):1])
					}
					outRaster <- writeValues(outRaster, vv, trinv$newrows[i])
					pbStep(pb, i) 
				}
				
			} else {
			
				for (i in 1:tr$n) {
					vv = getValues(x, row=tr$row[i], nrows=tr$nrows[i])
					for (j in 1:NCOL(vv)) {
						v <- matrix(vv[,j], nrow=nc)
						vv[,j] <- as.vector(v[nrow(v):1, ])
					}
					outRaster <- writeValues(outRaster, vv, tr$row[i])
					pbStep(pb, i) 
				}  
			}
			
			outRaster <- writeStop(outRaster)
			pbClose(pb)
		}
		return(outRaster)
	}
)


