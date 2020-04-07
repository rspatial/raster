# Author: Robert J. Hijmans
# Date : November 2009
# Version 1.0
# Licence GPL v3



setMethod('mask', signature(x='Raster', mask='sf'), 
	function(x, mask, ...) {
		mask <- .sf2sp(mask)
		mask(x, mask, ...)
	}
)


setMethod('mask', signature(x='Raster', mask='Spatial'), 
	function(x, mask, filename="", inverse=FALSE, updatevalue=NA, updateNA=FALSE, ...){ 
		if (inherits(mask, 'SpatialPolygons')) {
			m <- .fasterize(mask, x, values=rep(1,length(mask)))
		} else {
			m <- rasterize(mask, x, 1, silent=TRUE)
		}
		mask(x, m, filename=filename, inverse=inverse, maskvalue=NA, updatevalue=updatevalue, ...)
	} 
)



setMethod('mask', signature(x='RasterLayer', mask='RasterLayer'), 
function(x, mask, filename="", inverse=FALSE, maskvalue=NA, updatevalue=NA, updateNA=FALSE, ...){ 

	maskvalue <- maskvalue[1]
	updatevalue <- updatevalue[1]

	compareRaster(x, mask)
	out <- .copyWithProperties(x)	
	
	if ( canProcessInMemory(x, 3)) {

		x <- getValues(x)
		mask <- getValues(mask)
		if (is.na(maskvalue)) {
			if (updateNA) {
				if (inverse) {
					x[!is.na(mask)] <- updatevalue
				} else {
					x[is.na(mask)] <- updatevalue
				}			
			} else {
				if (inverse) {
					x[!is.na(mask) & !is.na(x)] <- updatevalue
				} else {
					x[is.na(mask) & !is.na(x)] <- updatevalue
				}
			}
		} else {
			if (updateNA) {
				if (inverse) {
					x[mask != maskvalue] <- updatevalue
				} else {
					x[mask == maskvalue] <- updatevalue
				}
			} else {
				if (inverse) {
					x[mask != maskvalue & !is.na(x)] <- updatevalue
				} else {
					x[mask == maskvalue & !is.na(x)] <- updatevalue
				}
			}
		}
		x <- setValues(out, x)
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return(x)
		
	} else {

		if (filename=='') { 	
			filename <- rasterTmpFile() 
		}

		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='mask', ...)

		if (is.na(updatevalue)) {
			if (is.na(maskvalue)) {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[!is.na(m)] <- NA
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 		
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[is.na(m)] <- NA
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			} else {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[m != maskvalue] <- NA
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 		
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[m==maskvalue] <- NA
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			}
		} else {
			if (is.na(maskvalue)) {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[!is.na(m) & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 		
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[is.na(m) & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			} else {
				if (updateNA) {
					if (inverse) {
						for (i in 1:tr$n) {
							v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
							m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
							v[m != maskvalue] <- updatevalue
							out <- writeValues(out, v, tr$row[i])
							pbStep(pb, i)
						} 		
					} else {
						for (i in 1:tr$n) {
							v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
							m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
							v[m==maskvalue] <- updatevalue
							out <- writeValues(out, v, tr$row[i])
							pbStep(pb, i)
						} 
					}				
				} else {
					if (inverse) {
						for (i in 1:tr$n) {
							v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
							m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
							v[m != maskvalue & !is.na(v)] <- updatevalue
							out <- writeValues(out, v, tr$row[i])
							pbStep(pb, i)
						} 		
					} else {
						for (i in 1:tr$n) {
							v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
							m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
							v[m==maskvalue & !is.na(v)] <- updatevalue
							out <- writeValues(out, v, tr$row[i])
							pbStep(pb, i)
						} 
					}
				}
			}
		}


		pbClose(pb)
		out <- writeStop(out)
		return(out)
	}
}
)


setMethod('mask', signature(x='RasterStackBrick', mask='RasterLayer'), 
function(x, mask, filename="", inverse=FALSE, maskvalue=NA, updatevalue=NA, updateNA=FALSE, ...){ 

	compareRaster(x, mask)
	maskvalue <- maskvalue[1]
	updatevalue <- updatevalue[1]
	
	out <- .copyWithProperties(x)	

	
	if (canProcessInMemory(x, nlayers(x)+4)) {

		x <- getValues(x)
		
		if (is.na(maskvalue)) {
			if (updateNA) {
				if (inverse) {
					x[!is.na(getValues(mask))] <- updatevalue
				} else {
					x[is.na(getValues(mask))] <- updatevalue
				}
			} else {
				if (inverse) {
					x[!is.na(getValues(mask)) & !is.na(x)] <- updatevalue
				} else {
					x[is.na(getValues(mask)) & !is.na(x)] <- updatevalue
				}
			}
		} else {
			if (updateNA) {
				if (inverse) {
					x[getValues(mask) != maskvalue] <- updatevalue
				} else {
					x[getValues(mask) == maskvalue] <- updatevalue
				}
			} else {
				if (inverse) {
					x[getValues(mask) != maskvalue & !is.na(x)] <- updatevalue
				} else {
					x[getValues(mask) == maskvalue & !is.na(x)] <- updatevalue
				}
			}
		}
		out <- setValues(out, x)
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		} 
		return(out)
		
	} else {

		if ( filename=='') { 
			filename <- rasterTmpFile() 
		}

		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='mask', ...)

		if (is.na(updatevalue)) {

			if (is.na(maskvalue)) {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[!is.na(m)] <- NA
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[is.na(m)] <- NA
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			} else {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[m != maskvalue] <- NA
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[m == maskvalue] <- NA
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			}

		
		} else {
			if (is.na(maskvalue)) {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[!is.na(m) & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[is.na(m) & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			} else {
			
				if (updateNA) {
					if (inverse) {
						for (i in 1:tr$n) {
							v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
							m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
							v[m != maskvalue] <- updatevalue
							out <- writeValues(out, v, tr$row[i])
							pbStep(pb, i)
						} 
					} else {
						for (i in 1:tr$n) {
							v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
							m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
							v[m == maskvalue] <- updatevalue
							out <- writeValues(out, v, tr$row[i])
							pbStep(pb, i)
						} 
					}
					
				} else {
				
					if (inverse) {
						for (i in 1:tr$n) {
							v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
							m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
							v[m != maskvalue & !is.na(v)] <- updatevalue
							out <- writeValues(out, v, tr$row[i])
							pbStep(pb, i)
						} 
					} else {
						for (i in 1:tr$n) {
							v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
							m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
							v[m == maskvalue & !is.na(v)] <- updatevalue
							out <- writeValues(out, v, tr$row[i])
							pbStep(pb, i)
						} 
					}
				}
			}
		}
		pbClose(pb)
		out <- writeStop(out)
		return(out)
	}
}
)


setMethod('mask', signature(x='RasterLayer', mask='RasterStackBrick'), 
function(x, mask, filename="", inverse=FALSE, maskvalue=NA, updatevalue=NA, updateNA=FALSE, ...){ 

	compareRaster(x, mask)

	out <- brick(mask, values=FALSE)
	maskvalue <- maskvalue[1]
	updatevalue <- updatevalue[1]
	
	if (canProcessInMemory(mask, nlayers(x)*2+2)) {

		x <- getValues(x)
		x <- matrix(rep(x, nlayers(out)), ncol=nlayers(out))
		
		if (updateNA) {

			if (is.na(maskvalue)) {
				if (inverse) {
					x[!is.na(getValues(mask))] <- updatevalue
				} else {
					x[is.na(getValues(mask))] <- updatevalue
				}
			} else {
				if (inverse) {
					x[getValues(mask)!=maskvalue] <- updatevalue
				} else {
					x[getValues(mask)==maskvalue] <- updatevalue
				}
			}
		} else {	
			if (is.na(maskvalue)) {
				if (inverse) {
					x[!is.na(getValues(mask)) & !is.na(x)] <- updatevalue
				} else {
					x[is.na(getValues(mask)) & !is.na(x)] <- updatevalue
				}
			} else {
				if (inverse) {
					x[getValues(mask)!=maskvalue & !is.na(x)] <- updatevalue
				} else {
					x[getValues(mask)==maskvalue & !is.na(x)] <- updatevalue
				}
			}
		}	
		out <- setValues(out, x)
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
		} 
		return(out)
		
	} else {
	
		if ( filename=='') { filename <- rasterTmpFile() }
		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='mask', ...)

		if (updateNA) {

			if (is.na(maskvalue)) {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						v <- matrix(rep(v, nlayers(out)), ncol=nlayers(out))
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[!is.na(m)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						v <- matrix(rep(v, nlayers(out)), ncol=nlayers(out))
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[is.na(m)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			} else {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						v <- matrix(rep(v, nlayers(out)), ncol=nlayers(out))
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[m != maskvalue] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						v <- matrix(rep(v, nlayers(out)), ncol=nlayers(out))
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[m == maskvalue] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			}
		} else {
		
			if (is.na(maskvalue)) {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						v <- matrix(rep(v, nlayers(out)), ncol=nlayers(out))
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[!is.na(m) & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						v <- matrix(rep(v, nlayers(out)), ncol=nlayers(out))
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[is.na(m) & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			} else {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						v <- matrix(rep(v, nlayers(out)), ncol=nlayers(out))
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[m != maskvalue & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						v <- matrix(rep(v, nlayers(out)), ncol=nlayers(out))
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[m == maskvalue & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			}
		}
		
		pbClose(pb)
		out <- writeStop(out)
		return(out)
	}
}
)



setMethod('mask', signature(x='RasterStackBrick', mask='RasterStackBrick'), 
function(x, mask, filename="", inverse=FALSE, maskvalue=NA, updatevalue=NA, updateNA=FALSE, ...){ 


	nlx <- nlayers(x)
	nlk <- nlayers(mask)
	if ( nlx != nlk ) {
		if (nlx == 1) {
			x <- raster(x, 1)
			return(mask(x, mask, filename=filename, inverse=inverse, maskvalue=maskvalue, updatevalue=updatevalue, ...))
		} 
		if (nlk == 1) {
			mask <- raster(mask, 1)
			return(mask(x, mask, filename=filename, inverse=inverse, maskvalue=maskvalue, updatevalue=updatevalue, ...))
		}
		
		if (! ((nlx > nlk) & (nlx %% nlk == 0)) ) {
			stop('number of layers of x and mask must be the same,\nor one of the two should be 1, or the number of layers of x\nshould be divisible by the number of layers of mask')
		}
	}
	
	updatevalue <- updatevalue[1]
	maskvalue <- maskvalue[1]
	
	compareRaster(x, mask)
	out <- brick(x, values=FALSE)
	ln <- names(x)
	names(out) <- ln
	
	if (canProcessInMemory(x, nlx*2)) {

		x <- getValues(x)
		
		if (updateNA) {
		
			if (is.na(maskvalue)) {
				if (inverse) {
					x[!is.na(as.vector(getValues(mask)))] <- updatevalue
				} else {
					x[is.na(as.vector(getValues(mask)))] <- updatevalue
				}
			} else {
				if (inverse) {
					x[as.vector(getValues(mask)) != maskvalue] <- updatevalue
				} else {
					x[as.vector(getValues(mask)) == maskvalue] <- updatevalue
				}
			}

		
		} else {
			if (is.na(maskvalue)) {
				if (inverse) {
					x[!is.na(as.vector(getValues(mask))) & !is.na(x)] <- updatevalue
				} else {
					x[is.na(as.vector(getValues(mask))) & !is.na(x)] <- updatevalue
				}
			} else {
				if (inverse) {
					x[as.vector(getValues(mask)) != maskvalue  & !is.na(x)] <- updatevalue
				} else {
					x[as.vector(getValues(mask)) == maskvalue & !is.na(x)] <- updatevalue
				}
			}
		}
		out <- setValues(out, x)
		if (filename != '') {
			out <- writeRaster(out, filename, ...)
			names(out) <- ln
		} 
		return(out)
		
	} else {

		if ( filename=='') { filename <- rasterTmpFile() }

		out <- writeStart(out, filename=filename, ...)
		tr <- blockSize(out)
		pb <- pbCreate(tr$n, label='mask', ...)

		if (updateNA) {
			if (is.na(maskvalue)) {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[as.vector(!is.na(m))] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[as.vector(is.na(m))] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			} else {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[as.vector(m != maskvalue)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[as.vector(m == maskvalue)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 	
				}
			}
		} else {
			if (is.na(maskvalue)) {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[as.vector(!is.na(m)) & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[as.vector(is.na(m)) &  !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				}
			} else {
				if (inverse) {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[as.vector(m != maskvalue) & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 
				} else {
					for (i in 1:tr$n) {
						v <- getValues( x, row=tr$row[i], nrows=tr$nrows[i] )
						m <- getValues( mask, row=tr$row[i], nrows=tr$nrows[i] )
						v[as.vector(m == maskvalue) & !is.na(v)] <- updatevalue
						out <- writeValues(out, v, tr$row[i])
						pbStep(pb, i)
					} 	
				}
			}
		}
		pbClose(pb)
		out <- writeStop(out)
		names(out) <- ln
		return(out)
	}
}
)

