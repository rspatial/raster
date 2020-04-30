

# to do: should allow index to be a vector

setMethod('interpolate', signature(object='Raster'), 
	
	function(object, model, filename="", fun=predict, xyOnly=TRUE, xyNames=c('x','y'), ext=NULL, const=NULL, index=1, na.rm=TRUE, debug.level=1, ...) {
		
		predrast <- raster(object)
		filename <- trim(filename)
		ln <- NULL
				
		if (!is.null(ext)) {
			predrast <- crop(predrast, extent(ext))
			firstrow <- rowFromY(object, yFromRow(predrast, 1))
			firstcol <- colFromX(object, xFromCol(predrast, 1))
		} else {
			firstrow <- 1
			firstcol <- 1
		}
		ncols <- ncol(predrast)
			
		lyrnames <- names(object)
		xylyrnames <- c('x', 'y', lyrnames)

		haveFactor <- FALSE
		dataclasses <- try( attr(model$terms, "dataClasses")[-1], silent=TRUE)
		if (!is.null(dataclasses)) {
			varnames <- names(dataclasses)
			if (! inherits(dataclasses, "try-error")) {
				if ( length( unique(lyrnames[(lyrnames %in% varnames)] )) != length(lyrnames[(lyrnames %in% varnames)] )) {
					stop('duplicate names in Raster* object: ', lyrnames)
				}
				f <- names( which(dataclasses == 'factor') )
				if (length(f) > 0) { haveFactor <- TRUE } 
			}
		}
			
		
		if (!canProcessInMemory(predrast) && filename == '') {
			filename <- rasterTmpFile()	
		} 


		if (! xyOnly) {
			if (inherits(object, 'RasterStack')) {
				if (nlayers(object)==0) { 
					warning('"object" has no data, xyOnly set to TRUE')
					xyOnly <- TRUE 
				}
			} else {
				if ( !  fromDisk(object) ) {
					if (! inMemory(object) ) {
						warning('"object" has no data, xyOnly set to TRUE')
						xyOnly <- TRUE 
					}
				}				
			}
		}
		if (xyOnly) {
			na.rm <- FALSE
		}
		
		if (inherits(model, "gstat")) { 
			gstatmod <- TRUE 
			if (!is.null(model$locations) && inherits(model$locations, "formula"))  {
				# should be ~x + y  ; need to check if it is ~lon + lat; or worse ~y+x
				sp <- FALSE
			} else {
				sp <- TRUE
			}
		} else { 
			gstatmod <- FALSE 
		}
		

		tr <- blockSize(predrast, n=nlayers(object)+3)
		ablock <- 1:(ncol(predrast) * tr$nrows[1])
		napred <- rep(NA, ncol(predrast)*tr$nrows[1])
				
		pb <- pbCreate(tr$n, label='interpolate',  ... )			
		
		if (filename == '') {
			v <- matrix(NA, ncol=nrow(predrast), nrow=ncol(predrast))
		} else {
			predrast <- writeStart(predrast, filename=filename, ... )
		}

		for (i in 1:tr$n) {
			if (i==tr$n) { 
				ablock <- 1:(ncol(predrast) * tr$nrows[i])
				napred <- rep(NA, ncol(predrast) * tr$nrows[i])
			}

			rr <- firstrow + tr$row[i] - 1
		
			if (xyOnly) {
				p <- xyFromCell(predrast, ablock + (tr$row[i]-1) * ncol(predrast)) 
				p <- stats::na.omit(p)
				blockvals <- data.frame(x=p[,1], y=p[,2])
			} else {
				blockvals <- data.frame(getValuesBlock(object, row=rr, nrows=tr$nrows[i], firstcol, ncols))
				colnames(blockvals) <- lyrnames # necessary if there is only one layer
				
				p <- xyFromCell(predrast, ablock + (tr$row[i]-1) * ncol(predrast)) 
				blockvals <- cbind(data.frame( x=p[,1], y=p[,2]), blockvals) 

			} 
			if (!is.null(const)) {
				blockvals <- cbind(blockvals, const)
			}
			if (haveFactor) {
				for (j in 1:length(f)) {
					blockvals[,f[j]] <- as.factor(blockvals[,f[j]])
				}
			}
			
			colnames(blockvals)[1:2] <- xyNames[1:2]
			
			if (gstatmod) { 
				if (sp) { 
					row.names(p) <- 1:nrow(p)
					blockvals <- SpatialPointsDataFrame(coords=p, data = blockvals, proj4string=.getCRS((predrast)))
				}
				if (i == 1) { 
					predv <- predict(model, blockvals, debug.level=debug.level, ...) 
					ln <- names(predv)[index]
				} else { 
					predv <- predict(model, blockvals, debug.level=0, ...) 
				}
				if (sp) { 
					predv <- predv@data[,index] 
				} else { 
					predv <- predv[,index+2] 
				}
					
			} else {  
			
				if (na.rm) {  
					blockvals <- stats::na.omit(blockvals)		
				}
				if (nrow(blockvals) == 0 ) {
					predv <- napred
				} else {
					predv <- fun(model, blockvals, ...)	
				}

				if (class(predv)[1] == 'list') {
					predv <- unlist(predv, use.names = FALSE)
					if (length(predv) != nrow(blockvals)) {
						predv <- matrix(predv, nrow=nrow(blockvals))
					}					
				}
				if (isTRUE(dim(predv)[2] > 1)) {
					predv = predv[,index]
				}						
				if (na.rm) {  
					naind <- as.vector(attr(blockvals, "na.action"))
					if (!is.null(naind)) {
						p <- napred
						p[-naind] <- predv
						predv <- p
						rm(p)
					}
				}
				
				# to change factor to numeric; should keep track of this to return a factor type RasterLayer
				predv <- as.numeric(predv)
							
			}
			
			if (filename == '') {
				predv = matrix(predv, nrow=ncol(predrast))
				cols = tr$row[i]:(tr$row[i]+dim(predv)[2]-1)
				v[,cols] <- predv 
			} else {
				predrast <- writeValues(predrast, predv, tr$row[i])
			}
			pbStep(pb, i) 
		}
		pbClose(pb)

		if (gstatmod) { 
			names(predrast) <- ln
		}
		
		if (filename == '') {
			predrast <- setValues(predrast, as.numeric(v))  # or as.vector
		} else {
			predrast <- writeStop(predrast)
		}
		
		return(predrast)
	}
)
