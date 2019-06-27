# Author: Robert J. Hijmans
# Date :  August 2009
# Version 0.9
# Licence GPL v3


setMethod('predict', signature(object='Raster'), 
	function(object, model, filename="", fun=predict, ext=NULL, const=NULL, index=1, na.rm=TRUE, inf.rm=FALSE, factors=NULL, format, datatype, overwrite=FALSE, progress='', ...) {
	
		filename <- trim(filename)
		if (missing(format)) { format <- .filetype(filename=filename) } 
		if (missing(datatype)) { datatype <- .datatype() } 
	
		if ( ! hasValues(object) ) {
			stop('No values associated with this Raster object')
		}
	
		if (inherits(model, 'DistModel')) {	# models defined in package 'dismo'
			return ( predict(model, object, filename=filename, ext=ext, progress=progress, format=format, overwrite=overwrite, ...) ) 
		}

		if (length(index) > 1) {
			predrast <- brick(object, values=FALSE, nl=length(index))
		} else {
			predrast <- raster(object)
		}
				
		if (!is.null(ext)) {
			predrast <- crop(predrast, extent(ext))
			firstrow <- rowFromY(object, yFromRow(predrast, 1))
			firstcol <- colFromX(object, xFromCol(predrast, 1))
		} else {
			firstrow <- 1
			firstcol <- 1
		}
		ncols <- ncol(predrast)
		if (ncol(predrast) < ncol(object)) {
			gvb <- TRUE
		} else {
			gvb <- FALSE
		}
			
		lyrnames <- names(object)
		
		haveFactor <- FALSE
		facttest <- TRUE

		if (!is.null(factors)) {
			stopifnot(is.list(factors))
			f <- names(factors)
			if (any(trimws(f) == "")) {
				stop("all factors must be named")
			}
		} else {
			if (inherits(model, "randomForest")) {
				f <- names(which(sapply(model$forest$xlevels, max) != "0"))
				if (length(f) > 0) { 
					factors <- model$forest$xlevels[f]
				}
			} else if (inherits(model, "gbm")) {
				dafr <- model$gbm.call$dataframe 
				vars <- model$gbm.call$gbm.x
				dafr <- dafr[,vars]
				i <- sapply(dafr, is.factor)
				if (any(i)) {
					j <- which(i)
					factors <- list()
					for (k in 1:length(j)) {
						factors[[k]] <- levels(dafr[[ j[k] ]])
					}
					f <- colnames(dafr)[j]
				}
			} else { #glm and others
				try(factors <- model$xlevels, silent=TRUE)
				f <- names(factors)
			}		
		}
		if (length(factors) > 0) {
			haveFactor <- TRUE 
			lyrnamesc <- c(lyrnames, names(const))
			if (!all(f %in% lyrnamesc)) {
				ff <- f[!(f %in% lyrnamesc)]
				warning(paste("factor name(s):", paste(ff, collapse=", "), " not in layer names"))
				f[(f %in% lyrnamesc)]
			}			
		}
		if (!canProcessInMemory(predrast) && filename == '') {
			filename <- rasterTmpFile()
		} 

		if (filename == "") {
			v <- matrix(NA, ncol=nlayers(predrast), nrow=ncell(predrast))
		} else {
			predrast <- writeStart(predrast, filename=filename, format=format, datatype=datatype, overwrite=overwrite )
		}
		
		tr <- blockSize(predrast, n=nlayers(predrast)+3)
		
		napred <- matrix(rep(NA, ncol(predrast) * tr$nrows[1] * nlayers(predrast)), ncol=nlayers(predrast))
		factres	<- FALSE
		pb <- pbCreate(tr$n,  progress=progress, label='predict' )			

		for (i in 1:tr$n) {
		
			if (i==tr$n) {
				ablock <- 1:(ncol(object) * tr$nrows[i])
				napred <- matrix(rep(NA, ncol(predrast) * tr$nrows[i] * nlayers(predrast)), ncol=nlayers(predrast))
			}

			rr <- firstrow + tr$row[i] - 1

			if (gvb) {
				blockvals <- data.frame(getValuesBlock(object, row=rr, nrows=tr$nrows[i], firstcol, ncols))
			} else {
				blockvals <- data.frame(getValues(object, row=rr, nrows=tr$nrows[i]))	# faster
			}
			# need to do this if using a single variable
			colnames(blockvals) <- lyrnames
			
			if (! is.null(const)) {
				blockvals <- cbind(blockvals, const)
			} 
			if (haveFactor) {
				for (j in 1:length(f)) {
					flev <- factors[[j]]
					fv <- blockvals[, f[j]]
					fv[!(fv %in% flev)] <- NA 
					blockvals[,f[j]] <- factor(fv, levels=flev)
				}
			}
			

			if (na.rm) { 
				if (inf.rm) {
					blockvals[!is.finite(as.matrix(blockvals))] <- NA
				}
				blockvals <- stats::na.omit(blockvals)						
			}

			nrb <- nrow(blockvals)
			if (nrb == 0 ) {
				predv <- napred
			} else {
	
				predv <- fun(model, blockvals, ...)
		
				if (class(predv)[1] == 'list') {
					predv <- unlist(predv, use.names = FALSE)
					if (length(predv) != nrow(blockvals)) {
						predv <- matrix(predv, nrow=nrow(blockvals))
					}					
				} else if (is.array(predv)) {
					predv <- as.matrix(predv)
				}
				
				if (isTRUE(dim(predv)[2] > 1)) {
					predv <- predv[,index, drop=FALSE]
					for (fi in 1:ncol(predv)) {
						if (is.factor(predv[,fi])) {
							predv[,fi] <- as.integer(as.character(predv[,fi]))
						}
					}
					# if data.frame
					predv <- as.matrix(predv)
					
				} else if (is.factor(predv)) {
					# should keep track of this to return a factor type RasterLayer
					factres <- TRUE
					if (facttest) {
						w <- getOption('warn')
						options('warn'=-1) 
						tst <- as.integer(as.character(levels(predv)))
						options('warn'= w) 
						if (any(is.na(tst))) {
							factaschar = FALSE
						} else {
							factaschar = TRUE
						}
						levs <- levels(predv)
						predrast@data@attributes <- list(data.frame(ID=1:length(levs), value=levs))
						predrast@data@isfactor <- TRUE
						facttest <- FALSE
					}
					if (factaschar) {
						predv <- as.integer(as.character(predv))
					} else {
						predv <- as.integer(predv)
					}
				}

				if (na.rm) {  
					naind <- as.vector(attr(blockvals, "na.action"))
					if (!is.null(naind)) {
						p <- napred
						p[-naind,] <- predv
						predv <- p
						rm(p)
					}
				}
			}
			
		
			if (filename == '') {
				cells <- cellFromRowCol(predrast, tr$row[i], 1):cellFromRowCol(predrast, tr$row[i]+tr$nrows[i]-1, ncol(predrast))
				v[cells, ] <- predv 
			} else {
				predrast <- writeValues(predrast, predv, tr$row[i])
			}
			pbStep(pb, i) 
		}
		pbClose(pb)

		if (length(index) > 1) {
			try(names(predrast) <- colnames(predv), silent=TRUE)
		}

		if (filename == '') {
			predrast <- setValues(predrast, v)  # or as.vector
		} else {
			predrast <- writeStop(predrast)
		}
		return(predrast)
	}
)


