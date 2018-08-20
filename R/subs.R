# Authors: Robert J. Hijmans 
# Date :  February 2010
# Version 0.9
# Licence GPL v3

	
if (!isGeneric("subs")) {
	setGeneric("subs", function(x, y, ...)
		standardGeneric("subs"))
}


.localmerge <- function(x, y, subNA, byc=1) {
	
	if (byc==1) {
		nc <- NCOL(x)
		nr <- NROW(x)
		x <- cbind(1:length(x), as.vector(x))
		if (! subNA ) {
			y <- merge(x, y, by.x=2, by.y=1)
			x[y[,2], 2] <- y[,3]
			x <- x[,2]
			if (nc > 1) {
				x <- matrix(as.vector(x), nrow=nr)
			}
		} else {
			x <- as.matrix(merge(x, y, by.x=2, by.y=1, all.x=TRUE))
			x <- x[order(x[,2]), -c(1:2)]
		}
		if (nc > 1) {
			x <- matrix(as.vector(x), nrow = nr)
		}

	} else {
		x <- cbind(1:nrow(x), x)
		x <- as.matrix(merge(x, y, by.x=(1:byc)+1, by.y=1:byc, all.x=TRUE))
		x <- x[, -(1:byc)]
		x <- x[order(x[,1]), -1]
	}

	return(x)
}


setMethod('subs', signature(x='Raster', y='data.frame'), 
	function(x, y, by=1, which=2, subsWithNA=TRUE, filename='', ...)  { 

		
		if (!subsWithNA) {
			if (length(which) > 1) {
				stop('you cannot use subsWithNA=FALSE if length(which) > 1')
			}
			if (length(by) > 1) {
				stop('you cannot use subsWithNA=FALSE if length(by) > 1')
			}			
		}
		
		stopifnot(length(by) == 1 | length(by) == nlayers(x))
		
		if (is.character(by)) {
			by <- match(by, colnames(y))
			if (any(is.na(by))) {
				stop("'by' is not a valid column name") 
			}
		}
		if (is.character(which)) {
			which <- which(which == colnames(y))[1]
			if (is.na(which)) { stop("'which' is not valid column name") }
		}
		
		byc <- length(by)
		
		y <- y[ , c(by, which)]

		tt <- table(y[,by])
		tt <- tt[ which(tt > 1) ]
		if (length(tt) > 0) {
			stop('duplicate "by" values not allowed')
		}

		out <- raster(x)
		nlx <- nlayers(x)
		
		cls <- sapply(y, class)
		hasfactor <- rep(FALSE, length(cls)-1)
		levs <- list()
		for (i in 2:length(cls)) {
			if (cls[i] == 'character') {
				w <- getOption('warn')
				options('warn'=-1) 
				tmp <- as.numeric(y[,i])
				options('warn'= w)
				if (all(is.na(tmp) == is.na(y[,i]))) {
					y[,i] <- tmp
					cls[i] <- 'numeric'				
				} else {
					y[,i] <- factor(y[,i])
					cls[i] <- 'factor'
				}
			}
			if (cls[i] == 'factor') {
				uny <- unique(y[,i])
				lv <- data.frame(ID=1:length(uny), uny)
				colnames(lv)[2] <- colnames(y)[i]
				levs[[i-1]] <- lv
				hasfactor[i-1] <- TRUE
				m <- match(y[,i], uny)
				y[,i] <- m #as.numeric(uny[m])
			}
		}
		
		if (nlx == 1) {
			ln <- colnames(y)[which]
			if (length(which) > 1) {
				out <- brick(out, nl=length(which))
			}
		} else {
			if (byc == 1) {
				out <- brick(out, nl=nlx * length(which))
				ln <- rep(names(x), length(which))
				if (length(which) > 1) {
					ln2 <- rep(colnames(y)[which], each=nlx)
					ln <- paste(ln, paste('_', ln2, sep=''), sep='')
				}
			} else {
				if (length(which) > 1) {
					out <- brick(out, nl=length(which))
				}
				ln <- colnames(y)[which]
			} 
		} 
		names(out) <- ln

		filename <- trim(filename)
		
		if (canProcessInMemory(x, 3)) {
			if (any(hasfactor)) {
				out@data@isfactor <- hasfactor
				out@data@attributes <- levs
			}
			v <- .localmerge( getValues(x), y, subsWithNA, byc )
			out <- setValues(out, v)
			if (filename != '') {
				out <- writeRaster(out, filename=filename, ...)
			}
			return(out)
			
		} else {
			if (filename == '') {
				filename <- rasterTmpFile()
			}
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='subs', ...)
			out <- writeStart(out, filename=filename, ...)
			for (i in 1:tr$n) {
				v <- getValues(x, row=tr$row[i], nrows=tr$nrows[i])
				out <- writeValues(out, .localmerge(v, y, subsWithNA, byc), tr$row[i])
				pbStep(pb) 
			}
			pbClose(pb)	
			
			if (any(hasfactor)) {
				out@data@isfactor <- TRUE
				out@data@attributes <- levs
			}		
			out <- writeStop(out)
			return(out)
		}
	}
)



