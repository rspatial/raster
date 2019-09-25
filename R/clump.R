# Authors: Robert J. Hijmans and Jacob van Etten, 
# Date : May 2010
# Version 1.0
# Licence GPL v3

# RH: updated for igraph (from igraph0)
# sept 23, 2012



.smallClump <- function(x, directions=8) {
	x1 <- raster(x)
	val <- which(getValues(x) != 0)
	if (length(val) == 0) { 
		return( setValues(x1, NA) )
	}
	adjv <- as.vector(t(adjacent(x1, val, directions=directions, target=val, pairs=TRUE)))
	# RH. To fix problem of missing single cells, perhaps more efficient than "include=T" in adjacent
	add <- val[! val %in% adjv]		   
	adjv <- c(adjv, rep(add, each=2))  
	cl <- igraph::clusters(igraph::graph(adjv, directed=FALSE))$membership[val]
	cl <- as.numeric(as.factor(cl)) # RH force 1 to n
	x1[val] <- cl
	return(x1)
}


setMethod('clump', signature(x='RasterLayer'), 
function(x, filename='', directions=8, gaps=TRUE, ...) {

	if( !requireNamespace("igraph")) {
		stop('you need to install the igraph package to be able to use this function')
	}

	if (! directions %in% c(4,8)) { stop('directions should be 4 or 8') }

	filename <- trim(filename)
	if (filename != ""  & file.exists(filename)) {
		if (! .overwrite(...)) {
			stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
		}
	}

	datatype <- list(...)$datatype
	
	out <- raster(x)
	
	if (canProcessInMemory(out, 3)) {
		x <- .smallClump(x, directions)
		names(x) <- 'clumps'
		if (filename != '') {
			if (is.null(datatype)) {
				x <- writeRaster(x, filename, datatype='INT4S')
			} else {
				x <- writeRaster(x, filename, ...)
			}
		}
		return(x)
	} 
	# else 

	names(out) <- 'clumps'
	out <- writeStart(out, filename=rasterTmpFile(), datatype='INT4S')

	tr <- blockSize(out, minrows=3)
	pb <- pbCreate(tr$n, label='clump', ...)
	
	ext <- c(xmin(out), xmax(out), ymax(out), NA)
	maxval <- 0
	
	rcl <- matrix(nrow=0, ncol=2)
	
	for (i in 1:tr$n) {
	
		ext[4] <- yFromRow(out, tr$row[i]) + 0.5 * yres(out)
		
		endrow <- tr$row[i] + tr$nrows[i] - 1 
		ext[3] <- yFromRow(out, endrow) - 1.5 * yres(out) # one additional row for overlap
		xc <- crop(x, extent(ext))
		
		xc <- .smallClump(xc, directions) + maxval
		if (i > 1) {
			firstrow <- getValues(xc, 1)
			rc <- stats::na.omit(unique(cbind(lastrow, firstrow)))
			rcl <- rbind(rcl, rc)
		}
		lastrow <- getValues(xc, nrow(xc))
		
		mv <- maxValue(xc)
		if (!is.na(mv)) {
			maxval <- mv
		}
		out <- writeValues(out, getValues(xc, 1, tr$nrows[i]), tr$row[i])
		pbStep(pb)
	}
	out <- writeStop(out)
	pbClose(pb)
	
	
	if (nrow(rcl) > 0) {
		g <- igraph::graph.edgelist(rcl, directed=FALSE)
		clumps <- igraph::clusters(g)$membership
		rc <- cbind(igraph::V(g), clumps)
		i <- rc[,1] != rc[,2]
		rc <- rc[i, ,drop=FALSE]
		if (is.null(datatype)) {
			out <- subs(out, data.frame(rc), subsWithNA=FALSE, filename=filename, datatype='INT4S', ...)
		} else {
			out <- subs(out, data.frame(rc), subsWithNA=FALSE, filename=filename, ...)
		}
		return(out)
		
	} else if (!gaps) {
		un <- unique(out)
		un <- data.frame(cbind(un, clumps=1:length(un)))
		if (is.null(datatype)) {
			return( subs(out, un, subsWithNA=FALSE, filename=filename, datatype='INT4S', ...) )
		} else {
			return( subs(out, un, subsWithNA=FALSE, filename=filename, ...) )
		}
	} else if (filename != '') {
		if (is.null(datatype)) {
			return( writeRaster(out, filename=filename, datatype='INT4S', ...) )
		} else {
			return( writeRaster(out, filename=filename, ...) )
		}
		
	} else {
		return(out)
	}
}
)
