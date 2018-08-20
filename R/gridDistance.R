# Author: Jacob van Etten
# email jacobvanetten@yahoo.com
# Date :  May 2010
# Version 1.1
# Licence GPL v3

# RH: updated for igraph (from igraph0)
# sept 23, 2012



if (!isGeneric("gridDistance")) {
	setGeneric("gridDistance", function(x, ...)
		standardGeneric("gridDistance"))
}	

setMethod("gridDistance", signature("RasterLayer"), 

function(x, origin, omit=NULL, filename="", ...) {

	if( !requireNamespace("igraph")) {
		stop('you need to install the igraph0 package to be able to use this function')
	}
	if (missing(origin)) {
		stop("you must supply an 'origin' argument")
	}
	if (! hasValues(x) ) {
		stop('cannot compute distance on a RasterLayer with no data')
	}

	lonlat <- couldBeLonLat(x)
	filename <- trim(filename)
	
	if (filename != ""  & file.exists(filename)) {
		if (! .overwrite(...)) {
			stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
		}
	}
	
	# keep canProcessInMemory for debugging
	# need to test more to see how much igraph can deal with
	if ( canProcessInMemory(x, n=10) ) { 
		out <- raster(x)
		x <- getValues(x) # to avoid keeping values in memory twice
		
		oC <- which(x %in% origin) 
		ftC <- which(!(x %in% omit))
		v <- .calcDist(out, ncell(out), ftC, oC, lonlat=lonlat)
		v[is.infinite(v)] <- NA
		
		out <- setValues(out, v)
		if (filename != "") {
			out <- writeRaster(out, filename, ...)
		}
		return(out)
		
	} else 	{
	
		tr <- blockSize(x, n=1)
		pb <- pbCreate(tr$n*2 - 1, ...)

		#going up
		r1 <- writeStart(raster(x), rasterTmpFile(), overwrite=TRUE)
		for (i in tr$n:1) {
			chunk <- getValues(x, row=tr$row[i], nrows=tr$nrows[i]) 
			startCell <- (tr$row[i]-1) * ncol(x)
			chunkSize <- length(chunk)
			oC <- which(chunk %in% origin) 
			ftC <- which(!(chunk %in% omit))
			if (length(ftC) != 0) {

				if (i < tr$n) {
					firstRowftC <- firstRowftC + chunkSize 
					chunkDist <- .calcDist(x, 
								chunkSize=chunkSize + ncol(x), 
								ftC=c(ftC, firstRowftC), 
								oC=c(oC, firstRowftC), 
								perCell=c(rep(0,times=length(oC)),firstRowDist), 
								startCell=startCell,
								lonlat=lonlat)[1:chunkSize]
				} else {
					chunkDist <- .calcDist(x, chunkSize=chunkSize, 
								ftC=ftC, oC=oC, perCell=0,
								startCell=startCell, lonlat=lonlat)
				}
			} else {
				if (i < tr$n) {
					firstRowftC <- firstRowftC + chunkSize 
				}
				chunkDist <- rep(NA, tr$nrows[i] * ncol(r1))
			}
			firstRow <- chunk[1:ncol(x)]
			firstRowDist <- chunkDist[1:ncol(x)]
			firstRowftC <- which(!(firstRow %in% omit))
			firstRowDist <- firstRowDist[firstRowftC]
			chunkDist[is.infinite(chunkDist)] <- NA

			r1 <- writeValues(r1, chunkDist, tr$row[i])
			pbStep(pb) 
		}
		r1 <- writeStop(r1)
		
		#going down
		
		out <- writeStart(raster(x), filename=filename, overwrite=TRUE, ...)			
		for (i in 1:tr$n) {
			chunk <- getValues(x, row=tr$row[i], nrows=tr$nrows[i]) 
			chunkSize <- length(chunk)
			startCell <- (tr$row[i]-1) * ncol(x)
			oC <- which(chunk %in% origin) 
			ftC <- which(!(chunk %in% omit))
			
			if (length(ftC) != 0) {
			
				if (i > 1) {
					chunkDist <- getValues(r1, row=tr$row[i], nrows=tr$nrows[i]) 
					chunkDist[is.na(chunkDist)] <- Inf 
				
					chunkDist <- pmin(chunkDist,
						.calcDist(x, chunkSize=chunkSize+ncol(x), 
							ftC = c(lastRowftC, ftC+ncol(x)), 
							oC = c(lastRowftC, oC+ncol(x)), 
							perCell = c(lastRowDist, rep(0,times=length(oC))), 
							startCell = startCell - ncol(x),
							lonlat=lonlat)[-(1:ncol(r1))])
							
				} else {
					chunkDist <- getValues(r1, row=tr$row[i], nrows=tr$nrows[i])
					chunkDist[is.na(chunkDist)] <- Inf
			
					chunkDist <- pmin(chunkDist,
						.calcDist(x, chunkSize=chunkSize, 
							ftC=ftC, oC=oC, perCell=0, 
							startCell=startCell, lonlat=lonlat))
				}
			} else {			
				chunkDist <- rep(NA, tr$nrows[i] * ncol(out))						
			}

			lastRow <- chunk[(length(chunk)-ncol(x)+1):length(chunk)]
			lastRowDist <- chunkDist[(length(chunkDist)-ncol(x)+1):length(chunkDist)]
			lastRowftC <- which(!(lastRow %in% omit))
			lastRowDist <- lastRowDist[lastRowftC]
			chunkDist[is.infinite(chunkDist)] <- NA

			out <- writeValues(out, chunkDist, tr$row[i])
			pbStep(pb) 
		}
		out <- writeStop(out)
		pbClose(pb)
		return(out)
	}
}
)


.calcDist <- function(x, chunkSize, ftC, oC, perCell=0, startCell=0, lonlat) {
	
	if (length(oC) > 0) {
		#adj <- adjacency(x, fromCells=ftC, toCells=ftC, directions=8)
		adj <- adjacent(x, ftC, directions=8, target=ftC, pairs=TRUE)
		startNode <- max(adj)+1 #extra node to serve as origin
		adjP <- rbind(adj, cbind(rep(startNode, times=length(oC)), oC))
		distGraph <- igraph::graph.edgelist(adjP, directed=TRUE)
		if (length(perCell) == 1) {
			if (perCell == 0) {
				perCell <- rep(0, times=length(oC))
			}
		}

		if (lonlat) {
			distance <- pointDistance(xyFromCell(x,adj[,1]+startCell), xyFromCell(x,adj[,2]+startCell), longlat=TRUE) 
			igraph::E(distGraph)$weight <- c(distance, perCell)

		} else {
			sameRow <- which(rowFromCell(x, adj[,1]) == rowFromCell(x, adj[,2]))
			sameCol <- which(colFromCell(x, adj[,1]) == colFromCell(x, adj[,2]))
			igraph::E(distGraph)$weight <- sqrt(xres(x)^2 + yres(x)^2)
			igraph::E(distGraph)$weight[sameRow] <- xres(x)
			igraph::E(distGraph)$weight[sameCol] <- yres(x)
			igraph::E(distGraph)$weight[(length(adj[,1])+1):(length(adj[,1])+length(oC))] <- perCell
		}
		
		shortestPaths <- igraph::shortest.paths(distGraph, startNode)
		shortestPaths <- shortestPaths[-(length(shortestPaths))] #chop startNode off
		
		if (length(shortestPaths) < chunkSize) { 
			#add Inf values where shortest.paths() leaves off before completing all nodes
			shortestPaths <- c(shortestPaths, rep(Inf, times=chunkSize-length(shortestPaths))) 
		}
		
	} else {
		shortestPaths <- rep(Inf, times=chunkSize)
	}
	
	return(shortestPaths)
}
