
 
.readCellsGDAL <- function(x, cells, layers) {

	nl <- nlayers(x)
	if (nl == 1) {
		if (inherits(x, 'RasterLayer')) {
			layers <- bandnr(x)
		} else {
			layers <- 1		
		}
	}
	laysel <- length(layers)
	
	colrow <- matrix(ncol=2+laysel, nrow=length(cells))
	colrow[,1] <- colFromCell(x, cells)
	colrow[,2] <- rowFromCell(x, cells)
	colrow[,3] <- NA
	rows <- sort(unique(colrow[,2]))

	nc <- x@ncols
	con <- rgdal::GDAL.open(x@file@name, silent=TRUE)
	
	if (laysel == 1) {
		for (i in 1:length(rows)) {
			offs <- c(rows[i]-1, 0) 
			v <- rgdal::getRasterData(con, offset=offs, region.dim=c(1, nc), band = layers)
			thisrow <- colrow[colrow[,2] == rows[i], , drop=FALSE]
			colrow[colrow[,2]==rows[i], 3] <- v[thisrow[,1]]
		}
	} else {
		for (i in 1:length(rows)) {
			thisrow <- colrow[colrow[,2] == rows[i], , drop=FALSE]
			if (nrow(thisrow) == 1) {
				offs <- c(rows[i]-1, thisrow[,1]-1)
				v <- as.vector( rgdal::getRasterData(con, offset=offs, region.dim=c(1, 1)) )
				colrow[colrow[,2]==rows[i], 2+(1:laysel)] <- v[layers]

			} else {
				offs <- c(rows[i]-1, 0)
				v <- rgdal::getRasterData(con, offset=offs, region.dim=c(1, nc))
				v <- do.call(cbind, lapply(1:nl, function(i) v[,,i]))
			
				colrow[colrow[,2]==rows[i], 2+(1:laysel)] <- v[thisrow[,1], layers]
			}
		}
	}
	rgdal::closeDataset(con)
	colnames(colrow)[2+(1:laysel)] <- names(x)[layers]
	colrow[, 2+(1:laysel)]
}	




...readCellsGDAL <- function(x, cells, layers) {
# new version by kendonB via mdsumner
# https://github.com/mdsumner/raster-rforge/pull/16/files#diff-5cf48e61a52c5d9bc1d671a341f80d77

# reverted --- too slow

	nl <- nlayers(x)
	if (nl == 1) {
		if (inherits(x, 'RasterLayer')) {
			layers <- bandnr(x)
		} else {
			layers <- 1
		}
	}
	laysel <- length(layers)

	colrow <- matrix(ncol=2+laysel, nrow=length(cells))
	colrow[,1] <- colFromCell(x, cells)
	colrow[,2] <- rowFromCell(x, cells)
	
	colrow <- colrow[order(colrow[,2], colrow[,1]), , drop = FALSE]
	
	# This is one if contiguous, something else if not (except for the end of a row)
	diffrowcol <- diff(colrow[,2]) + diff(colrow[,1])
	# Block numbers
	blocknums <- cumsum(c(TRUE, diffrowcol != 1))
	
	nc <- x@ncols
	
	con <- rgdal::GDAL.open(x@file@name, silent=TRUE)
	
	if (laysel == 1) {
		for (blocknum in unique(blocknums)) {
		  block_lgl <- blocknum == blocknums
		  offs <- c(colrow[block_lgl,2][1] - 1, colrow[block_lgl, 1][1] - 1)
			v <- rgdal::getRasterData(con, offset=offs, region.dim=c(1, sum(block_lgl)), band = layers)
			
			colrow[block_lgl, 3] <- v
		}
	} else {
		for (blocknum in unique(blocknums)) {
		  block_lgl <- blocknum == blocknums
		  this_block <- colrow[block_lgl, , drop = FALSE]
		  offs <- c(colrow[block_lgl,2][1] - 1, colrow[block_lgl, 1][1] - 1)
			if (nrow(this_block) == 1) {
			  v <- as.vector( rgdal::getRasterData(con, offset=offs, region.dim=c(1, 1)) )
				colrow[block_lgl, 2+(1:laysel)] <- v[layers]
			} else {
			  v <- rgdal::getRasterData(con, offset=offs, region.dim=c(1, sum(block_lgl)), band = layers)
				v <- do.call(cbind, lapply(1:nl, function(i) v[,,i]))

				colrow[block_lgl, 2 + (1:laysel)] <- v
			}
		}
	}
	rgdal::closeDataset(con)
	colnames(colrow)[2+(1:laysel)] <- names(x)[layers]
	colrow[, 2+(1:laysel), drop = laysel == 1]
}
