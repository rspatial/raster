# drain.R
# This script calculates the drainage of a point on a DEM - in R!
# written by A. Shortridge, 10/2013
# changes by Robert Hijmans

flowPath <- function(x, p, ...) {
	r <- raster(x)
	if (length(p) > 1) {
		p <- cellFromXY(r, p[1:2])
	}
	cell <- p
	row <- rowFromCell(r, cell)
	col <- colFromCell(r, cell)		
	nr <- nrow(r)
	nc <- ncol(r)
	path <- NULL
    while (!is.na(x[cell])) {  
        path <- c(path, cell)
        fd <- x[cell]
        row <- if(fd %in% c(32, 64, 128)) row - 1 else
                if(fd %in% c(8, 4, 2)) row + 1 else row
        col <- if(fd %in% c(32, 16, 8)) col - 1 else 
                if(fd %in% c(128, 1, 2)) col + 1 else col
		cell <- cellFromRowCol(r, row, col)
       # Don't drain off the raster or drain NA cells on x!
        if (is.na(x[cell])) break 
        # avoid cell i draining to j and j draining to i traps
        if (cell %in% path) break  
    }
    return(path)
}



.flowPath1 <- function(x, p) {
    # This function creates a raster with 1s representing a path from
    # the start cell to the end of the flowpath. x is a flow raster
    # created with the terrain() function in raster. Returns a raster
    # where 1 represents a part of this path and 0 is off-path.
    
	out <- raster(x)
	if (length(p) > 1) {
		p <- cellFromXY(out, p[1:2])
	}
	row <- rowFromCell(out, p)
	col <- colFromCell(out, p)
	
    out[row, col] <- 1
    while (!is.na(x[row, col])) {  # not in a pit
        out[row, col] <- 1
        fdval <- x[row, col]
        
        col <- if(fdval %in% c(32, 16, 8)) col - 1 else 
                if(fdval %in% c(128, 1, 2)) col + 1 else col
        
        row <- if(fdval %in% c(32, 64, 128)) row - 1 else
                if(fdval %in% c(8, 4, 2)) row + 1 else row
        
        # Don't drain off the raster!
        if (row < 1 || row > dim(x)[1] || col < 1 || col > dim(x)[2]) break
        # Don't drain NA cells on x!
        if (is.na(x[row, col])) break 
        # avoid cell i draining to j and j draining to i traps
        if (!is.na(out[row, col])) break  
    }
    return(out)
}
