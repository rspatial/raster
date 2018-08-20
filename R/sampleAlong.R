# Based on code by Barry Rowlingson
#http://r-sig-geo.2731867.n2.nabble.com/how-to-generate-perpendicular-transects-along-a-line-feature-td7583710.html

# Some adaptations by Robert Hijmans


.evenspace <- function(xy, sep, start=0.5*sep, direction=TRUE){ 

	dx <- c(0,diff(xy[,1])) 
	dy <- c(0,diff(xy[,2])) 
	dseg <- sqrt(dx^2+dy^2) 
	dtotal <- cumsum(dseg) 

	linelength <- sum(dseg) 
	pos <- seq(start,linelength, by=sep) 

	whichseg <- unlist(lapply(pos, function(x){sum(dtotal<=x)})) 

	x0 <- xy[whichseg,1]
	y0 <- xy[whichseg,2]
	x1 <- xy[whichseg+1,1]
	y1 <- xy[whichseg+1,2]
	dtotal <- dtotal[whichseg]

	further <- pos - dtotal 
	dseg <- dseg[whichseg+1]
	f <- further/dseg 
	
	x <- x0 + f * (x1-x0) 
	y <- y0 + f * (y1-y0) 

	r <- data.frame(x, y)
	
	if (direction) {
		r$direction <- atan2(y0-y1,x0-x1)
	} 
	r
} 

.transect <- function(pts, len){ 
  directionT = pts$direction+pi/2 
  dx <- len*cos(directionT) 
  dy <- len*sin(directionT) 
  data.frame(x = c(pts$x + dx, pts$x - dx), 
             y = c(pts$y + dy, pts$y - dy)) 
} 


.sampleAlong <- function(x, interval) {
	if (inherits(x, 'SpatialPolygons')) {
		line <- methods::as(line, 'SpatialLines')
	}
	if (inherits(x, 'SpatialLines')) {
		requireNamespace('raster')
		x <- geom(x)
		allpts <- NULL
		for (p in unique(x[, 'cump'])) {
			y <- x[x[, 'cump']==p, c('x', 'y')]
			pts <- .evenspace(y, interval, direction=FALSE)
			allpts <- rbind(allpts, pts)
		}
		return(allpts)
	} else {
		x <- .pointsToMatrix(x)
		.evenspace(x, interval, direction=FALSE)
	}
}
    

.sampleAlongPerpendicular <- function(x, interval, pdist, np=1 ) {
	if (inherits(x, 'SpatialPolygons')) {
		line <- methods::as(line, 'SpatialLines')
	}
	if (inherits(x, 'SpatialLines')) {
		requireNamespace('raster')
		x <- geom(x)
		allpts <- NULL
		for (p in unique(x[, 'cump'])) {
			y <- x[x[, 'cump']==p, c('x', 'y')]
			tspts <- .evenspace(y, interval, direction=TRUE) 
			pts <- NULL
			for (i in 1:np) {
				pts1 <- .transect(tspts, i * pdist)
				pts <- cbind(pts, pts1)
			}
			allpts <- rbind(allpts, pts)
		}
		return(allpts)
	} else {
		x <- .pointsToMatrix(x)
		y <- .evenspace(x, interval, direction=TRUE) 
		pts <- NULL
		for (i in 1:np) {
			pts1 <- .transect(y, i * pdist)
			pts <- rbind(pts, pts1)
		}
		return(pts)
	}    

}
    


