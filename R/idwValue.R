# Author: Robert J. Hijmans
# Date :  November 2009
# Version 1.0
# Licence GPL v3

# under development

..idwValue <- function(raster, xy, ngb=4, pow=1, layer, n) {
	r <- raster(raster)
	longlat <- couldBeLonLat(r)
	cells <- cellFromXY(r, xy)
	adj <- adjacent(r, cells, ngb, pairs=TRUE, include=TRUE, id=TRUE)

	uc <- unique(adj[,3])
	row1 <- rowFromCell(r, min(uc, na.rm=TRUE))
	nrows <- row1 - 1 + rowFromCell(r, max(uc, na.rm=TRUE))
	offs <- cellFromRowCol(r, row1, 1) - 1
	cs <- uc - offs

	nl <- nlayers(raster)
	if (nl==1) {
		v <- cbind(uc, v=getValues(raster, row1, nrows)[cs])
	} else {
		v <- cbind(uc, v=getValues(raster, row1, nrows)[cs,])
	}
	m <- merge(adj, v, by.x='to', by.y=1)
	colnames(xy) <- c('x', 'y')
	m <- merge(m, cbind(1:nrow(xy), xy), by.x='id', by.y=1)
	
	pd <- pointDistance(m[,c('x', 'y')], xyFromCell(r, m$to), lonlat=longlat) / 1000
	pd <- pd^pow
	pd[pd==0] <- 1e-12
	
	if (nl==1) {
		pd[is.na(m$v)] <- NA
		as.vector( tapply(m$v*(1/pd), m$id, sum, na.rm=TRUE) / tapply(1/pd, m$id, sum, na.rm=TRUE) )
		#cbind(as.integer(names(res)), res)
	} else {
		lys <- 4:(4+nl-1)
		a1 <- aggregate(m[,lys]*(1/pd), list(m$id), sum) 
		a2 <- aggregate(1/pd, list(m$id), sum)
		res <- as.matrix(a1[,-1]) / as.vector(as.matrix(a2[,-1]))
		res <- cbind(as.vector(a1[,1]), res)
		res[, -1]
	}
}

#a=raster(nc=10,nr=10)
#xmin(a)=55
#projection(a) = "+proj=utm +zone=33"
#a[] = 1:ncell(a)
#a[50:75]=NA
#r = disaggregate(raster(a), 3)
#r[] = .idwValue(a, coordinates(r))
#plot(r)
