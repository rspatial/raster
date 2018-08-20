# Authors: Rafael Wueest, WSL Birmensdorf, Switzerland, rafael.wueest@wsl.ch, 
# Etienne B. Racine, Robert J. Hijmans
# Date : November 2012
# Version 1.0
# Licence GPL v3


# needs to be generalized to n input rasters and to multi-layer objects
.old_blend <- function(r1, r2) {
	i <- intersect(raster(r1), raster(r2))
	j <- extend(i, c(1,1)) 
	a <- crop(r1, j)
	b <- crop(r2, j)
	values(a) <- 1
	values(b) <- 2
	ab <- merge(a, b)
	ba <- merge(b, a)
	p1 <- rasterToPoints(ab, function(x) x==2)
	p2 <- rasterToPoints(ba, function(x) x==1)
	d1 <- distanceFromPoints(i, p1[,1:2])
	d2 <- distanceFromPoints(i, p2[,1:2])
	dsum <- d1 + d2

	z1 <- d1 * crop(r1, d1) / dsum
	z2 <- d2 * crop(r2, d2) / dsum
	merge(z1 + z2, r1, r2)
}


.blend <- function(x, y, logistic=FALSE, filename='', ...) {
   
   # check for difference in extent
	stopifnot( extent(x) != extent(y))
   
   # define logistic function
   if (logistic) {
		G <- 1
		f <- 0.001
		k <- log(G/f-1)/(0.5*G)
		logfun <- function(x) { 
			G /(1+exp(-k*G*x)*(G/f-1)) 
		}
	}
   
   # create intersection rasters
	i <- intersect(raster(x), raster(y))
	j <- extend(i, c(1,1)) 
   
   # is one of the rasters nested within the other?
	ex <- extent(x)
	ey <- extent(y)
	exy <- union(ex, ey)

	if (exy==ex | exy==ey){    # the nested case
      
      # which raster has the smaller extent?
		if (extent(x) < extent(y)){
			rlarge <- y
			rsmall <- x
		} else {
			rlarge <- x
			rsmall <- y
		}
      
      # create points around nested raster
		a <- crop(rlarge, j)
		a <- setValues(a, 1)
		b <- crop(rsmall, j)
		b <- setValues(b, 2)
		ba <- merge(b, a)
		p <- rasterToPoints(ba, function(x) x==1)
      
      # calculate distances to points in nested raster
		d <- distanceFromPoints(i, p[,1:2])
      
      # standardize these distances
		dmin <- cellStats(d,'min')
		d.sc <- (d - dmin + 1e-9) / (cellStats(d,'max') - dmin)
      
      # the logistic case
		if(logistic){
			d.sc<-logfun(d.sc)
		}
      
      # create distance weighted rasters
		z1 <- d.sc * crop(rsmall, d.sc)
		z2 <- abs(1-d.sc) * crop(rlarge, d.sc)
      
      # merge rasters
		m <- merge(z1 + z2, rsmall, rlarge, filename=filename, ...)


	} else {    # the overlapping case
      
		# create points around ovelapping area
		a <- crop(x, j)
		a <- setValues(a, 1)
		b <- crop(y, j)
		b <- setValues(b, 2)
		ab <- merge(a, b)
		ba <- merge(b, a)
		p1 <- rasterToPoints(ab, function(x) x==2)
		p2 <- rasterToPoints(ba, function(x) x==1)
      
      # calculate distances to points in overlapping area
		d1 <- distanceFromPoints(i, p1[,1:2])
		d2 <- distanceFromPoints(i, p2[,1:2])
      
      # the logistic case
		if(logistic){
			d1min <- cellStats(d1,'min')
			d2min <- cellStats(d2,'min')
			d1 <- logfun((d1 - d1min + 1e-9)/(cellStats(d1,'max') - d1min))
			d2 <- logfun((d2 - d2min + 1e-9)/(cellStats(d2,'max') - d2min))
		}	
      
      # sum distance rasters
		dsum <- d1 + d2
      
      # create distance weighted rasters
		z1 <- d1 * crop(x, d1) / dsum
		z2 <- d2 * crop(y, d2) / dsum
		z <- sum(z1, z2)
		
      # merge rasters
		m <- merge(z, x, y, filename=filename, ...)
   }
   
   m
}

