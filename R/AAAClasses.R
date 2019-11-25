# R classes for raster (grid) type spatial data
# Robert J. Hijmans
# November 2008
# Version 1.0
# Licence GPL v3


setClass('Extent',
	representation (
		xmin = 'numeric',
		xmax = 'numeric',
		ymin = 'numeric',
		ymax = 'numeric'
	),	
	prototype (	
		xmin = 0,
		xmax = 1,
		ymin = 0,
		ymax = 1
	),
	validity = function(object)	{
		c1 <- (object@xmin <= object@xmax)
		c2 <- (object@ymin <= object@ymax)
		# fix to not break dependencies
		if (is.na(c1)) c1 <- TRUE
		if (is.na(c2)) c2 <- TRUE
		if (!c1) { stop('invalid extent: xmin >= xmax') }
		if (!c2) { stop('invalid extent: ymin >= ymax') }
		return(c1 & c2)
		# fix to not break dependencies
		#v <- c(object@xmin, object@xmax, object@ymin, object@ymax)
		#c3 <- all(!is.infinite(v))
		#if (!c3) { stop('invalid extent: infinite value') }		
		#return(c1 & c2 & c3)
	}
)


setClass('.Rotation',
	representation (
		geotrans = 'numeric',
		transfun = 'function'
	)
)


setClass ('BasicRaster',
	representation (
		title = 'character',
		extent = 'Extent',
		rotated = 'logical',
		rotation = '.Rotation',
		ncols ='integer',
		nrows ='integer',
		crs = 'CRS',
		history = 'list',
		#meta = 'list',
		z = 'list'
	),
	prototype (	
		rotated = FALSE,
		ncols= as.integer(1),
		nrows= as.integer(1),
		crs = CRS(),
		history = list(),
		#meta = list(),
		z = list()
	),
	validity = function(object) {
		methods::validObject(extent(object))
		c1 <- (object@ncols > 0)
		if (!c1) { stop('ncols < 1') }
		c2 <- (object@nrows > 0)
		if (!c2) { stop('nrows < 1') }		
		return(c1 & c2)
	}
)

setClass ('Raster', contains = c('BasicRaster', 'VIRTUAL') )

	
setClass('.RasterFile', 
	representation (
		name ='character',
		datanotation='character',
		byteorder ='character',
		nodatavalue ='numeric', # on disk, in ram it is NA
		NAchanged ='logical',
		nbands ='integer',
		bandorder ='character',
		offset='integer',
		toptobottom='logical',
		blockrows='integer',
		blockcols='integer',
		driver ='character',
		dimreadorder = 'numeric',
		open = 'logical'
		),
	prototype (	
	    name = '',
		datanotation='FLT4S',
		byteorder = .Platform$endian,
		nodatavalue = -Inf,
		NAchanged = FALSE,
		nbands = as.integer(1),
		bandorder = 'BIL',
		offset = as.integer(0),
		toptobottom = TRUE,
		blockrows = as.integer(0),
		blockcols= as.integer(0),
		driver = '', 
		dimreadorder = c(1),
		open = FALSE
	),
	validity = function(object) {
		c1 <- object@datanotation %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S')
		return(c1)
	}
)


setClass('.SingleLayerData', 
	representation (
		values='vector', 
		offset='numeric',
		gain='numeric',
		
		inmemory='logical',
		fromdisk='logical',
		
		isfactor = 'logical',
		attributes = 'list',
		
		haveminmax = 'logical',
		min = 'vector',
		max = 'vector',
		band = 'integer',
		unit = 'character',
		names = 'vector'
		),
	prototype (	
		values=vector(),
		offset=0,
		gain=1,
		
		inmemory=FALSE,
		fromdisk=FALSE,

		isfactor = FALSE,
		attributes = list(),
		
		haveminmax = FALSE,
		min = c(Inf),
		max = c(-Inf),
		band = as.integer(1),
		unit = '',
		names=c("")
		
	),	
	validity = function(object) {
	}
)




setClass ('.RasterLegend',
	representation (
		type = 'character',
		values = 'vector',
		color = 'vector',
		names = 'vector',
		colortable = 'vector'
		),
	prototype (
		)
	)
	

	
setClass ('RasterLayer',
	contains = 'Raster',
	representation (
		file = '.RasterFile',
		data = '.SingleLayerData',
		legend = '.RasterLegend'
	)
)



setClass('.MultipleRasterData', 
	representation (
		values='matrix', 
		offset='numeric',
		gain='numeric',
		inmemory='logical',
		fromdisk='logical',
		nlayers='integer',
		dropped = 'vector',
		isfactor = 'logical',
		attributes = 'list',
		haveminmax = 'logical',
		min = 'vector',
		max = 'vector',
		unit = 'vector',
		names= 'vector'
		
		),
	prototype (	
		values=matrix(NA,0,0),
		offset=0,
		gain=1,
		#indices =vector(mode='numeric'),
		inmemory=FALSE,
		fromdisk=FALSE,
		nlayers=as.integer(0),
		dropped=NULL,
		isfactor = FALSE,
		attributes = list(),
		haveminmax = FALSE,
		min = c(Inf),
		max = c(-Inf),
		unit = c(''),
		names = c('')
	),	
	validity = function(object) {
	}
)


setClass ('RasterBrick',
	contains = 'Raster',
	representation (
		file = '.RasterFile',
		data = '.MultipleRasterData',
		legend = '.RasterLegend'
	)
)

	
	
setClass ('RasterStack',
	contains = 'Raster',
	representation (
	    filename ='character',
		layers ='list'
		),
	prototype (
		filename='',
		layers = list()
		),
	validity = function(object) {
		if (length(object@layers) > 1) {
			cond <- compareRaster(object@layers, extent=TRUE, rowcol=TRUE, tolerance=0.05, stopiffalse=FALSE, showwarning=FALSE) 
		} else {
			cond <- TRUE
		}
		return(cond)
	}
)



setClassUnion("RasterStackBrick", c("RasterStack", "RasterBrick"))


setClass ('RasterLayerSparse',
	contains = 'RasterLayer',
	representation (
		index = 'vector'
	),
	prototype (
		index = vector(mode='numeric')
	)
)	

setClass ('.RasterBrickSparse',
	contains = 'RasterBrick',
	representation (
		index = 'vector'
	),
	prototype (
		index = vector(mode='numeric')
	)
)	


setClass ('.RasterQuad',
	contains = 'Raster',
	representation (
	    filename ='character',
		bricks ='list'
		),
	prototype (
		filename='',
		bricks = list()
		),
	validity = function(object) {
		if (length(object@bricks) > 1) {
			test <- compareRaster(object@bricks, extent=TRUE, rowcol=TRUE, tolerance=0.05, stopiffalse=FALSE, showwarning=FALSE) 
		} else {
			test <- TRUE
		}
		return(test)
	}
)


#setClassUnion("RasterStackBrickList", c("RasterStack", "RasterBrick", "RasterList"))

setClassUnion("SpatialVector", c("SpatialPoints", "SpatialLines", "SpatialPolygons"))


setClass ('.RasterList',
	contains = 'list',
	representation (),
	prototype (),
	validity = function(object) {
		s <- sapply(object, function(x) inherits(x, 'Raster'))
		return( sum(s) == length(s))
	}
)



