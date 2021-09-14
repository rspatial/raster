# Derived, with only minor changes, from functions GE_SpatialGrid and kml Overlay 
# in the maptools package. These were written by Duncan Golicher, David Forrest and Roger Bivand 
# Adaptation for the raster packcage by Robert J. Hijmans, 
# Date : March 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("KML")) {
	setGeneric("KML", function(x, ...)
		standardGeneric("KML"))
}	

setMethod('KML', signature(x='Spatial'), 
	function (x, filename, zip='', overwrite=FALSE, ...) {
		.requireRgdal()
		if (! is.na(projection(x))) {
			if (! isLonLat(x) ) {
				warning('transforming data to longitude/latitude')
				sp::spTranform(x, sp::CRS('+proj=longlat +datum=WGS84'))
			}
		}
		
		if (!.hasSlot(x, 'data') ) {
			x <- sp::addAttrToGeom(x, data.frame(id=1:length(x)), match.ID=FALSE)
		}
		
		extension(filename) <- '.kml'
		if (file.exists(filename)) {
			if (overwrite) {
				file.remove(filename)
			} else {
				stop('file exists, use "overwrite=TRUE" to overwrite it')
			}
		}
		name <- list(...)$name
		if (is.null(name)) {
			name <- deparse(substitute(x))
		}
		rgdal::writeOGR(x, filename, name, 'KML', ...)
		.zipKML(filename, '', zip, overwrite=overwrite) 
	}
)
	


	
setMethod('KML', signature(x='RasterLayer'), 

function (x, filename, col=rev(terrain.colors(255)), colNA=NA, maxpixels=100000, blur=1, zip='', overwrite=FALSE, ...) {

    if (! couldBeLonLat(x)) { 
        stop("CRS of x must be longitude / latitude")
	}
	
	if (nlayers(x) > 1) {
		x <- x[[1]]
	}
	stopifnot(hasValues(x))

	if (missing(filename)) { 
		filename <- extension(basename(rasterTmpFile('G_')), '.kml')
	}
		
	x <- sampleRegular(x, size=maxpixels, asRaster = TRUE, useGDAL=TRUE)

	imagefile <- filename
	extension(imagefile) <- '.png'
	kmlfile <- kmzfile <- filename
	extension(kmlfile) <- '.kml'
	
	if (file.exists(kmlfile)) {
		if (overwrite) {
			file.remove(kmlfile)
		} else {
			stop('kml file exists, use "overwrite=TRUE" to overwrite it')
		}
	}
	
	

	png(filename = imagefile, width=max(480, blur*ncol(x)), height=max(480,blur*nrow(x)), bg="transparent")
	if (!is.na(colNA)) {
		graphics::par(mar=c(0,0,0,0), bg=colNA)
	} else {
		graphics::par(mar=c(0,0,0,0))	
	}
	image(x, col=col, axes=FALSE, useRaster=TRUE, maxpixels=maxpixels, ...)
	dev.off()

	name <- names(x)[1]
	if (name == "") { name <- 'x' }
    kml <- c('<?xml version="1.0" encoding="UTF-8"?>', '<kml xmlns="http://www.opengis.net/kml/2.2">', "<GroundOverlay>")
    kmname <- paste("<name>", name, "</name>", sep = "")
    icon <- paste("<Icon><href>", basename(imagefile), "</href><viewBoundScale>0.75</viewBoundScale></Icon>", sep = "")
    e <- extent(x)
    latlonbox <- c("\t<LatLonBox>", paste("\t\t<north>", e@ymax, "</north><south>",  e@ymin, "</south><east>", e@xmax, "</east><west>", e@xmin, "</west>", sep = ""), "\t</LatLonBox>")
    footer <- "</GroundOverlay></kml>"
	
    kml <- c(kml, kmname, icon, latlonbox, footer)
	
	f <- file(kmlfile, 'wt', encoding='UTF-8')
    cat(paste(kml, sep="", collapse="\n"), file=f, sep="")
	close(f)
	
	.zipKML(kmlfile, imagefile, zip, overwrite=overwrite)
}
)
