# Derived from functions GE_SpatialGrid and kmlOverlay 
# in the maptools package by Duncan Golicher, David Forrest and Roger Bivand 
# Adaptation for the raster package by Robert J. Hijmans
# Date : October 2011
# Version 0.9
# Licence GPL v3


.zipKML <- function(kml, image, zip, overwrite=FALSE) {
	if (zip == "") {
		zip <- Sys.getenv('R_ZIPCMD', 'zip')
	}
	if (zip !=  "") {
		wd <- getwd()
		on.exit( setwd(wd) )
		setwd(dirname(kml))
		kml <- basename(kml)
		kmz <- extension(kml, '.kmz')
		
		if (file.exists(kmz)) {
			if (overwrite) {
				file.remove(kmz)
			} else {
				stop('kml file created, but kmz file exists, use "overwrite=TRUE" to overwrite it')
			}
		}	
		
		image <- basename(image)
		if (zip=='7z') {
			kmzzip <- extension(kmz, '.zip')
			cmd <- paste(zip, 'a', kmzzip, kml, image, collapse=" ")
			file.rename(kmzzip, kmz)
		} else {
			cmd <- paste(c(zip, kmz, kml, image), collapse=" ")
		}
		sss <- try( system(cmd, intern=TRUE), silent=TRUE )
		if (file.exists(kmz)) {
			files <- c(kml, image)
			files <- files[file.exists(files)]
			x <- file.remove(files)
			return(invisible(kmz))
		} else {
			return(invisible(kml))
		}
	} else {
		return(invisible(kml))
	}
}


setMethod('KML', signature(x='RasterStackBrick'), 

function (x, filename, time=NULL, col=rev(terrain.colors(255)), colNA=NA, maxpixels=100000, blur=1, zip='', overwrite=FALSE, ...) {

    if (! couldBeLonLat(x)) { 
        stop("CRS of x must be longitude/latitude")
	}
	stopifnot(hasValues(x))
	if (missing(filename)) { 
		filename <- extension(basename(rasterTmpFile('G_')), '.kml')
	}
	
	nl <- nlayers(x)
	if (is.null(time)) { 
		dotime <- FALSE
		atime <- time
	} else {
		dotime <- TRUE
		if (length(time) == nl) {
			when <- TRUE
		} else if (length(time) == nl+1) {
			when <- FALSE
		} else {
			stop('length(time) should equall nlayers(x) for "when", or (nlayers(x)+1) for "begin-end"')
		}
	}

	x <- sampleRegular(x, size=maxpixels, asRaster = TRUE, useGDAL=TRUE)
	kmlfile <- filename
	extension(kmlfile) <- '.kml'
	if (file.exists(kmlfile)) {
		if (overwrite) {
			file.remove(kmlfile)
		} else {
			stop('kml file exists, use "overwrite=TRUE" to overwrite it')
		}
	}	
	
	
	name <- names(x)

    kml <- c('<?xml version="1.0" encoding="UTF-8"?>', '<kml xmlns="http://www.opengis.net/kml/2.2">')
    kml <- c(kml, c("<Folder>", paste("<name>", extension(basename(filename), ''), "</name>", sep='')))
    e <- extent(x)
    latlonbox <- c("\t<LatLonBox>", paste("\t\t<north>", e@ymax, "</north><south>",  e@ymin, "</south><east>", 
						e@xmax, "</east><west>", e@xmin, "</west>", sep = ""), "\t</LatLonBox>", "</GroundOverlay>")

	imagefile <- paste(extension(filename, ''), "_", 1:nl, ".png", sep="")
	
	
	for (i in 1:nl) {
		png(filename = imagefile[i], width=max(480, blur*ncol(x)), height=max(480,blur*nrow(x)), bg="transparent")
		if (!is.na(colNA)) {
			graphics::par(mar=c(0,0,0,0), bg=colNA)
		} else {
			graphics::par(mar=c(0,0,0,0))	
		}
		
		image(x[[i]], col=col, axes=FALSE, useRaster=TRUE, maxpixels=maxpixels, ...)
		dev.off()
		a <- c("<GroundOverlay>", paste("\t<name>", name[i], "</name>", sep=''))
		if (dotime) {
			if (when) {
				atime <- c("\t<TimeSpan>", paste("\t\t<when>", time[i], "</when>", sep=''), "\t</TimeSpan>")			
			} else {
				atime <- c("\t<TimeSpan>", paste("\t\t<begin>", time[i], "</begin>", sep=''), 
					paste("\t\t<end>", time[i+1], "</end>", sep=''), "\t</TimeSpan>")
			}
		}
		kml <- c(kml, a, atime, paste("\t<Icon><href>", basename(imagefile[i]), "</href></Icon>", sep=''), latlonbox)
	}

    kml <- c(kml, "</Folder>", "</kml>")
    cat(paste(kml, sep="", collapse="\n"), file=kmlfile, sep = "")
	.zipKML(kmlfile, imagefile, zip, overwrite=overwrite)
}
)

