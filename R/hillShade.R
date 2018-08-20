# Author: Andrew Bevan, Oscar Perpinan Lamigueiro, and Robert J. Hijmans
# Date : March 2010
# Version 1.0
# Licence GPL v3

hillShade <- function(slope, aspect, angle=45, direction=0, filename='', normalize=FALSE, ...) {
	compareRaster(slope, aspect)

	direction <- direction * pi/180
	zenith <- (90 - angle)*pi/180
	
	#x <- cos(slope) * cos(declination) + sin(slope) * sin(declination) * cos(direction-aspect)
	if (normalize) {
		fun <- function(slp, asp) { 
			shade <- cos(slp) * cos(zenith) + sin(slp) * sin(zenith) * cos(direction-asp) 
			shade[shade < 0] <- 0
			shade * 255
		}
	} else {
		fun <- function(slp, asp) { cos(slp) * cos(zenith) + sin(slp) * sin(zenith) * cos(direction-asp) }
	}
	x <- overlay(slope, aspect, fun=fun, filename=filename, ...)		
	return(x)
}

