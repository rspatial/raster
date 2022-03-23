# Authors: Robert J. Hijmans 
# Date : October 2008
# Version 0.9
# Licence GPL v3


.strSplitOnFirstToken <- function(s, token="=") {
	pos <- which(strsplit(s, '')[[1]]==token)[1]
	if (is.na(pos)) {
		return(c(trim(s), NA)) 
	} else {
		first <- substr(s, 1, (pos-1))
		second <- substr(s, (pos+1), nchar(s))
		return(trim(c(first, second)))
	}
}


.strSplitOnLastToken <- function(s, token="=") {
	# not used here
	pos <- unlist(strsplit(s, ''), use.names = FALSE)
	pos <- max(which(pos==token))
	if (!is.finite(pos)) {
		return(c(s, NA)) 
	} else {
		first <- substr(s, 1, (pos-1))
		second <- substr(s, (pos+1), nchar(s))
		return(trim(c(first, second)))
	}
}

	
readIniFile <- function(filename, token='=', commenttoken=';', aslist=FALSE, case) {

    stopifnot(file.exists(filename))
	
	Lines <- trim(readLines(filename,  warn = FALSE))
	
	ini <- lapply(Lines, function(s){ 
            if (strsplit(s, "=")[[1]][1] != "wkt") {
                res <- .strSplitOnFirstToken(s, token=commenttoken)
            } else {
# if WKT2 do not split, no comment permitted
                res <- c(trim(s), NA)
            } 
            res 
        } ) 
	Lines <- matrix(unlist(ini), ncol=2, byrow=TRUE)[,1]
	ini <- lapply(Lines, function(s){ .strSplitOnFirstToken(s, token=token) }) 
	
 	ini <- matrix(unlist(ini), ncol=2, byrow=TRUE)
	ini <- ini[ ini[,1] != "", , drop=FALSE]

	ns <- length(which(is.na(ini[,2])))
	if (ns > 0) {
		sections <- c(which(is.na(ini[,2])), length(ini[,2]))

# here I should check whether the section text is enclosed in [ ]. If not, it is junk text that should be removed, rather than used as a section
		ini <- cbind("", ini)
		for (i in 1:(length(sections)-1)) {
			ini[sections[i]:(sections[i+1]), 1] <- ini[sections[i],2]
		}	
		ini[,1] <- gsub("\\[", "", ini[,1])
		ini[,1] <- gsub("\\]", "", ini[,1])
		sections <- sections[1:(length(sections)-1)]
		ini <- ini[-sections,]
	} else {
		ini <- cbind("", ini)	
	}
		
	if (!missing(case)) {
		ini <- case(ini)
	}	
		
	colnames(ini) <- c("section", "name", "value")
	
	if (aslist) {

		iniToList <- function(ini) {
			un <- unique(ini[,1])
			LST <- list()
			for (i in 1:length(un)) {
				sel <- ini[ini[,1] == un[i], 2:3, drop=FALSE]
				lst <- as.list(sel[,2])
				names(lst) <- sel[,1]
				LST[[i]] <- lst
			}
			names(LST) <- un
			return(LST)
		}

		ini <- iniToList(ini)
	}
	
	return(ini)
}



