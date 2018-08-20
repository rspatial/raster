# Author: Jacob van Etten jacobvanetten@yahoo.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


.cs <- function(a,b) {
	aRep <- rep(a,times=length(b))
	cbind(aRep,as.integer(aRep+rep(b,each=length(a))),deparse.level=0)
}


.adjacency <- function(x, ...) {

	warning('function "adjaceny" is obsolete and will be removed from the "raster" package.\nUse function "adjacent" in stead')


	dots <- list(...)
	fromCells <- dots$fromCells
	toCells <- dots$toCells
	directions <- dots$directions 

	
	if (is.character(directions)) { directions <- tolower(directions) }
	stopifnot(directions %in% c(4,8,16) | directions=='bishop')

	x <- raster(x)

	outerMeridianConnect <- .isGlobalLonLat(x)
	
	if (directions=="bishop") { 
		return(.adjBishop(x, fromCells, toCells, outerMeridianConnect)) 
	}

	
	nCols <- ncol(x)
	nCells <- ncell(x)
	
	left <- seq(nCols+1,(nCells-2*nCols+1),by=nCols) 
	right <- seq(2*nCols,nCells-nCols,by=nCols)
	upper <- 2:(nCols-1)
	lower <- seq((nCells-nCols+2),(nCells-1),by=1)
	upperleft <- 1
	upperright <- nCols
	lowerleft <- nCells-nCols+1
	lowerright <- nCells

	fromCellsCore <- as.integer(setdiff(fromCells,(c(left,right,upper,lower,upperleft,upperright,lowerleft,lowerright))))
	fromCellsUpper <- as.integer(intersect(fromCells,upper))
	fromCellsLower <- as.integer(intersect(fromCells,lower))
	fromCellsLeft <- as.integer(intersect(fromCells,left))
	fromCellsRight <- as.integer(intersect(fromCells,right))
	fromCellUpperleft <- as.integer(intersect(fromCells,upperleft))
	fromCellUpperright <- as.integer(intersect(fromCells,upperright))
	fromCellLowerleft <- as.integer(intersect(fromCells,lowerleft))
	fromCellLowerright <- as.integer(intersect(fromCells,lowerright))

	rook <- c(1,-1,nCols,-nCols)

	coreFromToRook <- .cs(fromCellsCore,rook)
	upperFromToRook <- .cs(fromCellsUpper,rook[1:3])
	lowerFromToRook <- .cs(fromCellsLower,rook[c(1,2,4)])
	leftFromToRook <- .cs(fromCellsLeft,rook[c(1,3,4)])
	rightFromToRook <- .cs(fromCellsRight,rook[2:4])
	upperleftFromToRook <- .cs(fromCellUpperleft,rook[c(1,3)])
	upperrightFromToRook <- .cs(fromCellUpperright,rook[2:3])
	lowerleftFromToRook <- .cs(fromCellLowerleft,rook[c(1,4)])
	lowerrightFromToRook <- .cs(fromCellLowerright,rook[c(2,4)])
	fromto1 <- rbind(coreFromToRook,upperFromToRook,lowerFromToRook,leftFromToRook,rightFromToRook,upperleftFromToRook,upperrightFromToRook,lowerleftFromToRook,lowerrightFromToRook)
	
	if (outerMeridianConnect) {
		meridianFromLeft <- rbind(
			cbind(fromCellsLeft,as.integer(fromCellsLeft+nCols-1)),
			cbind(fromCellUpperleft,as.integer(fromCellUpperleft+nCols-1)),
			cbind(fromCellLowerleft,as.integer(fromCellLowerleft+nCols-1))
			)
		meridianFromRight <- rbind(
			cbind(fromCellsRight,as.integer(fromCellsRight-nCols+1)),
			cbind(fromCellUpperright,as.integer(fromCellUpperright-nCols+1)),
			cbind(fromCellLowerright,as.integer(fromCellLowerright-nCols+1))
			)
		fromto1 <- rbind(fromto1,meridianFromLeft,meridianFromRight)
	}

	fromto <- subset(fromto1,fromto1[,2] %in% toCells)

	if (directions > 4)	{
		bishop <- as.integer(c(-nCols-1, -nCols+1, nCols-1,+nCols+1))
		
		coreFromToBishop <- .cs(fromCellsCore,bishop)
		upperFromToBishop <- .cs(fromCellsUpper,bishop[3:4])
		lowerFromToBishop <- .cs(fromCellsLower,bishop[1:2])
		leftFromToBishop <- .cs(fromCellsLeft,bishop[c(2,4)])
		rightFromToBishop <- .cs(fromCellsRight,bishop[c(1,3)])
		upperleftFromToBishop <- .cs(fromCellUpperleft,bishop[4])
		upperrightFromToBishop <- .cs(fromCellUpperright,bishop[3])
		lowerleftFromToBishop <- .cs(fromCellLowerleft,bishop[2])
		lowerrightFromToBishop <- .cs(fromCellLowerright,bishop[1])

		fromto2 <- rbind(coreFromToBishop,upperFromToBishop,lowerFromToBishop,leftFromToBishop,rightFromToBishop,upperleftFromToBishop,upperrightFromToBishop,lowerleftFromToBishop,lowerrightFromToBishop)
		
		if (outerMeridianConnect) {
			meridianFromLeft <- rbind(
				.cs(fromCellsLeft,c(2*nCols-1,-1)),
				cbind(fromCellUpperleft,as.integer(fromCellUpperleft+2*nCols-1)),
				cbind(fromCellLowerleft,as.integer(fromCellLowerleft-1))
				) 
			meridianFromRight <- rbind(
				cbind(rep(fromCellsRight,times=2),as.integer(c(fromCellsRight-2*nCols+1,fromCellsRight+1))),
				cbind(fromCellUpperright,as.integer(fromCellUpperright+1)),
				cbind(fromCellLowerright,as.integer(fromCellLowerright-2*nCols+1))
				)
			fromto2 <- rbind(fromto2,meridianFromLeft,meridianFromRight)
		}

		fromto2 <- subset(fromto2,fromto2[,2] %in% toCells)
		fromto <- rbind(fromto,fromto2)
	}

	if (directions > 8) {

		leftOuter <- seq(2*nCols+1,nCells-3*nCols+1,by=nCols) 
		rightOuter <- seq(3*nCols,nCells-2*nCols,by=nCols)
		upperOuter <- seq(3,nCols-2,by=1)
		lowerOuter <- seq(nCells-nCols+3,nCells-2,by=1)

		upperleftUnder <- nCols+1
		upperrightLeft <- nCols-1
		lowerleftUp <- nCells-2*nCols+1
		lowerrightUp <- nCells-nCols		
		upperleftRight <- 2
		upperrightUnder <- 2*nCols
		lowerleftRight <- nCells-nCols+2
		lowerrightLeft <- nCells-1

		leftInner <- seq(2*nCols+2,(nCells-3*nCols+2),by=nCols) 
		rightInner <- seq(3*nCols-1,nCells-2*nCols-1,by=nCols)
		upperInner <- seq(nCols+3,2*nCols-2,by=1)
		lowerInner <- seq(nCells-2*nCols+3,nCells-nCols-2,by=1)

		upperleftInner <- nCols+2
		upperrightInner <- 2*nCols-1
		lowerleftInner <- nCells-2*nCols+2
		lowerrightInner <- nCells-nCols-1

		fromCellsCoreInner <- setdiff(fromCells,(c(leftOuter,rightOuter,upperOuter,lowerOuter,upperleft,upperright,lowerleft,lowerright, upperleftUnder, upperrightLeft, lowerleftUp, lowerrightUp, upperleftRight, upperrightUnder, lowerleftRight, lowerrightLeft, leftInner, rightInner, upperInner, lowerInner, upperleftInner, upperrightInner, lowerleftInner, lowerrightInner))) 
		
		fromCellsUpperInner <- as.integer(intersect(fromCells,upperInner))
		fromCellsLowerInner <- as.integer(intersect(fromCells,lowerInner))
		fromCellsLeftInner <- as.integer(intersect(fromCells,leftInner))
		fromCellsRightInner <- as.integer(intersect(fromCells,rightInner))

		fromCellUpperleftInner <- as.integer(intersect(fromCells,upperleftInner))
		fromCellUpperrightInner <- as.integer(intersect(fromCells,upperrightInner))
		fromCellLowerleftInner <- as.integer(intersect(fromCells,lowerleftInner))
		fromCellLowerrightInner <- as.integer(intersect(fromCells,lowerrightInner))	

		fromCellsLeftOuter <- as.integer(intersect(fromCells,leftOuter))
		fromCellsRightOuter <- as.integer(intersect(fromCells,rightOuter))
		fromCellsUpperOuter <- as.integer(intersect(fromCells,upperOuter))
		fromCellsLowerOuter <- as.integer(intersect(fromCells,lowerOuter))

		fromCellUpperleftUnder <- as.integer(intersect(fromCells,upperleftUnder))
		fromCellUpperrightLeft <- as.integer(intersect(fromCells,upperrightLeft))
		fromCellLowerleftUp <- as.integer(intersect(fromCells,lowerleftUp))
		fromCellLowerrightUp <- as.integer(intersect(fromCells,lowerrightUp))
		fromCellUpperleftRight <- as.integer(intersect(fromCells,upperleftRight))
		fromCellUpperrightUnder <- as.integer(intersect(fromCells,upperrightUnder))
		fromCellLowerleftRight <- as.integer(intersect(fromCells,lowerleftRight))
		fromCellLowerrightLeft <- as.integer(intersect(fromCells,lowerrightLeft))

		knight <- c(-2*nCols-1, -2*nCols+1, -nCols-2, -nCols+2, nCols-2, nCols+2, 2*nCols-1, 2*nCols+1)	
		
		coreInnerFromToKnight <- .cs(fromCellsCoreInner, knight) 
		
		upperInnerFromToKnight <- .cs(fromCellsUpperInner, knight[3:8])
		lowerInnerFromToKnight <- .cs(fromCellsLowerInner, knight[1:6])
		leftInnerFromToKnight <- .cs(fromCellsLeftInner, knight[c(1,2,4,6:8)])
		rightInnerFromToKnight <- .cs(fromCellsRightInner, knight[c(1:3,5,7,8)])

		upperleftInnerFromToKnight <- .cs(fromCellUpperleftInner, knight[c(4,6:8)])
		upperrightInnerFromToKnight <- .cs(fromCellUpperrightInner, knight[c(3,5,7,8)])
		lowerleftInnerFromToKnight <- .cs(fromCellLowerleftInner, knight[c(1,2,4,6)])
		lowerrightInnerFromToKnight <- .cs(fromCellLowerrightInner, knight[c(1:3,5)])
		
		leftOuterFromToKnight <- .cs(fromCellsLeftOuter, knight[c(2,4,6,8)])
		rightOuterFromToKnight <- .cs(fromCellsRightOuter, knight[c(1,3,5,7)])
		upperOuterFromToKnight <- .cs(fromCellsUpperOuter, knight[5:8])
		lowerOuterFromToKnight <- .cs(fromCellsLowerOuter, knight[1:4])

		upperleftUnderFromToKnight <- .cs(fromCellUpperleftUnder, knight[c(4,6,8)])
		upperrightLeftFromToKnight <- .cs(fromCellUpperrightLeft, knight[c(5,7,8)])
		lowerleftUpFromToKnight <- .cs(fromCellLowerleftUp, knight[c(2,4,6)])
		lowerrightUpFromToKnight <- .cs(fromCellLowerrightUp, knight[c(1,3,5)])
		upperleftRightFromToKnight <- .cs(fromCellUpperleftRight, knight[6:8])
		upperrightUnderFromToKnight <- .cs(fromCellUpperrightUnder, knight[c(3,5,7)])
		lowerleftRightFromToKnight <- .cs(fromCellLowerleftRight, knight[c(1,2,4)])
		lowerrightLeftFromToKnight <- .cs(fromCellLowerrightLeft, knight[1:3])

		upperleftFromToKnight <- .cs(fromCellUpperleft, knight[c(6,8)])
		upperrightFromToKnight <- .cs(fromCellUpperright, knight[c(5,7)])
		lowerleftFromToKnight <- .cs(fromCellLowerleft, knight[c(2,4)])
		lowerrightFromToKnight <- .cs(fromCellLowerright, knight[c(1,3)])
		
		fromto3 <- rbind(coreInnerFromToKnight, upperInnerFromToKnight, lowerInnerFromToKnight, leftInnerFromToKnight, rightInnerFromToKnight, upperleftInnerFromToKnight, upperrightInnerFromToKnight, lowerleftInnerFromToKnight, lowerrightInnerFromToKnight, leftOuterFromToKnight, rightOuterFromToKnight, upperOuterFromToKnight,	lowerOuterFromToKnight, upperleftUnderFromToKnight, upperrightLeftFromToKnight,	lowerleftUpFromToKnight, lowerrightUpFromToKnight, upperleftRightFromToKnight, upperrightUnderFromToKnight, lowerleftRightFromToKnight, lowerrightLeftFromToKnight, upperleftFromToKnight, upperrightFromToKnight, lowerleftFromToKnight, lowerrightFromToKnight)
		fromto3 <- subset(fromto3,fromto3[,2] %in% toCells)
		
		if (outerMeridianConnect) {
			knightLeft <- c(-nCols-1, -2, +2*nCols-2, 3*nCols-1)
			knightRight <- c(-3*nCols+1, -2*nCols+2, +2, nCols+1)

			leftInnerFromToKnight <- .cs(fromCellsLeftInner, knightLeft[c(2,3)])
			rightInnerFromToKnight <- .cs(fromCellsRightInner, knightRight[c(2,3)])

			upperleftInnerFromToKnight <- .cs(fromCellUpperleftInner, knightLeft[c(2,3)])
			upperrightInnerFromToKnight <- .cs(fromCellUpperrightInner, knightRight[c(2,3)])
			lowerleftInnerFromToKnight <- .cs(fromCellLowerleftInner, knightLeft[c(2,3)])
			lowerrightInnerFromToKnight <- .cs(fromCellLowerrightInner, knightRight[c(2,3)])
		
			leftOuterFromToKnight <- .cs(fromCellsLeftOuter, knightLeft)
			rightOuterFromToKnight <- .cs(fromCellsRightOuter, knightRight)

			upperleftUnderFromToKnight <- .cs(fromCellUpperleftUnder, knightLeft[2:4])
			upperrightLeftFromToKnight <- .cs(fromCellUpperrightLeft, knightRight[3])
			lowerleftUpFromToKnight <- .cs(fromCellLowerleftUp, knightLeft[1:3])
			lowerrightUpFromToKnight <- .cs(fromCellLowerrightUp, knightRight[1:3])
			upperleftRightFromToKnight <- .cs(fromCellUpperleftRight, knightLeft[c(3)])
			upperrightUnderFromToKnight <- .cs(fromCellUpperrightUnder, knightRight[2:4])
			lowerleftRightFromToKnight <- .cs(fromCellLowerleftRight, knightLeft[2])
			lowerrightLeftFromToKnight <- .cs(fromCellLowerrightLeft, knightRight[2])

			upperleftFromToKnight <- .cs(fromCellUpperleft, knightLeft[c(3,4)])
			upperrightFromToKnight <- .cs(fromCellUpperright, knightRight[c(3,4)])
			lowerleftFromToKnight <- .cs(fromCellLowerleft, knightLeft[c(1,2)])
			lowerrightFromToKnight <- .cs(fromCellLowerright, knightRight[c(1,2)])
			
			fromto3 <- rbind(fromto3, leftInnerFromToKnight, rightInnerFromToKnight, upperleftInnerFromToKnight, upperrightInnerFromToKnight, lowerleftInnerFromToKnight, lowerrightInnerFromToKnight, leftOuterFromToKnight, rightOuterFromToKnight, upperleftUnderFromToKnight, upperrightLeftFromToKnight, lowerleftUpFromToKnight, lowerrightUpFromToKnight, upperleftRightFromToKnight, upperrightUnderFromToKnight, lowerleftRightFromToKnight, lowerrightLeftFromToKnight, upperleftFromToKnight, upperrightFromToKnight, lowerleftFromToKnight, lowerrightFromToKnight)
		}
		
		fromto3 <- subset(fromto3,fromto3[,2] %in% toCells)	
		fromto <- rbind(fromto,fromto3)
	}

	colnames(fromto) <- c("from","to")
	return(fromto)
}



.adjBishop <- function(raster, fromCells, toCells, outerMeridianConnect)  {
	nCols <- ncol(raster)
	nCells <- ncell(raster)
	
	left <- seq(nCols+1,(nCells-2*nCols+1),by=nCols) 
	right <- seq(2*nCols,nCells-nCols,by=nCols)
	upper <- 2:(nCols-1)
	lower <- seq((nCells-nCols+2),(nCells-1),by=1)
	upperleft <- 1
	upperright <- nCols
	lowerleft <- nCells-nCols+1
	lowerright <- nCells

	fromCellsCore <- as.integer(setdiff(fromCells,(c(left,right,upper,lower,upperleft,upperright,lowerleft,lowerright))))
	fromCellsUpper <- as.integer(intersect(fromCells,upper))
	fromCellsLower <- as.integer(intersect(fromCells,lower))
	fromCellsLeft <- as.integer(intersect(fromCells,left))
	fromCellsRight <- as.integer(intersect(fromCells,right))
	fromCellUpperleft <- as.integer(intersect(fromCells,upperleft))
	fromCellUpperright <- as.integer(intersect(fromCells,upperright))
	fromCellLowerleft <- as.integer(intersect(fromCells,lowerleft))
	fromCellLowerright <- as.integer(intersect(fromCells,lowerright))
	
	bishop <- as.integer(c(-nCols-1, -nCols+1, nCols-1,+nCols+1))
		
	coreFromToBishop <- .cs(fromCellsCore,bishop)
	upperFromToBishop <- .cs(fromCellsUpper,bishop[3:4])
	lowerFromToBishop <- .cs(fromCellsLower,bishop[1:2])
	leftFromToBishop <- .cs(fromCellsLeft,bishop[c(2,4)])
	rightFromToBishop <- .cs(fromCellsRight,bishop[c(1,3)])
	upperleftFromToBishop <- .cs(fromCellUpperleft,bishop[4])
	upperrightFromToBishop <- .cs(fromCellUpperright,bishop[3])
	lowerleftFromToBishop <- .cs(fromCellLowerleft,bishop[2])
	lowerrightFromToBishop <- .cs(fromCellLowerright,bishop[1])

	fromto <- rbind(coreFromToBishop,upperFromToBishop,lowerFromToBishop,leftFromToBishop,rightFromToBishop,upperleftFromToBishop,upperrightFromToBishop,lowerleftFromToBishop,lowerrightFromToBishop)
	
	if (outerMeridianConnect) {
		meridianFromLeft <- rbind(
			.cs(fromCellsLeft,c(2*nCols-1,-1)),
			cbind(fromCellUpperleft,as.integer(fromCellUpperleft+2*nCols-1)),
			cbind(fromCellLowerleft,as.integer(fromCellLowerleft-1))
			) 
		meridianFromRight <- rbind(
			cbind(rep(fromCellsRight,times=2),as.integer(c(fromCellsRight-2*nCols+1,fromCellsRight+1))),
			cbind(fromCellUpperright,as.integer(fromCellUpperright+1)),
			cbind(fromCellLowerright,as.integer(fromCellLowerright-2*nCols+1))
			)
		fromto <- rbind(fromto,meridianFromLeft,meridianFromRight)
	}
	fromto <- subset(fromto,fromto[,2] %in% toCells)
	return(fromto)
}

