
.fixDBFNames <- function(x, verbose=TRUE) {
    n <- gsub('^[[:space:]]+', '',  gsub('[[:space:]]+$', '', x) )
    nn <- n
    n <- gsub('[^[:alnum:]]', '_', n)
    n[nchar(n) > 10] <- gsub('_', '', n[nchar(n) > 10])
    n[n==''] <- 'field'
    n <- gsub('^[^[:alpha:]]', 'X', n)
    n <- substr(n, 1, 10)

       # duplicate names
    nn  <- as.matrix(table(n))
    i <- which(nn > 1)
    if (! is.null(i)) {
        names <- rownames(nn)[i]
        n[n %in% names] <- substr(n[n %in% names], 1, 9)
        n <- make.unique(n, sep = "")
    }
	if (verbose) {
		i <- x == n
		if (! all(i)) {
			x <- rbind(x, n)
			colnames(x) <- paste('col_', 1:ncol(x), sep="")
			x <- x[, !i, drop=FALSE]
			rownames(x) = c('original name', 'adjusted name')
			print(x)
		}
    }
    return(n)
}

