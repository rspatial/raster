/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"


SEXP _focal_get(SEXP d, SEXP dim, SEXP ngb) {

	R_len_t i, j;
	int a, aa, b;
	
	SEXP val;
	int nrow, ncol, n;
	double *xd, *xval;

	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];

	int wrows = INTEGER(ngb)[0];
	int wcols = INTEGER(ngb)[1];
	
	if ((wrows % 2 == 0) | (wcols % 2 == 0))
		error("weights matrix must have uneven sides");

	PROTECT(d = coerceVector(d, REALSXP));
		
	n = (nrow-wrows+1) * (ncol-wcols+1) * wrows * wcols;
	PROTECT( val = allocVector(REALSXP, n) );

	int wr = floor(wrows / 2);
	int wc = floor(wcols / 2);

	xd = REAL(d);
	xval = REAL(val);

	int f = 0;
	
	for (i = 0+wr; i < nrow-wr; i++) {
		for (j = 0+wc; j < ncol-wc; j++) {
			for (a=-wr; a <= wr ; a++) {
			aa = (i+a) * ncol;
				for (b=-wc; b <= wc ; b++) {
					xval[f] = xd[aa+j+b];
					f++;
				}
			}
		}
	}	
	
	UNPROTECT(2);
	return(val);
}



