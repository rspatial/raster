/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
#include "Rmath.h"
#include "util.h"


SEXP _do_clamp(SEXP d, SEXP rr, SEXP usevals) {
					
	R_len_t i;
	SEXP val;
	double *xd, *r, *xval;
	PROTECT(d = coerceVector(d, REALSXP));
	PROTECT(rr = coerceVector(rr, REALSXP));
  	int uv = INTEGER(usevals)[0];
	r = REAL(rr);
	xd = REAL(d);

	int n = length(d);
	PROTECT( val = allocVector(REALSXP, n) );
	xval = REAL(val);
	
	if (uv) {
		for (i=0; i<n; i++) {
			if ( xd[i] < r[0] ) { 
				xval[i] = r[0];
			} else if ( xd[i] > r[1] ) { 
				xval[i] = r[1];
			} else {
				xval[i] = xd[i];
			}
		}		
	} else {
		for (i=0; i<n; i++) {
			if ( (xd[i] < r[0]) | (xd[i] > r[1])) {
				xval[i] = R_NaReal;
			} else {
				xval[i] = xd[i];
			}
		}	
	}
	UNPROTECT(3);
	return(val);
}


