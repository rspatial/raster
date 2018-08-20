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


SEXP _reclass(SEXP d, SEXP r, SEXP low, SEXP right, SEXP onlyNA, SEXP valNA) {
					
	R_len_t i, j;
	SEXP val;
	double *xd, *rcl, *xval, lowval, lowres;

	PROTECT(d = coerceVector(d, REALSXP));

	SEXP rdim = getAttrib(r, R_DimSymbol);
	int a = INTEGER(rdim)[0];
	int nc = INTEGER(rdim)[1];
	int b = a * 2;
	
	PROTECT(r = coerceVector(r, REALSXP));
	rcl = REAL(r);
	xd = REAL(d);

	int doright = INTEGER(right)[0];
	int doleftright = 0;
	if (doright == R_NaInt) {
		doleftright = 1;
	}
	int dolowest  = INTEGER(low)[0];
	int NAonly = INTEGER(onlyNA)[0];
	double NAval = REAL(valNA)[0];

	int n = length(d);
	
	PROTECT( val = allocVector(REALSXP, n) );
	xval = REAL(val);

	
	if (NAonly) {  // only change NA values
	
		for (i=0; i<n; i++) {
			if (R_IsNA(xd[i])) {
				xval[i] = NAval;
			} else {
				xval[i] = xd[i];
			}
		}
		
	} else { // "is - becomes" reclassification
		
		if (nc == 2) {
		
			for (i=0; i<n; i++) {
				if (R_IsNA(xd[i])) {
					xval[i] = NAval;
				} else {
					xval[i] = xd[i];
					for (j=0; j<a; j++) {
						if (xd[i] == rcl[j]) {
							xval[i] = rcl[j+a];
							break;
						}
					}
				}
			}
		
		// "from - to - becomes" reclassification
		} else if (doleftright) {   // interval closed at left and right
			
			for (i=0; i<n; i++) {
				if (R_IsNA(xd[i])) {
					xval[i] = NAval;
				} else {
					xval[i] = xd[i];
					for (j=0; j<a; j++) {
						if ((xd[i] >= rcl[j]) & (xd[i] <= rcl[j+a])) {
							xval[i] = rcl[j+b];
							break;
						}
					}
				}
			}

		} else if (doright) {  // interval closed at right

			if (dolowest) {  // include lowest value (left) of interval
			
				lowval = rcl[0];
				lowres = rcl[b];
				for (j=1; j<a; j++) {
					if (rcl[j] < lowval) {
						lowval = rcl[j];
						lowres = rcl[b+j];
					}
				}
				
//				Rprintf ("lowval = %f \n", lowval);
//				Rprintf ("lowres = %f \n", lowres);
				
				for (i=0; i<n; i++) {
					if (R_IsNA(xd[i])) {
						xval[i] = NAval;
					} else if (xd[i] == lowval) {
						xval[i] = lowres;
					} else {
						xval[i] = xd[i];
						for (j=0; j<a; j++) {
							if ((xd[i] > rcl[j]) & (xd[i] <= rcl[j+a])) {
								xval[i] = rcl[j+b];
								break;
							}
						}
					}
				}
				
			} else { // !dolowest

				for (i=0; i<n; i++) {
					if (R_IsNA(xd[i])) {
						xval[i] = NAval;
					} else {
						xval[i] = xd[i];
						for (j=0; j<a; j++) {
							if ((xd[i] > rcl[j]) & (xd[i] <= rcl[j+a])) {
								xval[i] = rcl[j+b];
								break;
							}
						}
					}
				}			
			}
			
		} else { // !doright
		
			if (dolowest) { // which here means highest because right=FALSE
			
				lowval = rcl[a];
				lowres = rcl[b];
				for (j=a+1; j<b; j++) {
					if (rcl[j] > lowval) {
						lowval = rcl[j];
						lowres = rcl[a+j];
					}
				}
				
				for (i=0; i<n; i++) {
					if (R_IsNA(xd[i])) {
						xval[i] = NAval;
					} else if (xd[i] == lowval) {
						xval[i] = lowres;
					} else {
						xval[i] = xd[i];
						for (j=0; j<a; j++) {
							if ((xd[i] >= rcl[j]) & (xd[i] < rcl[j+a])) {
								xval[i] = rcl[j+b];
								break;
							}
						}
					}
				}
				
			} else { //!dolowest
			
				for (i=0; i<n; i++) {
					if (R_IsNA(xd[i])) {
						xval[i] = NAval;
					} else {
						xval[i] = xd[i];
						for (j=0; j<a; j++) {	
							if ((xd[i] >= rcl[j]) & (xd[i] < rcl[j+a])) {
								xval[i] = rcl[j+b];
								break;
							}
						}
					}
				}
			}
		}
	}
	
	UNPROTECT(3);
	return(val);
}


