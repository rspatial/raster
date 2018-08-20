/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"


SEXP _focal_sum(SEXP d, SEXP w, SEXP dim, SEXP rmNA, SEXP NAonly, SEXP domean) {

	R_len_t i, j, k, q, p;
	SEXP val;
	int nrow, ncol, n, bemean;
	double *xd, *xval, *xw, a;


	SEXP wdim = getAttrib(w, R_DimSymbol);

	if (isNull(wdim))
		error("wdim is null");

	int wrows = INTEGER(wdim)[0];
	int wcols = INTEGER(wdim)[1];
	PROTECT(w = coerceVector(w, REALSXP));
	
	if ((wrows % 2 == 0) | (wcols % 2 == 0))
		error("weights matrix must have uneven sides");

	int narm = INTEGER(rmNA)[0];
	int naonly = INTEGER(NAonly)[0];

	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];
	bemean = INTEGER(domean)[0];
	n = nrow * ncol;
	PROTECT( val = allocVector(REALSXP, n) );
	PROTECT(d = coerceVector(d, REALSXP));

	int wr = floor(wrows / 2);
	int wc = floor(wcols / 2);

	xd = REAL(d);
	xval = REAL(val);
	xw = REAL(w);

	int nwc = ncol - wc - 1;
	int col = 0;
	
	if (narm) {
		if (naonly) {
		// first rows
			for (i = 0; i < ncol*wr; i++) {  
				xval[i] = xd[i];
			}

			for (i = ncol*wr; i < ncol*(nrow-wr); i++) {
				if (! R_IsNA(xd[i])) {
					xval[i] = xd[i];
				} else {
					col = i % ncol;
					if ((col < wc) | (col > nwc)) {
						xval[i] = xd[i];
					} else {
						xval[i] = 0;
						q = 0;
						p = 0;
						for (j = -wr; j <= wr; j++) {
							for (k = -wc; k <= wc; k++) {
								a = xd[j * ncol + k + i];
								if ( !R_IsNA(a) ) {
									xval[i] += a * xw[q];
									p++;
								}
								q++;
							}
						}
						if (p==0) {
							xval[i] = R_NaReal;
						} else if (bemean) {
							xval[i] = xval[i] / p;
						}
					}
				}
			}
			
		// last rows
			for (i = ncol * (nrow-wr); i < n; i++) {  
				xval[i] = xd[i];
			}
			
		} else {

			// first rows
			for (i = 0; i < ncol*wr; i++) {  
				xval[i] = R_NaReal;
			}

			for (i = ncol*wr; i < ncol * (nrow-wr); i++) {
				col = i % ncol;
				if ((col < wc) | (col > nwc)) {
					xval[i] = R_NaReal;
				} else {
					q = 0;
					p = 0;
					xval[i] = 0;
					for (j = -wr; j <= wr; j++) {
						for (k = -wc; k <= wc; k++) {
							a = xd[j * ncol + k + i];
							if ( !R_IsNA(a) ) {
								xval[i] += a * xw[q];
								p++;
							}
							q++;
						}
					}
					if (p==0) {
						xval[i] = R_NaReal;
					} else if (bemean) {
						xval[i] = xval[i] / p;
					}			
				}
			}
			
			// last rows
			for (i = ncol * (nrow-wr); i < n; i++) {  
				xval[i] = R_NaReal;
			}
		}
	} else {

		// first rows
		for (i = 0; i < ncol*wr; i++) {  
			xval[i] = R_NaReal;
		}
		for (i = ncol*wr; i < ncol * (nrow-wr); i++) {
			col = i % ncol;
			if ((col < wc) | (col > nwc)) {
				xval[i] = R_NaReal;
			} else {
				xval[i] = 0;
				q = 0;
				for (j = -wr; j <= wr; j++) {
					for (k = -wc; k <= wc; k++) {
						xval[i] += xd[j * ncol + k + i]  * xw[q];
						q++;
					}
				}
				
				if (bemean) {
					xval[i] = xval[i] / q;
				}			
			}
		}
		// last rows
		for (i = ncol * (nrow-wr); i < n; i++) {  
			xval[i] = R_NaReal;
		}		
	}
	
	UNPROTECT(3);
	return(val);
}



