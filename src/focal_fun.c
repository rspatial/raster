/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

SEXP _focal_fun(SEXP d, SEXP w, SEXP dim, SEXP fun, SEXP NAonly, SEXP rho) {

	R_len_t i, j, k, q;
    SEXP R_fcall, ans, x;	
	int nrow, ncol, n, wn;
	double *xd, *xans, *xw, *xx;

    if(!isFunction(fun)) error("'fun' must be a function");
    if(!isEnvironment(rho)) error("'rho' should be an environment");
    PROTECT(R_fcall = lang2(fun, R_NilValue));
	
	SEXP wdim = getAttrib(w, R_DimSymbol);
	int wrows = INTEGER(wdim)[0];
	int wcols = INTEGER(wdim)[1];
	wn = wrows * wcols;
    if(wn==0) error("'w' has zero dimension(s)");
 	
	PROTECT(d = coerceVector(d, REALSXP));
	PROTECT(w = coerceVector(w, REALSXP));
	
	nrow = INTEGER(dim)[0];
	ncol = INTEGER(dim)[1];
	int naonly = INTEGER(NAonly)[0];

	n = nrow * ncol;
	PROTECT( ans = allocVector(REALSXP, n) );
	PROTECT(x = allocVector(REALSXP, wn));
	xx = REAL(x);
	
	if ((wrows % 2 == 0) | (wcols % 2 == 0))
		error("weights matrix must have uneven sides");
	int wr = floor(wrows / 2);
	int wc = floor(wcols / 2);

	xd = REAL(d);
	xans = REAL(ans);
	xw = REAL(w);

	int nwc = ncol - wc - 1;
	int col = 0;
	
	if (naonly) {
		// first rows
		for (i = 0; i < ncol*wr; i++) {  
			xans[i] = xd[i];
		}
		for (i = ncol*wr; i < ncol * (nrow-wr); i++) {
			if (!R_IsNA(xd[i])) {
				xans[i] = xd[i];
			} else {
				col = i % ncol;
				if ((col < wc) | (col > nwc)) {
					xans[i] = xd[i];
				} else {
					q = 0;
					for (j = -wr; j <= wr; j++) {
						for (k = -wc; k <= wc; k++) {
							xx[q] = xd[j * ncol + k + i] * xw[q];
							q++;
						}
					}
					SETCADR(R_fcall, x);
					xans[i] = REAL(eval(R_fcall, rho))[0];

					if (R_IsNaN(xans[i])) {
						xans[i] = R_NaReal;
					}
					
				}
			}
		}
		// last rows
		for (i = ncol * (nrow-wr); i < n; i++) {  
			xans[i] = xd[i];
		}
		
	} else {

		// first rows
		for (i = 0; i < ncol*wr; i++) {  
			xans[i] = R_NaReal;
		}
		
		for (i = ncol*wr; i < (ncol * (nrow-wr)); i++) {
			col = i % ncol;
			if ((col < wc) | (col > nwc)) {
				xans[i] = R_NaReal;
			} else {
				q = 0;
				for (j = -wr; j <= wr; j++) {
					for (k = -wc; k <= wc; k++) {
						xx[q] = xd[j * ncol + k + i] * xw[q];
						q++;
					}
				}
				SETCADR(R_fcall, x);
				xans[i] = REAL(eval(R_fcall, rho))[0];
				if (R_IsNaN(xans[i])) {
					xans[i] = R_NaReal;
				}
				
			}
		}
		// last rows
		for (i = ncol * (nrow-wr); i < n; i++) {  
			xans[i] = R_NaReal;
		}
	}
	
	UNPROTECT(5);
	return(ans);
}

