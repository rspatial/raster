/* Robert Hijmans, October 2011 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"


SEXP _do_layerize(SEXP d, SEXP cls, SEXP falsena) {
					
	R_len_t i, j;
	int *xd, *xc, *xv, vna;
	SEXP v;

	PROTECT(d = coerceVector(d, INTSXP));
	PROTECT(cls = coerceVector(cls, INTSXP));
	int fna = INTEGER(falsena)[0];
	
	xd = INTEGER(d);
	xc = INTEGER(cls);
	
	PROTECT( v = allocVector(INTSXP, length(d) * length(cls)) );
	xv = INTEGER(v);
	
	if (fna) {
		vna = R_NaInt;
	} else {
		vna = 0;
	}
	for (i=0; i<length(v); i++) {
		xv[i] = vna;
	}
	
	int m = length(d);
	int n = length(cls);
	
	for (i=0; i<length(d); i++) {
		if (xd[i] != R_NaInt) {
			for (j=0; j<n; j++) {
				if (xd[i] == xc[j]) {
					xv[j * m + i] = 1;
					break;
				}
			}
		} else {
			for (j=0; j<n; j++) {
				xv[j * m + i] = R_NaInt;
			}
		}
	}
	UNPROTECT(3);
	return(v);
}

