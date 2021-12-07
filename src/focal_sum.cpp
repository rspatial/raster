/* Robert Hijmans, October 2011 */

#include <Rcpp.h>

// [[Rcpp::export(name = ".focal_sum")]]
std::vector<double> do_focal_sum(std::vector<double> d, Rcpp::NumericMatrix w, std::vector<double> dim, bool narm, bool naonly, bool bemean) {

	int wrows = w.nrow();
	int wcols = w.ncol();
	int nrow = dim[0];
	int ncol = dim[1];
	int n = nrow * ncol;
	std::vector<double> val(n);
	
	if ((wrows % 2 == 0) | (wcols % 2 == 0)){
		Rcpp::Rcerr << wrows << " " << wcols << "\n";		
		Rcpp::Rcerr << "weights matrix must have uneven sides\n";
		return(val);
	}

	int wr = wrows / 2;
	int wc = wcols / 2;
	wr = std::min(wr, nrow);
	wc = std::min(wc, ncol);

	int nwc = ncol - wc - 1;
	int col = 0;
	
	if (narm) {
		if (naonly) {
		// first rows
			for (int i = 0; i < ncol*wr; i++) {  
				val[i] = d[i];
			}

			for (int i = ncol*wr; i < ncol*(nrow-wr); i++) {
				if (! std::isnan(d[i])) {
					val[i] = d[i];
				} else {
					col = i % ncol;
					if ((col < wc) | (col > nwc)) {
						val[i] = d[i];
					} else {
						val[i] = 0;
						size_t q = 0;
						size_t p = 0;
						for (int j = -wr; j <= wr; j++) {
							for (int k = -wc; k <= wc; k++) {
								if (!std::isnan(w[q])) {
									double a = d[j * ncol + k + i];
									if ( !std::isnan(a) ) {
										val[i] += a * w[q];
										p++;
									}
									q++;
								}
							}
						}
						if (p==0) {
							val[i] = NAN;
						} else if (bemean) {
							val[i] = val[i] / p;
						}
					}
				}
			}
			
		// last rows
			for (int i = ncol * (nrow-wr); i < n; i++) {  
				val[i] = d[i];
			}
			
		} else {

			// first rows
			for (int i = 0; i < ncol*wr; i++) {  
				val[i] = NAN;
			}

			for (int i = ncol*wr; i < ncol * (nrow-wr); i++) {
				col = i % ncol;
				if ((col < wc) | (col > nwc)) {
					val[i] = NAN;
				} else {
					size_t q = 0;
					size_t p = 0;
					val[i] = 0;
					for (int j = -wr; j <= wr; j++) {
						for (int k = -wc; k <= wc; k++) {
							if (!std::isnan(w[q])) {				
								double a = d[j * ncol + k + i];
								if ( !std::isnan(a) ) {
									val[i] += a * w[q];
									p++;
								}
							}
							q++;
						}
					}
					if (p==0) {
						val[i] = NAN;
					} else if (bemean) {
						val[i] = val[i] / p;
					}			
				}
			}
			
			// last rows
			for (int i = ncol * (nrow-wr); i < n; i++) {  
				val[i] = NAN;
			}
		}
	} else {
		if (naonly) {

			// first rows
			for (int i = 0; i < ncol*wr; i++) {  
				val[i] = d[i];
			}
			
			for (int i = ncol*wr; i < ncol * (nrow-wr); i++) {
				bool disnan = std::isnan(d[i]);
				if (!disnan) {
					val[i] = d[i];
				} else {

					col = i % ncol;
					if ((col < wc) | (col > nwc)) {
						val[i] = NAN;
					} else {
						val[i] = 0;
						size_t q = 0;
						if (disnan) {
							for (int j = -wr; j <= wr; j++) {
								bool jnot0 = j != 0;
								for (int k = -wc; k <= wc; k++) {
									if (!std::isnan(w[q])) {	
										if (jnot0 && (k != 0)) {
											val[i] += d[j * ncol + k + i]  * w[q];
										}
										q++;
									}
								}
							}
						} else {
							for (int j = -wr; j <= wr; j++) {
								for (int k = -wc; k <= wc; k++) {
									if (!std::isnan(w[q])) {
										val[i] += d[j * ncol + k + i]  * w[q];
									}
									q++;
								}
							}
						}
						if (bemean) {
							val[i] = val[i] / q;
						}			
					}
				}
				// last rows
				for (int i = ncol * (nrow-wr); i < n; i++) {  
					val[i] = d[i];
				}		
			}
			
		} else {

			// first rows
			for (int i = 0; i < ncol*wr; i++) {  
				val[i] = NAN;
			}
			for (int i = ncol*wr; i < ncol * (nrow-wr); i++) {
				col = i % ncol;
				if ((col < wc) | (col > nwc)) {
					val[i] = NAN;
				} else {
					val[i] = 0;
					size_t q = 0;
					for (int j = -wr; j <= wr; j++) {
						for (int k = -wc; k <= wc; k++) {
							if (!std::isnan(w[q])) {				
								val[i] += d[j * ncol + k + i]  * w[q];
							}
							q++;
						}
					}
					
					if (bemean) {
						val[i] = val[i] / q;
					}			
				}
			}
			// last rows
			for (int i = ncol * (nrow-wr); i < n; i++) {  
				val[i] = NAN;
			}		
		}
	}
	return(val);
}



