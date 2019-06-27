/* Robert Hijmans, October 2011 */

#include <Rcpp.h>

// [[Rcpp::export(name = ".clamp")]]
Rcpp::NumericVector do_clamp(std::vector<double> d, std::vector<double> r, bool usevals) {
					
	size_t n = d.size();
	Rcpp::NumericVector val(n);
	
	if (usevals) {
		for (size_t i=0; i<n; i++) {
			if ( d[i] < r[0] ) { 
				val[i] = r[0];
			} else if ( d[i] > r[1] ) { 
				val[i] = r[1];
			} else {
				val[i] = d[i];
			}
		}		
	} else {
		for (size_t i=0; i<n; i++) {
			if ( (d[i] < r[0]) | (d[i] > r[1])) {
				val[i] = NAN;
			} else {
				val[i] = d[i];
			}
		}	
	}
	return(val);
}


