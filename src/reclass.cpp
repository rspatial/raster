/* Robert Hijmans, October 2011 */

#include <Rcpp.h>

// [[Rcpp::export(name = ".reclassify")]]
Rcpp::NumericVector reclassify(Rcpp::NumericVector d, Rcpp::NumericMatrix rcl, bool dolowest, bool doright, bool NAonly, double NAval) {
					
	double lowval, lowres;
	size_t a = rcl.nrow(); 
	size_t nc = rcl.ncol();
	size_t b = a * 2;
	
	bool doleftright = false;
	if (std::isnan(doright)) {
		doleftright = true;
	}

	size_t n = d.size();
	
	Rcpp::NumericVector val(n);
	
	if (NAonly) {  // only change NA values
	
		for (size_t i=0; i<n; i++) {
			if (std::isnan(d[i])) {
				val[i] = NAval;
			} else {
				val[i] = d[i];
			}
		}
		
	} else { // "is - becomes" reclassification
		
		if (nc == 2) {
		
			for (size_t i=0; i<n; i++) {
				if (std::isnan(d[i])) {
					val[i] = NAval;
				} else {
					val[i] = d[i];
					for (size_t j=0; j<a; j++) {
						if (d[i] == rcl[j]) {
							val[i] = rcl[j+a];
							break;
						}
					}
				}
			}
		
		// "from - to - becomes" reclassification
		} else if (doleftright) {   // interval closed at left and right
			
			for (size_t i=0; i<n; i++) {
				if (std::isnan(d[i])) {
					val[i] = NAval;
				} else {
					val[i] = d[i];
					for (size_t j=0; j<a; j++) {
						if ((d[i] >= rcl[j]) & (d[i] <= rcl[j+a])) {
							val[i] = rcl[j+b];
							break;
						}
					}
				}
			}

		} else if (doright) {  // interval closed at right

			if (dolowest) {  // include lowest value (left) of interval
			
				lowval = rcl[0];
				lowres = rcl[b];
				for (size_t j=1; j<a; j++) {
					if (rcl[j] < lowval) {
						lowval = rcl[j];
						lowres = rcl[b+j];
					}
				}
				
//				Rprintf ("lowval = %f \n", lowval);
//				Rprintf ("lowres = %f \n", lowres);
				
				for (size_t i=0; i<n; i++) {
					if (std::isnan(d[i])) {
						val[i] = NAval;
					} else if (d[i] == lowval) {
						val[i] = lowres;
					} else {
						val[i] = d[i];
						for (size_t j=0; j<a; j++) {
							if ((d[i] > rcl[j]) & (d[i] <= rcl[j+a])) {
								val[i] = rcl[j+b];
								break;
							}
						}
					}
				}
				
			} else { // !dolowest

				for (size_t i=0; i<n; i++) {
					if (std::isnan(d[i])) {
						val[i] = NAval;
					} else {
						val[i] = d[i];
						for (size_t j=0; j<a; j++) {
							if ((d[i] > rcl[j]) & (d[i] <= rcl[j+a])) {
								val[i] = rcl[j+b];
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
				for (size_t j=a+1; j<b; j++) {
					if (rcl[j] > lowval) {
						lowval = rcl[j];
						lowres = rcl[a+j];
					}
				}
				
				for (size_t i=0; i<n; i++) {
					if (std::isnan(d[i])) {
						val[i] = NAval;
					} else if (d[i] == lowval) {
						val[i] = lowres;
					} else {
						val[i] = d[i];
						for (size_t j=0; j<a; j++) {
							if ((d[i] >= rcl[j]) & (d[i] < rcl[j+a])) {
								val[i] = rcl[j+b];
								break;
							}
						}
					}
				}
				
			} else { //!dolowest
			
				for (size_t i=0; i<n; i++) {
					if (std::isnan(d[i])) {
						val[i] = NAval;
					} else {
						val[i] = d[i];
						for (size_t j=0; j<a; j++) {	
							if ((d[i] >= rcl[j]) & (d[i] < rcl[j+a])) {
								val[i] = rcl[j+b];
								break;
							}
						}
					}
				}
			}
		}
	}
	
	return(val);
}


