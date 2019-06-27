/* Robert Hijmans, November 2011 */

#include <Rcpp.h>

// [[Rcpp::export(name = ".edge")]]
std::vector<double> do_edge(std::vector<double> d, std::vector<int> dim, bool classes, bool edgetype, unsigned dirs) {

	bool falseval = 0;
	
	size_t nrow = dim[0];
	size_t ncol = dim[1];
	size_t n = nrow * ncol;
	std::vector<double> val(n);
	
	int r[8] = { -1,0,0,1 , -1,-1,1,1};
	int c[8] = { 0,-1,1,0 , -1,1,-1,1};	
	
	if (!classes) {
		if (!edgetype) { // inner
			for (size_t i = 1; i < (nrow-1); i++) {
				for (size_t j = 1; j < (ncol-1); j++) {
					size_t cell = i*ncol+j;
					val[cell] = NAN;
					if ( !std::isnan(d[cell])) {
						val[cell] = falseval;
						for (size_t k=0; k< dirs; k++) {
							if ( std::isnan(d[cell + r[k] * ncol + c[k]])) {
								val[cell] = 1;
								break;
							}
						}
					}
				}
			}
		
		} else if (edgetype) { //outer
			for (size_t i = 1; i < (nrow-1); i++) {
				for (size_t j = 1; j < (ncol-1); j++) {
					size_t cell = i*ncol+j;
					val[cell] = falseval;
					if (std::isnan(d[cell])) {
						val[cell] = NAN;
						for (size_t k=0; k < dirs; k++) {			
							if ( !std::isnan(d[cell+ r[k] * ncol + c[k] ])) {
								val[cell] = 1;
								break;
							}
						}
					}
				}
			}
		} 
	} else { // by class
		int test;
		for (size_t i = 1; i < (nrow-1); i++) {
			for (size_t j = 1; j < (ncol-1); j++) {
				size_t cell = i*ncol+j;
				test = d[ cell+r[0]*ncol+c[0] ];
				if (std::isnan(test)) {
					val[cell] = NAN;			
				} else {
					val[cell] = falseval;
				}
				for (size_t k=1; k < dirs; k++) {
					if (test != d[ cell+r[k]*ncol +c[k] ]) {
						val[cell] = 1;
						break;
					}
				}
			}
		}

	}
	return(val);
}


