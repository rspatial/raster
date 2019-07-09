/* Robert Hijmans, October 2011 */

#include <Rcpp.h>

// [[Rcpp::export(name = ".layerize")]]
Rcpp::NumericVector layerize(std::vector<int> d, std::vector<int> cls, bool falsena) {
	
	int vna = falsena ? R_NaInt :  0;
	size_t m = d.size();
	size_t n = cls.size();
	Rcpp::NumericVector v(m * n, vna);
	
	for (size_t i=0; i<m; i++) {
		if (d[i] != R_NaInt) {
			for (size_t j=0; j<n; j++) {
				if (d[i] == cls[j]) {
					v[j * m + i] = 1;
					break;
				}
			}
		} 
	}
	return(v);
}

