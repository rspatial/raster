/* Robert Hijmans, October 2011 */

#include <Rcpp.h>

// [[Rcpp::export(name = ".focal_get")]]
std::vector<double> do_focal_get(std::vector<double> d, std::vector<unsigned> dim, std::vector<unsigned> ngb) {

	int nrow = dim[0];
	int ncol = dim[1];

	int wrows = ngb[0];
	int wcols = ngb[1];

	size_t n = (nrow-wrows+1) * (ncol-wcols+1) * wrows * wcols;
	std::vector<double> val(n);
	
	if ((wrows % 2 == 0) | (wcols % 2 == 0)) {
		Rcpp::Rcerr << "weights matrix must have uneven sides";
		return(val);
	}

	int wr = wrows / 2;
	int wc = wcols / 2;
	wr = std::min(wr, nrow);
	wc = std::min(wc, ncol);
	
	int f = 0;
	
	for (int i = 0+wr; i < nrow-wr; i++) {
		for (int j = 0+wc; j < ncol-wc; j++) {
			for (int a=-wr; a <= wr ; a++) {
			int aa = (i+a) * ncol;
				for (int b=-wc; b <= wc ; b++) {
					val[f] = d[aa+j+b];
					f++;
				}
			}
		}
	}	
	
	return(val);
}



