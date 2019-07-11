/* Robert Hijmans, October 2011 */

#include <Rcpp.h>

// [[Rcpp::export(name = ".focal_fun")]]
std::vector<double> do_focal_fun(std::vector<double> d, Rcpp::NumericMatrix w, std::vector<unsigned> dim, Rcpp::Function fun, bool naonly) {


	int nrow = dim[0];
	int ncol = dim[1];
	int n = nrow * ncol;

	int wrows = w.nrow();
	int wcols = w.ncol();
	size_t wn = wrows * wcols;

	std::vector<double> ans(n);
	std::vector<double> x(wn);
	
	if ((wrows % 2 == 0) | (wcols % 2 == 0)){
		Rcpp::Rcerr << "weights matrix must have uneven sides";
		return(ans);
	}
	int wr = wrows / 2;
	int wc = wcols / 2;
	wr = std::min(wr, nrow);
	wc = std::min(wc, ncol);
	

	int nwc = ncol - wc - 1;
	int col = 0;
	
	if (naonly) {
		// first rows
		for (int i = 0; i < ncol*wr; i++) {  
			ans[i] = d[i];
		}
		for (int i = ncol*wr; i < ncol * (nrow-wr); i++) {
			if (!std::isnan(d[i])) {
				ans[i] = d[i];
			} else {
				col = i % ncol;
				if ((col < wc) | (col > nwc)) {
					ans[i] = d[i];
				} else {
					size_t q = 0;
					for (int j = -wr; j <= wr; j++) {
						for (int k = -wc; k <= wc; k++) {
							x[q] = d[j * ncol + k + i] * w[q];
							q++;
						}
					}
					Rcpp::NumericVector out = fun(x);
					ans[i] = out[0];

					if (std::isnan(ans[i])) {
						ans[i] = NAN;
					}
					
				}
			}
		}
		// last rows
		for (int i = ncol * (nrow-wr); i < n; i++) {  
			ans[i] = d[i];
		}
		
	} else {

		// first rows
		for (int i = 0; i < ncol*wr; i++) {  
			ans[i] = NAN;
		}
		
		for (int i = ncol*wr; i < (ncol * (nrow-wr)); i++) {
			col = i % ncol;
			if ((col < wc) | (col > nwc)) {
				ans[i] = NAN;
			} else {
				size_t q = 0;
				for (int j = -wr; j <= wr; j++) {
					for (int k = -wc; k <= wc; k++) {
						x[q] = d[j * ncol + k + i] * w[q];
						q++;
					}
				}
				Rcpp::NumericVector out = fun(x);
				ans[i] = out[0];
				if (std::isnan(ans[i])) {
					ans[i] = NAN;
				}
				
			}
		}
		// last rows
		for (int i = ncol * (nrow-wr); i < n; i++) {  
			ans[i] = NAN;
		}
	}
	
	return(ans);
}

