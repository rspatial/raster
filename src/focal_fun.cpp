/* Robert Hijmans, October 2011 */

#include <Rcpp.h>

// [[Rcpp::export(name = ".focal_fun")]]
std::vector<double> do_focal_fun(std::vector<double> d, Rcpp::NumericMatrix w, std::vector<unsigned> dim, Rcpp::Function fun, bool naonly) {


	size_t nrow = dim[0];
	size_t ncol = dim[1];
	size_t n = nrow * ncol;

	size_t wrows = w.nrow();
	size_t wcols = w.ncol();
	size_t wn = wrows * wcols;
 	

	std::vector<double> ans(n);
	std::vector<double> x(wn);
	
	if ((wrows % 2 == 0) | (wcols % 2 == 0)){
		Rcpp::Rcerr << "weights matrix must have uneven sides";
		return(ans);
	}
	int wr = floor(wrows / 2);
	int wc = floor(wcols / 2);


	int nwc = ncol - wc - 1;
	int col = 0;
	
	if (naonly) {
		// first rows
		for (size_t i = 0; i < ncol*wr; i++) {  
			ans[i] = d[i];
		}
		for (size_t i = ncol*wr; i < ncol * (nrow-wr); i++) {
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
		for (size_t i = ncol * (nrow-wr); i < n; i++) {  
			ans[i] = d[i];
		}
		
	} else {

		// first rows
		for (size_t i = 0; i < ncol*wr; i++) {  
			ans[i] = NAN;
		}
		
		for (size_t i = ncol*wr; i < (ncol * (nrow-wr)); i++) {
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
		for (size_t i = ncol * (nrow-wr); i < n; i++) {  
			ans[i] = NAN;
		}
	}
	
	return(ans);
}

