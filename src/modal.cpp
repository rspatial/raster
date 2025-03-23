#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name = ".getMode")]]
double getMode(NumericVector values, int ties) {
	size_t n = values.length();
    IntegerVector counts(n);

	if (ties < 2) {
		std::sort(values.begin(), values.end());
	}
	
    for (size_t i = 0; i < n; ++i) {
        counts[i] = 0;
        size_t j = 0;
        while ((j < i) && (values[i] != values[j])) {
            ++j;
        }
        ++(counts[j]);
    }
	
    size_t maxCount = 0;
	// first (lowest due to sorting)
	if (ties == 0) {
		for (size_t i = 1; i < n; ++i) {
			if (counts[i] > counts[maxCount]) {
				maxCount = i;
			}
		}
	// last	
	} else if (ties == 1) {
		for (size_t i = 1; i < n; ++i) {
			if (counts[i] >= counts[maxCount]) {
				maxCount = i;
			}
		}

	// dont care (first, but not sorted)
	} else if (ties == 2) {
		for (size_t i = 1; i < n; ++i) {
			if (counts[i] > counts[maxCount]) {
				maxCount = i;
			}
		}

	// random
	} else if (ties == 3) {
		size_t tieCount = 1;
		for (size_t i = 1; i < n; ++i) {
			if (counts[i] > counts[maxCount]) {
				maxCount = i;
				tieCount = 1;
			} else if (counts[i] == counts[maxCount]) {
				tieCount++;
				double vv = (1.0 / (double)tieCount);
				if (R::runif(0,1) < vv) {
					maxCount = i;
				}			
			}
		}
		
   // NA		
	} else {
		size_t tieCount = 1;
		for (size_t i = 1; i < n; ++i) {
			if (counts[i] > counts[maxCount]) {
				maxCount = i;
				tieCount = 1;
			} else if (counts[i] == counts[maxCount]) {
				tieCount++;
			}
		}
		if (tieCount > 1 ) {
			return(NA_REAL);
		}
	}
	
    return values[maxCount];
}

