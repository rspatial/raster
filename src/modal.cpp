#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name = ".getMode")]]
double getMode(NumericVector values, int ties) {
	int n = values.length();
    IntegerVector counts(n);

	if (ties < 3) {
		std::sort(values.begin(), values.end());
	}
	
    for (int i = 0; i < n; ++i) {
        counts[i] = 0;
        int j = 0;
        while ((j < i) && (values[i] != values[j])) {
            ++j;
        }
        ++(counts[j]);
    }
	
    int maxCount = 0;
	// first (lowest due to sorting)
	if (ties == 0) {
		for (int i = 1; i < n; ++i) {
			if (counts[i] > counts[maxCount]) {
				maxCount = i;
			}
		}
	// last	
	} else if (ties == 1) {
		for (int i = 1; i < n; ++i) {
			if (counts[i] >= counts[maxCount]) {
				maxCount = i;
			}
		}

	// dont care (first, but not sorted)
	} else if (ties == 2) {
		for (int i = 1; i < n; ++i) {
			if (counts[i] > counts[maxCount]) {
				maxCount = i;
			}
		}

	// random
	} else if (ties == 3) {
		int tieCount = 1;
		for (int i = 1; i < n; ++i) {
			if (counts[i] > counts[maxCount]) {
				maxCount = i;
				tieCount = 1;
			} else if (counts[i] == counts[maxCount]) {
				tieCount++;
				if (R::runif(0,1) < (1.0 / tieCount)) {
					maxCount = i;
				}			
			}
		}
		
   // NA		
	} else {
		int tieCount = 1;
		for (int i = 1; i < n; ++i) {
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

