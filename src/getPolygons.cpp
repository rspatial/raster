#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name = ".getPolygons")]]
NumericMatrix getPolygons(NumericMatrix xyv, NumericVector res, int nodes) {

	int n = xyv.nrow();
	double xr = res(0)/2;
	double yr = res(1)/2;
	
	if (nodes == 4) {
		NumericMatrix cr(n, 10);
		for (int i = 0; i < n; i++) {
			cr(i, 0) = xyv(i, 0) - xr;
			cr(i, 1) = xyv(i, 0) + xr;
			cr(i, 2) = cr(i, 1);
			cr(i, 3) = cr(i, 0);
			cr(i, 4) = cr(i, 0);
			cr(i, 5) = xyv(i, 1) + yr;
			cr(i, 6) = cr(i, 5);
			cr(i, 7) = xyv(i, 1) - yr;
			cr(i, 8) = cr(i, 7);
			cr(i, 9) = cr(i, 5);
		}
		return cr;
	} else if (nodes == 8) {
		NumericMatrix cr(n, 18);
		for (int i = 0; i < n; i++) {
			cr(i, 0) = xyv(i, 0) - xr;
			cr(i, 1) = xyv(i, 0);
			cr(i, 2) = xyv(i, 0) + xr;
			cr(i, 3) = cr(i, 2);
			cr(i, 4) = cr(i, 2);
			cr(i, 5) = cr(i, 1);
			cr(i, 6) = cr(i, 0);
			cr(i, 7) = cr(i, 0);
			cr(i, 8) = cr(i, 0);			
			cr(i, 9) = xyv(i, 1) + yr;
			cr(i, 10) = cr(i, 9);
			cr(i, 11) = cr(i, 9);	
			cr(i, 12) = xyv(i, 1);		
			cr(i, 13) = xyv(i, 1) - yr;
			cr(i, 14) = cr(i, 13);
			cr(i, 15) = cr(i, 13);
			cr(i, 16) = cr(i, 12);
			cr(i, 17) = cr(i, 9);
		}
		return cr;
		
	} else {
	
		NumericMatrix cr(n, 34);
		for (int i = 0; i < n; i++) {
			cr(i, 0) = xyv(i, 0) - xr;
			cr(i, 1) = xyv(i, 0) - 0.5 * xr;
			cr(i, 2) = xyv(i, 0);
			cr(i, 3) = xyv(i, 0) + 0.5 * xr;
			cr(i, 4) = xyv(i, 0) + xr;
			cr(i, 5) = cr(i, 4);
			cr(i, 6) = cr(i, 4);
			cr(i, 7) = cr(i, 4);
			cr(i, 8) = cr(i, 4);
			cr(i, 9) = cr(i, 3);
			cr(i, 10) = cr(i, 2);
			cr(i, 11) = cr(i, 1);
			cr(i, 12) = cr(i, 0);
			cr(i, 13) = cr(i, 0);
			cr(i, 14) = cr(i, 0);
			cr(i, 15) = cr(i, 0);
			cr(i, 16) = cr(i, 0);
			
			cr(i, 17) = xyv(i, 1) + yr;
			cr(i, 18) = cr(i, 17);
			cr(i, 19) = cr(i, 17);
			cr(i, 20) = cr(i, 17);
			cr(i, 21) = cr(i, 17);	
			cr(i, 22) = xyv(i, 1) + 0.5 * yr;
			cr(i, 23) = xyv(i, 1);
			cr(i, 24) = xyv(i, 1) - 0.5 * yr;			
			cr(i, 25) = xyv(i, 1) - yr;
			cr(i, 26) = cr(i, 25);
			cr(i, 27) = cr(i, 25);
			cr(i, 28) = cr(i, 25);
			cr(i, 29) = cr(i, 25);
			cr(i, 30) = cr(i, 24);
			cr(i, 31) = cr(i, 23);
			cr(i, 32) = cr(i, 22);
			cr(i, 33) = cr(i, 17);
		
		}
		return cr;
		
	}
}
