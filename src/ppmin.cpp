#include <Rcpp.h>
using namespace Rcpp;

// Simple & fast pmin & pmax with no checking for NA values!

// [[Rcpp::export(name = ".doSpmin")]]
NumericVector doSpmin(NumericVector x, NumericVector y) {
	int n = x.length();
	// NumericVector out = clone(x);
	for (int i = 0; i < n; ++i) {
		if (x[i] > y[i]) {
			x[i] = y[i];
		}
	}
	return x;
}

// [[Rcpp::export(name = ".doSpmax")]]
NumericVector doSpmax(NumericVector x, NumericVector y) {
	int n = x.length();
	//NumericVector out = clone(x);
	for (int i = 0; i < n; ++i) {
		if (x[i] < y[i]) {
			x[i] = y[i];
		}
	}
	return x;
}


// These functions check for NA, but are not that much faster than pmin

// [[Rcpp::export(name = ".ppmin")]]
NumericVector ppmin(NumericVector x, NumericVector y, bool narm) {
	int n = x.length();
	//NumericVector out = clone(x);
	if (narm) {
		for (int i = 0; i < n; ++i) {
			if (NumericVector::is_na(x[i])) {
				x[i] = y[i];			
			} else if (x[i] > y[i]) {
				x[i] = y[i];
			}
		}  
	} else {
		for (int i = 0; i < n; ++i) {
			if (NumericVector::is_na(y[i])) {
				x[i] = y[i];			
			} else if (x[i] > y[i]) {
				x[i] = y[i];
			}
		}
	}
	return x;
}

// [[Rcpp::export(name = ".ppmax")]]
NumericVector ppmax(NumericVector x, NumericVector y, bool narm) {
	int n = x.length();
	//NumericVector out = clone(x);
	if (narm) {
		for (int i = 0; i < n; ++i) {
			if (NumericVector::is_na(x[i])) {
				x[i] = y[i];			
			} else if (x[i] < y[i]) {
				x[i] = y[i];
			}
		}  
	} else {
		for (int i = 0; i < n; ++i) {
			if (NumericVector::is_na(y[i])) {
				x[i] = y[i];			
			} else if (x[i] < y[i]) {
				x[i] = y[i];
			}
		}
	}
	return x;
}


// fast rowMin and rowMax

// [[Rcpp::export(name = ".doRowMin")]]
NumericVector doRowMin(NumericMatrix x, bool narm) {
	int nrow = x.nrow(), ncol = x.ncol();
	NumericVector out(nrow);
	
	if (narm) {
		for (int i = 0; i < nrow; i++) {
			out[i] = INFINITY;
			for (int j = 0; j < ncol; j++) {
				if (x(i,j) < out[i]) {
					out[i] = x(i,j);
				}
			}
			if (out[i] == INFINITY) {
				out[i] = NA_REAL;	
			} 
		}
	} else {
		for (int i = 0; i < nrow; i++) {
			out[i] = INFINITY;
			for (int j = 0; j < ncol; j++) {
				if (NumericVector::is_na(x(i,j))) {
					out[i] = NA_REAL;
					break;
				}
				if (x(i,j) < out[i]) {
					out[i] = x(i,j);
				}
			}
			if (out[i] == INFINITY) {
				out[i] = NA_REAL;	
			} 
		}
	}
	return out;
}


// [[Rcpp::export(name = ".doRowMax")]]
NumericVector doRowMax(NumericMatrix x, bool narm) {
	int nrow = x.nrow(), ncol = x.ncol();
	NumericVector out(nrow);
	
	if (narm) {
		for (int i = 0; i < nrow; i++) {
			out[i] = -INFINITY;
			for (int j = 0; j < ncol; j++) {
				if (x(i,j) > out[i]) {
					out[i] = x(i,j);
				}
			}
			if (out[i] == -INFINITY) {
				out[i] = NA_REAL;	
			} 
		}
	} else {
		for (int i = 0; i < nrow; i++) {
			out[i] = -INFINITY;
			for (int j = 0; j < ncol; j++) {
				if (NumericVector::is_na(x(i,j))) {
					out[i] = NA_REAL;
					break;
				}
				if (x(i,j) > out[i]) {
					out[i] = x(i,j);
				}
			}
			if (out[i] == -INFINITY) {
				out[i] = NA_REAL;	
			} 
		}
	}
	return out;
}

