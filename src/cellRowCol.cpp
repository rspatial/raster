#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name = ".doCellFromRowCol")]]
NumericVector doCellFromRowCol(IntegerVector nrow, IntegerVector ncol,
  IntegerVector rownr, IntegerVector colnr) {

  int nr = nrow[0];
  int nc = ncol[0];
  size_t rownr_size = rownr.size();
  size_t colnr_size = colnr.size();
  
  NumericVector result(std::max(rownr_size, colnr_size));

  // Manually recycle the shorter of rownr/colnr to match the other
  size_t len = std::max(rownr.size(), colnr.size());

  for (size_t i = 0; i < len; i++) {
    // The % is to recycle elements if they're not the same length
    double r = rownr[i < rownr_size ? i : i % rownr_size];
    double c = colnr[i < colnr_size ? i : i % colnr_size];

    // Detect out-of-bounds rows/cols and use NA for those
    result[i] = (r<1 || r>nr || c<1 || c>nc) ? NA_REAL : (r-1) * nc + c;
  }
  
  return result;
}
