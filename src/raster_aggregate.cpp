#include <Rcpp.h>
using namespace Rcpp;
#include "aggregate.h"



NumericMatrix std2rcp(	std::vector<std::vector<double > > x) {
  int nr = x.size(), nc = x[0].size() ;
  NumericMatrix m( nr, nc ) ;
  for( int i=0; i<nr; i++){
    for( int j=0; j<nc; j++) {
      m(i,j) = x[i][j] ;
    }
  }
  return(m);
}



std::vector<std::vector<double > > rcp2std( NumericMatrix x) {
  int nr = x.nrow(), nc = x.ncol();
  std::vector< std::vector<double> > m(nr, std::vector<double>(nc));
  for( int i=0; i<nr; i++) {
    for( int j=0; j<nc; j++){
      m[i][j] = x(i,j) ;
    }
  }
  return(m);
}



// [[Rcpp::export(name = ".aggregate_get")]]
NumericMatrix aggregate_get(NumericMatrix d, NumericVector dims) {
  
  std::vector<std::vector< double > > x = rcp2std(d);
  std::vector<int> y = Rcpp::as<std::vector<int> >(dims);
  
  y = get_dims(y);
  x = get_aggregates(x, y);
  
  NumericMatrix z = std2rcp(x);
  return(z);
}


// [[Rcpp::export(name = ".aggregate_fun")]]
NumericMatrix aggregate_fun(NumericMatrix d, NumericVector dims, bool narm, int fun) {
  
  std::vector<std::vector< double > > x = rcp2std(d);
  std::vector<int> y = Rcpp::as<std::vector<int> >(dims);
  
  y = get_dims(y);
  x = aggregate(x, y, narm, fun);
  
  NumericMatrix z = std2rcp(x);
  return(z);
}
