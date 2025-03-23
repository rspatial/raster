#include <Rcpp.h>
#include "aggregate.h"



Rcpp::NumericMatrix std2rcp( std::vector<std::vector<double>> x) {
  size_t nr = x.size(), nc = x[0].size() ;
  Rcpp::NumericMatrix m((int)nr, (int)nc ) ;
  for( size_t i=0; i<nr; i++){
    for( size_t j=0; j<nc; j++) {
      m(i,j) = x[i][j] ;
    }
  }
  return(m);
}



std::vector<std::vector<double > > rcp2std( Rcpp::NumericMatrix x) {
  size_t nr = x.nrow(), nc = x.ncol();
  std::vector< std::vector<double> > m(nr, std::vector<double>(nc));
  for( size_t i=0; i<nr; i++) {
    for( size_t j=0; j<nc; j++){
      m[i][j] = x(i,j) ;
    }
  }
  return(m);
}



// [[Rcpp::export(name = ".aggregate_get")]]
Rcpp::NumericMatrix aggregate_get(Rcpp::NumericMatrix d, Rcpp::NumericVector dims) {
  
  std::vector<std::vector< double > > x = rcp2std(d);
  std::vector<int> y = Rcpp::as<std::vector<int> >(dims);
  
  y = get_dims(y);
  x = get_aggregates(x, y);
  
  Rcpp::NumericMatrix z = std2rcp(x);
  return(z);
}


// [[Rcpp::export(name = ".aggregate_fun")]]
Rcpp::NumericMatrix aggregate_fun(Rcpp::NumericMatrix d, Rcpp::NumericVector dims, bool narm, int fun) {
  
  std::vector<std::vector< double > > x = rcp2std(d);
  std::vector<int> y = Rcpp::as<std::vector<int> >(dims);
  
  y = get_dims(y);
  x = aggregate(x, y, narm, fun);
  
  Rcpp::NumericMatrix z = std2rcp(x);
  return(z);
}

