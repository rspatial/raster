/* Robert Hijmans, October 2011 
July 2016
*/

#include <Rcpp.h>

#include "distance.h"
#include "area.h"


// [[Rcpp::export(name = ".get_area_polygon")]]
Rcpp::NumericVector get_area_polygon(Rcpp::NumericMatrix d, bool lonlat) {
	std::vector<int> pols(d(Rcpp::_,0).begin(), d(Rcpp::_,0).end());
	std::vector<int> parts(d(Rcpp::_,1).begin(), d(Rcpp::_,1).end());
	std::vector<int> holes(d(Rcpp::_,3).begin(), d(Rcpp::_,3).end());
	std::vector<double> x(d(Rcpp::_,4).begin(), d(Rcpp::_,4).end());
	std::vector<double> y(d(Rcpp::_,5).begin(), d(Rcpp::_,5).end());
	std::vector<double> out;
	
	if (lonlat) { // wgs84
		double a = 6378137;
		double f = 1/298.257223563;
		out = area_polygon_lonlat(x, y, pols, parts, holes, a, f);
	} else {
		out = area_polygon_plane(x, y, pols, parts, holes);
	}
	
	Rcpp::NumericVector r( out.begin(), out.end() );
	return( r );		
}


// [[Rcpp::export(name = ".point_distance")]]
Rcpp::NumericVector point_distance(Rcpp::NumericMatrix p1, Rcpp::NumericMatrix p2, bool lonlat, double a, double f) {
	std::vector<double> px1(p1(Rcpp::_,0).begin(), p1(Rcpp::_,0).end());
	std::vector<double> py1(p1(Rcpp::_,1).begin(), p1(Rcpp::_,1).end());
	std::vector<double> px2(p2(Rcpp::_,0).begin(), p2(Rcpp::_,0).end());
	std::vector<double> py2(p2(Rcpp::_,1).begin(), p2(Rcpp::_,1).end());
    Rcpp::NumericVector res;	

	if (lonlat) {
		res = distance_lonlat(px1, py1, px2, py2, a, f);
	} else {
		res = distance_plane(px1, py1, px2, py2);
	}
	
	return(res);
}


// [[Rcpp::export(name = ".distanceToNearestPoint")]]
Rcpp::NumericVector distanceToNearestPoint(Rcpp::NumericMatrix d, Rcpp::NumericMatrix p, bool lonlat, double a, double f) {
	
	std::vector<double> dx(d(Rcpp::_,0).begin(), d(Rcpp::_,0).end());
	std::vector<double> dy(d(Rcpp::_,1).begin(), d(Rcpp::_,1).end());
	std::vector<double> px(p(Rcpp::_,0).begin(), p(Rcpp::_,0).end());
	std::vector<double> py(p(Rcpp::_,1).begin(), p(Rcpp::_,1).end());
	
    Rcpp::NumericVector res;	

	if (lonlat) {
		res = distanceToNearest_lonlat(dx, dy, px, py, a, f);
	} else {
		res = distanceToNearest_plane(dx, dy, px, py);
	}
	return(res) ;
}


// [[Rcpp::export(name = ".directionToNearestPoint")]]
Rcpp::NumericVector directionToNearestPoint(Rcpp::NumericMatrix d, Rcpp::NumericMatrix p, bool lonlat, bool degrees, bool from, double a, double f) {

	std::vector<double> dx(d(Rcpp::_,0).begin(), d(Rcpp::_,0).end());
	std::vector<double> dy(d(Rcpp::_,1).begin(), d(Rcpp::_,1).end());
	std::vector<double> px(p(Rcpp::_,0).begin(), p(Rcpp::_,0).end());
	std::vector<double> py(p(Rcpp::_,1).begin(), p(Rcpp::_,1).end());

    Rcpp::NumericVector res;	
	if (lonlat) {
		res = directionToNearest_lonlat(dx, dy, px, py, degrees, from, a, f);
	} else {
		res = directionToNearest_plane(dx, dy, px, py, degrees, from);
	}
	return(res) ;
}




// [[Rcpp::export(name = ".dest_point")]]
Rcpp::NumericMatrix dest_point(Rcpp::NumericMatrix xybd, bool lonlat, double a, double f) {

	std::vector<double> x(xybd(Rcpp::_,0).begin(), xybd(Rcpp::_,0).end());
	std::vector<double> y(xybd(Rcpp::_,1).begin(), xybd(Rcpp::_,1).end());
	std::vector<double> b(xybd(Rcpp::_,2).begin(), xybd(Rcpp::_,2).end());
	std::vector<double> d(xybd(Rcpp::_,3).begin(), xybd(Rcpp::_,3).end());

    std::vector<std::vector<double> > res;	
	if (lonlat) {
		res = destpoint_lonlat(x, y, b, d, a, f);
	} else {
		res = destpoint_plane(x, y, b, d);		
	}
	int n = res.size();
	int m = res[0].size();
	
	Rcpp::NumericMatrix r(n, m);
	for (int i=0; i < n; i++) {
		for (int j=0; j < m; j++) {
			r(i,j) = res[i][j];
		}
	}	
	return(r);	
}

