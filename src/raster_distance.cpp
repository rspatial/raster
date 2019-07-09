/* Robert Hijmans, October 2011 
July 2016
*/

#include <Rcpp.h>
using namespace Rcpp;

#include "distance.h"
#include "area.h"


// [[Rcpp::export(name = ".get_area_polygon")]]
NumericVector get_area_polygon(NumericMatrix d, bool lonlat) {
	std::vector<int> pols(d(_,0).begin(), d(_,0).end());
	std::vector<int> parts(d(_,1).begin(), d(_,1).end());
	std::vector<int> holes(d(_,3).begin(), d(_,3).end());
	std::vector<double> x(d(_,4).begin(), d(_,4).end());
	std::vector<double> y(d(_,5).begin(), d(_,5).end());
	std::vector<double> out;
	
	if (lonlat) { // wgs84
		double a = 6378137;
		double f = 1/298.257223563;
		out = area_polygon_lonlat(x, y, pols, parts, holes, a, f);
	} else {
		out = area_polygon_plane(x, y, pols, parts, holes);
	}
	
	NumericVector r( out.begin(), out.end() );
	return( r );		
}


// [[Rcpp::export(name = ".point_distance")]]
NumericVector point_distance(NumericMatrix p1, NumericMatrix p2, bool lonlat, double a, double f) {
	std::vector<double> px1(p1(_,0).begin(), p1(_,0).end());
	std::vector<double> py1(p1(_,1).begin(), p1(_,1).end());
	std::vector<double> px2(p2(_,0).begin(), p2(_,0).end());
	std::vector<double> py2(p2(_,1).begin(), p2(_,1).end());
    NumericVector res;	

	if (lonlat) {
		res = distance_lonlat(px1, py1, px2, py2, a, f);
	} else {
		res = distance_plane(px1, py1, px2, py2);
	}
	
	return(res);
}


// [[Rcpp::export(name = ".distanceToNearestPoint")]]
NumericVector distanceToNearestPoint(NumericMatrix d, NumericMatrix p, bool lonlat, double a, double f) {
	
	std::vector<double> dx(d(_,0).begin(), d(_,0).end());
	std::vector<double> dy(d(_,1).begin(), d(_,1).end());
	std::vector<double> px(p(_,0).begin(), p(_,0).end());
	std::vector<double> py(p(_,1).begin(), p(_,1).end());
	
    NumericVector res;	

	if (lonlat) {
		res = distanceToNearest_lonlat(dx, dy, px, py, a, f);
	} else {
		res = distanceToNearest_plane(dx, dy, px, py);
	}
	return(res) ;
}


// [[Rcpp::export(name = ".directionToNearestPoint")]]
NumericVector directionToNearestPoint(NumericMatrix d, NumericMatrix p, bool lonlat, bool degrees, bool from, double a, double f) {

	std::vector<double> dx(d(_,0).begin(), d(_,0).end());
	std::vector<double> dy(d(_,1).begin(), d(_,1).end());
	std::vector<double> px(p(_,0).begin(), p(_,0).end());
	std::vector<double> py(p(_,1).begin(), p(_,1).end());

    NumericVector res;	
	if (lonlat) {
		res = directionToNearest_lonlat(dx, dy, px, py, degrees, from, a, f);
	} else {
		res = directionToNearest_plane(dx, dy, px, py, degrees, from);
	}
	return(res) ;
}




// [[Rcpp::export(name = ".dest_point")]]
NumericMatrix dest_point(NumericMatrix xybd, bool lonlat, double a, double f) {

	std::vector<double> x(xybd(_,0).begin(), xybd(_,0).end());
	std::vector<double> y(xybd(_,1).begin(), xybd(_,1).end());
	std::vector<double> b(xybd(_,2).begin(), xybd(_,2).end());
	std::vector<double> d(xybd(_,3).begin(), xybd(_,3).end());

    std::vector<std::vector<double> > res;	
	if (lonlat) {
		res = destpoint_lonlat(x, y, b, d, a, f);
	} else {
		res = destpoint_plane(x, y, b, d);		
	}
	int n = res.size();
	int m = res[0].size();
	
	NumericMatrix r(n, m);
	for (int i=0; i < n; i++) {
		for (int j=0; j < m; j++) {
			r(i,j) = res[i][j];
		}
	}	
	return(r);	
}
