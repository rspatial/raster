/* Robert Hijmans, June 2011, July 2016

*/
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
#include <vector>
#include "spat.h"
  
std::vector<double> rasterize_polygon(std::vector<double> r, double value, std::vector<double> pX, std::vector<double> pY, unsigned nrows, unsigned ncols, double xmin, double ymax, double rx, double ry) {

	unsigned n = pX.size();
	std::vector<unsigned> nCol(n);
  
	for (size_t row=0; row<nrows; row++) {
  
		double y = ymax - (row+0.5) * ry;
		
		//  Build a list of nodes.
		unsigned nodes = 0; 
		size_t j = n-1;
		for (size_t i=0; i<n; i++) {
			if (((pY[i] < y) && (pY[j] >= y)) || ((pY[j] < y) && (pY[i] >= y))) {
				nCol[nodes++]=(int)  (((pX[i] - xmin + (y-pY[i])/(pY[j]-pY[i]) * (pX[j]-pX[i])) + 0.5 * rx ) / rx); 
			}
			j = i; 
		}
		
		std::sort(nCol.begin(), nCol.begin()+nodes);
		
		//  Fill the pixels between node pairs.
		for (size_t i=0; i < nodes; i+=2) {
			if (nCol[i] >= ncols) break;
			if (nCol[i+1] > 0) {
				if (nCol[i] < 0) nCol[i]=0 ;
				if (nCol[i+1] > ncols) nCol[i+1] = ncols;
				int ncell = ncols * row;
				for (size_t col = nCol[i]; col < nCol[i+1]; col++) {
					r[col + ncell] = value;
				}
			}
		}
	}
	return(r);
}  




std::vector<double> SpatPolygons::rasterize(double resx, double resy, unsigned nrow, unsigned ncol, std::vector<double> extent, std::vector<double> values, double background) {

	unsigned n = size();
	
	std::vector<double> v(nrow *ncol, background);
	
	for (size_t j = 0; j < n; j++) {
			
			SpatPoly poly = getPoly(j);
			double value = values[j];
			
			unsigned np = poly.size();
			for (size_t k = 0; k < np; k++) {
				SpatPolyPart part = poly.getPart(k);
				
				if (part.hasHoles()) {
					std::vector<double> vv = rasterize_polygon(v, value, part.x, part.y, nrow, ncol, extent[0], extent[3], resx, resy);
					for (size_t h=0; h < part.nHoles(); h++) {
						vv = rasterize_polygon(vv, background, part.xHole[h], part.yHole[h], nrow, ncol, extent[0], extent[3], resx, resy);
					}
					for (size_t q=0; q < vv.size(); q++) {
						if (vv[q] != background) {
							v[q] = vv[q];
						}
					}
				} else {
					v = rasterize_polygon(v, value, part.x, part.y, nrow, ncol, extent[0], extent[3], resx, resy);
				}
			}
	}
	
	return(v);

}


