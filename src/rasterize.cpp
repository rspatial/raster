/* Robert Hijmans, June 2011, July 2016
// Based on  public-domain code by Darel Rex Finley, 2007
// http://alienryderflex.com/polygon_fill/

*/

#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
#include <vector>
#include "spat.h"
  
std::vector<double> rasterize_polygon(std::vector<double> r, double value, std::vector<double> pX, std::vector<double> pY, unsigned nrows, unsigned ncols, double xmin, double ymax, double rx, double ry) {

	size_t n = pX.size();
	std::vector<unsigned> nCol(n);
  
	for (size_t row=0; row<nrows; row++) {
 
		double y = ymax - ((double)row+0.5) * ry;		
		//  Get nodes.
		unsigned nodes = 0; 
		size_t j = n-1;
		for (size_t i=0; i<n; i++) {
			if (((pY[i] < y) && (pY[j] >= y)) || ((pY[j] < y) && (pY[i] >= y))) {
				double nds = ((pX[i] - xmin + (y-pY[i])/(pY[j]-pY[i]) * (pX[j]-pX[i])) + 0.5 * rx ) / rx; 
				nds = nds < 0 ? 0 : nds;
		        nds = nds > ncols ? ncols : nds;
				nCol[nodes] = (unsigned) nds;
				nodes++;
			}
			j = i; 
		}
		
		std::sort(nCol.begin(), nCol.begin()+nodes);
		size_t ncell = ncols * row;
		
		//  Fill the cells between node pairs.
		for (size_t i=0; i < nodes; i+=2) {
			if (nCol[i+1] > 0 && nCol[i] < ncols) {
			//if (nCol[i] >= ncols || nCol[i+1] <= 0) break;
				for (size_t col = nCol[i]; col < nCol[i+1]; col++) {
					r[col + ncell] = value;
				}
			}
		}
	}
	return(r);
}  




std::vector<double> SpPolygons::rasterize(unsigned nrow, unsigned ncol, std::vector<double> extent, std::vector<double> values, double background) {

	size_t n = size();
	
	std::vector<double> v(nrow*ncol, background);
	
	double resx = (extent[1] - extent[0]) / ncol;
	double resy = (extent[3] - extent[2]) / nrow;
	
	for (size_t j = 0; j < n; j++) {
			
		SpPoly poly = getPoly((unsigned)j);
		double value = values[j];		
		size_t np = poly.size();
		for (size_t k = 0; k < np; k++) {
			SpPolyPart part = poly.getPart((unsigned)k);
			
			if (part.hasHoles()) {
				std::vector<double> vv = rasterize_polygon(v, value, part.x, part.y, nrow, ncol, extent[0], extent[3], resx, resy);
				for (size_t h=0; h < part.nHoles(); h++) {
					vv = rasterize_polygon(vv, background, part.xHole[h], part.yHole[h], nrow, ncol, extent[0], extent[3], resx, resy);
				}
				for (size_t q=0; q < vv.size(); q++) {
					if ((vv[q] != background) && (!std::isnan(vv[q]))) {
					//if (vv[q] != background) {
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


