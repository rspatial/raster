/* Robert Hijmans, October 2011 
This is an implementation of J. Ronald Eastman's pushbroom algorithm
*/

#include <limits>
#include <Rcpp.h>

#define min( a, b ) ( ((a) < (b)) ? (a) : (b) )

// [[Rcpp::export(name = ".broom")]]
std::vector<double> broom(std::vector<double> d, std::vector<double> f, std::vector<double> dm, std::vector<double> dist, bool down) {

	double dx = dist[0];
	double dy = dist[1];
	double dxy = dist[2];
	int leftright = 2; //INTEGER(lr)[0];
	size_t nr = dm[0];
	size_t nc = dm[1];
	size_t n = nr * nc;
//	Rprintf ("n = %i \n", n);

	std::vector<double> dis(n);
	
	for (size_t i=0; i<n; i++) {
		dis[i] = std::numeric_limits<double>::infinity();
	}

	if (down) {	
		//left to right
		//r = 0; first row, no row above it, use 'f'
		
		if (leftright >= 1) {
		
			//i = 0; first cell, no cell left of it
			if ( std::isnan(d[0])) {
				dis[0] = f[0] + dy;
			} else {
				dis[0] = 0;
			}
			// other cells
			for (size_t i=1; i<nc; i++) {
				if (std::isnan(d[i])) {
					dis[i] = min(min(f[i] + dy, f[i-1] + dxy), dis[i-1] + dx);
				} else {
					dis[i] = 0;
				}
			}
			//other rows	
			for (size_t r=1; r<nr; r++) {
				size_t i = r*nc;
				if (std::isnan(d[i])) {
					dis[i] = dis[i-nc] + dy;
				} else {
					dis[i] = 0;
				}
				for (size_t i=r*nc+1; i<((r+1)*nc); i++) {
					if (std::isnan(d[i])) {
						dis[i] = min(min(dis[i-1] + dx, dis[i-nc] + dy), dis[i-nc-1] + dxy);
					} else {
						dis[i] = 0;
					}
				}
			}

		}
	//right to left
		//first row
			//first cell

		
		if ((leftright == 0) | (leftright > 1)) {

			if ( std::isnan(d[nc-1])) {
				dis[nc-1] = min(dis[nc-1], f[nc-1] + dy);
			} else {
				dis[nc-1] = 0;	
			}
			
				// other cells
			for (int i=(nc-2); i > -1; i--) {
				if (std::isnan(d[i])) {
					dis[i] = min(min(min(dis[i], f[i] + dy), f[i+1] + dxy), dis[i+1] + dx);
				} else {
					dis[i] = 0;
				}
			}
			// other rows
			for (size_t r=1; r<nr; r++) {
				size_t i=(r+1)*nc-1;
				if (std::isnan(d[i])) {
					dis[i] = min(dis[i], dis[i-nc] + dy);
				} else {
					dis[i] = 0;
				}
				for (size_t i=(r+1)*nc-2; i>(r*nc-1); i--) {
					if (std::isnan(d[i])) {
						dis[i] = min(min(min(dis[i], dis[i+1] + dx), dis[i-nc] + dy), dis[i-nc+1] + dxy);		
					} else {
						dis[i] = 0;
					}
				}
			}
		
		}
	
	} else { 
	// bottom to top
		// left to right
		// first (last) row
		if (leftright >= 1) {
		
			size_t r = nr-1;
			// first cell
			size_t i = r*nc;
			if (std::isnan(d[i])) {
				dis[i] = min(dis[i], f[0] + dy);
			} else {
				dis[i] = 0;
			}
			// other cells
			for (size_t i=(r*nc+1); i<n; i++) {
				if (std::isnan(d[i])) {
					size_t j = i - r*nc;
					dis[i] = min(min(min(dis[i], f[j] + dy), f[j-1] + dxy),  dis[i-1] + dx);
				} else {
					dis[i] = 0;
				}
			}
			// other rows
			for (size_t r=nr-2; r >= 0; r--) {
				i=r*nc;
				if (std::isnan(d[i])) {
					dis[i] = min(dis[i], dis[i+nc] + dy);
				}  else {
					dis[i] = 0;
				}
				for (size_t i=(r*nc+1); i<((r+1)*nc); i++) {
					if (std::isnan(d[i])) {
						dis[i] = min(min(min(dis[i], dis[i-1] + dx), dis[i+nc] + dy), dis[i+nc-1] + dxy);
					} else {
						dis[i] = 0;
					}
				}
			}
		} 
		
		if ((leftright == 0) | (leftright > 1)) {

			// right to left
			// first row
			// first cell
			if (std::isnan(d[n-1])) {
				dis[n-1] = min(dis[n-1], f[nc-1] + dy);
			} else {
				dis[n-1] = 0;
			}
			// other cells
			size_t r = nr-1;
			for (size_t i=n-2; i > (r*nc-1); i--) {
				if (std::isnan(d[i])) {
					size_t j = i - r*nc;
					dis[i] = min(min(min(dis[i], f[j] + dx), f[j+1] + dxy), dis[i+1] + dx);
				} else {
					dis[i] = 0;
				}
			}
			// other rows
			for (size_t r=nr-2; r >= 0; r--) {
				size_t i = (r+1)*nc-1;
				if (std::isnan(d[i])) {
					dis[i] = min(dis[i], dis[i+nc] + dy);
				} else {
					dis[i] = 0;
				}

				for (size_t i=(r+1)*nc-2; i>(r*nc-1); i--) {
					if (std::isnan(d[i])) {
						dis[i] = min(min(min(dis[i], dis[i+1] + dx), dis[i+nc] + dy), dis[i+nc+1] + dxy);
					} else {
						dis[i] = 0;
					}
				}
			}
		}
	}
	return(dis);
}

