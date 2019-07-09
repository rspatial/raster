
#include <Rcpp.h>
#include "util.h"

#include <random>


double dmod(double x, double n) {
	return(x - n * floor(x/n));
}


double distPlane(double x1, double y1, double x2, double y2) {
	return( sqrt(pow((x2-x1),2) + pow((y2-y1), 2)) );
}

double distHav(double lon1, double lat1, double lon2, double lat2, double r) {

	double dLat, dLon, a;

	lon1 = toRad(lon1);
	lon2 = toRad(lon2);
	lat1 = toRad(lat1);
	lat2 = toRad(lat2);

	dLat = lat2-lat1;
	dLon = lon2-lon1;
	a = sin(dLat/2.) * sin(dLat/2.) + cos(lat1) * cos(lat2) * sin(dLon/2.) * sin(dLon/2.);
	return 2. * atan2(sqrt(a), sqrt(1.-a)) * r;
}


// [[Rcpp::export(name = ".terrain")]]
std::vector<double> do_terrains(std::vector<double> d, std::vector<int> dim, std::vector<double> res, int unit, std::vector<bool> option, bool geo, std::vector<double> gy) {
					
	double zy, zx; 
	
	size_t nrow = dim[0];
	size_t ncol = dim[1];
	size_t n = nrow * ncol;
	
	double dx = res[0];
	double dy = res[1];

	int nopt = 0;
	for (size_t i =0; i<8; i++) {
		nopt += option[i];
	}

	std::vector<double> ddx;
	if (geo) {
		double r = 6378137;
		ddx.resize(nrow);	
		for (size_t i=0; i<nrow; i++) {
			ddx[i] = distHav(-dx, gy[i], dx, gy[i], r) / 2 ;
		}
	} 

	std::vector<double> val(n*nopt);

	size_t add=0;
	int addn=0;
	
	if (option[0]) {  
	// terrain ruggedness
		for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
			val[i] = (fabs(d[i-1-ncol]-d[i]) + fabs(d[i-1]-d[i]) + fabs(d[i-1+ncol]-d[i]) +  fabs(d[i-ncol]-d[i]) +
				fabs(d[i+ncol]-d[i]) +  fabs(d[i+1-ncol]-d[i]) + fabs(d[i+1]-d[i]) +  fabs(d[i+1+ncol]-d[i])) / 8;
		}
		add++;
	} 
	if (option[1]) {
		addn = add * n;
	// topograhic position
		for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
			val[i+addn] = d[i] - (d[i-1-ncol] + d[i-1] + d[i-1+ncol] + d[i-ncol]
			+ d[i+ncol] + d[i+1-ncol] + d[i+1] + d[i+1+ncol]) / 8;
		}
		add++;
	} 
	if (option[2]) {
	// roughness 
		addn = add * n;
		int incol = ncol;
		int a[9] = { -1-incol, -1, -1+incol, -incol, 0, incol, 1-incol, 1, 1+incol };
		double min, max, v;
		for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
			min = d[i + a[0]];
			max = d[i + a[0]];
			for (size_t j = 1; j < 9; j++) {
				v = d[i + a[j]]; 
				if (v > max) {
					max = v;
				} else if (v < min) {
					min = v;
				}
			}
			val[i+addn] = max - min;
		}
		add++;
	} 
	

	if (option[3]) {
	// slope 4 neighbors	
		addn = add * n;
		if (geo) {
			int q;
			double xwi[2] = {-1,1};
			double xw[2] = {0,0};
			double yw[2] = {-1,1};

			
			for (size_t i=0; i<2; i++) {
				yw[i] = yw[i] / (2 * dy);
			}			
			for (size_t i = ncol; i < (ncol * (nrow-1)-1); i++) {
				if (i % ncol == 0) {
					q = i / ncol;
					for (size_t k=0; k<2; k++) {
						xw[k] = xwi[k] / (-2 * ddx[q]);
					}
				}
				zx = d[i-1] * xw[0] + d[i+1] * xw[1];
				zy = d[i-ncol] * yw[0] + d[i+ncol] * yw[1];
				val[i+addn] = sqrt( pow(zy, 2) + pow(zx, 2) ) ;
			}
			
	
		} else {
		
			double xw[2] = {-1,1};
			double yw[2] = {-1,1};
			for (size_t i=0; i<2; i++) {
				xw[i] = xw[i] / (-2 * dx);
				yw[i] = yw[i] / (2 * dy);
			}
			for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
				zx = d[i-1] * xw[0] + d[i+1] * xw[1];
				zy = d[i-ncol] * yw[0] + d[i+ncol] * yw[1];
				val[i+addn] = sqrt( pow(zy, 2) + pow(zx, 2)  );
			}
		}

		if (unit == 0) {
			double adj = 180 / M_PI;
			for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
				val[i+addn] = atan(val[i+addn]) * adj;
			}
		} else if (unit == 1) {
			for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
				val[i+addn] = atan(val[i+addn]);
			}
		} 
		
		
		add++;		
	} 


	if (option[4]) {
	// aspect 4 neighbors	
		addn = add * n;

		if (geo) {
			int q;
			double xwi[2] = {-1,1};
			double xw[2] = {0,0};
			double yw[2] = {-1,1};

			
			for (size_t i=0; i<2; i++) {
				yw[i] = yw[i] / (2 * dy);
			}			
			for (size_t i = ncol; i < (ncol * (nrow-1)-1); i++) {
				if (i % ncol == 0) {
					q = i / ncol;
					for (size_t k=0; k<2; k++) {
						xw[k] = xwi[k] / (-2 * ddx[q]);
					}
				}
				zx = d[i-1] * xw[0] + d[i+1] * xw[1];
				zy = d[i-ncol] * yw[0] + d[i+ncol] * yw[1];
				zx = atan2(zy, zx);
				val[i+addn] = dmod( M_PI_2 - zx, M_2PI);
			}
			if (unit == 0) {
				double adj = 180 / M_PI;
				for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
					val[i+addn] = val[i+addn] * adj;
				}
			}				
	
		
		} else {
	
			double xw[2] = {-1,1};
			double yw[2] = {-1,1};
			for (size_t i=0; i<2; i++) {
				xw[i] = xw[i] / (-2 * dx);
				yw[i] = yw[i] / (2 * dy);
			}
			for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
				zx = d[i-1] * xw[0] + d[i+1] * xw[1];
				zy = d[i-ncol] * yw[0] + d[i+ncol] * yw[1];
				zx = atan2(zy, zx);
				val[i+addn] = dmod( M_PI_2 -zx, M_2PI);
			}
			if (unit == 0) {
				double adj = 180 / M_PI;
				for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
					val[i+addn] = val[i+addn] * adj;
				}
			}
		}
		
		add++;
	
	} 
	
	
	if (option[5]) {
	// slope 8 neighbors	
		addn = add * n;
		if (geo) {
			int q;
			double xwi[6] = {-1,-2,-1,1,2,1};
			double xw[6] = {0,0,0,0,0,0};
			double yw[6] = {-1,1,-2,2,-1,1};
			
			for (size_t i=0; i<6; i++) {
				yw[i] = yw[i] / (8 * dy);
			}
						
			for (size_t i = ncol; i < (ncol * (nrow-1)-1); i++) {
				if (i % ncol == 0) {
					q = i / ncol;
					for (size_t k=0; k<6; k++) {
						xw[k] = xwi[k] / (8 * ddx[q]);
					}
				}
				zx = d[i-1-ncol] * xw[0] + d[i-1] * xw[1] + d[i-1+ncol] * xw[2]
						+ d[i+1-ncol] * xw[3] + d[i+1] * xw[4] + d[i+1+ncol] * xw[5];
				zy = d[i-1-ncol] * yw[0] + d[i-1+ncol] * yw[1] + d[i-ncol] * yw[2] 
						+ d[i+ncol] * yw[3] + d[i+1-ncol] * yw[4] + d[i+1+ncol] * yw[5];
				val[i+addn] = sqrt( pow(zy, 2) + pow(zx, 2)  );
								
			}
			
		} else {
		
			double xw[6] = {-1,-2,-1,1,2,1};
			double yw[6] = {-1,1,-2,2,-1,1};
			for (size_t i=0; i<6; i++) {
				xw[i] = xw[i] / (-8 * dx);
				yw[i] = yw[i] / (8 * dy);
			}
			for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
				zx = d[i-1-ncol] * xw[0] + d[i-1] * xw[1] + d[i-1+ncol] * xw[2]
						+ d[i+1-ncol] * xw[3] + d[i+1] * xw[4] + d[i+1+ncol] * xw[5];
				zy = d[i-1-ncol] * yw[0] + d[i-1+ncol] * yw[1] + d[i-ncol] * yw[2] 
						+ d[i+ncol] * yw[3] + d[i+1-ncol] * yw[4] + d[i+1+ncol] * yw[5];
				val[i+addn] = sqrt( pow(zy, 2) + pow(zx, 2) );

			}
		}

		if (unit == 0) {
			double adj = 180 / M_PI;
			for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
				val[i+addn] = atan(val[i+addn]) * adj;
			}
		} else if (unit == 1) {
			for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
				val[i+addn] = atan(val[i+addn]);
			}
		} 
		
		add++;
		
	} 	
	
	if (option[6]) {
	// aspect 8 neighbors	
		addn = add * n;
	
		if (geo) {
			int q;
			double xwi[6] = {-1,-2,-1,1,2,1};
			double xw[6] = {0,0,0,0,0,0};
			double yw[6] = {-1,1,-2,2,-1,1};
			
			for (size_t i=0; i<6; i++) {
				yw[i] = yw[i] / (8 * dy);
			}
						
			for (size_t i = ncol; i < (ncol * (nrow-1)-1); i++) {
				if (i % ncol == 0) {
					q = i / ncol;
					for (size_t k=0; k<6; k++) {
						xw[k] = xwi[k] / (-8 * ddx[q]);
					}
				}
				zx = d[i-1-ncol] * xw[0] + d[i-1] * xw[1] + d[i-1+ncol] * xw[2]
						+ d[i+1-ncol] * xw[3] + d[i+1] * xw[4] + d[i+1+ncol] * xw[5];
				zy = d[i-1-ncol] * yw[0] + d[i-1+ncol] * yw[1] + d[i-ncol] * yw[2] 
						+ d[i+ncol] * yw[3] + d[i+1-ncol] * yw[4] + d[i+1+ncol] * yw[5];
				zx = atan2(zy, zx);
				val[i+addn] = dmod( M_PI_2 -zx, M_2PI);
			}
			if (unit == 0) {
				double adj = 180 / M_PI;
				for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
					val[i+addn] = val[i+addn] * adj;
				}
			}
		
		} else {
	
			double xw[6] = {-1,-2,-1,1,2,1};
			double yw[6] = {-1,1,-2,2,-1,1};
			for (size_t i=0; i<6; i++) {
				xw[i] = xw[i] / (-8 * dx);
				yw[i] = yw[i] / (8 * dy);
			}
			for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
				zx = d[i-1-ncol] * xw[0] + d[i-1] * xw[1] + d[i-1+ncol] * xw[2]
						+ d[i+1-ncol] * xw[3] + d[i+1] * xw[4] + d[i+1+ncol] * xw[5];
				zy = d[i-1-ncol] * yw[0] + d[i-1+ncol] * yw[1] + d[i-ncol] * yw[2] 
						+ d[i+ncol] * yw[3] + d[i+1-ncol] * yw[4] + d[i+1+ncol] * yw[5];
				zx = atan2(zy, zx);
				val[i+addn] = dmod( M_PI_2 -zx, M_2PI);
			}
			if (unit == 0) {
				double adj = 180 / M_PI;
				for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
					val[i+addn] = val[i+addn] * adj;
				}
			}
			
		}
		
		add++;
		
	} if (option[7]) { 
	  // flow direction

		std::default_random_engine generator(std::random_device{}());
		//generator.seed(seed);
		std::uniform_int_distribution<> distrib(0, 1);
	
		//auto gen = std::bind(std::uniform_int_distribution<>(0,1),std::default_random_engine());

		addn = add * n;
		double r[8] = {0,0,0,0,0,0,0,0};
		double p[8] = {1,2,4,8,16,32,64,128}; // pow(2, j)
		double dxy = sqrt(dx * dx + dy * dy);
		for (size_t i = ncol+1; i < (ncol * (nrow-1)-1); i++) {
			if (std::isnan(d[i])) {
				val[i+addn] = NAN;
			} else {
				r[0] = (d[i] - d[i+1]) / dx;
				r[1] = (d[i] - d[i+1+ncol]) / dxy;
				r[2] = (d[i] - d[i+ncol]) / dy;
				r[3] = (d[i] - d[i-1+ncol]) / dxy;
				r[4] = (d[i] - d[i-1]) / dx;
				r[5] = (d[i] - d[i-1-ncol]) / dxy;
				r[6] = (d[i] - d[i-ncol]) / dy;
				r[7] = (d[i] - d[i+1-ncol]) / dxy;
				// using the lowest neighbor, even if it is higher than the focal cell.
				double dmin = r[0];
				int k = 0;
				for (size_t j=1; j<8; j++) {
					if (r[j] > dmin) {
						dmin = r[j];
						k = j;
					} else if (r[j] == dmin) {
						bool b = distrib(generator);
						if (b) {
							dmin = r[j];
							k = j;
						}
					}
				}
				val[i+addn] = p[k];
			}
		}
		add++;
	}
	
// Set edges to NA	
// first row	
	for (size_t j=0; j<add; j++) {
	    int jn = j * n;
		for (size_t i = 0; i < ncol; i++) {  
			val[i+jn] = NAN;
		}
	// last row	
		for (size_t i = ncol * (nrow-1); i < n; i++) {  
			val[i+jn] = NAN; 
		}
	// first and last columns
		for (size_t i = 1; i < nrow; i++) {  
			val[i * ncol + jn] = NAN;
			val[i * ncol - 1 + jn] = NAN;
		}
	}

//	free(ddx);
//	UNPROTECT(4);
	return(val);
}


